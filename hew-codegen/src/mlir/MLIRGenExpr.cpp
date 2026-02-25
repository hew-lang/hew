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

// ============================================================================
// Expression generation
// ============================================================================

mlir::Value MLIRGen::generateExpression(const ast::Expr &expr) {
  currentLoc = loc(expr.span);

  if (auto *lit = std::get_if<ast::ExprLiteral>(&expr.kind))
    return generateLiteral(lit->lit);

  if (auto *ident = std::get_if<ast::ExprIdentifier>(&expr.kind)) {
    auto name = ident->name;
    // Check regular variables first
    auto val = lookupVariable(name);
    if (val)
      return val;

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
        else
          optionType = hew::OptionEnumType::get(&context, builder.getI32Type());
        mlir::Value result = builder.create<hew::EnumConstructOp>(
            location, optionType, static_cast<int32_t>(variantIndex),
            builder.getStringAttr("Option"), mlir::ValueRange{},
            /*payload_positions=*/nullptr);
        return result;
      }

      auto enumIt = enumTypes.find(enumName);
      if (enumIt != enumTypes.end()) {
        const auto &enumInfo = enumIt->second;

        if (enumInfo.hasPayloads) {
          // Unit variant of a payload enum: build struct { tag, undef... }
          auto location = currentLoc;
          mlir::Value result = builder.create<hew::EnumConstructOp>(
              location, enumInfo.mlirType, static_cast<int32_t>(variantIndex),
              builder.getStringAttr(enumName), mlir::ValueRange{},
              /*payload_positions=*/nullptr);
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
      return builder.create<mlir::func::ConstantOp>(currentLoc, funcOp.getFunctionType(),
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
    return generateIfExpr(*ifE);
  if (auto *blockExpr = std::get_if<ast::ExprBlock>(&expr.kind))
    return generateBlockExpr(blockExpr->block);
  if (auto *pf = std::get_if<ast::ExprPostfixTry>(&expr.kind))
    return generatePostfixExpr(*pf);
  if (auto *me = std::get_if<ast::ExprMatch>(&expr.kind))
    return generateMatchExpr(*me, expr.span);
  if (auto *se = std::get_if<ast::ExprScope>(&expr.kind))
    return generateScopeExpr(*se);
  if (auto *sle = std::get_if<ast::ExprScopeLaunch>(&expr.kind))
    return generateScopeLaunchExpr(*sle);
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
    return generateStructInit(*si);
  if (auto *mc = std::get_if<ast::ExprMethodCall>(&expr.kind))
    return generateMethodCall(*mc);
  if (auto *tup = std::get_if<ast::ExprTuple>(&expr.kind))
    return generateTupleExpr(*tup);
  if (auto *arr = std::get_if<ast::ExprArray>(&expr.kind))
    return generateArrayExpr(*arr);
  if (auto *lam = std::get_if<ast::ExprLambda>(&expr.kind))
    return generateLambdaExpr(*lam);
  if (auto *interp = std::get_if<ast::ExprInterpolatedString>(&expr.kind))
    return generateInterpolatedString(*interp);
  if (auto *regex = std::get_if<ast::ExprRegexLiteral>(&expr.kind))
    return generateRegexLiteral(*regex);
  if (std::get_if<ast::ExprTimeout>(&expr.kind)) {
    emitError(currentLoc) << "timeout expressions are not yet supported in codegen";
    return nullptr;
  }

  if (auto *ue = std::get_if<ast::ExprUnsafe>(&expr.kind)) {
    emitWarning(currentLoc)
        << "unsafe block treated as regular block (safety checks not yet enforced)";
    return generateBlock(ue->block);
  }

  if (auto *yield = std::get_if<ast::ExprYield>(&expr.kind)) {
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
          auto one = builder.create<mlir::arith::ConstantIntOp>(yieldLocation, i64Ty, 1);
          auto valAlloca = builder.create<mlir::LLVM::AllocaOp>(yieldLocation, ptrTy, valType, one);
          builder.create<mlir::LLVM::StoreOp>(yieldLocation, yieldVal, valAlloca);

          // Compute sizeof(valType)
          auto valSize = builder.create<hew::SizeOfOp>(yieldLocation, sizeType(),
                                                       mlir::TypeAttr::get(valType));

          // Call hew_gen_yield(ctx, &val, size) -> bool (i1)
          auto i1Ty = builder.getI1Type();
          builder.create<hew::GenYieldOp>(yieldLocation, i1Ty, currentGenCtx, valAlloca, valSize);
          // For now, ignore the return value (cancellation not handled)
          return yieldVal; // Return the yielded value (unused by caller)
        }
      }
    }
    // Yield expressions outside generator context are handled at the
    // statement level during static generator codegen.
    if (!currentGenCtx) {
      emitWarning(currentLoc) << "yield expression outside generator function";
    }
    return nullptr;
  }

  if (std::get_if<ast::ExprCooperate>(&expr.kind)) {
    // Cooperative scheduler yield point
    builder.create<hew::CooperateOp>(currentLoc);
    return createIntConstant(builder, currentLoc, builder.getI32Type(), 0);
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
            return builder
                .create<hew::RuntimeCallOp>(
                    location, mlir::TypeRange{strRefType},
                    mlir::SymbolRefAttr::get(&context, "hew_http_request_path"),
                    mlir::ValueRange{operandVal})
                .getResult();
          }
          if (fieldName == "method") {
            return builder
                .create<hew::RuntimeCallOp>(
                    location, mlir::TypeRange{strRefType},
                    mlir::SymbolRefAttr::get(&context, "hew_http_request_method"),
                    mlir::ValueRange{operandVal})
                .getResult();
          }
        }
      }

      // When accessing self.field, use currentActorName for precise lookup
      std::string targetStructName;
      if (!currentActorName.empty()) {
        if (auto *selfIdent = std::get_if<ast::ExprIdentifier>(&fa->object->value.kind)) {
          if (selfIdent->name == "self")
            targetStructName = currentActorName;
        }
      }

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
            auto fieldPtr = builder.create<mlir::LLVM::GEPOp>(
                location, ptrType, structType, operandVal,
                llvm::ArrayRef<mlir::LLVM::GEPArg>{0, static_cast<int32_t>(field.index)});
            auto fieldVal =
                builder.create<mlir::LLVM::LoadOp>(location, field.type, fieldPtr).getResult();
            if ((mlir::isa<hew::VecType>(field.semanticType) ||
                 mlir::isa<hew::HashMapType>(field.semanticType)) &&
                field.semanticType != field.type)
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
      return builder.create<hew::TupleExtractOp>(location, elemTypes[numericIdx], operandVal,
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
      return builder.create<mlir::LLVM::ExtractValueOp>(location, operandVal, numericIdx);
    }

    // Named field: look up struct info by type name
    if (!structType.isIdentified()) {
      emitError(location) << "named field access on anonymous struct type";
      return nullptr;
    }
    llvm::StringRef structName = structType.getName();
    auto it = structTypes.find(structName.str());
    if (it == structTypes.end()) {
      emitError(location) << "unknown struct type '" << structName << "'";
      return nullptr;
    }

    const auto &info = it->second;

    // Find the field
    for (const auto &field : info.fields) {
      if (field.name == fieldName) {
        auto fieldVal = builder
                            .create<hew::FieldGetOp>(location, field.type, operandVal,
                                                     builder.getStringAttr(fieldName),
                                                     builder.getI64IntegerAttr(field.index))
                            .getResult();
        if ((mlir::isa<hew::VecType>(field.semanticType) ||
             mlir::isa<hew::HashMapType>(field.semanticType)) &&
            field.semanticType != field.type)
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
        return builder.create<hew::ArrayExtractOp>(location, hewArrayType.getElementType(),
                                                   operandVal, idxConst);
      }

      // Dynamic index -> cast to LLVM array, spill to alloca, GEP, load
      auto llvmArrayType =
          mlir::LLVM::LLVMArrayType::get(hewArrayType.getElementType(), hewArrayType.getSize());
      auto llvmArray = builder.create<hew::BitcastOp>(location, llvmArrayType, operandVal);
      auto alloca = builder.create<mlir::LLVM::AllocaOp>(
          location, mlir::LLVM::LLVMPointerType::get(&context), llvmArrayType,
          builder.create<mlir::arith::ConstantIntOp>(location, 1, 64));
      builder.create<mlir::LLVM::StoreOp>(location, llvmArray, alloca);
      auto i64Type = builder.getI64Type();
      mlir::Value idx64 = indexVal;
      if (indexVal.getType() != i64Type)
        idx64 = builder.create<mlir::arith::ExtSIOp>(location, i64Type, indexVal);
      auto zero = builder.create<mlir::arith::ConstantIntOp>(location, 0, 64);
      auto elemPtr =
          builder.create<mlir::LLVM::GEPOp>(location, mlir::LLVM::LLVMPointerType::get(&context),
                                            llvmArrayType, alloca, mlir::ValueRange{zero, idx64});
      return builder.create<mlir::LLVM::LoadOp>(location, hewArrayType.getElementType(), elemPtr);
    }

    auto arrayType = mlir::dyn_cast<mlir::LLVM::LLVMArrayType>(operandVal.getType());
    if (arrayType) {
      auto indexVal = generateExpression(idx->index->value);
      if (!indexVal)
        return nullptr;

      // Constant index -> extractvalue
      if (auto constOp = indexVal.getDefiningOp<mlir::arith::ConstantIntOp>()) {
        auto idxConst = constOp.value();
        return builder.create<mlir::LLVM::ExtractValueOp>(location, operandVal, idxConst);
      }

      // Dynamic index -> spill array to alloca, GEP, load
      auto alloca = builder.create<mlir::LLVM::AllocaOp>(
          location, mlir::LLVM::LLVMPointerType::get(&context), arrayType,
          builder.create<mlir::arith::ConstantIntOp>(location, 1, 64));
      builder.create<mlir::LLVM::StoreOp>(location, operandVal, alloca);
      auto i64Type = builder.getI64Type();
      mlir::Value idx64 = indexVal;
      if (indexVal.getType() != i64Type)
        idx64 = builder.create<mlir::arith::ExtSIOp>(location, i64Type, indexVal);
      auto zero = builder.create<mlir::arith::ConstantIntOp>(location, 0, 64);
      auto elemPtr =
          builder.create<mlir::LLVM::GEPOp>(location, mlir::LLVM::LLVMPointerType::get(&context),
                                            arrayType, alloca, mlir::ValueRange{zero, idx64});
      return builder.create<mlir::LLVM::LoadOp>(location, arrayType.getElementType(), elemPtr);
    }

    if (auto vecType = mlir::dyn_cast<hew::VecType>(operandVal.getType())) {
      auto indexVal = generateExpression(idx->index->value);
      if (!indexVal)
        return nullptr;
      auto i64Type = builder.getI64Type();
      mlir::Value idx64 = indexVal;
      if (indexVal.getType() != i64Type)
        idx64 = builder.create<mlir::arith::ExtSIOp>(location, i64Type, indexVal);
      return builder.create<hew::VecGetOp>(location, vecType.getElementType(), operandVal, idx64);
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
      std::string actorTypeName;
      if (auto *ie = std::get_if<ast::ExprIdentifier>(&mc->receiver->value.kind)) {
        auto it = actorVarTypes.find(ie->name);
        if (it != actorVarTypes.end())
          actorTypeName = it->second;
      }
      if (actorTypeName.empty()) {
        if (auto *faInner = std::get_if<ast::ExprFieldAccess>(&mc->receiver->value.kind)) {
          std::string baseName;
          if (auto *baseIdent = std::get_if<ast::ExprIdentifier>(&faInner->object->value.kind)) {
            if (baseIdent->name == "self" && !currentActorName.empty())
              baseName = currentActorName;
            else {
              auto baseIt = actorVarTypes.find(baseIdent->name);
              if (baseIt != actorVarTypes.end())
                baseName = baseIt->second;
            }
          }
          if (!baseName.empty()) {
            auto key = baseName + "." + faInner->field;
            auto aft = actorFieldTypes.find(key);
            if (aft != actorFieldTypes.end() && actorRegistry.count(aft->second))
              actorTypeName = aft->second;
          }
        }
      }

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
    // Not a method call — operand might be a task handle.
    auto operand = generateExpression(awaitE->inner->value);
    if (!operand)
      return nullptr;

    auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
    if (auto handleTy = mlir::dyn_cast<hew::HandleType>(operand.getType())) {
      if (handleTy.getHandleKind() == "Task")
        operand = builder.create<hew::BitcastOp>(location, ptrType, operand);
    }
    if (mlir::isa<mlir::LLVM::LLVMPointerType>(operand.getType())) {
      auto resultPtr = builder.create<hew::ScopeAwaitOp>(location, ptrType, operand);

      mlir::Type resultType = builder.getI32Type();
      if (auto *ie = std::get_if<ast::ExprIdentifier>(&awaitE->inner->value.kind)) {
        auto it = taskResultTypes.find(ie->name);
        if (it != taskResultTypes.end())
          resultType = it->second;
      }

      auto loadedResult = builder.create<mlir::LLVM::LoadOp>(location, resultType, resultPtr);
      return loadedResult;
    }
    return operand;
  }

  if (std::get_if<ast::ExprRange>(&expr.kind)) {
    emitWarning(currentLoc) << "range expression used outside of for loop";
    return nullptr;
  }

  emitWarning(currentLoc) << "unsupported expression kind";
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
            builder.create<hew::ConstantOp>(location, strRefType, builder.getStringAttr(symName)));
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
          auto str = builder.create<hew::ToStringOp>(location, strRefType, val);
          partValues.push_back(str);
          ownedTemps.push_back(str); // heap-allocated — we own this
        } else {
          emitWarning(location) << "unsupported type in string interpolation";
        }
      }
    }
  }

  // Empty interpolation -> return empty string
  if (partValues.empty()) {
    auto symName = getOrCreateGlobalString("");
    return builder.create<hew::ConstantOp>(location, strRefType, builder.getStringAttr(symName));
  }

  // Single part — no concatenation needed
  if (partValues.size() == 1) {
    auto result = partValues[0];
    for (auto it = ownedTemps.begin(); it != ownedTemps.end(); ++it) {
      if (*it == result) {
        ownedTemps.erase(it);
        break;
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
    result = builder.create<hew::StringConcatOp>(location, strRefType, result, partValues[j]);
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

mlir::Value MLIRGen::generateRegexLiteral(const ast::ExprRegexLiteral &regex) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto handleType = hew::HandleType::get(&context, "regex.Pattern");

  auto symName = getOrCreateGlobalString(regex.pattern);
  auto patternStr =
      builder.create<hew::ConstantOp>(location, ptrType, builder.getStringAttr(symName));

  return builder.create<hew::RegexNewOp>(location, handleType, patternStr);
}

// ============================================================================
// Literal generation
// ============================================================================

mlir::Value MLIRGen::generateLiteral(const ast::Literal &lit) {
  auto location = currentLoc;

  if (auto *intLit = std::get_if<ast::LitInteger>(&lit)) {
    auto type = defaultIntType();
    return createIntConstant(builder, location, type, intLit->value);
  }
  if (auto *floatLit = std::get_if<ast::LitFloat>(&lit)) {
    auto type = defaultFloatType();
    return builder.create<mlir::arith::ConstantOp>(location,
                                                   builder.getFloatAttr(type, floatLit->value));
  }
  if (auto *boolLit = std::get_if<ast::LitBool>(&lit)) {
    auto type = builder.getI1Type();
    return createIntConstant(builder, location, type, boolLit->value ? 1 : 0);
  }
  if (auto *strLit = std::get_if<ast::LitString>(&lit)) {
    auto symName = getOrCreateGlobalString(strLit->value);
    return builder.create<hew::ConstantOp>(location, hew::StringRefType::get(&context),
                                           builder.getStringAttr(symName));
  }
  if (auto *charLit = std::get_if<ast::LitChar>(&lit)) {
    return builder.create<mlir::arith::ConstantIntOp>(location, builder.getI32Type(),
                                                      static_cast<int64_t>(charLit->value));
  }
  if (auto *durLit = std::get_if<ast::LitDuration>(&lit)) {
    auto type = defaultIntType();
    return createIntConstant(builder, location, type, durLit->value);
  }
  emitWarning(location) << "unsupported literal kind";
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
    auto ifOp = builder.create<mlir::scf::IfOp>(location, i1Type, lhs,
                                                /*withElseRegion=*/true);

    builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
    auto rhs = generateExpression(expr.right->value);
    if (!rhs)
      rhs = createIntConstant(builder, location, i1Type, 0);
    builder.create<mlir::scf::YieldOp>(location, mlir::ValueRange{rhs});

    builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
    auto falseVal = createIntConstant(builder, location, i1Type, 0);
    builder.create<mlir::scf::YieldOp>(location, mlir::ValueRange{falseVal});

    builder.setInsertionPointAfter(ifOp);
    return ifOp.getResult(0);
  }

  if (expr.op == ast::BinaryOp::Or) {
    auto lhs = generateExpression(expr.left->value);
    if (!lhs)
      return nullptr;

    auto i1Type = builder.getI1Type();
    auto ifOp = builder.create<mlir::scf::IfOp>(location, i1Type, lhs,
                                                /*withElseRegion=*/true);

    builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
    auto trueVal = createIntConstant(builder, location, i1Type, 1);
    builder.create<mlir::scf::YieldOp>(location, mlir::ValueRange{trueVal});

    builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
    auto rhs = generateExpression(expr.right->value);
    if (!rhs)
      rhs = createIntConstant(builder, location, i1Type, 0);
    builder.create<mlir::scf::YieldOp>(location, mlir::ValueRange{rhs});

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
    rhs = coerceType(rhs, lhs.getType(), location);
  } else if (rhsIsFloat && !lhsIsFloat) {
    lhs = coerceType(lhs, rhs.getType(), location);
  }

  // Integer width promotion
  auto lhsInt = mlir::dyn_cast<mlir::IntegerType>(lhs.getType());
  auto rhsInt = mlir::dyn_cast<mlir::IntegerType>(rhs.getType());
  if (lhsInt && rhsInt && lhsInt.getWidth() != rhsInt.getWidth()) {
    if (lhsInt.getWidth() < rhsInt.getWidth()) {
      lhs = lhsInt.isUnsigned()
                ? builder.create<mlir::arith::ExtUIOp>(location, rhs.getType(), lhs).getResult()
                : builder.create<mlir::arith::ExtSIOp>(location, rhs.getType(), lhs).getResult();
    } else {
      rhs = rhsInt.isUnsigned()
                ? builder.create<mlir::arith::ExtUIOp>(location, lhs.getType(), rhs).getResult()
                : builder.create<mlir::arith::ExtSIOp>(location, lhs.getType(), rhs).getResult();
    }
  }

  auto type = lhs.getType();
  bool isFloat = llvm::isa<mlir::FloatType>(type);
  bool isPtr = isPointerLikeType(type);
  bool isUnsigned = false;
  if (auto intType = mlir::dyn_cast<mlir::IntegerType>(type))
    isUnsigned = intType.isUnsigned();

  switch (expr.op) {
  // Arithmetic
  case ast::BinaryOp::Add:
    if (isPtr) {
      return builder
          .create<hew::StringConcatOp>(location, hew::StringRefType::get(&context), lhs, rhs)
          .getResult();
    }
    return isFloat ? builder.create<mlir::arith::AddFOp>(location, lhs, rhs).getResult()
                   : builder.create<mlir::arith::AddIOp>(location, lhs, rhs).getResult();
  case ast::BinaryOp::Subtract:
    return isFloat ? builder.create<mlir::arith::SubFOp>(location, lhs, rhs).getResult()
                   : builder.create<mlir::arith::SubIOp>(location, lhs, rhs).getResult();
  case ast::BinaryOp::Multiply:
    return isFloat ? builder.create<mlir::arith::MulFOp>(location, lhs, rhs).getResult()
                   : builder.create<mlir::arith::MulIOp>(location, lhs, rhs).getResult();
  case ast::BinaryOp::Divide:
    if (isFloat)
      return builder.create<mlir::arith::DivFOp>(location, lhs, rhs).getResult();
    if (isUnsigned)
      return builder.create<mlir::arith::DivUIOp>(location, lhs, rhs).getResult();
    return builder.create<mlir::arith::DivSIOp>(location, lhs, rhs).getResult();
  case ast::BinaryOp::Modulo:
    if (isFloat)
      return builder.create<mlir::arith::RemFOp>(location, lhs, rhs).getResult();
    if (isUnsigned)
      return builder.create<mlir::arith::RemUIOp>(location, lhs, rhs).getResult();
    return builder.create<mlir::arith::RemSIOp>(location, lhs, rhs).getResult();

  // Comparisons
  case ast::BinaryOp::Less:
    if (isFloat)
      return builder
          .create<mlir::arith::CmpFOp>(location, mlir::arith::CmpFPredicate::OLT, lhs, rhs)
          .getResult();
    return builder
        .create<mlir::arith::CmpIOp>(location,
                                     isUnsigned ? mlir::arith::CmpIPredicate::ult
                                                : mlir::arith::CmpIPredicate::slt,
                                     lhs, rhs)
        .getResult();
  case ast::BinaryOp::LessEqual:
    if (isFloat)
      return builder
          .create<mlir::arith::CmpFOp>(location, mlir::arith::CmpFPredicate::OLE, lhs, rhs)
          .getResult();
    return builder
        .create<mlir::arith::CmpIOp>(location,
                                     isUnsigned ? mlir::arith::CmpIPredicate::ule
                                                : mlir::arith::CmpIPredicate::sle,
                                     lhs, rhs)
        .getResult();
  case ast::BinaryOp::Greater:
    if (isFloat)
      return builder
          .create<mlir::arith::CmpFOp>(location, mlir::arith::CmpFPredicate::OGT, lhs, rhs)
          .getResult();
    return builder
        .create<mlir::arith::CmpIOp>(location,
                                     isUnsigned ? mlir::arith::CmpIPredicate::ugt
                                                : mlir::arith::CmpIPredicate::sgt,
                                     lhs, rhs)
        .getResult();
  case ast::BinaryOp::GreaterEqual:
    if (isFloat)
      return builder
          .create<mlir::arith::CmpFOp>(location, mlir::arith::CmpFPredicate::OGE, lhs, rhs)
          .getResult();
    return builder
        .create<mlir::arith::CmpIOp>(location,
                                     isUnsigned ? mlir::arith::CmpIPredicate::uge
                                                : mlir::arith::CmpIPredicate::sge,
                                     lhs, rhs)
        .getResult();
  case ast::BinaryOp::Equal:
    if (isFloat)
      return builder
          .create<mlir::arith::CmpFOp>(location, mlir::arith::CmpFPredicate::OEQ, lhs, rhs)
          .getResult();
    if (isPtr) {
      bool isActorPtr = false;
      if (auto *ie = std::get_if<ast::ExprIdentifier>(&expr.left->value.kind)) {
        if (actorVarTypes.count(ie->name))
          isActorPtr = true;
      }
      if (auto *ie = std::get_if<ast::ExprIdentifier>(&expr.right->value.kind)) {
        if (actorVarTypes.count(ie->name))
          isActorPtr = true;
      }
      if (isActorPtr) {
        auto i64Type = builder.getI64Type();
        auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
        mlir::Value lhsPtr = lhs, rhsPtr = rhs;
        if (!mlir::isa<mlir::LLVM::LLVMPointerType>(lhs.getType()))
          lhsPtr = builder.create<hew::BitcastOp>(location, ptrType, lhs);
        if (!mlir::isa<mlir::LLVM::LLVMPointerType>(rhs.getType()))
          rhsPtr = builder.create<hew::BitcastOp>(location, ptrType, rhs);
        auto lhsI = builder.create<mlir::LLVM::PtrToIntOp>(location, i64Type, lhsPtr);
        auto rhsI = builder.create<mlir::LLVM::PtrToIntOp>(location, i64Type, rhsPtr);
        return builder
            .create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::eq, lhsI, rhsI)
            .getResult();
      }
      auto eqResult = builder.create<hew::StringMethodOp>(location, builder.getI32Type(),
                                                          builder.getStringAttr("equals"), lhs,
                                                          mlir::ValueRange{rhs});
      auto zero = createIntConstant(builder, location, builder.getI32Type(), 0);
      return builder
          .create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::ne,
                                       eqResult.getResult(), zero)
          .getResult();
    }
    return builder.create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::eq, lhs, rhs)
        .getResult();
  case ast::BinaryOp::NotEqual:
    if (isFloat)
      return builder
          .create<mlir::arith::CmpFOp>(location, mlir::arith::CmpFPredicate::ONE, lhs, rhs)
          .getResult();
    if (isPtr) {
      bool isActorPtr = false;
      if (auto *ie = std::get_if<ast::ExprIdentifier>(&expr.left->value.kind)) {
        if (actorVarTypes.count(ie->name))
          isActorPtr = true;
      }
      if (auto *ie = std::get_if<ast::ExprIdentifier>(&expr.right->value.kind)) {
        if (actorVarTypes.count(ie->name))
          isActorPtr = true;
      }
      if (isActorPtr) {
        auto i64Type = builder.getI64Type();
        auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
        mlir::Value lhsPtr = lhs, rhsPtr = rhs;
        if (!mlir::isa<mlir::LLVM::LLVMPointerType>(lhs.getType()))
          lhsPtr = builder.create<hew::BitcastOp>(location, ptrType, lhs);
        if (!mlir::isa<mlir::LLVM::LLVMPointerType>(rhs.getType()))
          rhsPtr = builder.create<hew::BitcastOp>(location, ptrType, rhs);
        auto lhsI = builder.create<mlir::LLVM::PtrToIntOp>(location, i64Type, lhsPtr);
        auto rhsI = builder.create<mlir::LLVM::PtrToIntOp>(location, i64Type, rhsPtr);
        return builder
            .create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::ne, lhsI, rhsI)
            .getResult();
      }
      auto eqResult = builder.create<hew::StringMethodOp>(location, builder.getI32Type(),
                                                          builder.getStringAttr("equals"), lhs,
                                                          mlir::ValueRange{rhs});
      auto zero = createIntConstant(builder, location, builder.getI32Type(), 0);
      return builder
          .create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::eq,
                                       eqResult.getResult(), zero)
          .getResult();
    }
    return builder.create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::ne, lhs, rhs)
        .getResult();

  case ast::BinaryOp::RegexMatch: {
    return builder.create<hew::RegexIsMatchOp>(location, builder.getI1Type(), rhs, lhs);
  }
  case ast::BinaryOp::RegexNotMatch: {
    auto matchResult = builder.create<hew::RegexIsMatchOp>(location, builder.getI1Type(), rhs, lhs);
    auto trueVal = builder.create<mlir::arith::ConstantOp>(location, builder.getBoolAttr(true));
    return builder.create<mlir::arith::XOrIOp>(location, matchResult, trueVal).getResult();
  }

  // Bitwise operators
  case ast::BinaryOp::BitAnd:
    return builder.create<mlir::arith::AndIOp>(location, lhs, rhs).getResult();
  case ast::BinaryOp::BitOr:
    return builder.create<mlir::arith::OrIOp>(location, lhs, rhs).getResult();
  case ast::BinaryOp::BitXor:
    return builder.create<mlir::arith::XOrIOp>(location, lhs, rhs).getResult();
  case ast::BinaryOp::Shl:
    return builder.create<mlir::arith::ShLIOp>(location, lhs, rhs).getResult();
  case ast::BinaryOp::Shr:
    return isUnsigned ? builder.create<mlir::arith::ShRUIOp>(location, lhs, rhs).getResult()
                      : builder.create<mlir::arith::ShRSIOp>(location, lhs, rhs).getResult();

  case ast::BinaryOp::Range:
  case ast::BinaryOp::RangeInclusive:
    emitWarning(location) << "range operator used outside of for loop";
    return nullptr;

  default:
    emitWarning(location) << "unsupported binary operator";
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
      return builder.create<mlir::arith::NegFOp>(location, operand).getResult();
    }
    // Integer negate: 0 - operand
    auto zero = createIntConstant(builder, location, type, 0);
    return builder.create<mlir::arith::SubIOp>(location, zero, operand).getResult();
  }
  case ast::UnaryOp::Not: {
    auto type = operand.getType();
    if (type == builder.getI1Type()) {
      auto trueVal = createIntConstant(builder, location, type, 1);
      return builder.create<mlir::arith::XOrIOp>(location, operand, trueVal).getResult();
    }
    auto zero = createIntConstant(builder, location, type, 0);
    return builder
        .create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::eq, operand, zero)
        .getResult();
  }
  case ast::UnaryOp::BitNot: {
    auto type = operand.getType();
    auto allOnes = createIntConstant(builder, location, type, -1);
    return builder.create<mlir::arith::XOrIOp>(location, operand, allOnes).getResult();
  }
  }
  return nullptr;
}

// ============================================================================
// Call expression generation
// ============================================================================

mlir::Value MLIRGen::generateCallExpr(const ast::ExprCall &call) {
  auto location = currentLoc;

  // Check if the callee is a simple identifier (direct call)
  auto *calleeIdentExpr = std::get_if<ast::ExprIdentifier>(&call.function->value.kind);
  if (!calleeIdentExpr) {
    emitWarning(location) << "only direct function calls supported in Phase 1";
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
    auto infoLevel = builder.create<mlir::arith::ConstantIntOp>(location, 2, 32);
    auto funcType = mlir::FunctionType::get(&context, {i32Type}, {});
    getOrCreateExternFunc("hew_log_set_level", funcType);
    builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{},
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

  // Handle generic function calls with explicit type arguments
  if (call.type_args.has_value() && !call.type_args->empty()) {
    auto genIt = genericFunctions.find(calleeName);
    if (genIt != genericFunctions.end()) {
      std::vector<std::string> typeArgNames;
      for (const auto &ta : *call.type_args)
        typeArgNames.push_back(resolveTypeArgMangledName(ta.value));
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
      auto callOp = builder.create<mlir::func::CallOp>(location, specializedFunc, args);
      if (call.is_tail_call)
        callOp->setAttr("hew.tail_call", builder.getUnitAttr());
      if (callOp.getNumResults() > 0)
        return callOp.getResult(0);
      return nullptr;
    }
  }

  // Handle built-in print/println
  if (calleeName == "println") {
    return generatePrintCall(call, /*newline=*/true);
  }
  if (calleeName == "print") {
    return generatePrintCall(call, /*newline=*/false);
  }

  // Check for named builtins
  auto builtinResult = generateBuiltinCall(calleeName, call.args, location);
  static const char *builtinNames[] = {"println_str",
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
                                       "bytes::new",
                                       "bytes::from"};
  for (const auto *bn : builtinNames) {
    if (calleeName == bn)
      return builtinResult;
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
        auto optType = hew::OptionEnumType::get(&context, argVal.getType());
        mlir::Value result = builder.create<hew::EnumConstructOp>(
            location, optType, static_cast<int32_t>(variantIndex), builder.getStringAttr("Option"),
            mlir::ValueRange{argVal},
            /*payload_positions=*/nullptr);
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
        else if (currentFunction && currentFunction.getResultTypes().size() == 1)
          resultType = currentFunction.getResultTypes()[0];
        else {
          resultType = hew::ResultEnumType::get(&context, argVal.getType(), builder.getI32Type());
        }
        mlir::Value result = builder.create<hew::EnumConstructOp>(
            location, resultType, static_cast<int32_t>(variantIndex),
            builder.getStringAttr("__Result"), mlir::ValueRange{argVal},
            /*payload_positions=*/nullptr);
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
        else if (currentFunction && currentFunction.getResultTypes().size() == 1)
          resultType = currentFunction.getResultTypes()[0];
        else {
          resultType = hew::ResultEnumType::get(&context, builder.getI32Type(), argVal.getType());
        }
        mlir::Value result = builder.create<hew::EnumConstructOp>(
            location, resultType, static_cast<int32_t>(variantIndex),
            builder.getStringAttr("__Result"), mlir::ValueRange{argVal},
            /*payload_positions=*/nullptr);
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
            if (vi && i < vi->payloadTypes.size() && argVal.getType() != vi->payloadTypes[i])
              argVal = coerceType(argVal, vi->payloadTypes[i], location);
            payloads.push_back(argVal);
          }
          mlir::ArrayAttr payloadPositionsAttr = nullptr;
          if (vi && vi->payloadPositions.size() == payloads.size()) {
            bool needsExplicitPositions = false;
            for (size_t i = 0; i < vi->payloadPositions.size(); ++i) {
              if (vi->payloadPositions[i] != static_cast<int64_t>(i) + 1) {
                needsExplicitPositions = true;
                break;
              }
            }
            if (needsExplicitPositions)
              payloadPositionsAttr = builder.getI64ArrayAttr(vi->payloadPositions);
          }
          mlir::Value result = builder.create<hew::EnumConstructOp>(
              location, enumInfo.mlirType, static_cast<int32_t>(variantIndex),
              builder.getStringAttr(enumName), payloads, payloadPositionsAttr);
          return result;
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
    std::string modKey;
    for (const auto &seg : currentModulePath)
      modKey += (modKey.empty() ? "" : "::") + seg;
    auto aliasIt = aliasToFunction.find(modKey + "::" + calleeName);
    if (aliasIt != aliasToFunction.end()) {
      const auto &[origPath, origName] = aliasIt->second;
      std::string aliasMangled = mangleName(origPath, "", origName);
      callee = module.lookupSymbol<mlir::func::FuncOp>(aliasMangled);
    }
  }
  // Try imported module paths (for cross-module calls like diamond deps).
  // Search direct and transitive imports of the current module.
  if (!callee) {
    std::string modKey;
    for (const auto &seg : currentModulePath)
      modKey += (modKey.empty() ? "" : "::") + seg;
    auto impIt = moduleImports.find(modKey);
    if (impIt != moduleImports.end()) {
      for (const auto &impPath : impIt->second) {
        std::string impMangled = mangleName(impPath, "", calleeName);
        callee = module.lookupSymbol<mlir::func::FuncOp>(impMangled);
        if (callee)
          break;
      }
    }
  }
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

    // Coerce concrete struct -> dyn Trait fat pointer if parameter expects one
    if (calleeFuncType && i < calleeFuncType.getNumInputs()) {
      auto expectedType = calleeFuncType.getInput(i);
      auto valType = val.getType();
      if (expectedType != valType) {
        if (auto litStruct = llvm::dyn_cast<mlir::LLVM::LLVMStructType>(expectedType)) {
          if (!litStruct.isIdentified() && litStruct.getBody().size() == 2 &&
              mlir::isa<mlir::LLVM::LLVMPointerType>(litStruct.getBody()[0]) &&
              litStruct.getBody()[1].isInteger(32)) {
            if (auto identStruct = llvm::dyn_cast<mlir::LLVM::LLVMStructType>(valType)) {
              if (identStruct.isIdentified()) {
                std::string structName = identStruct.getName().str();
                for (const auto &[traitN, dispInfo] : traitDispatchRegistry) {
                  for (const auto &impl : dispInfo.impls) {
                    if (impl.typeName == structName) {
                      val = coerceToDynTrait(val, structName, traitN, location);
                      goto coercion_done;
                    }
                  }
                }
              coercion_done:;
              }
            }
          }
        }
      }
    }

    args.push_back(val);
  }

  if (callee) {
    auto funcType = callee.getFunctionType();
    for (size_t i = 0; i < args.size() && i < funcType.getNumInputs(); ++i) {
      auto expectedType = funcType.getInput(i);
      auto actualType = args[i].getType();
      if (actualType != expectedType && isPointerLikeType(actualType) &&
          mlir::isa<mlir::LLVM::LLVMPointerType>(expectedType)) {
        args[i] = builder.create<hew::BitcastOp>(location, expectedType, args[i]);
      } else if (actualType != expectedType && mlir::isa<mlir::LLVM::LLVMPointerType>(actualType) &&
                 isPointerLikeType(expectedType)) {
        args[i] = builder.create<hew::BitcastOp>(location, expectedType, args[i]);
      } else if (actualType != expectedType) {
        args[i] = coerceType(args[i], expectedType, location);
      }
    }
    auto callOp = builder.create<mlir::func::CallOp>(location, callee, args);
    if (call.is_tail_call)
      callOp->setAttr("hew.tail_call", builder.getUnitAttr());

    // Drop RC environments of temporary closure arguments after the call.
    // Closures bound to variables are dropped by popDropScope; only inline
    // lambdas (ExprLambda in the AST) need explicit drop here.
    {
      auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
      for (size_t i = 0; i < args.size() && i < call.args.size(); ++i) {
        if (!mlir::isa<hew::ClosureType>(args[i].getType()))
          continue;
        if (!std::holds_alternative<ast::ExprLambda>(ast::callArgExpr(call.args[i]).value.kind))
          continue;
        auto envPtr = builder.create<hew::ClosureGetEnvOp>(location, ptrType, args[i]);
        builder.create<hew::DropOp>(location, envPtr, "hew_rc_drop", false);
      }
    }

    if (callOp.getNumResults() > 0) {
      auto result = callOp.getResult(0);
      auto externRetIt = externSemanticReturnTypes.find(callee.getSymName().str());
      if (externRetIt != externSemanticReturnTypes.end() &&
          mlir::isa<mlir::LLVM::LLVMPointerType>(result.getType()) &&
          result.getType() != externRetIt->second) {
        result = builder.create<hew::BitcastOp>(location, externRetIt->second, result);
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
      auto fnPtr = builder.create<hew::ClosureGetFnOp>(location, closurePtrType, calleeVal);
      auto envPtr = builder.create<hew::ClosureGetEnvOp>(location, closurePtrType, calleeVal);

      llvm::SmallVector<mlir::Type, 8> indirectParamTypes;
      indirectParamTypes.push_back(closurePtrType);
      for (auto inTy : closureType.getInputTypes())
        indirectParamTypes.push_back(inTy);

      auto retType = closureType.getResultType();
      bool hasReturn = retType && !mlir::isa<mlir::NoneType>(retType);
      auto indirectFuncType = hasReturn
                                  ? mlir::FunctionType::get(&context, indirectParamTypes, {retType})
                                  : mlir::FunctionType::get(&context, indirectParamTypes, {});

      auto fnRef = builder.create<hew::BitcastOp>(location, indirectFuncType, fnPtr);

      llvm::SmallVector<mlir::Value, 8> indirectArgs;
      indirectArgs.push_back(envPtr);
      for (size_t i = 0; i < args.size() && i < closureType.getInputTypes().size(); ++i) {
        if (args[i].getType() != closureType.getInputTypes()[i])
          args[i] = coerceType(args[i], closureType.getInputTypes()[i], location);
        indirectArgs.push_back(args[i]);
      }

      auto callOp = builder.create<mlir::func::CallIndirectOp>(location, fnRef, indirectArgs);
      if (callOp.getNumResults() > 0)
        return callOp.getResult(0);
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
    emitWarning(location) << "print/println requires at least one argument";
    return nullptr;
  }

  auto val = generateExpression(ast::callArgExpr(call.args[0]).value);
  if (!val)
    return nullptr;

  builder.create<hew::PrintOp>(location, val, builder.getBoolAttr(newline));

  if (!std::holds_alternative<ast::ExprIdentifier>(ast::callArgExpr(call.args[0]).value.kind) &&
      isTemporaryString(val))
    emitStringDrop(val);

  return nullptr; // print returns void
}

// ============================================================================
// If expression generation (expression form that yields a value)
// ============================================================================

mlir::Value MLIRGen::generateIfExpr(const ast::ExprIf &ifE) {
  auto location = currentLoc;

  auto cond = generateExpression(ifE.condition->value);
  if (!cond)
    return nullptr;

  if (cond.getType() != builder.getI1Type()) {
    auto zero = createIntConstant(builder, location, cond.getType(), 0);
    cond =
        builder.create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::ne, cond, zero);
  }

  bool hasElse = ifE.else_block.has_value();

  if (!hasElse) {
    auto ifOp = builder.create<mlir::scf::IfOp>(location, /*resultTypes=*/mlir::TypeRange{}, cond,
                                                /*withElseRegion=*/false);

    builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
    if (ifE.then_block) {
      generateExpression(ifE.then_block->value);
    }
    auto *thenBlock = builder.getInsertionBlock();
    if (thenBlock->empty() || !thenBlock->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
      builder.create<mlir::scf::YieldOp>(location);
    }

    builder.setInsertionPointAfter(ifOp);
    return nullptr;
  }

  mlir::Type resultType;
  if (currentFunction && currentFunction.getResultTypes().size() == 1) {
    resultType = currentFunction.getResultTypes()[0];
  } else {
    resultType = defaultIntType();
  }

  auto ifOp = builder.create<mlir::scf::IfOp>(location, resultType, cond, /*withElseRegion=*/true);

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
      builder.create<mlir::scf::YieldOp>(location, mlir::ValueRange{thenVal});
    } else {
      auto defVal = createDefaultValue(builder, location, resultType);
      builder.create<mlir::scf::YieldOp>(location, mlir::ValueRange{defVal});
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
      builder.create<mlir::scf::YieldOp>(location, mlir::ValueRange{elseVal});
    } else {
      auto defVal = createDefaultValue(builder, location, resultType);
      builder.create<mlir::scf::YieldOp>(location, mlir::ValueRange{defVal});
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
  auto resType = mlir::dyn_cast<hew::ResultEnumType>(operandType);
  if (!resType) {
    emitError(location) << "? operator requires a Result type";
    return nullptr;
  }

  auto tag = builder.create<hew::EnumExtractTagOp>(location, builder.getI32Type(), operandVal);
  auto oneTag = createIntConstant(builder, location, builder.getI32Type(), 1);
  auto isErr =
      builder.create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::eq, tag, oneTag);

  auto okType = resType.getOkType();
  auto errType = resType.getErrType();
  auto okFieldIndex = enumPayloadFieldIndex("__Result", /*variantIndex=*/0);
  auto errFieldIndex = enumPayloadFieldIndex("__Result", /*variantIndex=*/1);

  mlir::Type funcRetType;
  if (currentFunction && currentFunction.getResultTypes().size() == 1)
    funcRetType = currentFunction.getResultTypes()[0];

  auto *errBlock = currentFunction.addBlock();
  auto *okBlock = currentFunction.addBlock();

  builder.create<mlir::cf::CondBranchOp>(location, isErr, errBlock, okBlock);

  builder.setInsertionPointToStart(errBlock);
  if (tryErrorDest) {
    auto errVal = builder.create<hew::EnumExtractPayloadOp>(location, errType, operandVal,
                                                            /*field_index=*/errFieldIndex);
    auto coerced = coerceType(errVal, builder.getI32Type(), location);
    if (coerced)
      builder.create<mlir::memref::StoreOp>(location, coerced, tryErrorSlot);
    builder.create<mlir::cf::BranchOp>(location, tryErrorDest);
  } else if (funcRetType) {
    auto errVal = builder.create<hew::EnumExtractPayloadOp>(location, errType, operandVal,
                                                            /*field_index=*/errFieldIndex);
    mlir::Value errResult = builder.create<hew::EnumConstructOp>(
        location, funcRetType, /*variant_index=*/1, builder.getStringAttr("__Result"),
        mlir::ValueRange{errVal}, /*payload_positions=*/nullptr);
    builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{errResult});
  } else {
    builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{});
  }

  builder.setInsertionPointToStart(okBlock);
  auto okVal = builder.create<hew::EnumExtractPayloadOp>(location, okType, operandVal,
                                                         /*field_index=*/okFieldIndex);
  return okVal;
}

// ============================================================================
// Struct initialization
// ============================================================================

mlir::Value MLIRGen::generateStructInit(const ast::ExprStructInit &si) {
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
    if (it == structTypes.end()) {
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

  return builder.create<hew::StructInitOp>(location, info.mlirType, fieldValues, fieldNamesAttr,
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
      auto infoLevel = builder.create<mlir::arith::ConstantIntOp>(location, 2, 32);
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
      auto op = builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{i32Type}, calleeAttr,
                                                   argVals);
      return op.getResult();
    }
    builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{}, calleeAttr, argVals);
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
      builder
          .create<hew::RuntimeCallOp>(location, mlir::TypeRange{i32Type},
                                      mlir::SymbolRefAttr::get(&context, "hew_log_get_level"),
                                      mlir::ValueRange{})
          .getResult();

  // 2. Compare: emit if levelInt <= currentLevel (i.e., the message level
  //    is at or above the configured filter).
  auto levelConst = createIntConstant(builder, location, i32Type, levelInt);
  auto enabled = builder.create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::sle,
                                                     levelConst, currentLevel);

  // 3. Wrap the log emission in scf.if(enabled).
  builder.create<mlir::scf::IfOp>(location, enabled, [&](mlir::OpBuilder &b, mlir::Location loc) {
    // Generate the message string from the first positional arg.
    mlir::Value msgStr = nullptr;
    if (!args.empty()) {
      msgStr = generateExpression(ast::callArgExpr(args[0]).value);
      if (!msgStr)
        return;
      // Ensure it is a string; convert non-strings via hew.to_string.
      if (!mlir::isa<hew::StringRefType>(msgStr.getType()) &&
          !mlir::isa<mlir::LLVM::LLVMPointerType>(msgStr.getType())) {
        msgStr = b.create<hew::ToStringOp>(loc, strRefType, msgStr);
      }
    } else {
      // No args at all — use empty string.
      auto sym = getOrCreateGlobalString("");
      msgStr = b.create<hew::ConstantOp>(loc, strRefType, b.getStringAttr(sym));
    }

    // Prepend actor context if inside an actor body.
    if (!currentActorName.empty()) {
      // " actor=TypeName" (compile-time constant)
      std::string actorPrefix = " actor=" + currentActorName;
      auto actorPrefixSym = getOrCreateGlobalString(actorPrefix);
      auto actorPrefixStr =
          b.create<hew::ConstantOp>(loc, strRefType, b.getStringAttr(actorPrefixSym));
      msgStr = b.create<hew::StringConcatOp>(loc, strRefType, msgStr, actorPrefixStr);

      // " actor_id=<runtime_id>" (runtime value)
      auto actorIdPrefixSym = getOrCreateGlobalString(" actor_id=");
      auto actorIdPrefixStr =
          b.create<hew::ConstantOp>(loc, strRefType, b.getStringAttr(actorIdPrefixSym));

      auto i64Type = b.getI64Type();
      auto getIdFuncType = mlir::FunctionType::get(&context, {}, {i64Type});
      getOrCreateExternFunc("hew_actor_current_id", getIdFuncType);
      auto actorId =
          b.create<hew::RuntimeCallOp>(loc, mlir::TypeRange{i64Type},
                                       mlir::SymbolRefAttr::get(&context, "hew_actor_current_id"),
                                       mlir::ValueRange{})
              .getResult();
      auto actorIdStr = b.create<hew::ToStringOp>(loc, strRefType, actorId);

      msgStr = b.create<hew::StringConcatOp>(loc, strRefType, msgStr, actorIdPrefixStr);
      msgStr = b.create<hew::StringConcatOp>(loc, strRefType, msgStr, actorIdStr);
    }

    // Append named arguments as " key=value" pairs.
    for (size_t i = 0; i < args.size(); ++i) {
      auto name = ast::callArgName(args[i]);
      if (name.empty())
        continue; // skip positional args (already handled above)

      // Create the " key=" prefix string.
      std::string prefix = " " + name + "=";
      auto prefixSym = getOrCreateGlobalString(prefix);
      auto prefixStr = b.create<hew::ConstantOp>(loc, strRefType, b.getStringAttr(prefixSym));

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
        valStr = b.create<hew::ToStringOp>(loc, strRefType, val);
      }

      // Concat: msgStr + " key=" + valStr
      msgStr = b.create<hew::StringConcatOp>(loc, strRefType, msgStr, prefixStr);
      msgStr = b.create<hew::StringConcatOp>(loc, strRefType, msgStr, valStr);
    }

    // Cast the final string to !llvm.ptr for the C ABI call.
    mlir::Value msgPtr = msgStr;
    if (!mlir::isa<mlir::LLVM::LLVMPointerType>(msgStr.getType()))
      msgPtr = b.create<hew::BitcastOp>(loc, ptrType, msgStr);

    // Declare and call hew_log_emit(level, msg).
    auto emitFuncType = mlir::FunctionType::get(&context, {i32Type, ptrType}, {});
    getOrCreateExternFunc("hew_log_emit", emitFuncType);

    b.create<hew::RuntimeCallOp>(loc, mlir::TypeRange{},
                                 mlir::SymbolRefAttr::get(&context, "hew_log_emit"),
                                 mlir::ValueRange{levelConst, msgPtr});

    b.create<mlir::scf::YieldOp>(loc);
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
      builder.create<hew::VecPushOp>(location, vecValue, val);
      resultOut = nullptr;
      return true;
    }
    if (method == "get") {
      auto idx = generateExpression(ast::callArgExpr(mc.args[0]).value);
      if (!idx)
        return true;
      if (idx.getType() != i64Type)
        idx = builder.create<mlir::arith::ExtSIOp>(location, i64Type, idx);
      resultOut = builder.create<hew::VecGetOp>(location, elemType, vecValue, idx).getResult();
      return true;
    }
    if (method == "set") {
      auto idx = generateExpression(ast::callArgExpr(mc.args[0]).value);
      auto val = generateExpression(ast::callArgExpr(mc.args[1]).value);
      if (!idx || !val)
        return true;
      if (idx.getType() != i64Type)
        idx = builder.create<mlir::arith::ExtSIOp>(location, i64Type, idx);
      val = coerceType(val, elemType, location);
      builder.create<hew::VecSetOp>(location, vecValue, idx, val);
      resultOut = nullptr;
      return true;
    }
    if (method == "pop") {
      resultOut = builder.create<hew::VecPopOp>(location, elemType, vecValue).getResult();
      return true;
    }
    if (method == "remove") {
      if (!mc.args.empty()) {
        auto argVal = generateExpression(ast::callArgExpr(mc.args[0]).value);
        if (!argVal)
          return true;
        argVal = coerceType(argVal, elemType, location);
        builder.create<hew::VecRemoveOp>(location, vecValue, argVal);
      }
      resultOut = nullptr;
      return true;
    }
    if (method == "len") {
      resultOut = builder.create<hew::VecLenOp>(location, i64Type, vecValue).getResult();
      return true;
    }
    if (method == "is_empty") {
      resultOut =
          builder.create<hew::VecIsEmptyOp>(location, builder.getI1Type(), vecValue).getResult();
      return true;
    }
    if (method == "clear") {
      builder.create<hew::VecClearOp>(location, vecValue);
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
      builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{}, calleeAttr,
                                         mlir::ValueRange{vecValue, src});
      resultOut = nullptr;
      return true;
    }
    if (method == "to_string") {
      auto strType = hew::StringRefType::get(&context);
      auto calleeAttr = mlir::SymbolRefAttr::get(&context, "hew_bytes_to_string");
      resultOut = builder
                      .create<hew::RuntimeCallOp>(location, mlir::TypeRange{strType}, calleeAttr,
                                                  mlir::ValueRange{vecValue})
                      .getResult();
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
      builder.create<hew::HashMapInsertOp>(location, mapValue, key, val);
      resultOut = nullptr;
      return true;
    }
    if (method == "get") {
      auto key = generateExpression(ast::callArgExpr(mc.args[0]).value);
      if (!key)
        return true;
      key = coerceType(key, keyType, location);
      resultOut = builder.create<hew::HashMapGetOp>(location, valueType, mapValue, key).getResult();
      return true;
    }
    if (method == "remove") {
      auto key = generateExpression(ast::callArgExpr(mc.args[0]).value);
      if (!key)
        return true;
      key = coerceType(key, keyType, location);
      builder.create<hew::HashMapRemoveOp>(location, mapValue, key);
      resultOut = nullptr;
      return true;
    }
    if (method == "contains_key" || method == "contains") {
      auto key = generateExpression(ast::callArgExpr(mc.args[0]).value);
      if (!key)
        return true;
      key = coerceType(key, keyType, location);
      resultOut =
          builder.create<hew::HashMapContainsKeyOp>(location, builder.getI1Type(), mapValue, key)
              .getResult();
      return true;
    }
    if (method == "keys") {
      auto keysType = hew::VecType::get(&context, keyType);
      resultOut = builder.create<hew::HashMapKeysOp>(location, keysType, mapValue).getResult();
      return true;
    }
    if (method == "values") {
      auto keysType = hew::VecType::get(&context, keyType);
      auto keysVec = builder.create<hew::HashMapKeysOp>(location, keysType, mapValue).getResult();
      auto valuesType = hew::VecType::get(&context, valueType);
      auto valuesVec = builder.create<hew::VecNewOp>(location, valuesType).getResult();
      auto len = builder.create<hew::VecLenOp>(location, i64Type, keysVec).getResult();
      auto zero = createIntConstant(builder, location, i64Type, 0);
      auto one = createIntConstant(builder, location, i64Type, 1);
      auto loop = builder.create<mlir::scf::ForOp>(location, zero, len, one);
      auto *body = loop.getBody();
      auto iv = loop.getInductionVar();
      builder.setInsertionPointToStart(body);
      auto key = builder.create<hew::VecGetOp>(location, keyType, keysVec, iv).getResult();
      auto val = builder.create<hew::HashMapGetOp>(location, valueType, mapValue, key).getResult();
      builder.create<hew::VecPushOp>(location, valuesVec, val);
      builder.create<mlir::scf::YieldOp>(location);
      builder.setInsertionPointAfter(loop);
      resultOut = valuesVec;
      return true;
    }
    if (method == "len") {
      resultOut = builder.create<hew::HashMapLenOp>(location, i64Type, mapValue).getResult();
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

  if (method == "trim") {
    return builder
        .create<hew::StringMethodOp>(location, hew::StringRefType::get(&context),
                                     builder.getStringAttr("trim"), receiver, mlir::ValueRange{})
        .getResult();
  }
  if (method == "to_lower" || method == "to_lowercase") {
    return builder
        .create<hew::StringMethodOp>(location, hew::StringRefType::get(&context),
                                     builder.getStringAttr("to_lowercase"), receiver,
                                     mlir::ValueRange{})
        .getResult();
  }
  if (method == "to_upper" || method == "to_uppercase") {
    return builder
        .create<hew::StringMethodOp>(location, hew::StringRefType::get(&context),
                                     builder.getStringAttr("to_uppercase"), receiver,
                                     mlir::ValueRange{})
        .getResult();
  }
  if (method == "replace") {
    auto old_s = generateExpression(ast::callArgExpr(mc.args[0]).value);
    auto new_s = generateExpression(ast::callArgExpr(mc.args[1]).value);
    if (!old_s || !new_s)
      return mlir::Value{};
    return builder
        .create<hew::StringMethodOp>(location, hew::StringRefType::get(&context),
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
    return builder
        .create<hew::StringMethodOp>(location, hew::StringRefType::get(&context),
                                     builder.getStringAttr("slice"), receiver,
                                     mlir::ValueRange{start, end})
        .getResult();
  }
  if (method == "repeat") {
    auto n = generateExpression(ast::callArgExpr(mc.args[0]).value);
    if (!n)
      return mlir::Value{};
    n = coerceType(n, i32Type, location);
    return builder
        .create<hew::StringMethodOp>(location, hew::StringRefType::get(&context),
                                     builder.getStringAttr("repeat"), receiver, mlir::ValueRange{n})
        .getResult();
  }
  if (method == "char_at") {
    auto idx = generateExpression(ast::callArgExpr(mc.args[0]).value);
    if (!idx)
      return mlir::Value{};
    idx = coerceType(idx, i32Type, location);
    auto charCode = builder.create<hew::StringMethodOp>(
        location, i32Type, builder.getStringAttr("char_at"), receiver, mlir::ValueRange{idx});
    auto conv = builder.create<hew::ToStringOp>(location, hew::StringRefType::get(&context),
                                                charCode.getResult());
    return conv.getResult();
  }
  if (method == "split") {
    auto sep = generateExpression(ast::callArgExpr(mc.args[0]).value);
    if (!sep)
      return mlir::Value{};
    return builder
        .create<hew::StringMethodOp>(location, hew::StringRefType::get(&context),
                                     builder.getStringAttr("split"), receiver,
                                     mlir::ValueRange{sep})
        .getResult();
  }
  if (method == "contains") {
    auto sub = generateExpression(ast::callArgExpr(mc.args[0]).value);
    if (!sub)
      return mlir::Value{};
    return builder
        .create<hew::StringMethodOp>(location, builder.getI1Type(),
                                     builder.getStringAttr("contains"), receiver,
                                     mlir::ValueRange{sub})
        .getResult();
  }
  if (method == "starts_with") {
    auto prefix = generateExpression(ast::callArgExpr(mc.args[0]).value);
    if (!prefix)
      return mlir::Value{};
    return builder
        .create<hew::StringMethodOp>(location, builder.getI1Type(),
                                     builder.getStringAttr("starts_with"), receiver,
                                     mlir::ValueRange{prefix})
        .getResult();
  }
  if (method == "ends_with") {
    auto suffix = generateExpression(ast::callArgExpr(mc.args[0]).value);
    if (!suffix)
      return mlir::Value{};
    return builder
        .create<hew::StringMethodOp>(location, builder.getI1Type(),
                                     builder.getStringAttr("ends_with"), receiver,
                                     mlir::ValueRange{suffix})
        .getResult();
  }
  if (method == "len") {
    return builder
        .create<hew::StringMethodOp>(location, i32Type, builder.getStringAttr("length"), receiver,
                                     mlir::ValueRange{})
        .getResult();
  }
  if (method == "find") {
    auto sub = generateExpression(ast::callArgExpr(mc.args[0]).value);
    if (!sub)
      return mlir::Value{};
    return builder
        .create<hew::StringMethodOp>(location, i32Type, builder.getStringAttr("find"), receiver,
                                     mlir::ValueRange{sub})
        .getResult();
  }
  if (method == "index_of") {
    auto sub = generateExpression(ast::callArgExpr(mc.args[0]).value);
    if (!sub)
      return mlir::Value{};
    auto startIdx = createIntConstant(builder, location, i32Type, 0);
    return builder
        .create<hew::StringMethodOp>(location, i32Type, builder.getStringAttr("index_of"), receiver,
                                     mlir::ValueRange{sub, startIdx})
        .getResult();
  }

  return std::nullopt;
}

// ============================================================================
// Method call generation
// ============================================================================

mlir::Value MLIRGen::generateMethodCall(const ast::ExprMethodCall &mc) {
  auto location = currentLoc;
  const auto &methodName = mc.method;

  // Intercept module-qualified calls before generating the receiver expression,
  // because module names (log, string, crypto, etc.) are not variables.
  if (auto *ident = std::get_if<ast::ExprIdentifier>(&mc.receiver->value.kind)) {
    // Special handling for log (has custom emit logic).
    if (ident->name == "log") {
      return generateLogCall(mc);
    }

    // General module-qualified call: string.from_int(), crypto.sha256(), etc.
    auto modIt = moduleNameToPath.find(ident->name);
    if (modIt != moduleNameToPath.end()) {
      const auto &modulePath = modIt->second;
      std::string mangledFunc = mangleName(modulePath, "", methodName);
      auto callee = module.lookupSymbol<mlir::func::FuncOp>(mangledFunc);
      if (!callee) {
        emitError(location) << "undefined function '" << ident->name << "." << methodName
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
        if (i < calleeFuncType.getNumInputs()) {
          auto expectedType = calleeFuncType.getInput(i);
          if (val.getType() != expectedType)
            val = coerceType(val, expectedType, location);
        }
        args.push_back(val);
      }

      auto callOp = builder.create<mlir::func::CallOp>(location, callee, args);
      if (callOp.getNumResults() > 0)
        return callOp.getResult(0);
      return nullptr;
    }
  }

  // Generate receiver
  auto receiver = generateExpression(mc.receiver->value);
  if (!receiver)
    return nullptr;

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

    auto emitRuntimeCall = [&](llvm::StringRef callee, mlir::Type resultType,
                               mlir::ValueRange args) -> mlir::Value {
      auto calleeAttr = mlir::SymbolRefAttr::get(&context, callee);
      if (resultType) {
        auto op = builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{resultType},
                                                     calleeAttr, args);
        return op.getResult();
      }
      builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{}, calleeAttr, args);
      return nullptr;
    };

    // http.Server methods
    if (handleType == "http.Server") {
      if (method == "accept")
        return emitRuntimeCall("hew_http_server_recv",
                               hew::HandleType::get(&context, "http.Request"), argVals);
      if (method == "close") {
        emitRuntimeCall("hew_http_server_close", {}, argVals);
        return nullptr;
      }
    }

    // http.Request methods
    if (handleType == "http.Request") {
      if (method == "path")
        return emitRuntimeCall("hew_http_request_path", hew::StringRefType::get(&context),
                               {receiver});
      if (method == "method")
        return emitRuntimeCall("hew_http_request_method", hew::StringRefType::get(&context),
                               {receiver});
      if (method == "body")
        return emitRuntimeCall("hew_http_request_body", hew::StringRefType::get(&context), argVals);
      if (method == "header")
        return emitRuntimeCall("hew_http_request_header", hew::StringRefType::get(&context),
                               argVals);
      if (method == "respond")
        return emitRuntimeCall("hew_http_respond", i32Type, argVals);
      if (method == "respond_text")
        return emitRuntimeCall("hew_http_respond_text", i32Type, argVals);
      if (method == "respond_json")
        return emitRuntimeCall("hew_http_respond_json", i32Type, argVals);
      if (method == "respond_stream")
        return emitRuntimeCall("hew_http_respond_stream", ptrType, argVals);
      if (method == "free") {
        emitRuntimeCall("hew_http_request_free", {}, argVals);
        return nullptr;
      }
    }

    // net.Listener methods
    if (handleType == "net.Listener") {
      if (method == "accept")
        return emitRuntimeCall("hew_tcp_accept", i32Type, argVals);
      if (method == "close")
        return emitRuntimeCall("hew_tcp_close", i32Type, argVals);
    }

    // net.Connection methods
    if (handleType == "net.Connection") {
      if (method == "read")
        return emitRuntimeCall("hew_tcp_read", ptrType, argVals);
      if (method == "write")
        return emitRuntimeCall("hew_tcp_write", i32Type, argVals);
      if (method == "close")
        return emitRuntimeCall("hew_tcp_close", i32Type, argVals);
      if (method == "set_read_timeout")
        return emitRuntimeCall("hew_tcp_set_read_timeout", i32Type, argVals);
      if (method == "set_write_timeout")
        return emitRuntimeCall("hew_tcp_set_write_timeout", i32Type, argVals);
    }

    // regex.Pattern methods
    if (handleType == "regex.Pattern") {
      if (method == "is_match")
        return builder.create<hew::RegexIsMatchOp>(location, builder.getI1Type(), argVals[0],
                                                   argVals[1]);
      if (method == "find")
        return builder.create<hew::RegexFindOp>(location, hew::StringRefType::get(&context),
                                                argVals[0], argVals[1]);
      if (method == "replace")
        return builder.create<hew::RegexReplaceOp>(location, hew::StringRefType::get(&context),
                                                   argVals[0], argVals[1], argVals[2]);
      if (method == "free") {
        builder.create<hew::RegexFreeOp>(location, argVals[0]);
        return nullptr;
      }
    }

    // process.Child methods
    if (handleType == "process.Child") {
      if (method == "wait")
        return emitRuntimeCall("hew_process_wait", i32Type, argVals);
      if (method == "kill")
        return emitRuntimeCall("hew_process_kill", i32Type, argVals);
    }
  }

  // Check if receiver is an i32-typed handle (net.Listener, net.Connection)
  // These types map to i32 at the MLIR level but need handle method dispatch.
  auto receiverType = receiver.getType();
  if (receiverType.isInteger(32)) {
    std::string handleType;
    // Prefer resolved type from the type checker
    if (auto *typeExpr = resolvedTypeOf(mc.receiver->span))
      handleType = typeExprToHandleString(*typeExpr);
    // Fall back to identifier-based map lookup
    if (handleType.empty()) {
      if (auto *ie = std::get_if<ast::ExprIdentifier>(&mc.receiver->value.kind)) {
        auto hit = handleVarTypes.find(ie->name);
        if (hit != handleVarTypes.end())
          handleType = hit->second;
      }
    }
    // Check field access (e.g. self.conn)
    if (handleType.empty()) {
      if (auto *fa = std::get_if<ast::ExprFieldAccess>(&mc.receiver->value.kind)) {
        if (auto *baseIdent = std::get_if<ast::ExprIdentifier>(&fa->object->value.kind)) {
          if (baseIdent->name == "self" && !currentActorName.empty()) {
            auto key = currentActorName + "." + fa->field;
            auto aft = actorFieldTypes.find(key);
            if (aft != actorFieldTypes.end() &&
                (aft->second == "net.Listener" || aft->second == "net.Connection"))
              handleType = aft->second;
          }
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

      auto emitRuntimeCall = [&](llvm::StringRef callee, mlir::Type resultType,
                                 mlir::ValueRange args) -> mlir::Value {
        auto calleeAttr = mlir::SymbolRefAttr::get(&context, callee);
        if (resultType) {
          auto op = builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{resultType},
                                                       calleeAttr, args);
          return op.getResult();
        }
        builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{}, calleeAttr, args);
        return nullptr;
      };

      if (handleType == "net.Listener") {
        if (method == "accept")
          return emitRuntimeCall("hew_tcp_accept", i32Type, argVals);
        if (method == "close")
          return emitRuntimeCall("hew_tcp_close", i32Type, argVals);
      }
      if (handleType == "net.Connection") {
        if (method == "read")
          return emitRuntimeCall("hew_tcp_read", vecType, argVals);
        if (method == "write") {
          emitRuntimeCall("hew_tcp_write", {}, argVals);
          return nullptr;
        }
        if (method == "close")
          return emitRuntimeCall("hew_tcp_close", i32Type, argVals);
        if (method == "set_read_timeout")
          return emitRuntimeCall("hew_tcp_set_read_timeout", i32Type, argVals);
        if (method == "set_write_timeout")
          return emitRuntimeCall("hew_tcp_set_write_timeout", i32Type, argVals);
      }
    }
  }

  // Check if receiver is an actor (ptr type + tracked in actorVarTypes)
  if (isPointerLikeType(receiverType)) {
    std::string actorTypeName;
    // Prefer resolved type from the type checker
    if (auto *typeExpr = resolvedTypeOf(mc.receiver->span))
      actorTypeName = typeExprToActorName(*typeExpr);
    // Fall back to identifier-based map lookup
    if (actorTypeName.empty()) {
      if (auto *ie = std::get_if<ast::ExprIdentifier>(&mc.receiver->value.kind)) {
        auto it = actorVarTypes.find(ie->name);
        if (it != actorVarTypes.end())
          actorTypeName = it->second;
      }
    }

    // If the receiver is a field access (e.g. self.target)
    if (actorTypeName.empty()) {
      if (auto *fa = std::get_if<ast::ExprFieldAccess>(&mc.receiver->value.kind)) {
        std::string baseName;
        if (auto *baseIdent = std::get_if<ast::ExprIdentifier>(&fa->object->value.kind)) {
          if (baseIdent->name == "self" && !currentActorName.empty()) {
            baseName = currentActorName;
          } else {
            auto baseIt = actorVarTypes.find(baseIdent->name);
            if (baseIt != actorVarTypes.end())
              baseName = baseIt->second;
          }
        }
        if (!baseName.empty()) {
          auto key = baseName + "." + fa->field;
          auto aft = actorFieldTypes.find(key);
          if (aft != actorFieldTypes.end() && actorRegistry.count(aft->second))
            actorTypeName = aft->second;
        }
      }
    }

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
      builder.create<hew::ActorStopOp>(location, receiver);
      return nullptr;
    }
    if (methodName == "close") {
      builder.create<hew::ActorCloseOp>(location, receiver);
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
      builder.create<hew::ActorSendOp>(location, receiver, builder.getI32IntegerAttr(0), argVals);
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
            auto callResult = builder.create<mlir::func::CallOp>(location, nextFuncOp,
                                                                 mlir::ValueRange{receiver});
            return callResult.getResult(0);
          }
        }
      }
    }

    if (auto builtinMethodResult = generateBuiltinMethodCall(mc, receiver, location))
      return *builtinMethodResult;
  }

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

      auto dataPtr = builder.create<hew::TraitObjectDataOp>(location, ptrType, receiver);
      auto vtablePtr = builder.create<hew::TraitObjectTagOp>(location, ptrType, receiver);

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
        builder.create<hew::TraitDispatchOp>(
            location, mlir::Type{}, builder.getStringAttr(traitName),
            builder.getStringAttr(methodName), dataPtr, vtablePtr, extraArgs, methodIndexAttr);
        return nullptr;
      }

      return builder
          .create<hew::TraitDispatchOp>(location, returnType, builder.getStringAttr(traitName),
                                        builder.getStringAttr(methodName), dataPtr, vtablePtr,
                                        extraArgs, methodIndexAttr)
          .getResult();
    }
  }

  // Determine struct type name from the receiver's MLIR type
  auto structType = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(receiverType);
  if (!structType || !structType.isIdentified()) {
    emitError(location) << "method call on non-struct type"
                        << " (method='" << methodName << "'"
                        << ", receiver type: " << receiverType << ")";
    return nullptr;
  }

  std::string funcName = mangleName(currentModulePath, structType.getName().str(), methodName);

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
  if (!callee) {
    std::string modKey;
    for (const auto &seg : currentModulePath)
      modKey += (modKey.empty() ? "" : "::") + seg;
    auto impIt = moduleImports.find(modKey);
    if (impIt != moduleImports.end()) {
      for (const auto &impPath : impIt->second) {
        std::string tryName = mangleName(impPath, structType.getName().str(), methodName);
        callee = module.lookupSymbol<mlir::func::FuncOp>(tryName);
        if (callee)
          break;
      }
    }
  }
  if (!callee) {
    emitError(location) << "undefined method '" << methodName << "' on type '"
                        << structType.getName() << "'";
    return nullptr;
  }

  auto funcType = callee.getFunctionType();
  for (size_t i = 0; i < args.size() && i < funcType.getNumInputs(); ++i) {
    if (args[i].getType() != funcType.getInput(i))
      args[i] = coerceType(args[i], funcType.getInput(i), location);
  }

  auto callOp = builder.create<mlir::func::CallOp>(location, callee, args);
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
  return builder.create<hew::TupleCreateOp>(location, tupleType, values);
}

// ============================================================================
// Array expression
// ============================================================================

mlir::Value MLIRGen::generateArrayExpr(const ast::ExprArray &arr) {
  auto location = currentLoc;

  if (arr.elements.empty()) {
    emitWarning(location) << "empty array literal";
    return nullptr;
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
  return builder.create<hew::ArrayCreateOp>(location, arrayType, values);
}

// ============================================================================
// Free variable collection (for lambda closure capture)
// ============================================================================

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
      if (arm.body)
        collectFreeVarsInExpr(arm.body->value, bound, freeVars);
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
  }
  // Literal, Lambda (nested), Spawn, Scope, etc. — skip
}

void MLIRGen::collectFreeVarsInStmt(const ast::Stmt &stmt, std::set<std::string> &bound,
                                    std::set<std::string> &freeVars) {
  if (auto *ls = std::get_if<ast::StmtLet>(&stmt.kind)) {
    if (ls->value.has_value())
      collectFreeVarsInExpr(ls->value->value, bound, freeVars);
    // After evaluating the RHS, bind the name
    if (auto *ip = std::get_if<ast::PatIdentifier>(&ls->pattern.value.kind))
      bound.insert(ip->name);
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
  } else if (auto *ws = std::get_if<ast::StmtWhile>(&stmt.kind)) {
    collectFreeVarsInExpr(ws->condition.value, bound, freeVars);
    collectFreeVarsInBlock(ws->body, bound, freeVars);
  } else if (auto *fs = std::get_if<ast::StmtFor>(&stmt.kind)) {
    collectFreeVarsInExpr(fs->iterable.value, bound, freeVars);
    collectFreeVarsInBlock(fs->body, bound, freeVars);
  } else if (auto *ls2 = std::get_if<ast::StmtLoop>(&stmt.kind)) {
    collectFreeVarsInBlock(ls2->body, bound, freeVars);
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
      userParamTypes.push_back(defaultIntType());
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

  struct CapturedVar {
    std::string name;
    mlir::Value value;
    bool isMutable = false;
    mlir::Type valueType; // for mutable captures: the actual value type
  };
  std::vector<CapturedVar> capturedVars;
  for (const auto &fv : freeVars) {
    if (module.lookupSymbol<mlir::func::FuncOp>(fv) ||
        module.lookupSymbol<mlir::func::FuncOp>(mangleName(currentModulePath, "", fv)))
      continue;
    if (variantLookup.count(fv))
      continue;
    if (moduleConstants.count(fv))
      continue;

    // Check if this is a mutable variable (in mutableVars)
    auto mutAlloca = mutableVars.lookup(fv);
    if (mutAlloca) {
      auto cellIt = heapCellValueTypes.find(mutAlloca);
      if (cellIt != heapCellValueTypes.end()) {
        // Already a heap cell — just capture the cell pointer
        auto cellPtr = builder.create<mlir::memref::LoadOp>(location, mutAlloca);
        capturedVars.push_back({fv, cellPtr, true, cellIt->second});
      } else {
        // Convert to heap cell for by-reference capture
        auto val = builder.create<mlir::memref::LoadOp>(location, mutAlloca);
        auto valueType = val.getType();

        // Allocate heap cell via hew.rc.new
        auto cellSize =
            builder.create<hew::SizeOfOp>(location, sizeType(), mlir::TypeAttr::get(valueType));
        auto nullPtr = builder.create<mlir::LLVM::ZeroOp>(location, ptrType);
        auto cellPtr = builder.create<hew::RcNewOp>(location, ptrType, nullPtr, cellSize, nullPtr);
        builder.create<mlir::LLVM::StoreOp>(location, val, cellPtr);

        // Replace outer scope's mutable var entry with the cell pointer
        auto ptrMemrefType = mlir::MemRefType::get({}, ptrType);
        mlir::Value newAlloca;
        if (returnFlag && currentFunction) {
          auto savedIP = builder.saveInsertionPoint();
          auto &entryBlock = currentFunction.front();
          builder.setInsertionPointToStart(&entryBlock);
          newAlloca =
              builder.create<mlir::memref::AllocaOp>(builder.getUnknownLoc(), ptrMemrefType);
          builder.restoreInsertionPoint(savedIP);
        } else {
          newAlloca = builder.create<mlir::memref::AllocaOp>(location, ptrMemrefType);
        }
        builder.create<mlir::memref::StoreOp>(location, cellPtr, newAlloca);

        auto internedName = intern(fv);
        mutableVars.insert(internedName, newAlloca);
        heapCellValueTypes[newAlloca] = valueType;

        capturedVars.push_back({fv, cellPtr, true, valueType});
      }
    } else {
      auto val = lookupVariable(fv);
      if (val)
        capturedVars.push_back({fv, val, false, nullptr});
    }
  }

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
  auto savedFunction = currentFunction;

  builder.setInsertionPointToEnd(module.getBody());
  auto funcOp = builder.create<mlir::func::FuncOp>(location, lambdaName, funcType);
  funcOp.setVisibility(mlir::SymbolTable::Visibility::Private);

  auto &entryBlock = *funcOp.addEntryBlock();
  builder.setInsertionPointToStart(&entryBlock);
  currentFunction = funcOp;

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
      auto gepOp = builder.create<mlir::LLVM::GEPOp>(
          location, ptrType, envStructType, envPtr,
          llvm::ArrayRef<mlir::LLVM::GEPArg>{static_cast<int32_t>(0), static_cast<int32_t>(i)});
      auto loadedVal = builder.create<mlir::LLVM::LoadOp>(location, capturedTypes[i], gepOp);

      if (capturedVars[i].isMutable) {
        // Mutable capture: loadedVal is the heap cell pointer (!llvm.ptr).
        // Create a local memref to hold it and register as a heap-cell var.
        auto ptrMemrefType = mlir::MemRefType::get({}, ptrType);
        auto lambdaAlloca = builder.create<mlir::memref::AllocaOp>(location, ptrMemrefType);
        builder.create<mlir::memref::StoreOp>(location, loadedVal, lambdaAlloca);
        auto internedName = intern(capturedVars[i].name);
        mutableVars.insert(internedName, lambdaAlloca);
        heapCellValueTypes[lambdaAlloca] = capturedVars[i].valueType;
      } else {
        mlir::Value capturedVal = loadedVal;
        if (capturedTypes[i] != capturedVars[i].value.getType()) {
          capturedVal =
              builder.create<hew::BitcastOp>(location, capturedVars[i].value.getType(), loadedVal);
        }
        declareVariable(capturedVars[i].name, capturedVal);
      }
    }
  }

  mlir::Value savedReturnFlag = returnFlag;
  mlir::Value savedReturnSlot = returnSlot;
  returnFlag = nullptr;
  returnSlot = nullptr;

  mlir::Value bodyVal = nullptr;
  if (lam.body) {
    bodyVal = generateExpression(lam.body->value);
  }

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
      bodyVal = coerceType(bodyVal, returnType, location);
      builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{bodyVal});
    } else if (returnType) {
      auto defVal = createDefaultValue(builder, location, returnType);
      builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{defVal});
    } else {
      builder.create<mlir::func::ReturnOp>(location);
    }
  }

  returnFlag = savedReturnFlag;
  returnSlot = savedReturnSlot;
  currentFunction = savedFunction;
  builder.restoreInsertionPoint(savedIP);

  mlir::Value envPtrVal;
  if (!capturedVars.empty()) {
    llvm::SmallVector<mlir::Type, 4> capturedTypes;
    for (const auto &cv : capturedVars)
      capturedTypes.push_back(toLLVMStorageType(cv.value.getType()));
    auto envStructType = mlir::LLVM::LLVMStructType::getLiteral(&context, capturedTypes);

    auto envSize =
        builder.create<hew::SizeOfOp>(location, sizeType(), mlir::TypeAttr::get(envStructType));

    auto nullData = builder.create<mlir::LLVM::ZeroOp>(location, ptrType);
    auto nullDropFn = builder.create<mlir::LLVM::ZeroOp>(location, ptrType);
    envPtrVal = builder.create<hew::RcNewOp>(location, ptrType, nullData, envSize, nullDropFn);

    for (size_t i = 0; i < capturedVars.size(); ++i) {
      if (mlir::isa<hew::ClosureType>(capturedVars[i].value.getType())) {
        auto innerEnv =
            builder.create<hew::ClosureGetEnvOp>(location, ptrType, capturedVars[i].value);
        builder.create<hew::RcCloneOp>(location, ptrType, innerEnv);
      }
      auto gepOp = builder.create<mlir::LLVM::GEPOp>(
          location, ptrType, envStructType, envPtrVal,
          llvm::ArrayRef<mlir::LLVM::GEPArg>{static_cast<int32_t>(0), static_cast<int32_t>(i)});
      mlir::Value storeVal = capturedVars[i].value;
      if (storeVal.getType() != capturedTypes[i]) {
        storeVal = builder.create<hew::BitcastOp>(location, capturedTypes[i], storeVal);
      }
      builder.create<mlir::LLVM::StoreOp>(location, storeVal, gepOp);
    }
  } else {
    envPtrVal = builder.create<mlir::LLVM::ZeroOp>(location, ptrType);
  }

  auto fnPtrVal =
      builder
          .create<hew::FuncPtrOp>(location, ptrType, mlir::SymbolRefAttr::get(&context, lambdaName))
          .getResult();

  mlir::Type closureRetType = returnType ? returnType : mlir::NoneType::get(&context);
  auto closureType = hew::ClosureType::get(&context, userParamTypes, closureRetType);

  return builder.create<hew::ClosureCreateOp>(location, closureType, fnPtrVal, envPtrVal);
}

// ============================================================================
// Scope expression codegen
// ============================================================================

mlir::Value MLIRGen::generateScopeExpr(const ast::ExprScope &se) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);

  auto scopeCreateOp = builder.create<hew::ScopeCreateOp>(location, ptrType, ptrType);
  auto scopePtr = scopeCreateOp.getActorScope();
  auto taskScopePtr = scopeCreateOp.getTaskScope();

  auto prevScope = currentScopePtr;
  auto prevTaskScope = currentTaskScopePtr;
  currentScopePtr = scopePtr;
  currentTaskScopePtr = taskScopePtr;

  auto savedReturnFlag = returnFlag;
  returnFlag = nullptr;
  auto bodyResult = generateBlock(se.block);
  returnFlag = savedReturnFlag;

  currentScopePtr = prevScope;
  currentTaskScopePtr = prevTaskScope;

  builder.create<hew::ScopeJoinOp>(location, scopePtr, taskScopePtr);
  builder.create<hew::ScopeDestroyOp>(location, scopePtr, taskScopePtr);

  return bodyResult;
}

// ============================================================================
// scope.launch { body }
// ============================================================================

mlir::Value MLIRGen::generateScopeLaunchExpr(const ast::ExprScopeLaunch &sle) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);

  auto savedIP = builder.saveInsertionPoint();

  std::string taskFnName = "__scope_task_" + std::to_string(taskCounter++);
  auto voidType = builder.getFunctionType({ptrType}, {});

  builder.setInsertionPointToEnd(module.getBody());
  auto taskFn = builder.create<mlir::func::FuncOp>(location, taskFnName, voidType);
  taskFn.setPrivate();

  auto *entryBlock = taskFn.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto taskArg = entryBlock->getArgument(0);

  auto savedFunction = currentFunction;
  auto savedReturnFlag = returnFlag;
  auto savedScopePtr = currentScopePtr;
  auto savedTaskScopePtr = currentTaskScopePtr;
  currentFunction = taskFn;
  returnFlag = nullptr;
  currentScopePtr = nullptr;
  currentTaskScopePtr = nullptr;

  SymbolTableScopeT taskVarScope(symbolTable);
  MutableTableScopeT taskMutScope(mutableVars);

  auto bodyResult = generateBlock(sle.block);

  currentFunction = savedFunction;
  returnFlag = savedReturnFlag;
  currentScopePtr = savedScopePtr;
  currentTaskScopePtr = savedTaskScopePtr;

  if (bodyResult) {
    lastScopeLaunchResultType = bodyResult.getType();

    auto sizeVal = builder.create<hew::SizeOfOp>(location, sizeType(),
                                                 mlir::TypeAttr::get(bodyResult.getType()));

    auto resultPtr = builder
                         .create<hew::RuntimeCallOp>(location, mlir::TypeRange{ptrType},
                                                     mlir::SymbolRefAttr::get(&context, "malloc"),
                                                     mlir::ValueRange{sizeVal})
                         .getResult();
    builder.create<mlir::LLVM::StoreOp>(location, bodyResult, resultPtr);

    builder.create<hew::TaskSetResultOp>(location, taskArg, resultPtr, sizeVal);
  }

  builder.create<hew::TaskCompleteOp>(location, taskArg);
  builder.create<mlir::func::ReturnOp>(location);

  builder.restoreInsertionPoint(savedIP);

  auto fnPtr =
      builder
          .create<hew::FuncPtrOp>(location, ptrType,
                                  mlir::SymbolRefAttr::get(builder.getContext(), taskFnName))
          .getResult();

  auto taskPtr = builder.create<hew::ScopeLaunchOp>(location, ptrType, currentTaskScopePtr, fnPtr);

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

  builder.create<hew::ScopeCancelOp>(location, currentTaskScopePtr);
  return nullptr;
}

// ============================================================================
// Helper: resolve actor type name from an expression
// ============================================================================

std::string MLIRGen::resolveActorTypeName(const ast::Expr &expr) {
  if (auto *ie = std::get_if<ast::ExprIdentifier>(&expr.kind)) {
    auto it = actorVarTypes.find(ie->name);
    if (it != actorVarTypes.end())
      return it->second;
  }
  if (auto *fa = std::get_if<ast::ExprFieldAccess>(&expr.kind)) {
    std::string baseName;
    if (auto *baseIE = std::get_if<ast::ExprIdentifier>(&fa->object->value.kind)) {
      if (baseIE->name == "self" && !currentActorName.empty())
        baseName = currentActorName;
      else {
        auto baseIt = actorVarTypes.find(baseIE->name);
        if (baseIt != actorVarTypes.end())
          baseName = baseIt->second;
      }
    }
    if (!baseName.empty()) {
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
      builder.create<hew::PackArgsOp>(location, mlir::TypeRange{ptrType, sizeType()}, args);
  return {packOp.getDataPtr(), packOp.getDataSize()};
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

    std::string actorTypeName = resolveActorTypeName(mcPtr->receiver->value);
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

    auto ch = builder.create<hew::SelectCreateOp>(location, ptrType);
    channels.push_back(ch);

    llvm::SmallVector<mlir::Value, 4> argVals;
    for (const auto &arg : mcPtr->args) {
      auto val = generateExpression(ast::callArgExpr(arg).value);
      if (!val)
        return nullptr;
      argVals.push_back(val);
    }
    auto [dataPtr, dataSize] = packArgsForSend(argVals, location);

    auto msgTypeVal = builder.create<mlir::arith::ConstantIntOp>(location, i32Type, msgIdx);
    builder.create<hew::SelectAddOp>(location, receiver, msgTypeVal, dataPtr, dataSize, ch);
  }

  auto armCountVal = builder.create<mlir::arith::ConstantIntOp>(location, i64Type, armCount);
  auto channelArray = builder.create<mlir::LLVM::AllocaOp>(location, ptrType, ptrType, armCountVal);

  for (size_t i = 0; i < armCount; ++i) {
    auto gep = builder.create<mlir::LLVM::GEPOp>(
        location, ptrType, ptrType, channelArray,
        llvm::ArrayRef<mlir::LLVM::GEPArg>{static_cast<int32_t>(i)});
    builder.create<mlir::LLVM::StoreOp>(location, channels[i], gep);
  }

  int64_t timeoutMs = 2147483647;
  if (sel.timeout.has_value() && *sel.timeout && (*sel.timeout)->duration) {
    auto timeoutVal = generateExpression((*sel.timeout)->duration->value);
    if (timeoutVal) {
      if (auto constOp = timeoutVal.getDefiningOp<mlir::arith::ConstantIntOp>()) {
        timeoutMs = constOp.value();
      }
    }
  }

  auto countVal =
      builder.create<mlir::arith::ConstantIntOp>(location, i32Type, static_cast<int64_t>(armCount));
  auto timeoutVal = builder.create<mlir::arith::ConstantIntOp>(location, i32Type, timeoutMs);
  auto winnerIdx =
      builder.create<hew::SelectFirstOp>(location, i32Type, channelArray, countVal, timeoutVal);

  mlir::Type selectResultType = resultTypes[0];

  bool hasTimeoutBody = sel.timeout.has_value() && *sel.timeout && (*sel.timeout)->body;

  auto generateArmChain = [&](auto &&self, size_t armIdx) -> mlir::Value {
    if (armIdx >= armCount) {
      if (hasTimeoutBody) {
        SymbolTableScopeT scope(symbolTable);
        MutableTableScopeT mutScope(mutableVars);
        return generateExpression((*sel.timeout)->body->value);
      }
      return createDefaultValue(builder, location, selectResultType);
    }

    bool isLast = (armIdx + 1 == armCount && !hasTimeoutBody);

    if (isLast) {
      SymbolTableScopeT scope(symbolTable);
      MutableTableScopeT mutScope(mutableVars);
      const auto &arm = arms[armIdx];

      auto replyPtr = builder.create<hew::SelectWaitOp>(location, ptrType, channels[armIdx]);
      auto resultVal = builder.create<mlir::LLVM::LoadOp>(location, resultTypes[armIdx], replyPtr);
      // Free the reply buffer (malloc'd by hew_reply, returned by hew_reply_wait)
      getOrCreateExternFunc("free", mlir::FunctionType::get(&context, {ptrType}, {}));
      builder.create<mlir::func::CallOp>(location, "free", mlir::TypeRange{},
                                         mlir::ValueRange{replyPtr});

      if (auto *ip = std::get_if<ast::PatIdentifier>(&arm.binding.value.kind))
        declareVariable(ip->name, resultVal);

      auto bodyVal = generateExpression(arm.body->value);
      builder.create<hew::SelectDestroyOp>(location, channels[armIdx]);

      return bodyVal ? bodyVal : createDefaultValue(builder, location, selectResultType);
    }

    auto armIdxVal =
        builder.create<mlir::arith::ConstantIntOp>(location, i32Type, static_cast<int64_t>(armIdx));
    auto cond = builder.create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::eq,
                                                    winnerIdx, armIdxVal);

    auto ifOp =
        builder.create<mlir::scf::IfOp>(location, selectResultType, cond, /*withElseRegion=*/true);

    builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
    {
      SymbolTableScopeT scope(symbolTable);
      MutableTableScopeT mutScope(mutableVars);
      const auto &arm = arms[armIdx];

      auto replyPtr = builder.create<hew::SelectWaitOp>(location, ptrType, channels[armIdx]);
      auto resultVal = builder.create<mlir::LLVM::LoadOp>(location, resultTypes[armIdx], replyPtr);
      // Free the reply buffer (malloc'd by hew_reply, returned by hew_reply_wait)
      getOrCreateExternFunc("free", mlir::FunctionType::get(&context, {ptrType}, {}));
      builder.create<mlir::func::CallOp>(location, "free", mlir::TypeRange{},
                                         mlir::ValueRange{replyPtr});

      if (auto *ip = std::get_if<ast::PatIdentifier>(&arm.binding.value.kind))
        declareVariable(ip->name, resultVal);

      auto bodyVal = generateExpression(arm.body->value);
      builder.create<hew::SelectDestroyOp>(location, channels[armIdx]);

      auto yieldVal = bodyVal ? bodyVal : createDefaultValue(builder, location, selectResultType);
      builder.create<mlir::scf::YieldOp>(location, mlir::ValueRange{yieldVal});
    }

    builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
    auto elseVal = self(self, armIdx + 1);
    auto *elseBlock = builder.getInsertionBlock();
    if (elseBlock->empty() || !elseBlock->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
      auto yieldVal = elseVal ? elseVal : createDefaultValue(builder, location, selectResultType);
      builder.create<mlir::scf::YieldOp>(location, mlir::ValueRange{yieldVal});
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

    std::string actorTypeName = resolveActorTypeName(mcPtr->receiver->value);
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

    auto ch = builder.create<hew::SelectCreateOp>(location, ptrType);
    channels.push_back(ch);

    llvm::SmallVector<mlir::Value, 4> argVals;
    for (const auto &arg : mcPtr->args) {
      auto val = generateExpression(ast::callArgExpr(arg).value);
      if (!val)
        return nullptr;
      argVals.push_back(val);
    }
    auto [dataPtr, dataSize] = packArgsForSend(argVals, location);

    auto msgTypeVal = builder.create<mlir::arith::ConstantIntOp>(location, i32Type, msgIdx);
    builder.create<hew::SelectAddOp>(location, receiver, msgTypeVal, dataPtr, dataSize, ch);
  }

  llvm::SmallVector<mlir::Value, 4> results;
  for (size_t i = 0; i < exprCount; ++i) {
    auto replyPtr = builder.create<hew::SelectWaitOp>(location, ptrType, channels[i]);
    auto resultVal = builder.create<mlir::LLVM::LoadOp>(location, resultTypes[i], replyPtr);
    results.push_back(resultVal);
    // Free the reply buffer (malloc'd by hew_reply, returned by hew_reply_wait)
    getOrCreateExternFunc("free", mlir::FunctionType::get(&context, {ptrType}, {}));
    builder.create<mlir::func::CallOp>(location, "free", mlir::TypeRange{},
                                       mlir::ValueRange{replyPtr});
    builder.create<hew::SelectDestroyOp>(location, channels[i]);
  }

  if (exprCount == 1)
    return results[0];

  llvm::SmallVector<mlir::Type, 4> tupleFieldTypes;
  for (auto &rt : resultTypes)
    tupleFieldTypes.push_back(rt);
  auto tupleType = hew::HewTupleType::get(&context, tupleFieldTypes);

  return builder.create<hew::TupleCreateOp>(location, tupleType, results);
}
