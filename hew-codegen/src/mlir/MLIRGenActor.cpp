//===- MLIRGenActor.cpp - Actor codegen for Hew MLIRGen -------------------===//
//
// Actor-related generation: generateActorDecl, generateSpawnExpr,
// generateSpawnLambdaActorExpr, generateActorMethodSend, generateSendExpr.
//
//===----------------------------------------------------------------------===//

#include "hew/ast_helpers.h"
#include "hew/mlir/HewDialect.h"
#include "hew/mlir/HewOps.h"
#include "hew/mlir/MLIRGen.h"
#include "MLIRGenHelpers.h"

#include "mlir/Dialect/Arith/IR/Arith.h"
#include "mlir/Dialect/Func/IR/FuncOps.h"
#include "mlir/Dialect/LLVMIR/LLVMDialect.h"
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

#include <algorithm>
#include <cassert>
#include <cstdlib>
#include <string>

using namespace hew;
using namespace mlir;

// ============================================================================
// Actor registration (phase 1): struct types, field tracking, registry entry
// ============================================================================

void MLIRGen::registerActorDecl(const ast::ActorDecl &decl,
                                std::optional<mlir::Location> fallbackLoc) {
  hasActors = true;
  const std::string &actorName = decl.name;
  auto typeLoc = [&](const ast::Spanned<ast::TypeExpr> &type) {
    return type.span.end > type.span.start ? loc(type.span) : fallbackLoc.value_or(currentLoc);
  };

  // De-duplicate: imported actors may be registered via both forEachItem
  // (module graph iteration) and flatten_import_items (top-level promotion).
  if (actorRegistry.count(actorName))
    return;

  // 1. Create actor state struct type from fields
  llvm::SmallVector<mlir::Type, 4> fieldTypes;
  std::vector<mlir::Type> fieldHewTypes; // Hew MLIR types before toLLVMStorageType
  for (const auto &field : decl.fields) {
    auto hewType = convertTypeOrError(field.ty.value,
                                      "cannot resolve type for actor field '" + field.name + "'",
                                      typeLoc(field.ty));
    if (!hewType)
      return;
    fieldHewTypes.push_back(hewType);
    fieldTypes.push_back(toLLVMStorageType(hewType));
  }

  // Record how many of those fields are user-declared state fields.
  size_t numUserFields = fieldTypes.size();

  // Add hidden init-param fields after user fields (before gen-frame fields).
  // These let spawn pass init arguments into the state struct, and the init
  // body accesses them by their original parameter names via the GEP helpers.
  std::vector<std::string> initParamNames;
  if (decl.init) {
    for (const auto &param : decl.init->params) {
      auto hewType = convertTypeOrError(
          param.ty.value, "cannot resolve type for init parameter '" + param.name + "'",
          typeLoc(param.ty));
      if (!hewType)
        return;
      initParamNames.push_back(param.name);
      fieldHewTypes.push_back(hewType);
      fieldTypes.push_back(toLLVMStorageType(hewType));
    }
  }

  // Add hidden __gen_frame fields for generator receive fns (ptr to HewGenCtx)
  auto ptrTypeForFields = mlir::LLVM::LLVMPointerType::get(&context);
  for (const auto &recv : decl.receive_fns) {
    if (recv.is_generator) {
      unsigned idx = static_cast<unsigned>(fieldTypes.size());
      genFrameFieldIdx[actorName + "." + recv.name] = idx;
      fieldTypes.push_back(ptrTypeForFields);
    }
  }

  auto stateType = mlir::LLVM::LLVMStructType::getIdentified(&context, actorName + "_state");
  (void)stateType.setBody(fieldTypes, /*isPacked=*/false);

  // Register field info in struct types for field access
  StructTypeInfo stInfo;
  stInfo.name = actorName;
  stInfo.mlirType = stateType;
  {
    unsigned i = 0;
    for (const auto &field : decl.fields) {
      StructFieldInfo fi;
      fi.name = field.name;
      fi.semanticType = fieldHewTypes[i];
      fi.type = fieldTypes[i];
      fi.index = i;
      stInfo.fields.push_back(std::move(fi));
      ++i;
    }
    // Add init params as named hidden fields so the init body can access them by
    // bare name through the actor-field GEP path (same as regular state fields).
    for (size_t j = 0; j < initParamNames.size(); ++j, ++i) {
      StructFieldInfo fi;
      fi.name = initParamNames[j];
      fi.semanticType = fieldHewTypes[i];
      fi.type = fieldTypes[i];
      fi.index = i;
      stInfo.fields.push_back(std::move(fi));
    }
  }
  structTypes[actorName] = std::move(stInfo);

  // Record actor-typed fields for field-access dispatch (e.g. self.target.method())
  // and collection-typed fields for Vec/HashMap method calls (e.g. self.items.push())
  for (const auto &field : decl.fields) {
    auto key = actorName + "." + field.name;
    auto resolveAliasExpr = [this](llvm::StringRef name) { return resolveTypeAliasExpr(name); };

    // Track actor-typed fields (ActorRef<T> → extract T)
    auto actorName2 = typeExprToActorName(field.ty.value, resolveAliasExpr);
    if (!actorName2.empty()) {
      actorFieldTypes[key] = actorName2;
    } else if (auto typeName = typeExprToTypeName(field.ty.value, resolveAliasExpr);
               !typeName.empty()) {
      actorFieldTypes[key] = typeName;
    }

    // Track collection-typed fields (Vec<T>, HashMap<K,V>)
    auto collStr = typeExprToCollectionString(field.ty.value, resolveAliasExpr);
    if (!collStr.empty())
      collectionFieldTypes[key] = collStr;
  }

  // Build actor registry info (signatures only, no body generation)
  ActorInfo actorInfo;
  actorInfo.name = actorName;
  actorInfo.sourceLoc = fallbackLoc.value_or(currentLoc);
  actorInfo.stateType = stateType;
  actorInfo.fieldHewTypes = std::move(fieldHewTypes);
  actorInfo.numUserFields = numUserFields;
  actorInfo.initParamNames = std::move(initParamNames);
  actorInfo.mailboxCapacity = decl.mailbox_capacity;

  if (decl.overflow_policy) {
    const auto &op = *decl.overflow_policy;
    if (std::holds_alternative<ast::OverflowDropNew>(op))
      actorInfo.overflowPolicy = 1;
    else if (std::holds_alternative<ast::OverflowDropOld>(op))
      actorInfo.overflowPolicy = 2;
    else if (std::holds_alternative<ast::OverflowBlock>(op))
      actorInfo.overflowPolicy = 3;
    else if (std::holds_alternative<ast::OverflowFail>(op))
      actorInfo.overflowPolicy = 4;
    else if (auto *coalesce = std::get_if<ast::OverflowCoalesce>(&op)) {
      actorInfo.overflowPolicy = 5;
      actorInfo.coalesceKey = coalesce->key_field;
      if (coalesce->fallback) {
        switch (*coalesce->fallback) {
        case ast::OverflowFallback::DropNew:
          actorInfo.coalesceFallback = 1;
          break;
        case ast::OverflowFallback::DropOld:
          actorInfo.coalesceFallback = 2;
          break;
        case ast::OverflowFallback::Block:
          actorInfo.coalesceFallback = 3;
          break;
        case ast::OverflowFallback::Fail:
          actorInfo.coalesceFallback = 4;
          break;
        }
      }
    }
  }

  auto i8Type = builder.getI8Type();
  for (const auto &recv : decl.receive_fns) {
    ActorReceiveInfo recvInfo;
    recvInfo.name = recv.name;
    recvInfo.periodicIntervalNs = recv.periodic_interval_ns;

    for (const auto &param : recv.params) {
      auto ty = convertTypeOrError(param.ty.value,
                                   "cannot resolve type for receive parameter '" + param.name + "'",
                                   typeLoc(param.ty));
      if (!ty)
        return;
      recvInfo.paramNames.push_back(param.name);
      recvInfo.paramTypes.push_back(ty);
    }

    if (recv.is_generator && recv.return_type) {
      // Generator return type: wrap YieldType → { i8 has_value, YieldType value }
      auto yieldType = convertType(recv.return_type->value, typeLoc(*recv.return_type));
      if (!llvm::isa<mlir::NoneType>(yieldType)) {
        auto wrapperType = mlir::LLVM::LLVMStructType::getLiteral(&context, {i8Type, yieldType});
        recvInfo.returnType = wrapperType;
        actorInfo.receiveFns.push_back(std::move(recvInfo));

        // Register a __next handler (no params, same wrapper return type)
        ActorReceiveInfo nextInfo;
        nextInfo.name = recv.name + "__next";
        nextInfo.returnType = wrapperType;
        actorInfo.receiveFns.push_back(std::move(nextInfo));
        continue;
      }
    } else if (recv.return_type) {
      auto retTy = convertType(recv.return_type->value, typeLoc(*recv.return_type));
      if (!llvm::isa<mlir::NoneType>(retTy))
        recvInfo.returnType = retTy;
    }

    actorInfo.receiveFns.push_back(std::move(recvInfo));
  }

  actorRegistry[actorName] = std::move(actorInfo);
}

// ============================================================================
// Actor body generation (phase 2): receive fn bodies, init, dispatch
// ============================================================================

void MLIRGen::generateActorDecl(const ast::ActorDecl &decl) {
  auto actorLoc = currentLoc;
  const std::string &actorName = decl.name;

  // De-duplicate: imported actors may appear in both forEachItem iterations
  // (module graph) and flattened root items. Only generate bodies once.
  if (!generatedActorBodies.insert(actorName).second)
    return;

  // State struct and registry entry already set up by registerActorDecl
  auto stIt = structTypes.find(actorName);
  if (stIt == structTypes.end())
    return;
  auto stateType = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(stIt->second.mlirType);

  auto regIt = actorRegistry.find(actorName);
  if (regIt == actorRegistry.end())
    return;
  const auto &actorInfo = regIt->second;

  auto firstBodyLoc = [&](const ast::Block &body, mlir::Location fallback) {
    if (!body.stmts.empty() && body.stmts.front())
      return loc(body.stmts.front()->span);
    if (body.trailing_expr)
      return loc(body.trailing_expr->span);
    return fallback;
  };

  auto receiveSourceLoc = [&](llvm::StringRef receiveName) {
    llvm::StringRef baseName = receiveName;
    if (baseName.ends_with("__body"))
      baseName = baseName.drop_back(sizeof("__body") - 1);
    else if (baseName.ends_with("__next"))
      baseName = baseName.drop_back(sizeof("__next") - 1);

    for (const auto &receiveDecl : decl.receive_fns) {
      if (receiveDecl.name == baseName)
        return loc(receiveDecl.span);
    }
    return actorLoc;
  };

  // Generate receive handler functions
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto prevActorName = currentActorName;
  currentActorName = actorName;

  for (const auto &recv : decl.receive_fns) {
    auto location = receiveSourceLoc(recv.name);
    std::string receiveName = actorName + "_" + recv.name;

    if (recv.is_generator && recv.return_type) {
      auto yieldType = convertType(recv.return_type->value);
      if (llvm::isa<mlir::NoneType>(yieldType)) {
        continue;
      }

      // ─── Generator receive fn: emit body, init, and __next functions ───
      auto i8Type = builder.getI8Type();
      auto i64Type = builder.getI64Type();
      auto wrapperType = mlir::LLVM::LLVMStructType::getLiteral(&context, {i8Type, yieldType});

      // Look up gen frame field index
      auto frameIt = genFrameFieldIdx.find(actorName + "." + recv.name);
      unsigned genFrameIdx = (frameIt != genFrameFieldIdx.end()) ? frameIt->second : 0;

      // Look up stored param types from registration (avoid re-converting)
      auto recvIt = std::find_if(actorInfo.receiveFns.begin(), actorInfo.receiveFns.end(),
                                 [&](const ActorReceiveInfo &r) { return r.name == recv.name; });

      // Build args struct type once: { ptr self, param1_type, param2_type, ... }
      // Pointer-like Hew types (StringRefType, VecType, HashMapType, …) are
      // stored as !llvm.ptr so the args struct is a valid LLVM aggregate.
      llvm::SmallVector<mlir::Type, 4> argsFieldTypes;
      argsFieldTypes.push_back(ptrType); // self
      if (recvIt != actorInfo.receiveFns.end()) {
        for (auto ty : recvIt->paramTypes)
          argsFieldTypes.push_back(toLLVMStorageType(ty));
      }
      auto argsStructType = mlir::LLVM::LLVMStructType::getLiteral(&context, argsFieldTypes);

      // ─── 1. Body function: void ActorName_method__body(ptr args, ptr gen_ctx) ───
      {
        std::string bodyFnName = receiveName + "__body";
        auto bodyFnType = builder.getFunctionType({ptrType, ptrType}, {});
        auto savedIP = builder.saveInsertionPoint();
        builder.setInsertionPointToEnd(module.getBody());
        auto bodyFnOp = mlir::func::FuncOp::create(builder, location, bodyFnName, bodyFnType);
        auto *entryBlock = bodyFnOp.addEntryBlock();
        builder.setInsertionPointToStart(entryBlock);

        SymbolTableScopeT varScope(symbolTable);
        MutableTableScopeT mutScope(mutableVars);
        FunctionGenerationScope funcScope(*this, bodyFnOp);

        // Mirror the non-generator receive-fn drop-scope machinery: owned
        // message params (String, Vec, HashMap, …) must be freed when the
        // body function returns (generator exhausted).  hew_gen_ctx_create
        // memcpy's the args struct into heap, so the body function owns
        // those heap-copied values and must drop them on exit.
        auto prevFuncLevelDropScopeBase = funcLevelDropScopeBase;
        funcLevelDropScopeBase = dropScopes.size();
        auto prevBodyDropExcludeVars = std::move(funcLevelDropExcludeVars);
        auto prevBodyDropExcludeValues = std::move(funcLevelDropExcludeValues);
        auto prevBodyDropExcludeResolvedNames = std::move(funcLevelDropExcludeResolvedNames);
        auto prevBodyEarlyReturnExcludeValues = std::move(funcLevelEarlyReturnExcludeValues);
        auto prevBodyEarlyReturnExcludeResolvedNames =
            std::move(funcLevelEarlyReturnExcludeResolvedNames);
        funcLevelDropExcludeVars.clear();
        funcLevelDropExcludeValues.clear();
        funcLevelDropExcludeResolvedNames.clear();
        funcLevelEarlyReturnExcludeValues.clear();
        funcLevelEarlyReturnExcludeResolvedNames.clear();

        auto argsPtr = entryBlock->getArgument(0);
        auto genCtxArg = entryBlock->getArgument(1);

        // Set currentGenCtx so yield expressions emit hew_gen_yield calls
        auto prevGenCtx = currentGenCtx;
        currentGenCtx = genCtxArg;

        // Load the args struct from the args pointer
        auto argsStruct = mlir::LLVM::LoadOp::create(builder, location, argsStructType, argsPtr);

        // Extract actor state pointer (field 0) and bind as internal variable "self".
        // NOTE: "self" is an internal codegen name for the actor state pointer, NOT
        // a Hew language concept. Hew source uses bare field names; codegen resolves
        // them via lookupVariable("self") + GEP. See MLIRGenExpr.cpp bare-field path.
        auto selfPtr = mlir::LLVM::ExtractValueOp::create(builder, location, argsStruct,
                                                          llvm::ArrayRef<int64_t>{0});
        declareVariable("self", selfPtr);

        // Push a drop scope for receive-body params.  The gen context owns a
        // heap copy of the args (via hew_gen_ctx_create memcpy), so the body
        // function is responsible for freeing any owned heap values on exit.
        pushDropScope();

        // Extract and bind message parameters (fields 1..N)
        {
          size_t pi = 0;
          for (const auto &param : recv.params) {
            mlir::Value paramVal = mlir::LLVM::ExtractValueOp::create(
                builder, location, argsStruct,
                llvm::ArrayRef<int64_t>{static_cast<int64_t>(pi + 1)});
            // Coerce from LLVM storage type back to semantic Hew type
            // (e.g., !llvm.ptr → !hew.string_ref) so the body code and drop
            // registration work with the canonical value type.
            if (recvIt != actorInfo.receiveFns.end() && pi < recvIt->paramTypes.size()) {
              auto semanticType = recvIt->paramTypes[pi];
              if (argsFieldTypes[pi + 1] != semanticType) {
                paramVal = coerceType(paramVal, semanticType, location);
                if (!paramVal)
                  return;
              }
            }
            bindParam(param, paramVal);

            // Register ActorRef<T> params for method dispatch
            {
              auto resolveAliasExpr = [this](llvm::StringRef name) {
                return resolveTypeAliasExpr(name);
              };
              auto actorName = typeExprToActorName(param.ty.value, resolveAliasExpr);
              if (!actorName.empty())
                actorVarTypes[param.name] = actorName;
            }

            // Register drops for owned types (deep-copied into gen ctx args)
            auto paramType = paramVal.getType();
            if (auto dropFn = dropFuncForMLIRType(paramType, /*includeStructTypes=*/true);
                !dropFn.empty()) {
              bool isUserDrop = false;
              if (auto structTy = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(paramType);
                  structTy && structTy.isIdentified()) {
                isUserDrop = userDropFuncs.find(structTy.getName().str()) != userDropFuncs.end();
              }
              registerDroppable(param.name, dropFn, isUserDrop);
            }

            ++pi;
          }
        }

        // Generate the receive fn body (yields will call hew_gen_yield)
        generateBlock(recv.body, /*statementPosition=*/true);

        // Pop the param drop scope — emits drops for owned params at generator
        // exhaustion (normal exit) or early return (via emitAllDrops).
        popDropScope();
        funcLevelDropScopeBase = prevFuncLevelDropScopeBase;
        funcLevelDropExcludeVars = std::move(prevBodyDropExcludeVars);
        funcLevelDropExcludeValues = std::move(prevBodyDropExcludeValues);
        funcLevelDropExcludeResolvedNames = std::move(prevBodyDropExcludeResolvedNames);
        funcLevelEarlyReturnExcludeValues = std::move(prevBodyEarlyReturnExcludeValues);
        funcLevelEarlyReturnExcludeResolvedNames =
            std::move(prevBodyEarlyReturnExcludeResolvedNames);

        // Ensure terminator
        if (!hasRealTerminator(builder.getInsertionBlock()))
          mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{});

        currentGenCtx = prevGenCtx;
        builder.restoreInsertionPoint(savedIP);
      }

      // ─── 2. Init handler: {i8,Y} ActorName_method(ptr self, params...) ───
      {
        // Build param types: { ptr self, param1, param2, ... }
        // Use semantic Hew types (not LLVM storage types) so the signature
        // matches what the dispatcher and ReceiveOpLowering expect.
        llvm::SmallVector<mlir::Type, 4> initParamTypes;
        initParamTypes.push_back(ptrType); // self
        if (recvIt != actorInfo.receiveFns.end()) {
          for (auto ty : recvIt->paramTypes)
            initParamTypes.push_back(ty);
        }
        auto initFuncType = builder.getFunctionType(initParamTypes, {wrapperType});
        auto savedIP = builder.saveInsertionPoint();
        builder.setInsertionPointToEnd(module.getBody());
        auto initFuncOp = mlir::func::FuncOp::create(builder, location, receiveName, initFuncType);
        auto *entryBlock = initFuncOp.addEntryBlock();
        builder.setInsertionPointToStart(entryBlock);

        FunctionGenerationScope funcScope(*this, initFuncOp);

        auto selfPtr = entryBlock->getArgument(0);

        // Allocate args struct on stack
        auto one64 = mlir::arith::ConstantIntOp::create(builder, location, i64Type, 1);
        auto argsAlloca =
            mlir::LLVM::AllocaOp::create(builder, location, ptrType, argsStructType, one64);

        // Pack self + params into the struct
        auto argsUndef = mlir::LLVM::UndefOp::create(builder, location, argsStructType);
        mlir::Value argsStruct = mlir::LLVM::InsertValueOp::create(
            builder, location, argsUndef, selfPtr, llvm::ArrayRef<int64_t>{0});
        for (size_t pi = 0; pi < recv.params.size(); ++pi) {
          mlir::Value arg = entryBlock->getArgument(pi + 1);
          // Coerce pointer-like Hew params (string, vec, …) to their LLVM
          // storage type (!llvm.ptr) before inserting into the args struct.
          auto storageType = argsFieldTypes[pi + 1];
          if (storageType != arg.getType()) {
            arg = coerceType(arg, storageType, location);
            if (!arg)
              return;
          }
          argsStruct = mlir::LLVM::InsertValueOp::create(
              builder, location, argsStruct, arg,
              llvm::ArrayRef<int64_t>{static_cast<int64_t>(pi + 1)});
        }
        mlir::LLVM::StoreOp::create(builder, location, argsStruct, argsAlloca);

        // Compute args struct size
        auto argsSizeVal = hew::SizeOfOp::create(builder, location, sizeType(),
                                                 mlir::TypeAttr::get(argsStructType));

        // Get body function pointer using func.constant + cast to ptr
        std::string bodyFnName = receiveName + "__body";
        auto bodyFnPtrType = builder.getFunctionType({ptrType, ptrType}, {});
        getOrCreateExternFunc(bodyFnName, bodyFnPtrType);
        auto bodyFnAddr = hew::FuncPtrOp::create(builder, location, ptrType,
                                                 mlir::SymbolRefAttr::get(&context, bodyFnName))
                              .getResult();

        // Call hew_gen_ctx_create(body_fn, args_ptr, args_size) → ctx
        auto ctx = hew::GenCtxCreateOp::create(builder, location, ptrType, bodyFnAddr, argsAlloca,
                                               argsSizeVal)
                       .getResult();

        // Store ctx in state.__gen_frame_N
        auto genFrameGEP = mlir::LLVM::GEPOp::create(
            builder, location, ptrType, stateType, selfPtr,
            llvm::ArrayRef<mlir::LLVM::GEPArg>{0, static_cast<int32_t>(genFrameIdx)});
        mlir::LLVM::StoreOp::create(builder, location, ctx, genFrameGEP);

        // Emit gen-next → null check → wrap/cleanup → return
        emitGenNextResult(ctx, selfPtr, stateType, genFrameIdx, yieldType, wrapperType, location);

        builder.restoreInsertionPoint(savedIP);
      }

      // ─── 3. Next handler: {i8,Y} ActorName_method__next(ptr self) ───
      {
        std::string nextHandlerName = receiveName + "__next";
        auto nextFuncType = builder.getFunctionType({ptrType}, {wrapperType});
        auto savedIP = builder.saveInsertionPoint();
        builder.setInsertionPointToEnd(module.getBody());
        auto nextFuncOp =
            mlir::func::FuncOp::create(builder, location, nextHandlerName, nextFuncType);
        auto *entryBlock = nextFuncOp.addEntryBlock();
        builder.setInsertionPointToStart(entryBlock);

        FunctionGenerationScope funcScope(*this, nextFuncOp);

        auto selfPtr = entryBlock->getArgument(0);

        // Load gen ctx from state.__gen_frame_N
        auto genFrameGEP = mlir::LLVM::GEPOp::create(
            builder, location, ptrType, stateType, selfPtr,
            llvm::ArrayRef<mlir::LLVM::GEPArg>{0, static_cast<int32_t>(genFrameIdx)});
        auto ctx = mlir::LLVM::LoadOp::create(builder, location, ptrType, genFrameGEP);

        // Emit gen-next → null check → wrap/cleanup → return
        emitGenNextResult(ctx, selfPtr, stateType, genFrameIdx, yieldType, wrapperType, location);

        builder.restoreInsertionPoint(savedIP);
      }

      continue; // Skip normal receive fn generation for generators
    }

    // ─── Non-generator receive fn: normal handler generation ───
    // Look up stored param/return types from registration (avoid re-converting)
    auto recvIt = std::find_if(actorInfo.receiveFns.begin(), actorInfo.receiveFns.end(),
                               [&](const ActorReceiveInfo &r) { return r.name == recv.name; });

    llvm::SmallVector<mlir::Type, 4> paramTypes;
    paramTypes.push_back(ptrType); // self: ptr
    if (recvIt != actorInfo.receiveFns.end()) {
      for (auto ty : recvIt->paramTypes)
        paramTypes.push_back(ty);
    }

    llvm::SmallVector<mlir::Type, 1> resultTypes;
    if (recvIt != actorInfo.receiveFns.end() && recvIt->returnType.has_value())
      resultTypes.push_back(*recvIt->returnType);

    auto funcType = builder.getFunctionType(paramTypes, resultTypes);
    auto savedIP = builder.saveInsertionPoint();
    builder.setInsertionPointToEnd(module.getBody());
    auto funcOp = mlir::func::FuncOp::create(builder, location, receiveName, funcType);
    auto *entryBlock = funcOp.addEntryBlock();
    builder.setInsertionPointToStart(entryBlock);

    // Create scopes for this function
    SymbolTableScopeT varScope(symbolTable);
    MutableTableScopeT mutScope(mutableVars);

    FunctionGenerationScope funcScope(*this, funcOp);
    if (recv.return_type)
      currentFunctionReturnTypeExpr = &recv.return_type->value;
    auto prevFuncLevelDropScopeBase = funcLevelDropScopeBase;
    funcLevelDropScopeBase = dropScopes.size();

    // Bind actor state pointer as internal variable for field access
    auto selfPtr = entryBlock->getArgument(0);
    declareVariable("self", selfPtr);

    // Push a drop scope for receive handler params.  Actor message args
    // are deep-copied by the sender (strdup/clone), so the handler owns
    // them and must free at scope exit.
    pushDropScope();

    // Bind message parameters (starting at argument 1)
    {
      size_t pi = 0;
      for (const auto &param : recv.params) {
        auto argVal = entryBlock->getArgument(pi + 1);
        bindParam(param, argVal);
        // Register ActorRef<T> parameters for actor method dispatch
        {
          auto resolveAliasExpr = [this](llvm::StringRef name) {
            return resolveTypeAliasExpr(name);
          };
          auto actorName = typeExprToActorName(param.ty.value, resolveAliasExpr);
          if (!actorName.empty())
            actorVarTypes[param.name] = actorName;
        }

        // Register drops for owned types (deep-copied at actor boundary)
        auto argType = argVal.getType();
        if (auto dropFn = dropFuncForMLIRType(argType, /*includeStructTypes=*/true);
            !dropFn.empty()) {
          bool isUserDrop = false;
          if (auto structTy = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(argType);
              structTy && structTy.isIdentified()) {
            isUserDrop = userDropFuncs.find(structTy.getName().str()) != userDropFuncs.end();
          }
          registerDroppable(param.name, dropFn, isUserDrop);
        }

        ++pi;
      }
    }

    // Exclude trailing-expression variables from param drops so returning
    // an owned param doesn't free it before the return.  Uses the same
    // funcLevelDropExcludeVars mechanism as normal functions.
    auto prevFuncLevelDropExcludeVars = std::move(funcLevelDropExcludeVars);
    auto prevFuncLevelDropExcludeValues = std::move(funcLevelDropExcludeValues);
    auto prevFuncLevelDropExcludeResolvedNames = std::move(funcLevelDropExcludeResolvedNames);
    auto prevFuncLevelEarlyReturnExcludeValues = std::move(funcLevelEarlyReturnExcludeValues);
    auto prevFuncLevelEarlyReturnExcludeResolvedNames =
        std::move(funcLevelEarlyReturnExcludeResolvedNames);
    funcLevelDropExcludeVars.clear();
    funcLevelDropExcludeValues.clear();
    funcLevelDropExcludeResolvedNames.clear();
    funcLevelEarlyReturnExcludeValues.clear();
    funcLevelEarlyReturnExcludeResolvedNames.clear();
    if (recv.body.trailing_expr) {
      if (auto *id = std::get_if<ast::ExprIdentifier>(&recv.body.trailing_expr->value.kind))
        funcLevelDropExcludeVars.insert({id->name, 0});
    } else if (!recv.body.stmts.empty()) {
      const auto &last = recv.body.stmts.back()->value;
      if (auto *es = std::get_if<ast::StmtExpression>(&last.kind)) {
        if (auto *id = std::get_if<ast::ExprIdentifier>(&es->expr.value.kind))
          funcLevelDropExcludeVars.insert({id->name, 0});
      }
    }
    resolveFunctionDropExclusionCandidates();

    // Generate function body
    mlir::Value bodyValue = generateBlock(recv.body, /*statementPosition=*/resultTypes.empty());

    // Pop the param drop scope — popDropScope emits drops for owned params
    // (excluding any trailing-expression variable via funcLevelDropExcludeVars)
    popDropScope();
    funcLevelDropExcludeVars = std::move(prevFuncLevelDropExcludeVars);
    funcLevelDropExcludeValues = std::move(prevFuncLevelDropExcludeValues);
    funcLevelDropExcludeResolvedNames = std::move(prevFuncLevelDropExcludeResolvedNames);
    funcLevelEarlyReturnExcludeValues = std::move(prevFuncLevelEarlyReturnExcludeValues);
    funcLevelEarlyReturnExcludeResolvedNames =
        std::move(prevFuncLevelEarlyReturnExcludeResolvedNames);

    // Emit return
    if (!hasRealTerminator(builder.getInsertionBlock())) {
      if (!resultTypes.empty() && bodyValue) {
        bodyValue = coerceTypeForSink(bodyValue, resultTypes[0], location);
        mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{bodyValue});
      } else {
        mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{});
      }
    }

    funcLevelDropScopeBase = prevFuncLevelDropScopeBase;
    builder.restoreInsertionPoint(savedIP);
  }

  // 2b. Generate init function if the actor has an init block
  //     void ActorName_init(ptr state)
  if (decl.init) {
    auto location = firstBodyLoc(decl.init->body, actorLoc);
    std::string initName = actorName + "_init";
    auto initFuncType = builder.getFunctionType({ptrType}, {});

    auto savedIP = builder.saveInsertionPoint();
    builder.setInsertionPointToEnd(module.getBody());
    auto initFuncOp = mlir::func::FuncOp::create(builder, location, initName, initFuncType);
    auto *entryBlock = initFuncOp.addEntryBlock();
    builder.setInsertionPointToStart(entryBlock);

    SymbolTableScopeT varScope(symbolTable);
    MutableTableScopeT mutScope(mutableVars);

    FunctionGenerationScope funcScope(*this, initFuncOp);

    // Bind actor state pointer as internal variable for field access
    auto selfPtr = entryBlock->getArgument(0);
    declareVariable("self", selfPtr);

    // Bind init parameters by loading them from their hidden state-struct fields.
    // The hidden fields sit after the user-declared fields; the GEP helpers in
    // MLIRGenExpr/MLIRGenStmt can also access them by name, but we bind them
    // explicitly here so that the init body can use them as ordinary variables.
    {
      const auto &actorIt = actorRegistry.find(actorName);
      if (actorIt != actorRegistry.end()) {
        const auto &aInfo = actorIt->second;
        auto structType = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(aInfo.stateType);
        auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
        for (size_t pi = 0; pi < decl.init->params.size(); ++pi) {
          const auto &param = decl.init->params[pi];
          size_t fieldIdx = aInfo.numUserFields + pi;
          auto fieldType = aInfo.fieldHewTypes[fieldIdx];
          auto storageType = toLLVMStorageType(fieldType);
          auto fieldPtr = mlir::LLVM::GEPOp::create(
              builder, location, ptrType, structType, selfPtr,
              llvm::ArrayRef<mlir::LLVM::GEPArg>{0, static_cast<int32_t>(fieldIdx)});
          mlir::Value paramVal =
              mlir::LLVM::LoadOp::create(builder, location, storageType, fieldPtr).getResult();
          // Coerce pointer storage types back to semantic types (e.g., string_ref).
          if (storageType != fieldType) {
            paramVal = coerceType(paramVal, fieldType, location);
            if (!paramVal)
              return;
          }
          bindParam(param, paramVal);
        }
      }
    }

    // Generate init block body
    generateBlock(decl.init->body, /*statementPosition=*/true);

    // Ensure terminator
    if (!hasRealTerminator(builder.getInsertionBlock()))
      mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{});

    builder.restoreInsertionPoint(savedIP);
  }

  // 2c. Generate terminate function if the actor has a terminate block
  //     void ActorName_terminate(ptr state)
  if (decl.terminate) {
    auto location = firstBodyLoc(decl.terminate->body, actorLoc);
    std::string terminateName = actorName + "_terminate";
    auto terminateFuncType = builder.getFunctionType({ptrType}, {});

    auto savedIP = builder.saveInsertionPoint();
    builder.setInsertionPointToEnd(module.getBody());
    auto terminateFuncOp =
        mlir::func::FuncOp::create(builder, location, terminateName, terminateFuncType);
    auto *entryBlock = terminateFuncOp.addEntryBlock();
    builder.setInsertionPointToStart(entryBlock);

    SymbolTableScopeT varScope(symbolTable);
    MutableTableScopeT mutScope(mutableVars);

    FunctionGenerationScope funcScope(*this, terminateFuncOp);

    // Bind actor state pointer as internal variable for field access
    auto selfPtr = entryBlock->getArgument(0);
    declareVariable("self", selfPtr);

    // Generate terminate block body
    generateBlock(decl.terminate->body, /*statementPosition=*/true);

    // Ensure terminator
    if (!hasRealTerminator(builder.getInsertionBlock()))
      mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{});

    builder.restoreInsertionPoint(savedIP);
  }

  // 3. Generate dispatch function:
  //    void ActorName_dispatch(ptr state, i32 msg_type, ptr data, size_t data_size)
  {
    auto location =
        decl.receive_fns.size() == 1 ? receiveSourceLoc(decl.receive_fns.front().name) : actorLoc;
    std::string dispatchName = actorName + "_dispatch";
    auto i32Type = builder.getI32Type();
    auto dispatchType = builder.getFunctionType({ptrType, i32Type, ptrType, sizeType()}, {});

    auto savedIP = builder.saveInsertionPoint();
    builder.setInsertionPointToEnd(module.getBody());
    auto dispatchOp = mlir::func::FuncOp::create(builder, location, dispatchName, dispatchType);
    auto *entryBlock = dispatchOp.addEntryBlock();
    builder.setInsertionPointToStart(entryBlock);

    auto stateArg = entryBlock->getArgument(0);   // ptr (state)
    auto msgTypeArg = entryBlock->getArgument(1); // i32 (msg_type)
    auto dataArg = entryBlock->getArgument(2);    // ptr (data)

    auto dataSizeArg = entryBlock->getArgument(3); // size_t (data_size)

    // Check if any handler has a wire-typed single parameter
    bool hasWireHandlers = false;
    for (size_t i = 0; i < actorInfo.receiveFns.size(); ++i) {
      const auto &recvFn = actorInfo.receiveFns[i];
      if (recvFn.paramTypes.size() == 1) {
        if (auto st = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(recvFn.paramTypes[0])) {
          auto name = st.getName();
          if (!name.empty() && wireStructNames.count(name.str()))
            hasWireHandlers = true;
        }
      }
    }

    if (hasWireHandlers) {
      // Generate explicit dispatch with wire decode for wire handlers
      for (size_t i = 0; i < actorInfo.receiveFns.size(); ++i) {
        const auto &recvFn = actorInfo.receiveFns[i];
        std::string recvHandlerName = actorName + "_" + recvFn.name;

        // Ensure handler function is declared
        llvm::SmallVector<mlir::Type, 4> recvParamTypes;
        recvParamTypes.push_back(ptrType); // self/state
        for (const auto &pt : recvFn.paramTypes)
          recvParamTypes.push_back(pt);
        llvm::SmallVector<mlir::Type, 1> recvResultTypes;
        if (recvFn.returnType.has_value())
          recvResultTypes.push_back(*recvFn.returnType);
        auto recvFuncType = builder.getFunctionType(recvParamTypes, recvResultTypes);
        getOrCreateExternFunc(recvHandlerName, recvFuncType);

        // if (msg_type == i)
        auto msgIdx =
            mlir::arith::ConstantIntOp::create(builder, location, i32Type, static_cast<int64_t>(i));
        auto cond = mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::eq,
                                                msgTypeArg, msgIdx);
        auto ifOp = mlir::scf::IfOp::create(builder, location, cond, /*withElseRegion=*/false);
        builder.setInsertionPointToStart(&ifOp.getThenRegion().front());

        // Check if this handler uses wire encoding
        const WireWrapperNames *wireNames = nullptr;
        if (recvFn.paramTypes.size() == 1) {
          if (auto st = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(recvFn.paramTypes[0])) {
            auto name = st.getName();
            if (!name.empty()) {
              auto it = wireStructNames.find(name.str());
              if (it != wireStructNames.end())
                wireNames = &it->second;
            }
          }
        }

        llvm::SmallVector<mlir::Value, 4> callArgs;
        callArgs.push_back(stateArg);

        if (wireNames && recvFn.paramTypes.size() == 1) {
          // Wire decode path: create bytes from raw data, then decode
          // bytes = hew_vec_from_raw_bytes(data, data_size)
          auto vecFromRawType = builder.getFunctionType({ptrType, sizeType()}, {ptrType});
          getOrCreateExternFunc("hew_vec_from_raw_bytes", vecFromRawType);
          auto bytesVec = mlir::func::CallOp::create(builder, location, "hew_vec_from_raw_bytes",
                                                     mlir::TypeRange{ptrType},
                                                     mlir::ValueRange{dataArg, dataSizeArg})
                              .getResult(0);

          // msg = decode_wrapper(bytes) → struct
          auto decodeFuncType = builder.getFunctionType({ptrType}, {recvFn.paramTypes[0]});
          getOrCreateExternFunc(wireNames->decodeName, decodeFuncType);
          auto decoded = mlir::func::CallOp::create(builder, location, wireNames->decodeName,
                                                    mlir::TypeRange{recvFn.paramTypes[0]},
                                                    mlir::ValueRange{bytesVec})
                             .getResult(0);

          // Free the temporary bytes vec
          auto vecFreeType = builder.getFunctionType({ptrType}, {});
          getOrCreateExternFunc("hew_vec_free", vecFreeType);
          mlir::func::CallOp::create(builder, location, "hew_vec_free", mlir::TypeRange{},
                                     mlir::ValueRange{bytesVec});

          callArgs.push_back(decoded);
        } else {
          // Non-wire path: load args from data buffer (same as ReceiveOpLowering)
          if (recvFn.paramTypes.size() == 1) {
            auto loaded =
                mlir::LLVM::LoadOp::create(builder, location, recvFn.paramTypes[0], dataArg);
            callArgs.push_back(loaded);
          } else if (recvFn.paramTypes.size() > 1) {
            llvm::SmallVector<mlir::Type, 4> fieldTypes(recvFn.paramTypes.begin(),
                                                        recvFn.paramTypes.end());
            auto packType = mlir::LLVM::LLVMStructType::getLiteral(&context, fieldTypes);
            auto packed = mlir::LLVM::LoadOp::create(builder, location, packType, dataArg);
            for (unsigned pi = 0; pi < fieldTypes.size(); ++pi) {
              auto field = mlir::LLVM::ExtractValueOp::create(
                  builder, location, packed, llvm::ArrayRef<int64_t>{static_cast<int64_t>(pi)});
              callArgs.push_back(field);
            }
          }
        }

        // Call handler
        mlir::func::CallOp::create(builder, location, recvHandlerName, recvResultTypes, callArgs);

        builder.setInsertionPointAfter(ifOp);
      }
    } else {
      // No wire handlers: use standard hew.receive op
      llvm::SmallVector<mlir::Attribute, 4> handlerRefs;
      for (size_t i = 0; i < actorInfo.receiveFns.size(); ++i) {
        const auto &recvFn = actorInfo.receiveFns[i];
        std::string recvHandlerName = actorName + "_" + recvFn.name;

        llvm::SmallVector<mlir::Type, 4> recvParamTypes;
        recvParamTypes.push_back(ptrType); // self
        for (const auto &pt : recvFn.paramTypes)
          recvParamTypes.push_back(pt);
        llvm::SmallVector<mlir::Type, 1> recvResultTypes;
        if (recvFn.returnType.has_value())
          recvResultTypes.push_back(*recvFn.returnType);
        auto recvFuncType = builder.getFunctionType(recvParamTypes, recvResultTypes);
        getOrCreateExternFunc(recvHandlerName, recvFuncType);

        handlerRefs.push_back(mlir::FlatSymbolRefAttr::get(&context, recvHandlerName));
      }

      hew::ReceiveOp::create(builder, location, stateArg, msgTypeArg, dataArg, dataSizeArg,
                             builder.getArrayAttr(handlerRefs));
    }

    // Return void from dispatch
    mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{});
    builder.restoreInsertionPoint(savedIP);
  }

  currentActorName = prevActorName;
}

// ============================================================================
// Coalesce key function generation
// ============================================================================

/// Generates a coalesce key function for actors with coalesce overflow policy.
/// Signature: u64 key_fn(i32 msg_type, ptr data, u64 data_size)
/// For each receive handler, checks if msg_type matches and extracts the
/// coalesce key field value as u64.
void MLIRGen::generateCoalesceKeyFn(const ActorInfo &actorInfo, const std::string &fnName) {
  auto location = actorInfo.sourceLoc.value_or(currentLoc);
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();

  auto funcType = builder.getFunctionType({i32Type, ptrType, i64Type}, {i64Type});

  auto savedIP = builder.saveInsertionPoint();
  builder.setInsertionPointToEnd(module.getBody());
  auto funcOp = mlir::func::FuncOp::create(builder, location, fnName, funcType);
  auto *entryBlock = funcOp.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto msgType = entryBlock->getArgument(0);
  auto dataPtr = entryBlock->getArgument(1);

  // Default: return msg_type as u64 (so each msg_type is its own key bucket)
  auto defaultKey = mlir::arith::ExtUIOp::create(builder, location, i64Type, msgType);

  // For each receive fn, check if it has the coalesce key field as a parameter
  // If so, switch on msg_type, extract the field, return as u64
  mlir::Value result = defaultKey;

  for (size_t i = 0; i < actorInfo.receiveFns.size(); ++i) {
    const auto &recv = actorInfo.receiveFns[i];

    // Build the message struct type for this handler
    llvm::SmallVector<mlir::Type, 4> paramTypes;
    int keyFieldIdx = -1;
    mlir::Type keyFieldType;

    // Find the coalesce key field in the params by name.
    for (size_t pi = 0; pi < recv.paramNames.size(); ++pi) {
      if (recv.paramNames[pi] == actorInfo.coalesceKey) {
        keyFieldIdx = static_cast<int>(pi);
        keyFieldType = recv.paramTypes[pi];
        break;
      }
    }
    // Fallback: use first param if name not found
    if (keyFieldIdx < 0 && !recv.paramTypes.empty()) {
      keyFieldIdx = 0;
      keyFieldType = recv.paramTypes[0];
    }

    if (keyFieldIdx < 0)
      continue;

    auto uloc = location;
    auto msgTypeConst =
        mlir::arith::ConstantIntOp::create(builder, uloc, i32Type, static_cast<int64_t>(i));
    auto isThisMsg = mlir::arith::CmpIOp::create(builder, uloc, mlir::arith::CmpIPredicate::eq,
                                                 msgType, msgTypeConst);

    // Build the struct type for this message's packed args
    llvm::SmallVector<mlir::Type, 4> msgStructFields;
    for (const auto &pt : recv.paramTypes) {
      msgStructFields.push_back(pt);
    }
    auto msgStructType = mlir::LLVM::LLVMStructType::getLiteral(&context, msgStructFields);

    auto ifOp = mlir::scf::IfOp::create(builder, uloc, i64Type, isThisMsg, /*withElseRegion=*/true);

    // Then: extract key field
    builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
    auto fieldGEP = mlir::LLVM::GEPOp::create(builder, uloc, ptrType, msgStructType, dataPtr,
                                              llvm::ArrayRef<mlir::LLVM::GEPArg>{0, keyFieldIdx});

    mlir::Value keyVal;
    if (keyFieldType == i32Type) {
      auto loaded = mlir::LLVM::LoadOp::create(builder, uloc, i32Type, fieldGEP);
      keyVal = mlir::arith::ExtUIOp::create(builder, uloc, i64Type, loaded);
    } else if (keyFieldType == i64Type) {
      keyVal = mlir::LLVM::LoadOp::create(builder, uloc, i64Type, fieldGEP);
    } else if (keyFieldType == ptrType) {
      auto loaded = mlir::LLVM::LoadOp::create(builder, uloc, ptrType, fieldGEP);
      keyVal = mlir::LLVM::PtrToIntOp::create(builder, uloc, i64Type, loaded);
    } else {
      keyVal = mlir::arith::ExtUIOp::create(builder, uloc, i64Type, msgType);
    }
    mlir::scf::YieldOp::create(builder, uloc, keyVal);

    // Else: pass through previous result
    builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
    mlir::scf::YieldOp::create(builder, uloc, result);

    builder.setInsertionPointAfter(ifOp);
    result = ifOp.getResult(0);
  }

  mlir::func::ReturnOp::create(builder, location, result);

  builder.restoreInsertionPoint(savedIP);
}

// ============================================================================
// Spawn expression generation
// ============================================================================

mlir::Value MLIRGen::generateSpawnExpr(const ast::ExprSpawn &expr) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);

  // The target is an identifier (actor name) or a field access (module.ActorName)
  std::string actorName;
  if (auto *identExpr = std::get_if<ast::ExprIdentifier>(&expr.target->value.kind)) {
    actorName = identExpr->name;
  } else if (auto *fieldExpr = std::get_if<ast::ExprFieldAccess>(&expr.target->value.kind)) {
    // spawn module.ActorName(...) — use the field (actor name) for registry lookup
    actorName = fieldExpr->field;
  }

  if (actorName.empty()) {
    ++errorCount_;
    emitError(location) << "spawn requires an actor name";
    return nullptr;
  }

  // Check if this is a supervisor spawn (not a regular actor)
  if (supervisorChildren.count(actorName)) {
    // Call SupervisorName_init() which creates, populates, and starts the
    // supervisor with all its children.  The init function is generated in
    // a later pass (Pass 2) but the symbol reference resolves before
    // module verification.
    std::string initName = actorName + "_init";
    auto call = mlir::func::CallOp::create(builder, location, initName, mlir::TypeRange{ptrType},
                                           mlir::ValueRange{});
    return call.getResult(0);
  }

  auto it = actorRegistry.find(actorName);
  if (it == actorRegistry.end()) {
    ++errorCount_;
    emitError(location) << "unknown actor type: " << actorName;
    return nullptr;
  }
  const auto &actorInfo = it->second;

  // Helper: produce a zero/default value for a Hew MLIR type.
  auto makeDefaultVal = [&](mlir::Type hewType) -> mlir::Value {
    if (auto vecType = mlir::dyn_cast<hew::VecType>(hewType))
      return hew::VecNewOp::create(builder, location, vecType).getResult();
    if (auto hmType = mlir::dyn_cast<hew::HashMapType>(hewType))
      return hew::HashMapNewOp::create(builder, location, hmType).getResult();
    if (mlir::isa<hew::StringRefType>(hewType)) {
      auto symName = getOrCreateGlobalString("");
      return hew::ConstantOp::create(builder, location, hew::StringRefType::get(&context),
                                     builder.getStringAttr(symName))
          .getResult();
    }
    return createDefaultValue(builder, location, toLLVMStorageType(hewType));
  };

  // Generate init argument values.
  // Layout in initArgVals: [user_field_0..N, init_param_0..M]
  //   - Actors with init params: user fields are zero-initialized; spawn args fill init-param
  //   slots.
  //   - Actors without init params: spawn args fill user field slots directly (original behaviour).
  llvm::SmallVector<mlir::Value, 4> initArgVals;
  if (!actorInfo.initParamNames.empty()) {
    // Zero-initialize all user state fields.
    for (size_t i = 0; i < actorInfo.numUserFields; ++i)
      initArgVals.push_back(makeDefaultVal(actorInfo.fieldHewTypes[i]));
    // Append spawn call args as init-param values.
    for (const auto &[fieldName, argExpr] : expr.args) {
      auto argVal = generateExpression(argExpr->value);
      if (!argVal)
        return nullptr;
      initArgVals.push_back(argVal);
    }
    // Pad any missing init-param slots with defaults.
    size_t totalExpected = actorInfo.fieldHewTypes.size();
    for (size_t i = initArgVals.size(); i < totalExpected; ++i)
      initArgVals.push_back(makeDefaultVal(actorInfo.fieldHewTypes[i]));
  } else {
    // No init params: spawn args initialize user fields directly.
    for (const auto &[fieldName, argExpr] : expr.args) {
      auto argVal = generateExpression(argExpr->value);
      if (!argVal)
        return nullptr;
      initArgVals.push_back(argVal);
    }
    // Zero-arg spawn: pad missing user fields with Go-style zero values.
    size_t numUserFields = actorInfo.fieldHewTypes.size();
    for (size_t i = initArgVals.size(); i < numUserFields; ++i)
      initArgVals.push_back(makeDefaultVal(actorInfo.fieldHewTypes[i]));
  }

  // Add zero-initialized hidden gen frame fields (ptr null)
  // These are appended after user fields by registerActorDecl
  {
    std::string prefix = actorName + ".";
    size_t genFrameCount = 0;
    for (const auto &[key, idx] : genFrameFieldIdx) {
      if (key.compare(0, prefix.size(), prefix) == 0)
        ++genFrameCount;
    }
    for (size_t i = 0; i < genFrameCount; ++i)
      initArgVals.push_back(mlir::LLVM::ZeroOp::create(builder, location, ptrType));
  }

  // Emit hew.actor_spawn — the lowering pass handles alloca, field stores,
  // sizeof computation, dispatch ptr, and the runtime call.
  std::string dispatchName = actorName + "_dispatch";

  mlir::IntegerAttr mailboxCapAttr;
  if (actorInfo.mailboxCapacity.has_value()) {
    mailboxCapAttr = builder.getI64IntegerAttr(static_cast<int64_t>(*actorInfo.mailboxCapacity));
  }

  mlir::IntegerAttr overflowPolicyAttr;
  mlir::FlatSymbolRefAttr coalesceKeyFnAttr;
  mlir::IntegerAttr coalesceFallbackAttr;

  if (actorInfo.overflowPolicy != 0) {
    overflowPolicyAttr = builder.getI32IntegerAttr(static_cast<int32_t>(actorInfo.overflowPolicy));
  }
  if (actorInfo.overflowPolicy == 5 && !actorInfo.coalesceKey.empty()) {
    // Generate coalesce key function
    std::string keyFnName = actorName + "_coalesce_key";
    generateCoalesceKeyFn(actorInfo, keyFnName);
    coalesceKeyFnAttr = mlir::SymbolRefAttr::get(&context, keyFnName);

    int32_t fallback = actorInfo.coalesceFallback;
    if (fallback == 0)
      fallback = 1; // default fallback = drop_new
    coalesceFallbackAttr = builder.getI32IntegerAttr(fallback);
  }

  auto spawnOp = hew::ActorSpawnOp::create(
      builder, location, hew::TypedActorRefType::get(&context, builder.getStringAttr(actorName)),
      builder.getStringAttr(actorName), mlir::SymbolRefAttr::get(&context, dispatchName),
      mlir::TypeAttr::get(actorInfo.stateType), initArgVals, mailboxCapAttr, overflowPolicyAttr,
      coalesceKeyFnAttr, coalesceFallbackAttr);

  auto result = spawnOp.getResult();

  // Register with enclosing scope, if any.
  if (currentScopePtr) {
    auto i32Type = builder.getI32Type();
    hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{i32Type},
                               mlir::SymbolRefAttr::get(&context, "hew_scope_spawn"),
                               mlir::ValueRange{currentScopePtr, result});
  }

  // Schedule periodic timers for #[every(duration)] receive fns.
  for (size_t i = 0; i < actorInfo.receiveFns.size(); ++i) {
    const auto &recvFn = actorInfo.receiveFns[i];
    if (recvFn.periodicIntervalNs.has_value()) {
      auto i32Type = builder.getI32Type();
      auto i64Type = builder.getI64Type();

      auto msgTypeVal =
          mlir::arith::ConstantIntOp::create(builder, location, i32Type, static_cast<int64_t>(i));

      // Convert nanoseconds to milliseconds for the runtime.
      int64_t intervalMs = *recvFn.periodicIntervalNs / 1'000'000;
      if (intervalMs <= 0)
        intervalMs = 1; // minimum 1ms
      auto intervalVal = mlir::arith::ConstantIntOp::create(builder, location, i64Type, intervalMs);

      // Cast typed_actor_ref → ptr for the runtime call.
      auto actorPtr = hew::BitcastOp::create(builder, location, ptrType, result).getResult();

      // void* hew_actor_schedule_periodic(ptr actor, i32 msg_type, i64 interval_ms)
      auto schedFnType = builder.getFunctionType({ptrType, i32Type, i64Type}, {ptrType});
      getOrCreateExternFunc("hew_actor_schedule_periodic", schedFnType);
      mlir::func::CallOp::create(builder, location, "hew_actor_schedule_periodic",
                                 mlir::TypeRange{ptrType},
                                 mlir::ValueRange{actorPtr, msgTypeVal, intervalVal});
    }
  }

  return result;
}

// ============================================================================
// SpawnLambdaActor expression generation
// ============================================================================

mlir::Value MLIRGen::generateSpawnLambdaActorExpr(const ast::ExprSpawnLambdaActor &expr) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i32Type = builder.getI32Type();

  unsigned actorId = lambdaActorCounter++;
  std::string actorName = "__lambda_actor_" + std::to_string(actorId);

  // ── Lambda lifting: collect free variables from the body ──
  struct CapturedVar {
    std::string name;
    mlir::Value value;
  };
  std::vector<CapturedVar> capturedVars;
  if (expr.body) {
    std::set<std::string> bound;
    for (const auto &param : expr.params)
      bound.insert(param.name);
    bound.insert("self");
    bound.insert("println_int");
    bound.insert("println_str");
    bound.insert("print_int");
    bound.insert("print_str");
    std::set<std::string> freeVars;
    collectFreeVarsInExpr(expr.body->value, bound, freeVars);
    for (const auto &fv : freeVars) {
      if (module.lookupSymbol<mlir::func::FuncOp>(fv) ||
          module.lookupSymbol<mlir::func::FuncOp>(mangleName(currentModulePath, "", fv)))
        continue; // module-level function
      if (variantLookup.count(fv))
        continue; // enum variant constructor
      if (moduleConstants.count(fv))
        continue; // module-level constant
      auto val = lookupVariable(fv);
      if (val)
        capturedVars.push_back({fv, val});
    }
  }

  // Build state struct with captured variable types as fields
  auto stateType = mlir::LLVM::LLVMStructType::getIdentified(&context, actorName + "_state");
  llvm::SmallVector<mlir::Type, 4> stateFields;
  StructTypeInfo stInfo;
  stInfo.name = actorName;
  for (size_t i = 0; i < capturedVars.size(); ++i) {
    auto ty = toLLVMStorageType(capturedVars[i].value.getType());
    stateFields.push_back(ty);
    StructFieldInfo fi;
    fi.name = capturedVars[i].name;
    fi.semanticType = capturedVars[i].value.getType();
    fi.type = ty;
    fi.index = static_cast<unsigned>(i);
    stInfo.fields.push_back(std::move(fi));
  }
  (void)stateType.setBody(stateFields, /*isPacked=*/false);
  stInfo.mlirType = stateType;
  structTypes[actorName] = std::move(stInfo);

  // Collect parameter types for the receive function
  llvm::SmallVector<mlir::Type, 4> recvParamTypes;
  recvParamTypes.push_back(ptrType); // self
  ActorReceiveInfo recvInfo;
  recvInfo.name = "receive";
  for (const auto &param : expr.params) {
    if (!param.ty) {
      ++errorCount_;
      emitError(location) << "actor receive parameter '" << param.name
                          << "' has no type annotation";
      return nullptr;
    }
    auto ty = convertType(param.ty->value);
    recvParamTypes.push_back(ty);
    recvInfo.paramTypes.push_back(ty);
  }

  // Generate receive function
  std::string receiveName = actorName + "_receive";
  auto recvFuncType = builder.getFunctionType(recvParamTypes, {});

  auto savedIP = builder.saveInsertionPoint();
  builder.setInsertionPointToEnd(module.getBody());
  auto recvFuncOp = mlir::func::FuncOp::create(builder, location, receiveName, recvFuncType);
  auto *recvEntry = recvFuncOp.addEntryBlock();
  builder.setInsertionPointToStart(recvEntry);

  {
    SymbolTableScopeT varScope(symbolTable);
    MutableTableScopeT mutScope(mutableVars);
    FunctionGenerationScope funcScope(*this, recvFuncOp);
    auto prevFuncLevelDropScopeBase = funcLevelDropScopeBase;
    funcLevelDropScopeBase = dropScopes.size();

    // Bind actor state pointer as internal variable for field access
    auto selfPtr = recvEntry->getArgument(0);
    declareVariable("self", selfPtr);
    {
      size_t pi = 0;
      for (const auto &param : expr.params) {
        declareVariable(param.name, recvEntry->getArgument(pi + 1));
        ++pi;
      }
    }

    // Load captured variables from the actor state struct and bind them.
    // Use declareMutableVariable to shadow any outer-scope mutable bindings
    // (which would otherwise reference memrefs from the spawning function).
    for (size_t i = 0; i < capturedVars.size(); ++i) {
      auto fieldPtr =
          mlir::LLVM::GEPOp::create(builder, location, ptrType, stateType, selfPtr,
                                    llvm::ArrayRef<mlir::LLVM::GEPArg>{0, static_cast<int32_t>(i)});
      auto llvmFieldType = toLLVMStorageType(capturedVars[i].value.getType());
      mlir::Value loaded = mlir::LLVM::LoadOp::create(builder, location, llvmFieldType, fieldPtr);
      auto hewType = capturedVars[i].value.getType();
      if (hewType != llvmFieldType)
        loaded = hew::BitcastOp::create(builder, location, hewType, loaded);
      declareMutableVariable(capturedVars[i].name, hewType, loaded);
    }

    if (expr.body) {
      generateExpression(expr.body->value);
    }

    if (!hasRealTerminator(builder.getInsertionBlock()))
      mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{});

    funcLevelDropScopeBase = prevFuncLevelDropScopeBase;
  }
  std::string dispatchName = actorName + "_dispatch";
  auto dispatchType = builder.getFunctionType({ptrType, i32Type, ptrType, sizeType()}, {});
  builder.setInsertionPointToEnd(module.getBody());
  auto dispatchOp = mlir::func::FuncOp::create(builder, location, dispatchName, dispatchType);
  auto *dispEntry = dispatchOp.addEntryBlock();
  builder.setInsertionPointToStart(dispEntry);

  {
    auto stateArg = dispEntry->getArgument(0);
    auto msgTypeArg = dispEntry->getArgument(1);
    auto dataArg = dispEntry->getArgument(2);
    auto dataSizeArg = dispEntry->getArgument(3);

    // Ensure handler function is declared in the module
    llvm::SmallVector<mlir::Type, 4> recvParamTypesDisp;
    recvParamTypesDisp.push_back(ptrType); // self
    for (const auto &pt : recvInfo.paramTypes)
      recvParamTypesDisp.push_back(pt);
    auto recvFuncTypeDisp = builder.getFunctionType(recvParamTypesDisp, {});
    getOrCreateExternFunc(receiveName, recvFuncTypeDisp);

    llvm::SmallVector<mlir::Attribute, 1> handlerRefs;
    handlerRefs.push_back(mlir::FlatSymbolRefAttr::get(&context, receiveName));

    hew::ReceiveOp::create(builder, location, stateArg, msgTypeArg, dataArg, dataSizeArg,
                           builder.getArrayAttr(handlerRefs));
    mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{});
  }

  builder.restoreInsertionPoint(savedIP);

  // Register actor info
  ActorInfo actorInfoEntry;
  actorInfoEntry.name = actorName;
  actorInfoEntry.stateType = stateType;
  actorInfoEntry.receiveFns.push_back(std::move(recvInfo));
  actorRegistry[actorName] = std::move(actorInfoEntry);

  // Spawn the lambda actor via hew.actor_spawn with captured values as init args
  llvm::SmallVector<mlir::Value, 4> initArgVals;
  for (const auto &cv : capturedVars)
    initArgVals.push_back(cv.value);

  auto spawnOp = hew::ActorSpawnOp::create(
      builder, location, hew::ActorRefType::get(&context), builder.getStringAttr(actorName),
      mlir::SymbolRefAttr::get(&context, dispatchName), mlir::TypeAttr::get(stateType), initArgVals,
      /*mailbox_capacity=*/mlir::IntegerAttr{},
      /*overflow_policy=*/mlir::IntegerAttr{},
      /*coalesce_key_fn=*/mlir::FlatSymbolRefAttr{},
      /*coalesce_fallback=*/mlir::IntegerAttr{});

  hasActors = true;
  auto result = spawnOp.getResult();

  // Register with enclosing scope, if any.
  if (currentScopePtr) {
    hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{i32Type},
                               mlir::SymbolRefAttr::get(&context, "hew_scope_spawn"),
                               mlir::ValueRange{currentScopePtr, result});
  }

  return result;
}

// ============================================================================
// Shared helpers for actor send/ask
// ============================================================================

bool MLIRGen::actorBoundarySenderRetainsOwnership(mlir::Type valueType) const {
  if (mlir::isa<hew::StringRefType, hew::VecType, hew::HashMapType, hew::ClosureType>(valueType))
    return true;
  if (auto structTy = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(valueType)) {
    if (structTy.isIdentified()) {
      auto structName = structTy.getName().str();
      return structHasOwnedFields(structName) && !userDropFuncs.count(structName);
    }
  }
  return false;
}

std::optional<llvm::SmallVector<mlir::Value, 4>>
MLIRGen::generateActorCallArgs(const std::vector<ast::CallArg> &args, mlir::Location location,
                               bool retainAllTemporaries) {
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  llvm::SmallVector<mlir::Value, 4> argVals;
  for (const auto &arg : args) {
    const auto &argSpanned = ast::callArgExpr(arg);
    auto val = generateExpression(argSpanned.value);
    if (!val)
      return std::nullopt;
    // Materialize only when the sender keeps an owned copy after the actor
    // boundary. User-Drop structs transfer ownership to the receiver even when
    // passed inline as temporaries.
    if (retainAllTemporaries || actorBoundarySenderRetainsOwnership(val.getType()))
      materializeTemporary(val, argSpanned.value);
    argVals.push_back(val);
  }
  return argVals;
}

/// Emit the gen-next null-check, wrap, cleanup, and return sequence.
/// Shared by generator init handlers and __next handlers.
void MLIRGen::emitGenNextResult(mlir::Value ctx, mlir::Value selfPtr,
                                mlir::LLVM::LLVMStructType stateType, unsigned genFrameIdx,
                                mlir::Type yieldType, mlir::Type wrapperType,
                                mlir::Location location) {
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i64Type = builder.getI64Type();

  auto one64 = mlir::arith::ConstantIntOp::create(builder, location, i64Type, 1);
  auto outSizeAlloca = mlir::LLVM::AllocaOp::create(builder, location, ptrType, i64Type, one64);
  auto valuePtr =
      hew::GenNextOp::create(builder, location, ptrType, ctx, outSizeAlloca).getResult();

  // Check if value_ptr is null (done)
  auto nullCmp = mlir::LLVM::ZeroOp::create(builder, location, ptrType);
  auto isNull = mlir::LLVM::ICmpOp::create(builder, location, mlir::LLVM::ICmpPredicate::eq,
                                           valuePtr, nullCmp);

  auto ifOp =
      mlir::scf::IfOp::create(builder, location, wrapperType, isNull, /*withElseRegion=*/true);

  // Then block (null → done): free gen ctx, clear gen frame in state
  builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
  auto doneWrap = hew::GenWrapDoneOp::create(builder, location, wrapperType);
  hew::GenFreeOp::create(builder, location, ctx);
  auto nullForClear = mlir::LLVM::ZeroOp::create(builder, location, ptrType);
  auto genFrameGEP = mlir::LLVM::GEPOp::create(
      builder, location, ptrType, stateType, selfPtr,
      llvm::ArrayRef<mlir::LLVM::GEPArg>{0, static_cast<int32_t>(genFrameIdx)});
  mlir::LLVM::StoreOp::create(builder, location, nullForClear, genFrameGEP);
  mlir::scf::YieldOp::create(builder, location, doneWrap.getResult());

  // Else block (non-null → has value): load, free malloc'd buf
  builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
  auto loadedVal = mlir::LLVM::LoadOp::create(builder, location, yieldType, valuePtr);
  hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                             mlir::SymbolRefAttr::get(&context, "free"),
                             mlir::ValueRange{valuePtr});
  auto valWrap = hew::GenWrapValueOp::create(builder, location, wrapperType, loadedVal);
  mlir::scf::YieldOp::create(builder, location, valWrap.getResult());

  builder.setInsertionPointAfter(ifOp);
  mlir::func::ReturnOp::create(builder, location, ifOp.getResults());
}

// ============================================================================
// Actor method send: actor.method(args) → hew_actor_send(actor, idx, data, sz)
// ============================================================================

mlir::Value MLIRGen::generateActorMethodSend(mlir::Value actorPtr, const ActorInfo &actorInfo,
                                             const std::string &methodName,
                                             const std::vector<ast::CallArg> &args,
                                             mlir::Location location) {
  // Find receive function index by name
  int64_t msgIdx = -1;
  for (size_t i = 0; i < actorInfo.receiveFns.size(); ++i) {
    if (actorInfo.receiveFns[i].name == methodName) {
      msgIdx = static_cast<int64_t>(i);
      break;
    }
  }

  // Also handle "send" method for lambda actors (msg_type = 0)
  if (msgIdx < 0 && methodName == "send" && !actorInfo.receiveFns.empty()) {
    msgIdx = 0;
  }

  if (msgIdx < 0) {
    ++errorCount_;
    emitError(location) << "unknown receive handler '" << methodName << "' on actor '"
                        << actorInfo.name << "'";
    return nullptr;
  }

  // Check if this is a wire-encoded message (single param that is a #[wire] struct)
  const auto &recvFn = actorInfo.receiveFns[msgIdx];
  const WireWrapperNames *wireNames = nullptr;
  if (recvFn.paramTypes.size() == 1) {
    auto paramType = recvFn.paramTypes[0];
    if (auto structType = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(paramType)) {
      auto name = structType.getName();
      if (!name.empty()) {
        auto it = wireStructNames.find(name.str());
        if (it != wireStructNames.end())
          wireNames = &it->second;
      }
    }
  }

  auto argVals =
      generateActorCallArgs(args, location, /*retainAllTemporaries=*/wireNames != nullptr);
  if (!argVals)
    return nullptr;

  if (wireNames) {
    // Wire send path: encode struct → bytes, send bytes via runtime
    auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
    auto i32Type = builder.getI32Type();

    // Call encode wrapper: Foo_encode_wrapper(struct) → HewVec* (bytes)
    auto encodeFuncType = builder.getFunctionType({recvFn.paramTypes[0]}, {ptrType});
    getOrCreateExternFunc(wireNames->encodeName, encodeFuncType);
    auto bytesVec = mlir::func::CallOp::create(builder, location, wireNames->encodeName,
                                               mlir::TypeRange{ptrType}, *argVals)
                        .getResult(0);

    // Cast actor ref to !llvm.ptr for runtime call
    auto actorPtrCast = hew::BitcastOp::create(builder, location, ptrType, actorPtr).getResult();

    // Call hew_actor_send_wire(actor, msg_type, bytes)
    auto sendWireFuncType = builder.getFunctionType({ptrType, i32Type, ptrType}, {});
    getOrCreateExternFunc("hew_actor_send_wire", sendWireFuncType);
    auto msgTypeVal = mlir::arith::ConstantIntOp::create(builder, location, i32Type,
                                                         static_cast<int64_t>(msgIdx));
    mlir::func::CallOp::create(builder, location, "hew_actor_send_wire", mlir::TypeRange{},
                               mlir::ValueRange{actorPtrCast, msgTypeVal, bytesVec});
  } else if (mlir::isa<mlir::IntegerType>(actorPtr.getType())) {
    // Remote dispatch: target is a PID (i64 from Node::lookup).
    // Route through hew_actor_send_by_id which handles both local and
    // remote delivery transparently via the node mesh.
    hew::ActorSendOp::create(builder, location, actorPtr,
                             builder.getI32IntegerAttr(static_cast<int32_t>(msgIdx)), *argVals);
  } else {
    // Standard path: hew.actor_send — the lowering pass handles arg packing
    hew::ActorSendOp::create(builder, location, actorPtr,
                             builder.getI32IntegerAttr(static_cast<int32_t>(msgIdx)), *argVals);
  }

  // Ownership at the actor boundary depends on the transport:
  //
  // Wire sends: the struct is serialized into an independent bytes buffer.
  // No pointer sharing — sender always retains ownership and drops normally.
  //
  // Non-wire sends (deepCopyOwnedArgs in codegen.cpp): String, Vec, HashMap,
  // Closure, and struct fields thereof are deep-copied — sender drops normally.
  // Handle fields are transferred with source null-out — sender drop is a
  // no-op for those.  Everything else passes the raw pointer through
  // (shared) — sender must NOT drop.
  if (!wireNames) {
    for (size_t i = 0; i < args.size() && i < argVals->size(); ++i) {
      const auto &argSpanned = ast::callArgExpr(args[i]);
      auto *identExpr = std::get_if<ast::ExprIdentifier>(&argSpanned.value.kind);
      if (!identExpr)
        continue;
      auto argType = (*argVals)[i].getType();
      if (mlir::isa<hew::StringRefType, hew::VecType, hew::HashMapType, hew::ClosureType>(argType))
        continue;
      // Auto-field-drop structs retain an independent sender copy after the
      // deep-copy boundary. User-Drop structs still transfer ownership to the
      // receiver, so the sender must not keep dropping them.
      if (actorBoundarySenderRetainsOwnership(argType)) {
        if (auto structTy = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(argType);
            structTy && structTy.isIdentified())
          nullOutTransferredHandleFields(identExpr->name, location);
        continue;
      }
      unregisterDroppable(identExpr->name);
    }
  }

  return nullptr; // send is void
}

// ============================================================================
// Actor method ask: await actor.method(args) → hew_actor_ask(actor, idx, data, sz)
// ============================================================================

mlir::Value MLIRGen::generateActorMethodAsk(mlir::Value actorPtr, const ActorInfo &actorInfo,
                                            const std::string &methodName,
                                            const std::vector<ast::CallArg> &args,
                                            mlir::Location location,
                                            std::optional<int64_t> timeoutMs) {
  // Find receive function index by name
  int64_t msgIdx = -1;
  const ActorReceiveInfo *recvInfo = nullptr;
  for (size_t i = 0; i < actorInfo.receiveFns.size(); ++i) {
    if (actorInfo.receiveFns[i].name == methodName) {
      msgIdx = static_cast<int64_t>(i);
      recvInfo = &actorInfo.receiveFns[i];
      break;
    }
  }

  if (msgIdx < 0 || !recvInfo) {
    ++errorCount_;
    emitError(location) << "unknown receive handler '" << methodName << "' on actor '"
                        << actorInfo.name << "'";
    return nullptr;
  }

  auto argVals = generateActorCallArgs(args, location);
  if (!argVals)
    return nullptr;

  // Emit hew.actor_ask — blocking request-response.
  // Void-return handlers use NoneType; the caller discards the result.
  mlir::Type resultType =
      recvInfo->returnType.has_value() ? *recvInfo->returnType : mlir::NoneType::get(&context);
  mlir::IntegerAttr timeoutAttr;
  if (timeoutMs.has_value()) {
    if (!recvInfo->returnType.has_value()) {
      ++errorCount_;
      emitError(location) << "timed actor ask requires receive handler '" << methodName
                          << "' with a return type";
      return nullptr;
    }
    resultType = hew::OptionEnumType::get(&context, *recvInfo->returnType);
    timeoutAttr = builder.getI64IntegerAttr(*timeoutMs);
  }
  auto askOp = hew::ActorAskOp::create(builder, location, resultType, actorPtr,
                                       builder.getI32IntegerAttr(static_cast<int32_t>(msgIdx)),
                                       *argVals, timeoutAttr);

  // Ownership parity with the non-wire send path: String, Vec, HashMap,
  // Closure, and struct fields thereof are deep-copied at the actor boundary
  // so the sender retains ownership and drops normally.  Handle fields are
  // transferred with source null-out — sender drop is a no-op for those.
  // Everything else passes the raw pointer through — the receiver owns it,
  // so the sender must NOT drop.
  // ask has no wire path, so this applies unconditionally.
  for (size_t i = 0; i < args.size() && i < argVals->size(); ++i) {
    const auto &argSpanned = ast::callArgExpr(args[i]);
    auto *identExpr = std::get_if<ast::ExprIdentifier>(&argSpanned.value.kind);
    if (!identExpr)
      continue;
    auto argType = (*argVals)[i].getType();
    if (mlir::isa<hew::StringRefType, hew::VecType, hew::HashMapType, hew::ClosureType>(argType))
      continue;
    // Auto-field-drop structs retain an independent sender copy after the
    // deep-copy boundary. User-Drop structs still transfer ownership to the
    // receiver, so the sender must not keep dropping them.
    if (actorBoundarySenderRetainsOwnership(argType)) {
      if (auto structTy = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(argType);
          structTy && structTy.isIdentified())
        nullOutTransferredHandleFields(identExpr->name, location);
      continue;
    }
    unregisterDroppable(identExpr->name);
  }

  if (!recvInfo->returnType.has_value())
    return nullptr; // void handler — result is discarded by the caller
  return askOp.getResult();
}

// ============================================================================
// Send expression generation (actor <- message)
// ============================================================================

mlir::Value MLIRGen::generateSendExpr(const ast::ExprSend &expr) {
  auto location = currentLoc;

  auto actorVal = generateExpression(expr.target->value);
  auto msgVal = generateExpression(expr.message->value);
  if (!actorVal || !msgVal)
    return nullptr;

  // Check if the message type is a wire struct
  const WireWrapperNames *wireNames = nullptr;
  auto msgType = msgVal.getType();
  if (auto structType = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(msgType)) {
    auto name = structType.getName();
    if (!name.empty()) {
      auto it = wireStructNames.find(name.str());
      if (it != wireStructNames.end())
        wireNames = &it->second;
    }
  }

  if (wireNames || actorBoundarySenderRetainsOwnership(msgType))
    materializeTemporary(msgVal, expr.message->value);

  if (wireNames) {
    // Wire send path: encode struct → bytes, send bytes via runtime
    auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
    auto i32Type = builder.getI32Type();

    // Call encode wrapper: Foo_encode_wrapper(struct) → HewVec* (bytes)
    auto encodeFuncType = builder.getFunctionType({msgType}, {ptrType});
    getOrCreateExternFunc(wireNames->encodeName, encodeFuncType);
    auto bytesVec = mlir::func::CallOp::create(builder, location, wireNames->encodeName,
                                               mlir::TypeRange{ptrType}, mlir::ValueRange{msgVal})
                        .getResult(0);

    // Cast actor ref to !llvm.ptr for runtime call
    auto actorPtrCast = hew::BitcastOp::create(builder, location, ptrType, actorVal).getResult();

    // Call hew_actor_send_wire(actor, msg_type=0, bytes)
    auto sendWireFuncType = builder.getFunctionType({ptrType, i32Type, ptrType}, {});
    getOrCreateExternFunc("hew_actor_send_wire", sendWireFuncType);
    auto msgTypeVal = mlir::arith::ConstantIntOp::create(builder, location, i32Type, 0);
    mlir::func::CallOp::create(builder, location, "hew_actor_send_wire", mlir::TypeRange{},
                               mlir::ValueRange{actorPtrCast, msgTypeVal, bytesVec});
  } else {
    // Standard path: hew.actor_send with msg_type = 0
    hew::ActorSendOp::create(builder, location, actorVal, builder.getI32IntegerAttr(0),
                             mlir::ValueRange{msgVal});
  }

  // Wire sends serialize the value into independent bytes — no sharing.
  // Non-wire sends deep-copy String/Vec/HashMap/Closure and struct fields
  // thereof — sender retains ownership.  Handle fields are transferred with
  // source null-out.  Everything else shares the raw pointer — sender must
  // not drop.
  if (!wireNames) {
    if (auto *identExpr = std::get_if<ast::ExprIdentifier>(&expr.message->value.kind)) {
      auto msgType = msgVal.getType();
      bool senderRetains =
          mlir::isa<hew::StringRefType, hew::VecType, hew::HashMapType, hew::ClosureType>(msgType);
      if (!senderRetains && actorBoundarySenderRetainsOwnership(msgType)) {
        senderRetains = true;
        if (auto structTy = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(msgType);
            structTy && structTy.isIdentified())
          nullOutTransferredHandleFields(identExpr->name, location);
      }
      if (!senderRetains)
        unregisterDroppable(identExpr->name);
    }
  }

  return nullptr; // send is a statement, returns void
}
