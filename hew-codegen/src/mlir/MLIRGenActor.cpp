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
#include <string>

using namespace hew;
using namespace mlir;

// ============================================================================
// Actor registration (phase 1): struct types, field tracking, registry entry
// ============================================================================

void MLIRGen::registerActorDecl(const ast::ActorDecl &decl) {
  hasActors = true;
  const std::string &actorName = decl.name;

  // 1. Create actor state struct type from fields
  llvm::SmallVector<mlir::Type, 4> fieldTypes;
  std::vector<mlir::Type> fieldHewTypes; // Hew MLIR types before toLLVMStorageType
  for (const auto &field : decl.fields) {
    auto hewType = convertType(field.ty.value);
    fieldHewTypes.push_back(hewType);
    fieldTypes.push_back(toLLVMStorageType(hewType));
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
  }
  structTypes[actorName] = std::move(stInfo);

  // Record actor-typed fields for field-access dispatch (e.g. self.target.method())
  // and collection-typed fields for Vec/HashMap method calls (e.g. self.items.push())
  for (const auto &field : decl.fields) {
    auto key = actorName + "." + field.name;

    // Track actor-typed fields (ActorRef<T> → extract T)
    auto actorName2 = typeExprToActorName(field.ty.value);
    if (!actorName2.empty()) {
      actorFieldTypes[key] = actorName2;
    } else if (auto *named = std::get_if<ast::TypeNamed>(&field.ty.value.kind)) {
      actorFieldTypes[key] = resolveTypeAlias(named->name);
    }

    // Track collection-typed fields (Vec<T>, HashMap<K,V>)
    auto resolveAlias = [this](const std::string &n) { return resolveTypeAlias(n); };
    auto collStr = typeExprToCollectionString(field.ty.value, resolveAlias);
    if (!collStr.empty())
      collectionFieldTypes[key] = collStr;
  }

  // Build actor registry info (signatures only, no body generation)
  ActorInfo actorInfo;
  actorInfo.name = actorName;
  actorInfo.stateType = stateType;
  actorInfo.fieldHewTypes = std::move(fieldHewTypes);
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

    for (const auto &param : recv.params) {
      auto ty = convertType(param.ty.value);
      recvInfo.paramNames.push_back(param.name);
      recvInfo.paramTypes.push_back(ty);
    }

    if (recv.is_generator && recv.return_type) {
      // Generator return type: wrap YieldType → { i8 has_value, YieldType value }
      auto yieldType = convertType(recv.return_type->value);
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
      auto retTy = convertType(recv.return_type->value);
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
  auto location = currentLoc;
  const std::string &actorName = decl.name;

  // State struct and registry entry already set up by registerActorDecl
  auto stIt = structTypes.find(actorName);
  if (stIt == structTypes.end())
    return;
  auto stateType = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(stIt->second.mlirType);

  auto regIt = actorRegistry.find(actorName);
  if (regIt == actorRegistry.end())
    return;
  const auto &actorInfo = regIt->second;

  // Generate receive handler functions
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto prevActorName = currentActorName;
  currentActorName = actorName;

  for (const auto &recv : decl.receive_fns) {
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
      llvm::SmallVector<mlir::Type, 4> argsFieldTypes;
      argsFieldTypes.push_back(ptrType); // self
      if (recvIt != actorInfo.receiveFns.end()) {
        for (auto ty : recvIt->paramTypes)
          argsFieldTypes.push_back(ty);
      }
      auto argsStructType = mlir::LLVM::LLVMStructType::getLiteral(&context, argsFieldTypes);

      // ─── 1. Body function: void ActorName_method__body(ptr args, ptr gen_ctx) ───
      {
        std::string bodyFnName = receiveName + "__body";
        auto bodyFnType = builder.getFunctionType({ptrType, ptrType}, {});
        auto savedIP = builder.saveInsertionPoint();
        builder.setInsertionPointToEnd(module.getBody());
        auto bodyFnOp = builder.create<mlir::func::FuncOp>(location, bodyFnName, bodyFnType);
        auto *entryBlock = bodyFnOp.addEntryBlock();
        builder.setInsertionPointToStart(entryBlock);

        SymbolTableScopeT varScope(symbolTable);
        MutableTableScopeT mutScope(mutableVars);
        auto prevFunction = currentFunction;
        currentFunction = bodyFnOp;
        auto prevReturnFlag = returnFlag;
        auto prevReturnSlot = returnSlot;
        returnFlag = nullptr;
        returnSlot = nullptr;

        auto argsPtr = entryBlock->getArgument(0);
        auto genCtxArg = entryBlock->getArgument(1);

        // Set currentGenCtx so yield expressions emit hew_gen_yield calls
        auto prevGenCtx = currentGenCtx;
        currentGenCtx = genCtxArg;

        // Load the args struct from the args pointer
        auto argsStruct = builder.create<mlir::LLVM::LoadOp>(location, argsStructType, argsPtr);

        // Extract self pointer (field 0)
        auto selfPtr = builder.create<mlir::LLVM::ExtractValueOp>(location, argsStruct,
                                                                  llvm::ArrayRef<int64_t>{0});
        declareVariable("self", selfPtr);

        // Extract and bind message parameters (fields 1..N)
        {
          size_t pi = 0;
          for (const auto &param : recv.params) {
            auto paramVal = builder.create<mlir::LLVM::ExtractValueOp>(
                location, argsStruct, llvm::ArrayRef<int64_t>{static_cast<int64_t>(pi + 1)});
            declareVariable(param.name, paramVal);

            // Register ActorRef<T> params for method dispatch
            {
              auto actorName = typeExprToActorName(param.ty.value);
              if (!actorName.empty())
                actorVarTypes[param.name] = actorName;
            }
            ++pi;
          }
        }

        // Generate the receive fn body (yields will call hew_gen_yield)
        generateBlock(recv.body);

        // Ensure terminator
        if (!hasRealTerminator(builder.getInsertionBlock()))
          builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{});

        currentGenCtx = prevGenCtx;
        currentFunction = prevFunction;
        returnFlag = prevReturnFlag;
        returnSlot = prevReturnSlot;
        builder.restoreInsertionPoint(savedIP);
      }

      // ─── 2. Init handler: {i8,Y} ActorName_method(ptr self, params...) ───
      {
        // Build param types: { ptr self, param1, param2, ... }
        llvm::SmallVector<mlir::Type, 4> initParamTypes(argsFieldTypes);
        auto initFuncType = builder.getFunctionType(initParamTypes, {wrapperType});
        auto savedIP = builder.saveInsertionPoint();
        builder.setInsertionPointToEnd(module.getBody());
        auto initFuncOp = builder.create<mlir::func::FuncOp>(location, receiveName, initFuncType);
        auto *entryBlock = initFuncOp.addEntryBlock();
        builder.setInsertionPointToStart(entryBlock);

        auto prevFunction = currentFunction;
        currentFunction = initFuncOp;

        auto selfPtr = entryBlock->getArgument(0);

        // Allocate args struct on stack
        auto one64 = builder.create<mlir::arith::ConstantIntOp>(location, i64Type, 1);
        auto argsAlloca =
            builder.create<mlir::LLVM::AllocaOp>(location, ptrType, argsStructType, one64);

        // Pack self + params into the struct
        auto argsUndef = builder.create<mlir::LLVM::UndefOp>(location, argsStructType);
        mlir::Value argsStruct = builder.create<mlir::LLVM::InsertValueOp>(
            location, argsUndef, selfPtr, llvm::ArrayRef<int64_t>{0});
        for (size_t pi = 0; pi < recv.params.size(); ++pi) {
          argsStruct = builder.create<mlir::LLVM::InsertValueOp>(
              location, argsStruct, entryBlock->getArgument(pi + 1),
              llvm::ArrayRef<int64_t>{static_cast<int64_t>(pi + 1)});
        }
        builder.create<mlir::LLVM::StoreOp>(location, argsStruct, argsAlloca);

        // Compute args struct size
        auto argsSizeVal = builder.create<hew::SizeOfOp>(location, sizeType(),
                                                         mlir::TypeAttr::get(argsStructType));

        // Get body function pointer using func.constant + cast to ptr
        std::string bodyFnName = receiveName + "__body";
        auto bodyFnPtrType = builder.getFunctionType({ptrType, ptrType}, {});
        getOrCreateExternFunc(bodyFnName, bodyFnPtrType);
        auto bodyFnAddr = builder
                              .create<hew::FuncPtrOp>(
                                  location, ptrType, mlir::SymbolRefAttr::get(&context, bodyFnName))
                              .getResult();

        // Call hew_gen_ctx_create(body_fn, args_ptr, args_size) → ctx
        auto ctx =
            builder
                .create<hew::GenCtxCreateOp>(location, ptrType, bodyFnAddr, argsAlloca, argsSizeVal)
                .getResult();

        // Store ctx in state.__gen_frame_N
        auto genFrameGEP = builder.create<mlir::LLVM::GEPOp>(
            location, ptrType, stateType, selfPtr,
            llvm::ArrayRef<mlir::LLVM::GEPArg>{0, static_cast<int32_t>(genFrameIdx)});
        builder.create<mlir::LLVM::StoreOp>(location, ctx, genFrameGEP);

        // Emit gen-next → null check → wrap/cleanup → return
        emitGenNextResult(ctx, selfPtr, stateType, genFrameIdx, yieldType, wrapperType, location);

        currentFunction = prevFunction;
        builder.restoreInsertionPoint(savedIP);
      }

      // ─── 3. Next handler: {i8,Y} ActorName_method__next(ptr self) ───
      {
        std::string nextHandlerName = receiveName + "__next";
        auto nextFuncType = builder.getFunctionType({ptrType}, {wrapperType});
        auto savedIP = builder.saveInsertionPoint();
        builder.setInsertionPointToEnd(module.getBody());
        auto nextFuncOp =
            builder.create<mlir::func::FuncOp>(location, nextHandlerName, nextFuncType);
        auto *entryBlock = nextFuncOp.addEntryBlock();
        builder.setInsertionPointToStart(entryBlock);

        auto prevFunction = currentFunction;
        currentFunction = nextFuncOp;

        auto selfPtr = entryBlock->getArgument(0);

        // Load gen ctx from state.__gen_frame_N
        auto genFrameGEP = builder.create<mlir::LLVM::GEPOp>(
            location, ptrType, stateType, selfPtr,
            llvm::ArrayRef<mlir::LLVM::GEPArg>{0, static_cast<int32_t>(genFrameIdx)});
        auto ctx = builder.create<mlir::LLVM::LoadOp>(location, ptrType, genFrameGEP);

        // Emit gen-next → null check → wrap/cleanup → return
        emitGenNextResult(ctx, selfPtr, stateType, genFrameIdx, yieldType, wrapperType, location);

        currentFunction = prevFunction;
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
    auto funcOp = builder.create<mlir::func::FuncOp>(location, receiveName, funcType);
    auto *entryBlock = funcOp.addEntryBlock();
    builder.setInsertionPointToStart(entryBlock);

    // Create scopes for this function
    SymbolTableScopeT varScope(symbolTable);
    MutableTableScopeT mutScope(mutableVars);

    auto prevFunction = currentFunction;
    currentFunction = funcOp;
    auto prevReturnFlag = returnFlag;
    auto prevReturnSlot = returnSlot;
    returnFlag = nullptr;
    returnSlot = nullptr;

    // Bind self pointer — store in symbol table so field access works
    auto selfPtr = entryBlock->getArgument(0);
    declareVariable("self", selfPtr);

    // Bind message parameters (starting at argument 1)
    {
      size_t pi = 0;
      for (const auto &param : recv.params) {
        declareVariable(param.name, entryBlock->getArgument(pi + 1));
        // Register ActorRef<T> parameters for actor method dispatch
        {
          auto actorName = typeExprToActorName(param.ty.value);
          if (!actorName.empty())
            actorVarTypes[param.name] = actorName;
        }
        // Register HashMap parameters for erased-pointer fallback dispatch.
        {
          auto resolveAlias = [this](const std::string &n) { return resolveTypeAlias(n); };
          auto collStr = typeExprToCollectionString(param.ty.value, resolveAlias);
          if (collStr.rfind("HashMap<", 0) == 0)
            collectionVarTypes[param.name] = collStr;
        }
        ++pi;
      }
    }

    // Generate function body
    mlir::Value bodyValue = generateBlock(recv.body);

    // Emit return
    if (!hasRealTerminator(builder.getInsertionBlock())) {
      if (!resultTypes.empty() && bodyValue) {
        bodyValue = coerceType(bodyValue, resultTypes[0], location);
        builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{bodyValue});
      } else {
        builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{});
      }
    }

    currentFunction = prevFunction;
    returnFlag = prevReturnFlag;
    returnSlot = prevReturnSlot;
    builder.restoreInsertionPoint(savedIP);
  }

  // 2b. Generate init function if the actor has an init block
  //     void ActorName_init(ptr state)
  if (decl.init) {
    std::string initName = actorName + "_init";
    auto initFuncType = builder.getFunctionType({ptrType}, {});

    auto savedIP = builder.saveInsertionPoint();
    builder.setInsertionPointToEnd(module.getBody());
    auto initFuncOp = builder.create<mlir::func::FuncOp>(location, initName, initFuncType);
    auto *entryBlock = initFuncOp.addEntryBlock();
    builder.setInsertionPointToStart(entryBlock);

    SymbolTableScopeT varScope(symbolTable);
    MutableTableScopeT mutScope(mutableVars);

    auto prevFunction = currentFunction;
    currentFunction = initFuncOp;
    auto prevReturnFlag = returnFlag;
    auto prevReturnSlot = returnSlot;
    returnFlag = nullptr;
    returnSlot = nullptr;

    // Bind self pointer for field access
    auto selfPtr = entryBlock->getArgument(0);
    declareVariable("self", selfPtr);

    // Generate init block body
    generateBlock(decl.init->body);

    // Ensure terminator
    if (!hasRealTerminator(builder.getInsertionBlock()))
      builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{});

    currentFunction = prevFunction;
    returnFlag = prevReturnFlag;
    returnSlot = prevReturnSlot;
    builder.restoreInsertionPoint(savedIP);
  }

  // NOTE: Actor terminate blocks are not yet in the AST. When added, generate
  // a `ActorName_terminate(ptr state)` function that cleans up actor resources.

  // 3. Generate dispatch function:
  //    void ActorName_dispatch(ptr state, i32 msg_type, ptr data, size_t data_size)
  {
    std::string dispatchName = actorName + "_dispatch";
    auto i32Type = builder.getI32Type();
    auto dispatchType = builder.getFunctionType({ptrType, i32Type, ptrType, sizeType()}, {});

    auto savedIP = builder.saveInsertionPoint();
    builder.setInsertionPointToEnd(module.getBody());
    auto dispatchOp = builder.create<mlir::func::FuncOp>(location, dispatchName, dispatchType);
    auto *entryBlock = dispatchOp.addEntryBlock();
    builder.setInsertionPointToStart(entryBlock);

    auto stateArg = entryBlock->getArgument(0);   // ptr (state)
    auto msgTypeArg = entryBlock->getArgument(1); // i32 (msg_type)
    auto dataArg = entryBlock->getArgument(2);    // ptr (data)

    // Build handler symbol array for hew.receive op
    auto dataSizeArg = entryBlock->getArgument(3); // size_t (data_size)
    llvm::SmallVector<mlir::Attribute, 4> handlerRefs;
    for (size_t i = 0; i < actorInfo.receiveFns.size(); ++i) {
      const auto &recvFn = actorInfo.receiveFns[i];
      std::string recvHandlerName = actorName + "_" + recvFn.name;

      // Ensure handler function is declared in the module
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

    builder.create<hew::ReceiveOp>(location, stateArg, msgTypeArg, dataArg, dataSizeArg,
                                   builder.getArrayAttr(handlerRefs));

    // Return void from dispatch
    builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{});
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
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();

  auto funcType = builder.getFunctionType({i32Type, ptrType, i64Type}, {i64Type});

  auto savedIP = builder.saveInsertionPoint();
  builder.setInsertionPointToEnd(module.getBody());
  auto funcOp = builder.create<mlir::func::FuncOp>(builder.getUnknownLoc(), fnName, funcType);
  auto *entryBlock = funcOp.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto msgType = entryBlock->getArgument(0);
  auto dataPtr = entryBlock->getArgument(1);

  // Default: return msg_type as u64 (so each msg_type is its own key bucket)
  auto defaultKey = builder.create<mlir::arith::ExtUIOp>(builder.getUnknownLoc(), i64Type, msgType);

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

    auto uloc = builder.getUnknownLoc();
    auto msgTypeConst =
        builder.create<mlir::arith::ConstantIntOp>(uloc, i32Type, static_cast<int64_t>(i));
    auto isThisMsg = builder.create<mlir::arith::CmpIOp>(uloc, mlir::arith::CmpIPredicate::eq,
                                                         msgType, msgTypeConst);

    // Build the struct type for this message's packed args
    llvm::SmallVector<mlir::Type, 4> msgStructFields;
    for (const auto &pt : recv.paramTypes) {
      msgStructFields.push_back(pt);
    }
    auto msgStructType = mlir::LLVM::LLVMStructType::getLiteral(&context, msgStructFields);

    auto ifOp = builder.create<mlir::scf::IfOp>(uloc, i64Type, isThisMsg, /*withElseRegion=*/true);

    // Then: extract key field
    builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
    auto fieldGEP = builder.create<mlir::LLVM::GEPOp>(
        uloc, ptrType, msgStructType, dataPtr, llvm::ArrayRef<mlir::LLVM::GEPArg>{0, keyFieldIdx});

    mlir::Value keyVal;
    if (keyFieldType == i32Type) {
      auto loaded = builder.create<mlir::LLVM::LoadOp>(uloc, i32Type, fieldGEP);
      keyVal = builder.create<mlir::arith::ExtUIOp>(uloc, i64Type, loaded);
    } else if (keyFieldType == i64Type) {
      keyVal = builder.create<mlir::LLVM::LoadOp>(uloc, i64Type, fieldGEP);
    } else if (keyFieldType == ptrType) {
      auto loaded = builder.create<mlir::LLVM::LoadOp>(uloc, ptrType, fieldGEP);
      keyVal = builder.create<mlir::LLVM::PtrToIntOp>(uloc, i64Type, loaded);
    } else {
      keyVal = builder.create<mlir::arith::ExtUIOp>(uloc, i64Type, msgType);
    }
    builder.create<mlir::scf::YieldOp>(uloc, keyVal);

    // Else: pass through previous result
    builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
    builder.create<mlir::scf::YieldOp>(uloc, result);

    builder.setInsertionPointAfter(ifOp);
    result = ifOp.getResult(0);
  }

  builder.create<mlir::func::ReturnOp>(builder.getUnknownLoc(), result);

  builder.restoreInsertionPoint(savedIP);
}

// ============================================================================
// Spawn expression generation
// ============================================================================

mlir::Value MLIRGen::generateSpawnExpr(const ast::ExprSpawn &expr) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);

  // The target is an identifier (actor name)
  // In the AST, spawn ActorName(field: value, ...) has target = Ident("ActorName") + named args
  std::string actorName;
  if (auto *identExpr = std::get_if<ast::ExprIdentifier>(&expr.target->value.kind)) {
    actorName = identExpr->name;
  }

  if (actorName.empty()) {
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
    auto call = builder.create<mlir::func::CallOp>(location, initName, mlir::TypeRange{ptrType},
                                                   mlir::ValueRange{});
    return call.getResult(0);
  }

  auto it = actorRegistry.find(actorName);
  if (it == actorRegistry.end()) {
    emitError(location) << "unknown actor type: " << actorName;
    return nullptr;
  }
  const auto &actorInfo = it->second;

  // Generate init argument values from named args
  llvm::SmallVector<mlir::Value, 4> initArgVals;
  for (const auto &[fieldName, argExpr] : expr.args) {
    auto argVal = generateExpression(argExpr->value);
    if (!argVal)
      return nullptr;
    initArgVals.push_back(argVal);
  }

  // Zero-arg spawn: pad missing user fields with Go-style zero values.
  // Vec/bytes → VecNewOp, HashMap → HashMapNewOp, string → "", others → 0/null.
  {
    size_t numUserFields = actorInfo.fieldHewTypes.size();
    for (size_t i = initArgVals.size(); i < numUserFields; ++i) {
      auto hewType = actorInfo.fieldHewTypes[i];
      if (auto vecType = mlir::dyn_cast<hew::VecType>(hewType)) {
        initArgVals.push_back(builder.create<hew::VecNewOp>(location, vecType).getResult());
      } else if (auto hmType = mlir::dyn_cast<hew::HashMapType>(hewType)) {
        initArgVals.push_back(builder.create<hew::HashMapNewOp>(location, hmType).getResult());
      } else if (mlir::isa<hew::StringRefType>(hewType)) {
        auto symName = getOrCreateGlobalString("");
        initArgVals.push_back(builder
                                  .create<hew::ConstantOp>(location,
                                                           hew::StringRefType::get(&context),
                                                           builder.getStringAttr(symName))
                                  .getResult());
      } else {
        initArgVals.push_back(createDefaultValue(builder, location, toLLVMStorageType(hewType)));
      }
    }
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
      initArgVals.push_back(builder.create<mlir::LLVM::ZeroOp>(location, ptrType));
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

  auto spawnOp = builder.create<hew::ActorSpawnOp>(
      location, hew::TypedActorRefType::get(&context, builder.getStringAttr(actorName)),
      builder.getStringAttr(actorName), mlir::SymbolRefAttr::get(&context, dispatchName),
      mlir::TypeAttr::get(actorInfo.stateType), initArgVals, mailboxCapAttr, overflowPolicyAttr,
      coalesceKeyFnAttr, coalesceFallbackAttr);

  auto result = spawnOp.getResult();

  // Register with enclosing scope, if any.
  if (currentScopePtr) {
    auto i32Type = builder.getI32Type();
    builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{i32Type},
                                       mlir::SymbolRefAttr::get(&context, "hew_scope_spawn"),
                                       mlir::ValueRange{currentScopePtr, result});
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
      emitWarning(location) << "actor receive parameter '" << param.name
                            << "' has no type annotation; defaulting to i64";
    }
    auto ty = param.ty ? convertType(param.ty->value) : builder.getI64Type();
    recvParamTypes.push_back(ty);
    recvInfo.paramTypes.push_back(ty);
  }

  // Generate receive function
  std::string receiveName = actorName + "_receive";
  auto recvFuncType = builder.getFunctionType(recvParamTypes, {});

  auto savedIP = builder.saveInsertionPoint();
  builder.setInsertionPointToEnd(module.getBody());
  auto recvFuncOp = builder.create<mlir::func::FuncOp>(location, receiveName, recvFuncType);
  auto *recvEntry = recvFuncOp.addEntryBlock();
  builder.setInsertionPointToStart(recvEntry);

  {
    SymbolTableScopeT varScope(symbolTable);
    MutableTableScopeT mutScope(mutableVars);
    auto prevFunction = currentFunction;
    currentFunction = recvFuncOp;
    auto prevReturnFlag = returnFlag;
    auto prevReturnSlot = returnSlot;
    returnFlag = nullptr;
    returnSlot = nullptr;

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
      auto fieldPtr = builder.create<mlir::LLVM::GEPOp>(
          location, ptrType, stateType, selfPtr,
          llvm::ArrayRef<mlir::LLVM::GEPArg>{0, static_cast<int32_t>(i)});
      auto llvmFieldType = toLLVMStorageType(capturedVars[i].value.getType());
      mlir::Value loaded = builder.create<mlir::LLVM::LoadOp>(location, llvmFieldType, fieldPtr);
      auto hewType = capturedVars[i].value.getType();
      if (hewType != llvmFieldType)
        loaded = builder.create<hew::BitcastOp>(location, hewType, loaded);
      declareMutableVariable(capturedVars[i].name, hewType, loaded);
    }

    if (expr.body) {
      generateExpression(expr.body->value);
    }

    if (!hasRealTerminator(builder.getInsertionBlock()))
      builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{});

    currentFunction = prevFunction;
    returnFlag = prevReturnFlag;
    returnSlot = prevReturnSlot;
  }

  // Generate dispatch function (always calls receive, ignores msg_type)
  std::string dispatchName = actorName + "_dispatch";
  auto dispatchType = builder.getFunctionType({ptrType, i32Type, ptrType, sizeType()}, {});
  builder.setInsertionPointToEnd(module.getBody());
  auto dispatchOp = builder.create<mlir::func::FuncOp>(location, dispatchName, dispatchType);
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

    builder.create<hew::ReceiveOp>(location, stateArg, msgTypeArg, dataArg, dataSizeArg,
                                   builder.getArrayAttr(handlerRefs));
    builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{});
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

  auto spawnOp = builder.create<hew::ActorSpawnOp>(
      location, hew::ActorRefType::get(&context), builder.getStringAttr(actorName),
      mlir::SymbolRefAttr::get(&context, dispatchName), mlir::TypeAttr::get(stateType), initArgVals,
      /*mailbox_capacity=*/mlir::IntegerAttr{},
      /*overflow_policy=*/mlir::IntegerAttr{},
      /*coalesce_key_fn=*/mlir::FlatSymbolRefAttr{},
      /*coalesce_fallback=*/mlir::IntegerAttr{});

  hasActors = true;
  auto result = spawnOp.getResult();

  // Register with enclosing scope, if any.
  if (currentScopePtr) {
    builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{i32Type},
                                       mlir::SymbolRefAttr::get(&context, "hew_scope_spawn"),
                                       mlir::ValueRange{currentScopePtr, result});
  }

  return result;
}

// ============================================================================
// Shared helpers for actor send/ask
// ============================================================================

std::optional<llvm::SmallVector<mlir::Value, 4>>
MLIRGen::generateActorCallArgs(const std::vector<ast::CallArg> &args, mlir::Location location) {
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  llvm::SmallVector<mlir::Value, 4> argVals;
  for (const auto &arg : args) {
    const auto &argSpanned = ast::callArgExpr(arg);
    // When passing `self` as an argument (ActorRef<Self>), use hew_actor_self()
    // instead of the raw state pointer
    if (!currentActorName.empty()) {
      if (auto *identExpr = std::get_if<ast::ExprIdentifier>(&argSpanned.value.kind)) {
        if (identExpr->name == "self") {
          auto selfRef = builder.create<hew::ActorSelfOp>(location, ptrType).getResult();
          argVals.push_back(selfRef);
          continue;
        }
      }
    }
    auto val = generateExpression(argSpanned.value);
    if (!val)
      return std::nullopt;
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

  auto one64 = builder.create<mlir::arith::ConstantIntOp>(location, i64Type, 1);
  auto outSizeAlloca = builder.create<mlir::LLVM::AllocaOp>(location, ptrType, i64Type, one64);
  auto valuePtr =
      builder.create<hew::GenNextOp>(location, ptrType, ctx, outSizeAlloca).getResult();

  // Check if value_ptr is null (done)
  auto nullCmp = builder.create<mlir::LLVM::ZeroOp>(location, ptrType);
  auto isNull = builder.create<mlir::LLVM::ICmpOp>(location, mlir::LLVM::ICmpPredicate::eq,
                                                    valuePtr, nullCmp);

  auto ifOp =
      builder.create<mlir::scf::IfOp>(location, wrapperType, isNull, /*withElseRegion=*/true);

  // Then block (null → done): free gen ctx, clear gen frame in state
  builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
  auto doneWrap = builder.create<hew::GenWrapDoneOp>(location, wrapperType);
  builder.create<hew::GenFreeOp>(location, ctx);
  auto nullForClear = builder.create<mlir::LLVM::ZeroOp>(location, ptrType);
  auto genFrameGEP = builder.create<mlir::LLVM::GEPOp>(
      location, ptrType, stateType, selfPtr,
      llvm::ArrayRef<mlir::LLVM::GEPArg>{0, static_cast<int32_t>(genFrameIdx)});
  builder.create<mlir::LLVM::StoreOp>(location, nullForClear, genFrameGEP);
  builder.create<mlir::scf::YieldOp>(location, doneWrap.getResult());

  // Else block (non-null → has value): load, free malloc'd buf
  builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
  auto loadedVal = builder.create<mlir::LLVM::LoadOp>(location, yieldType, valuePtr);
  builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{},
                                     mlir::SymbolRefAttr::get(&context, "free"),
                                     mlir::ValueRange{valuePtr});
  auto valWrap = builder.create<hew::GenWrapValueOp>(location, wrapperType, loadedVal);
  builder.create<mlir::scf::YieldOp>(location, valWrap.getResult());

  builder.setInsertionPointAfter(ifOp);
  builder.create<mlir::func::ReturnOp>(location, ifOp.getResults());
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
    emitError(location) << "unknown receive handler '" << methodName << "' on actor '"
                        << actorInfo.name << "'";
    return nullptr;
  }

  auto argVals = generateActorCallArgs(args, location);
  if (!argVals)
    return nullptr;

  // Emit hew.actor_send — the lowering pass handles arg packing and runtime call
  builder.create<hew::ActorSendOp>(
      location, actorPtr, builder.getI32IntegerAttr(static_cast<int32_t>(msgIdx)), *argVals);

  // Suppress sender-side Drop for moved (non-Copy) identifier arguments.
  // Ownership transfers to the actor; the sender must not free the handle.
  for (const auto &arg : args) {
    const auto &argSpanned = ast::callArgExpr(arg);
    if (auto *identExpr = std::get_if<ast::ExprIdentifier>(&argSpanned.value.kind)) {
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
                                            mlir::Location location) {
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
    emitError(location) << "unknown receive handler '" << methodName << "' on actor '"
                        << actorInfo.name << "'";
    return nullptr;
  }

  if (!recvInfo->returnType.has_value()) {
    emitError(location) << "await requires a receive handler with a return type, "
                        << "but '" << methodName << "' returns void";
    return nullptr;
  }

  auto argVals = generateActorCallArgs(args, location);
  if (!argVals)
    return nullptr;

  // Emit hew.actor_ask — blocking request-response
  auto askOp = builder.create<hew::ActorAskOp>(
      location, *recvInfo->returnType, actorPtr,
      builder.getI32IntegerAttr(static_cast<int32_t>(msgIdx)), *argVals,
      /*timeout_ms=*/mlir::IntegerAttr{});

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

  // Emit hew.actor_send with msg_type = 0 (send expressions use handler 0)
  builder.create<hew::ActorSendOp>(location, actorVal, builder.getI32IntegerAttr(0),
                                   mlir::ValueRange{msgVal});

  // Suppress sender-side Drop for the moved message variable.
  if (auto *identExpr = std::get_if<ast::ExprIdentifier>(&expr.message->value.kind)) {
    unregisterDroppable(identExpr->name);
  }

  return nullptr; // send is a statement, returns void
}
