//===- MLIRGenSupervisor.cpp - Supervisor tree codegen for Hew MLIRGen ----===//
//
// Supervisor tree generation: generateSupervisorDecl creates a supervisor
// function that initializes a runtime supervisor, spawns child actors via
// HewChildSpec, and starts the supervisor event loop.
//
//===----------------------------------------------------------------------===//

#include "hew/mlir/HewDialect.h"
#include "hew/mlir/HewOps.h"
#include "hew/mlir/MLIRGen.h"
#include "MLIRGenHelpers.h"

#include "mlir/Dialect/Arith/IR/Arith.h"
#include "mlir/Dialect/Func/IR/FuncOps.h"
#include "mlir/Dialect/LLVMIR/LLVMDialect.h"
#include "mlir/Dialect/MemRef/IR/MemRef.h"
#include "mlir/Dialect/SCF/IR/SCF.h"
#include "mlir/IR/Builders.h"
#include "mlir/IR/BuiltinAttributes.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/BuiltinTypes.h"

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/raw_ostream.h"

#include <string>

using namespace hew;
using namespace mlir;

// ============================================================================
// Supervisor declaration generation
// ============================================================================

void MLIRGen::generateSupervisorDecl(const ast::SupervisorDecl &decl) {
  auto location = currentLoc;
  std::string supervisorName = decl.name;

  // Register this supervisor with its child types
  std::vector<std::string> childTypes;
  for (const auto &child : decl.children) {
    childTypes.push_back(child.actor_type);
  }
  supervisorChildren[supervisorName] = std::move(childTypes);

  // Create a function that initializes and returns the supervisor.
  // Signature: supervisor_init() -> !llvm.ptr (returns supervisor pointer)
  auto funcName = supervisorName + "_init";
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();

  auto funcType = mlir::FunctionType::get(&context, {}, {ptrType});
  auto func = mlir::func::FuncOp::create(location, funcName, funcType);
  func.setVisibility(mlir::SymbolTable::Visibility::Private);

  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  // Convert supervisor strategy to integer code
  // OneForOne=0, OneForAll=1, RestForOne=2
  int strategyCode = 0;
  if (decl.strategy.has_value()) {
    switch (*decl.strategy) {
    case ast::SupervisorStrategy::OneForOne:
      strategyCode = 0;
      break;
    case ast::SupervisorStrategy::OneForAll:
      strategyCode = 1;
      break;
    case ast::SupervisorStrategy::RestForOne:
      strategyCode = 2;
      break;
    }
  }

  auto maxRestarts = decl.max_restarts.has_value()
                         ? builder.create<mlir::arith::ConstantOp>(
                               location, i64Type, builder.getI64IntegerAttr(*decl.max_restarts))
                         : builder.create<mlir::arith::ConstantOp>(location, i64Type,
                                                                   builder.getI64IntegerAttr(5));

  auto window = decl.window.has_value()
                    ? builder.create<mlir::arith::ConstantOp>(
                          location, i32Type, builder.getI32IntegerAttr(std::stoi(*decl.window)))
                    : builder.create<mlir::arith::ConstantOp>(location, i32Type,
                                                              builder.getI32IntegerAttr(60));

  auto strategy = builder
                      .create<mlir::arith::ConstantOp>(location, i32Type,
                                                       builder.getI32IntegerAttr(strategyCode))
                      .getResult();

  // Cast max_restarts to i32 for the API call
  auto maxRestartsI32 =
      builder.create<mlir::arith::TruncIOp>(location, i32Type, maxRestarts.getResult());

  // Call hew_supervisor_new(strategy, max_restarts, window_secs)
  auto supervisorPtr =
      builder
          .create<hew::SupervisorNewOp>(location, ptrType, strategy, maxRestartsI32.getResult(),
                                        window.getResult())
          .getResult();

  // Iterate over children and add each to the supervisor
  for (const auto &child : decl.children) {
    std::string childName = child.name;
    std::string actorTypeName = child.actor_type;

    // Check if this child is another supervisor (nested supervision tree)
    if (supervisorChildren.count(actorTypeName)) {
      // Generate: child_sup = ChildSupervisorName_init()
      std::string childInitName = actorTypeName + "_init";
      auto childSupPtr =
          builder
              .create<hew::RuntimeCallOp>(location, mlir::TypeRange{ptrType},
                                          mlir::SymbolRefAttr::get(&context, childInitName),
                                          mlir::ValueRange{})
              .getResult();

      // Get init function pointer for restart capability
      auto initFuncType = builder.getFunctionType({}, {ptrType});
      if (!module.lookupSymbol<mlir::func::FuncOp>(childInitName)) {
        auto savedIP = builder.saveInsertionPoint();
        builder.setInsertionPointToEnd(module.getBody());
        auto initDecl = builder.create<mlir::func::FuncOp>(location, childInitName, initFuncType);
        initDecl.setVisibility(mlir::SymbolTable::Visibility::Private);
        builder.restoreInsertionPoint(savedIP);
      }
      auto initFuncPtr =
          builder
              .create<hew::FuncPtrOp>(location, ptrType,
                                      mlir::SymbolRefAttr::get(&context, childInitName))
              .getResult();

      // Call hew_supervisor_add_child_supervisor_with_init(parent, child, init_fn)
      builder.create<hew::SupervisorAddChildSupervisorOp>(location, i32Type, supervisorPtr,
                                                          childSupPtr, initFuncPtr);
      continue;
    }

    // Look up actor in registry for state type and receive info
    auto actorIt = actorRegistry.find(actorTypeName);
    if (actorIt == actorRegistry.end()) {
      emitError(location) << "supervisor '" << supervisorName << "': unknown child actor type '"
                          << actorTypeName << "'";
      builder.create<mlir::func::ReturnOp>(location, supervisorPtr);
      module.push_back(func);
      return;
    }
    const auto &actorInfo = actorIt->second;
    auto stateType = actorInfo.stateType;
    std::string dispatchName = actorTypeName + "_dispatch";

    // 1. Allocate state struct on stack and store init args
    auto one = builder.create<mlir::arith::ConstantIntOp>(location, i64Type, 1);
    auto stateAlloca = builder.create<mlir::LLVM::AllocaOp>(location, ptrType, stateType, one);

    // Generate and store init arg values from child spec args
    auto stateStructType = llvm::dyn_cast<mlir::LLVM::LLVMStructType>(stateType);
    if (!child.args.empty()) {
      unsigned fieldIdx = 0;
      for (const auto &argExpr : child.args) {
        auto argVal = generateExpression(argExpr.value);
        if (!argVal)
          continue;
        auto fieldPtr = builder.create<mlir::LLVM::GEPOp>(
            location, ptrType, stateType, stateAlloca,
            llvm::ArrayRef<mlir::LLVM::GEPArg>{0, static_cast<int32_t>(fieldIdx)});
        // Coerce arg value to match the state field type
        if (stateStructType && fieldIdx < stateStructType.getBody().size()) {
          auto fieldType = stateStructType.getBody()[fieldIdx];
          auto valType = argVal.getType();
          if (valType != fieldType) {
            if (valType.isInteger() && fieldType.isInteger()) {
              unsigned srcWidth = valType.getIntOrFloatBitWidth();
              unsigned dstWidth = fieldType.getIntOrFloatBitWidth();
              if (srcWidth > dstWidth) {
                argVal = builder.create<mlir::arith::TruncIOp>(location, fieldType, argVal);
              } else if (srcWidth < dstWidth) {
                argVal = builder.create<mlir::arith::ExtSIOp>(location, fieldType, argVal);
              }
            } else if (mlir::isa<mlir::LLVM::LLVMPointerType>(fieldType) &&
                       !mlir::isa<mlir::LLVM::LLVMPointerType>(valType)) {
              argVal = builder.create<hew::BitcastOp>(location, fieldType, argVal);
            }
          }
        }
        builder.create<mlir::LLVM::StoreOp>(location, argVal, fieldPtr);
        fieldIdx++;
      }
      // Zero-initialize remaining fields (hidden gen frame fields)
      if (stateStructType) {
        unsigned totalFields = stateStructType.getBody().size();
        for (unsigned i = fieldIdx; i < totalFields; i++) {
          auto fieldType = stateStructType.getBody()[i];
          auto fieldPtr = builder.create<mlir::LLVM::GEPOp>(
              location, ptrType, stateType, stateAlloca,
              llvm::ArrayRef<mlir::LLVM::GEPArg>{0, static_cast<int32_t>(i)});
          auto zero = builder.create<mlir::LLVM::ZeroOp>(location, fieldType);
          builder.create<mlir::LLVM::StoreOp>(location, zero, fieldPtr);
        }
      }
    }

    // 2. Call ActorName_init(state) if it exists
    {
      std::string initName = actorTypeName + "_init";
      // Check if an init function was generated for this actor
      if (module.lookupSymbol<mlir::func::FuncOp>(initName)) {
        builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{},
                                           mlir::SymbolRefAttr::get(&context, initName),
                                           mlir::ValueRange{stateAlloca});
      }
    }

    // 3. Compute sizeof(state) using GEP trick: (uintptr_t)&((T*)null)[1]
    auto nullPtr = builder.create<mlir::LLVM::ZeroOp>(location, ptrType);
    auto sizeGep = builder.create<mlir::LLVM::GEPOp>(location, ptrType, stateType, nullPtr,
                                                     llvm::ArrayRef<mlir::LLVM::GEPArg>{1});
    auto stateSize = builder.create<mlir::LLVM::PtrToIntOp>(location, sizeType(), sizeGep);

    // 4. Get dispatch function pointer
    auto dispatchFuncType = builder.getFunctionType({ptrType, i32Type, ptrType, sizeType()}, {});
    // Declare the dispatch function if not already declared
    if (!module.lookupSymbol<mlir::func::FuncOp>(dispatchName)) {
      auto savedIP = builder.saveInsertionPoint();
      builder.setInsertionPointToEnd(module.getBody());
      auto dispatchDecl =
          builder.create<mlir::func::FuncOp>(location, dispatchName, dispatchFuncType);
      dispatchDecl.setVisibility(mlir::SymbolTable::Visibility::Private);
      builder.restoreInsertionPoint(savedIP);
    }
    auto dispatchPtr = builder
                           .create<hew::FuncPtrOp>(location, ptrType,
                                                   mlir::SymbolRefAttr::get(&context, dispatchName))
                           .getResult();

    // 5. Create child name as a C string (global string constant)
    auto nameSym = getOrCreateGlobalString(childName);
    auto strRefType = hew::StringRefType::get(&context);
    auto nameStrRef =
        builder.create<hew::ConstantOp>(location, strRefType, builder.getStringAttr(nameSym))
            .getResult();
    // Cast !hew.string_ref to !llvm.ptr for C struct compatibility
    auto nameStr = builder.create<hew::BitcastOp>(location, ptrType, nameStrRef).getResult();

    // 6. Determine restart policy
    // 0=Permanent, 1=Transient, 2=Temporary
    int restartCode = 0; // default: Permanent
    if (child.restart.has_value()) {
      switch (*child.restart) {
      case ast::RestartPolicy::Permanent:
        restartCode = 0;
        break;
      case ast::RestartPolicy::Transient:
        restartCode = 1;
        break;
      case ast::RestartPolicy::Temporary:
        restartCode = 2;
        break;
      }
    }

    // 7. Build HewChildSpec struct via the high-level dialect op
    auto restartVal = builder.create<mlir::arith::ConstantIntOp>(location, i32Type, restartCode);
    int mbCap =
        actorInfo.mailboxCapacity.has_value() ? static_cast<int>(*actorInfo.mailboxCapacity) : -1;
    auto mbCapVal = builder.create<mlir::arith::ConstantIntOp>(location, i32Type, mbCap);
    auto overflowVal = builder.create<mlir::arith::ConstantIntOp>(
        location, i32Type, static_cast<int>(actorInfo.overflowPolicy));

    auto specPtr =
        builder
            .create<hew::ChildSpecCreateOp>(location, ptrType, nameStr, stateAlloca, stateSize,
                                            dispatchPtr, restartVal, mbCapVal, overflowVal)
            .getResult();

    // 8. Call hew_supervisor_add_child_spec(supervisor, &spec)
    builder.create<hew::SupervisorAddChildOp>(location, i32Type, supervisorPtr, specPtr);
  }

  // Start the supervisor (begins watching for child crashes)
  builder.create<hew::SupervisorStartOp>(location, i32Type, supervisorPtr);

  builder.create<mlir::func::ReturnOp>(location, supervisorPtr);

  module.push_back(func);
}
