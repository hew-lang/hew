//===- codegen.cpp - Hew MLIR-to-native codegen pipeline -------------------===//
//
// Lowers Hew MLIR modules through the standard pipeline:
//   hew dialect -> func/arith/llvm ops
//   SCF -> ControlFlow -> LLVM dialect
//   LLVM dialect -> LLVM IR -> object file -> linked executable
//
//===----------------------------------------------------------------------===//

#include "hew/codegen.h"
#include "hew/mlir/HewDialect.h"
#include "hew/mlir/HewOps.h"
#include "hew/mlir/HewTypes.h"

// MLIR dialect includes
#include "mlir/Dialect/Arith/IR/Arith.h"
#include "mlir/Dialect/ControlFlow/IR/ControlFlowOps.h"
#include "mlir/Dialect/Func/IR/FuncOps.h"
#include "mlir/Dialect/LLVMIR/LLVMDialect.h"
#include "mlir/Dialect/MemRef/IR/MemRef.h"
#include "mlir/Dialect/SCF/IR/SCF.h"

// MLIR conversion includes
#include "mlir/Conversion/ArithToLLVM/ArithToLLVM.h"
#include "mlir/Conversion/ControlFlowToLLVM/ControlFlowToLLVM.h"
#include "mlir/Conversion/FuncToLLVM/ConvertFuncToLLVM.h"
#include "mlir/Conversion/FuncToLLVM/ConvertFuncToLLVMPass.h"
#include "mlir/Conversion/LLVMCommon/ConversionTarget.h"
#include "mlir/Conversion/LLVMCommon/TypeConverter.h"
#include "mlir/Conversion/MemRefToLLVM/MemRefToLLVM.h"
#include "mlir/Conversion/Passes.h"
#include "mlir/Conversion/ReconcileUnrealizedCasts/ReconcileUnrealizedCasts.h"
#include "mlir/Conversion/SCFToControlFlow/SCFToControlFlow.h"
#include "mlir/Dialect/Func/Transforms/FuncConversions.h"
#include "mlir/Dialect/SCF/Transforms/Patterns.h"

// MLIR pass infrastructure
#include "mlir/IR/Builders.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/BuiltinTypes.h"
#include "mlir/IR/PatternMatch.h"
#include "mlir/IR/Verifier.h"
#include "mlir/Pass/Pass.h"
#include "mlir/Pass/PassManager.h"
#include "mlir/Transforms/DialectConversion.h"
#include "mlir/Transforms/Passes.h"

// MLIR -> LLVM IR translation
#include "mlir/Target/LLVMIR/Dialect/Builtin/BuiltinToLLVMIRTranslation.h"
#include "mlir/Target/LLVMIR/Dialect/LLVMIR/LLVMToLLVMIRTranslation.h"
#include "mlir/Target/LLVMIR/Export.h"

// LLVM support
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/Program.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"
#include <filesystem>
#include <iostream>
#include <string>

using namespace hew;

// ============================================================================
// Hew dialect conversion patterns
// ============================================================================

namespace {

/// Return the MLIR integer type that matches the target's `size_t`/`usize`.
/// On wasm32, this is i32; on x86_64/aarch64, it is i64.
/// Reads the "hew.ptr_width" attribute on the module (set by Codegen::compile).
static mlir::IntegerType getSizeType(mlir::MLIRContext *ctx, mlir::ModuleOp module) {
  if (auto attr = module->getAttrOfType<mlir::IntegerAttr>("hew.ptr_width")) {
    return mlir::IntegerType::get(ctx, attr.getInt());
  }
  return mlir::IntegerType::get(ctx, 64); // default to 64-bit
}

/// Get or declare an external runtime function in the module (LLVM dialect).
[[maybe_unused]]
mlir::LLVM::LLVMFuncOp getOrInsertLLVMFunc(mlir::ModuleOp module, mlir::OpBuilder &builder,
                                           llvm::StringRef name, mlir::Type resultType,
                                           llvm::ArrayRef<mlir::Type> argTypes) {
  if (auto func = module.lookupSymbol<mlir::LLVM::LLVMFuncOp>(name))
    return func;

  auto savedIP = builder.saveInsertionPoint();
  builder.setInsertionPointToStart(module.getBody());

  auto funcType = mlir::LLVM::LLVMFunctionType::get(resultType, argTypes);
  auto funcOp = builder.create<mlir::LLVM::LLVMFuncOp>(builder.getUnknownLoc(), name, funcType);

  builder.restoreInsertionPoint(savedIP);
  return funcOp;
}

/// Get or declare an external function using func.func (used before
/// the full LLVM lowering, so we can use func.call).
mlir::func::FuncOp getOrInsertFuncDecl(mlir::ModuleOp module, mlir::OpBuilder &builder,
                                       llvm::StringRef name, mlir::FunctionType funcType) {
  if (auto func = module.lookupSymbol<mlir::func::FuncOp>(name))
    return func;

  auto savedIP = builder.saveInsertionPoint();
  builder.setInsertionPointToStart(module.getBody());

  auto funcOp = builder.create<mlir::func::FuncOp>(builder.getUnknownLoc(), name, funcType);
  funcOp.setVisibility(mlir::SymbolTable::Visibility::Private);

  builder.restoreInsertionPoint(savedIP);
  return funcOp;
}

/// Lower hew.print -> func.call to the appropriate runtime print function.
struct PrintOpLowering : public mlir::OpConversionPattern<hew::PrintOp> {
  using OpConversionPattern<hew::PrintOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::PrintOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto inputVal = adaptor.getInput();
    auto inputType = inputVal.getType();
    bool newline = op.getNewline();
    bool isUnsigned = op->hasAttrOfType<mlir::BoolAttr>("is_unsigned") &&
                      op->getAttrOfType<mlir::BoolAttr>("is_unsigned").getValue();

    std::string funcName;
    mlir::FunctionType funcType;

    // Promote sub-i32 integer types (e.g. i8 from u8/byte) to i32.
    // Use zero-extension for unsigned source types.
    if (inputType.isInteger(8) || inputType.isInteger(16)) {
      if (isUnsigned)
        inputVal = rewriter.create<mlir::arith::ExtUIOp>(loc, rewriter.getI32Type(), inputVal);
      else
        inputVal = rewriter.create<mlir::arith::ExtSIOp>(loc, rewriter.getI32Type(), inputVal);
      inputType = rewriter.getI32Type();
    }

    if (inputType.isInteger(32)) {
      if (isUnsigned) {
        funcName = newline ? "hew_println_u32" : "hew_print_u32";
      } else {
        funcName = newline ? "hew_println_i32" : "hew_print_i32";
      }
      funcType = rewriter.getFunctionType({rewriter.getI32Type()}, {});
    } else if (inputType.isInteger(64)) {
      if (isUnsigned) {
        funcName = newline ? "hew_println_u64" : "hew_print_u64";
      } else {
        funcName = newline ? "hew_println_i64" : "hew_print_i64";
      }
      funcType = rewriter.getFunctionType({rewriter.getI64Type()}, {});
    } else if (inputType.isF64()) {
      funcName = newline ? "hew_println_f64" : "hew_print_f64";
      funcType = rewriter.getFunctionType({rewriter.getF64Type()}, {});
    } else if (inputType.isInteger(1)) {
      funcName = newline ? "hew_println_bool" : "hew_print_bool";
      funcType = rewriter.getFunctionType({rewriter.getI1Type()}, {});
    } else if (mlir::isa<mlir::LLVM::LLVMPointerType>(inputType)) {
      funcName = newline ? "hew_println_str" : "hew_print_str";
      funcType =
          rewriter.getFunctionType({mlir::LLVM::LLVMPointerType::get(rewriter.getContext())}, {});
    } else {
      // Fallback: try i32
      funcName = newline ? "hew_println_i32" : "hew_print_i32";
      funcType = rewriter.getFunctionType({rewriter.getI32Type()}, {});
    }

    getOrInsertFuncDecl(module, rewriter, funcName, funcType);
    rewriter.create<mlir::func::CallOp>(loc, funcName, mlir::TypeRange{},
                                        mlir::ValueRange{inputVal});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

/// Lower hew.constant -> arith.constant (for int/float/bool)
/// or llvm.mlir.addressof (for string references).
struct ConstantOpLowering : public mlir::OpConversionPattern<hew::ConstantOp> {
  using OpConversionPattern<hew::ConstantOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::ConstantOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto value = op.getValue();

    // String constant: value is a StringAttr containing the global symbol name
    if (auto strAttr = mlir::dyn_cast<mlir::StringAttr>(value)) {
      auto module = op->getParentOfType<mlir::ModuleOp>();
      auto symName = strAttr.getValue();

      // The global string should already exist as hew.global_string.
      // We need to produce an addressof for it. But at this stage, we haven't
      // lowered global_string yet, so we produce an llvm.mlir.addressof.
      // First, check if the LLVM global already exists:
      auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());

      if (!module.lookupSymbol<mlir::LLVM::GlobalOp>(symName)) {
        // Look up the hew.global_string op
        auto globalStr = module.lookupSymbol<hew::GlobalStringOp>(symName);
        if (globalStr) {
          auto strValue = globalStr.getValue();
          // Create LLVM global
          auto savedIP = rewriter.saveInsertionPoint();
          rewriter.setInsertionPointToStart(module.getBody());
          auto i8Type = rewriter.getIntegerType(8);
          auto arrayType =
              mlir::LLVM::LLVMArrayType::get(i8Type, strValue.size() + 1); // +1 for null terminator
          rewriter.create<mlir::LLVM::GlobalOp>(
              loc, arrayType, /*isConstant=*/true, mlir::LLVM::Linkage::Internal, symName,
              rewriter.getStringAttr(std::string(strValue) + '\0'));
          rewriter.restoreInsertionPoint(savedIP);
        }
      }

      auto addrOp = rewriter.create<mlir::LLVM::AddressOfOp>(loc, ptrType, symName);
      rewriter.replaceOp(op, addrOp.getResult());
      return mlir::success();
    }

    // Integer constant
    if (auto intAttr = mlir::dyn_cast<mlir::IntegerAttr>(value)) {
      auto newOp = rewriter.create<mlir::arith::ConstantOp>(loc, intAttr);
      rewriter.replaceOp(op, newOp.getResult());
      return mlir::success();
    }

    // Float constant
    if (auto floatAttr = mlir::dyn_cast<mlir::FloatAttr>(value)) {
      auto newOp = rewriter.create<mlir::arith::ConstantOp>(loc, floatAttr);
      rewriter.replaceOp(op, newOp.getResult());
      return mlir::success();
    }

    // Bool constant (stored as IntegerAttr with i1 type)
    if (auto boolAttr = mlir::dyn_cast<mlir::BoolAttr>(value)) {
      auto intAttr = rewriter.getIntegerAttr(rewriter.getI1Type(), boolAttr.getValue() ? 1 : 0);
      auto newOp = rewriter.create<mlir::arith::ConstantOp>(loc, intAttr);
      rewriter.replaceOp(op, newOp.getResult());
      return mlir::success();
    }

    return mlir::failure();
  }
};

/// Lower hew.global_string -> llvm.mlir.global
struct GlobalStringOpLowering : public mlir::OpConversionPattern<hew::GlobalStringOp> {
  using OpConversionPattern<hew::GlobalStringOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::GlobalStringOp op, OpAdaptor /*adaptor*/,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto symName = op.getSymName();
    auto strValue = op.getValue();

    auto i8Type = rewriter.getIntegerType(8);
    auto arrayType =
        mlir::LLVM::LLVMArrayType::get(i8Type, strValue.size() + 1); // +1 for null terminator

    rewriter.replaceOpWithNewOp<mlir::LLVM::GlobalOp>(
        op, arrayType, /*isConstant=*/true, mlir::LLVM::Linkage::Internal, symName,
        rewriter.getStringAttr(std::string(strValue) + '\0'));
    return mlir::success();
  }
};

/// Lower hew.cast -> appropriate LLVM/arith cast operations
struct CastOpLowering : public mlir::OpConversionPattern<hew::CastOp> {
  using OpConversionPattern<hew::CastOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::CastOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto inputVal = adaptor.getInput();
    auto inputType = inputVal.getType();
    auto resultType = op.getType();
    bool isUnsigned = op->hasAttrOfType<mlir::BoolAttr>("is_unsigned") &&
                      op->getAttrOfType<mlir::BoolAttr>("is_unsigned").getValue();

    // Int to float
    if (inputType.isIntOrIndex() && mlir::isa<mlir::FloatType>(resultType)) {
      if (isUnsigned)
        rewriter.replaceOpWithNewOp<mlir::arith::UIToFPOp>(op, resultType, inputVal);
      else
        rewriter.replaceOpWithNewOp<mlir::arith::SIToFPOp>(op, resultType, inputVal);
      return mlir::success();
    }

    // Float to int
    if (mlir::isa<mlir::FloatType>(inputType) && resultType.isIntOrIndex()) {
      if (isUnsigned)
        rewriter.replaceOpWithNewOp<mlir::arith::FPToUIOp>(op, resultType, inputVal);
      else
        rewriter.replaceOpWithNewOp<mlir::arith::FPToSIOp>(op, resultType, inputVal);
      return mlir::success();
    }

    // Int widening/narrowing
    if (inputType.isIntOrIndex() && resultType.isIntOrIndex()) {
      auto inWidth = inputType.getIntOrFloatBitWidth();
      auto outWidth = resultType.getIntOrFloatBitWidth();
      if (outWidth > inWidth) {
        if (isUnsigned)
          rewriter.replaceOpWithNewOp<mlir::arith::ExtUIOp>(op, resultType, inputVal);
        else
          rewriter.replaceOpWithNewOp<mlir::arith::ExtSIOp>(op, resultType, inputVal);
        return mlir::success();
      }
      if (outWidth < inWidth) {
        rewriter.replaceOpWithNewOp<mlir::arith::TruncIOp>(op, resultType, inputVal);
        return mlir::success();
      }
      // Same width: just replace
      rewriter.replaceOp(op, inputVal);
      return mlir::success();
    }

    return mlir::failure();
  }
};

/// Lower hew.struct_init -> llvm.mlir.undef + llvm.insertvalue chain
struct StructInitOpLowering : public mlir::OpConversionPattern<hew::StructInitOp> {
  using OpConversionPattern<hew::StructInitOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::StructInitOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto structType = op.getResult().getType();

    // Start with an undef struct value
    mlir::Value result = rewriter.create<mlir::LLVM::UndefOp>(loc, structType);

    // Insert each field value
    auto fields = adaptor.getFields();
    for (auto [idx, fieldVal] : llvm::enumerate(fields)) {
      result = rewriter.create<mlir::LLVM::InsertValueOp>(loc, result, fieldVal, idx);
    }

    rewriter.replaceOp(op, result);
    return mlir::success();
  }
};

/// Lower hew.field_get -> llvm.extractvalue
struct FieldGetOpLowering : public mlir::OpConversionPattern<hew::FieldGetOp> {
  using OpConversionPattern<hew::FieldGetOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::FieldGetOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto fieldIndex = op.getFieldIndex();

    auto result =
        rewriter.create<mlir::LLVM::ExtractValueOp>(loc, adaptor.getStructVal(), fieldIndex);

    rewriter.replaceOp(op, result.getResult());
    return mlir::success();
  }
};

/// Lower hew.field_set -> llvm.insertvalue
struct FieldSetOpLowering : public mlir::OpConversionPattern<hew::FieldSetOp> {
  using OpConversionPattern<hew::FieldSetOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::FieldSetOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto fieldIndex = op.getFieldIndex();

    auto result = rewriter.create<mlir::LLVM::InsertValueOp>(loc, adaptor.getStructVal(),
                                                             adaptor.getValue(), fieldIndex);

    rewriter.replaceOp(op, result.getResult());
    return mlir::success();
  }
};

// ============================================================================
// Actor operation lowering patterns (Phase 1)
// ============================================================================

/// Helper: compute sizeof(type) using the GEP-null trick.
/// Returns an i64 value representing the byte size of `structType`.
static mlir::Value emitSizeOf(mlir::ConversionPatternRewriter &rewriter, mlir::Location loc,
                              mlir::Type structType) {
  auto *ctx = rewriter.getContext();
  auto ptrType = mlir::LLVM::LLVMPointerType::get(ctx);
  auto module = rewriter.getInsertionBlock()->getParentOp()->getParentOfType<mlir::ModuleOp>();
  auto sizeType = getSizeType(ctx, module);
  auto nullPtr = rewriter.create<mlir::LLVM::ZeroOp>(loc, ptrType);
  auto sizeGep = rewriter.create<mlir::LLVM::GEPOp>(loc, ptrType, structType, nullPtr,
                                                    llvm::ArrayRef<mlir::LLVM::GEPArg>{1});
  return rewriter.create<mlir::LLVM::PtrToIntOp>(loc, sizeType, sizeGep);
}

/// Helper: allocate one element of `elemType` in the enclosing function entry
/// block when possible to avoid path/loop-local allocas.
static mlir::Value emitEntryAlloca(mlir::ConversionPatternRewriter &rewriter, mlir::Location loc,
                                   mlir::Type elemType) {
  auto *ctx = rewriter.getContext();
  auto ptrType = mlir::LLVM::LLVMPointerType::get(ctx);
  auto module = rewriter.getInsertionBlock()
                    ? rewriter.getInsertionBlock()->getParentOp()->getParentOfType<mlir::ModuleOp>()
                    : mlir::ModuleOp();
  auto sizeType = getSizeType(ctx, module);
  auto createAtCurrentIP = [&]() -> mlir::Value {
    auto one = rewriter.create<mlir::arith::ConstantIntOp>(loc, sizeType, 1);
    return rewriter.create<mlir::LLVM::AllocaOp>(loc, ptrType, elemType, one).getResult();
  };

  auto *insertionBlock = rewriter.getInsertionBlock();
  if (!insertionBlock) {
    return createAtCurrentIP();
  }

  auto funcOp = insertionBlock->getParentOp()->getParentOfType<mlir::func::FuncOp>();
  if (!funcOp || funcOp.empty()) {
    return createAtCurrentIP();
  }

  mlir::OpBuilder::InsertionGuard guard(rewriter);
  rewriter.setInsertionPointToStart(&funcOp.front());
  return createAtCurrentIP();
}

/// Deep-copy owned payload values (strings, vecs, hashmaps, closure envs) so
/// the message buffer holds independently-owned data. Without this, the
/// sender's scope-exit drops would free memory while the receiver still holds
/// dangling references.
static llvm::SmallVector<mlir::Value, 4>
deepCopyOwnedArgs(mlir::ConversionPatternRewriter &rewriter, mlir::Location loc,
                  mlir::ModuleOp module, mlir::ValueRange originalArgs,
                  mlir::ValueRange convertedArgs) {
  auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
  llvm::SmallVector<mlir::Value, 4> result;
  for (auto [origArg, convArg] : llvm::zip(originalArgs, convertedArgs)) {
    auto origType = origArg.getType();
    if (mlir::isa<hew::StringRefType>(origType)) {
      auto ft = rewriter.getFunctionType({ptrType}, {ptrType});
      getOrInsertFuncDecl(module, rewriter, "strdup", ft);
      auto cloned = rewriter.create<mlir::func::CallOp>(loc, "strdup", mlir::TypeRange{ptrType},
                                                        mlir::ValueRange{convArg});
      result.push_back(cloned.getResult(0));
    } else if (mlir::isa<hew::VecType>(origType)) {
      auto ft = rewriter.getFunctionType({ptrType}, {ptrType});
      getOrInsertFuncDecl(module, rewriter, "hew_vec_clone", ft);
      auto cloned = rewriter.create<mlir::func::CallOp>(
          loc, "hew_vec_clone", mlir::TypeRange{ptrType}, mlir::ValueRange{convArg});
      result.push_back(cloned.getResult(0));
    } else if (mlir::isa<hew::HashMapType>(origType)) {
      auto ft = rewriter.getFunctionType({ptrType}, {ptrType});
      getOrInsertFuncDecl(module, rewriter, "hew_hashmap_clone_impl", ft);
      auto cloned = rewriter.create<mlir::func::CallOp>(
          loc, "hew_hashmap_clone_impl", mlir::TypeRange{ptrType}, mlir::ValueRange{convArg});
      result.push_back(cloned.getResult(0));
    } else if (mlir::isa<hew::ClosureType>(origType)) {
      auto closureType = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(convArg.getType());
      if (!closureType || closureType.getBody().size() != 2) {
        result.push_back(convArg);
        continue;
      }
      auto fnPtr = rewriter.create<mlir::LLVM::ExtractValueOp>(loc, convArg, 0).getResult();
      auto envPtr = rewriter.create<mlir::LLVM::ExtractValueOp>(loc, convArg, 1).getResult();
      auto ft = rewriter.getFunctionType({ptrType}, {ptrType});
      getOrInsertFuncDecl(module, rewriter, "hew_rc_clone", ft);
      auto clonedEnv = rewriter.create<mlir::func::CallOp>(
          loc, "hew_rc_clone", mlir::TypeRange{ptrType}, mlir::ValueRange{envPtr});
      mlir::Value rebuilt = rewriter.create<mlir::LLVM::UndefOp>(loc, closureType);
      rebuilt = rewriter.create<mlir::LLVM::InsertValueOp>(loc, rebuilt, fnPtr, 0);
      rebuilt = rewriter.create<mlir::LLVM::InsertValueOp>(loc, rebuilt, clonedEnv.getResult(0), 1);
      result.push_back(rebuilt);
    } else {
      result.push_back(convArg);
    }
  }
  return result;
}

/// Helper: pack variadic args into a stack-allocated struct and return
/// (data_ptr, data_size).  If args is empty, returns (null, 0).
static std::pair<mlir::Value, mlir::Value> emitPackArgs(mlir::ConversionPatternRewriter &rewriter,
                                                        mlir::Location loc, mlir::ValueRange args) {
  auto *ctx = rewriter.getContext();
  auto ptrType = mlir::LLVM::LLVMPointerType::get(ctx);
  auto module = rewriter.getInsertionBlock()->getParentOp()->getParentOfType<mlir::ModuleOp>();
  auto sizeType = getSizeType(ctx, module);

  if (args.empty()) {
    auto nullPtr = rewriter.create<mlir::LLVM::ZeroOp>(loc, ptrType);
    auto zero = rewriter.create<mlir::arith::ConstantIntOp>(loc, sizeType, 0);
    return {nullPtr, zero};
  }

  if (args.size() == 1) {
    auto argType = args[0].getType();
    auto alloca = emitEntryAlloca(rewriter, loc, argType);
    rewriter.create<mlir::LLVM::StoreOp>(loc, args[0], alloca);
    auto dataSize = emitSizeOf(rewriter, loc, argType);
    return {alloca, dataSize};
  }

  // Multiple args: pack into anonymous struct
  llvm::SmallVector<mlir::Type, 4> fieldTypes;
  for (auto v : args)
    fieldTypes.push_back(v.getType());
  auto packType = mlir::LLVM::LLVMStructType::getLiteral(rewriter.getContext(), fieldTypes);

  auto alloca = emitEntryAlloca(rewriter, loc, packType);

  for (auto [idx, val] : llvm::enumerate(args)) {
    auto fieldPtr = rewriter.create<mlir::LLVM::GEPOp>(
        loc, ptrType, packType, alloca,
        llvm::ArrayRef<mlir::LLVM::GEPArg>{0, static_cast<int32_t>(idx)});
    rewriter.create<mlir::LLVM::StoreOp>(loc, val, fieldPtr);
  }
  auto dataSize = emitSizeOf(rewriter, loc, packType);
  return {alloca, dataSize};
}

// ============================================================================
// New semantic ops — lowering patterns
// ============================================================================

/// Lower hew.sizeof -> null-GEP trick (ZeroOp + GEPOp + PtrToIntOp)
struct SizeOfOpLowering : public mlir::OpConversionPattern<hew::SizeOfOp> {
  using OpConversionPattern<hew::SizeOfOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::SizeOfOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto measuredType = op.getMeasuredType();
    rewriter.replaceOp(op, emitSizeOf(rewriter, loc, measuredType));
    return mlir::success();
  }
};

/// Lower hew.enum_construct -> undef + insertvalue chain
struct EnumConstructOpLowering : public mlir::OpConversionPattern<hew::EnumConstructOp> {
  using OpConversionPattern<hew::EnumConstructOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::EnumConstructOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto resultType = getTypeConverter()->convertType(op.getResult().getType());
    int32_t variantIdx = op.getVariantIndex();

    // Unit variant on bare i32 enum (no struct wrapper)
    if (mlir::isa<mlir::IntegerType>(resultType)) {
      auto tagVal = rewriter.create<mlir::arith::ConstantIntOp>(loc, resultType, variantIdx);
      rewriter.replaceOp(op, tagVal.getResult());
      return mlir::success();
    }

    // Struct-based enum: undef + insertvalue(tag at 0) + insertvalue(payloads)
    mlir::Value result = rewriter.create<mlir::LLVM::UndefOp>(loc, resultType);
    auto i32Type = rewriter.getI32Type();
    auto tagVal = rewriter.create<mlir::arith::ConstantIntOp>(loc, i32Type, variantIdx);
    result =
        rewriter.create<mlir::LLVM::InsertValueOp>(loc, result, tagVal, llvm::ArrayRef<int64_t>{0});

    auto payloads = adaptor.getPayloads();
    auto positions = op.getPayloadPositions();
    for (auto [i, payload] : llvm::enumerate(payloads)) {
      int64_t pos;
      if (positions) {
        pos = mlir::cast<mlir::IntegerAttr>((*positions)[i]).getInt();
      } else if (op.getEnumName() == "__Result") {
        pos = static_cast<int64_t>(variantIdx) + 1 + static_cast<int64_t>(i);
      } else {
        pos = static_cast<int64_t>(i) + 1;
      }
      result = rewriter.create<mlir::LLVM::InsertValueOp>(loc, result, payload,
                                                          llvm::ArrayRef<int64_t>{pos});
    }
    rewriter.replaceOp(op, result);
    return mlir::success();
  }
};

/// Lower hew.enum_extract_tag -> extractvalue [0] or passthrough for bare i32
struct EnumExtractTagOpLowering : public mlir::OpConversionPattern<hew::EnumExtractTagOp> {
  using OpConversionPattern<hew::EnumExtractTagOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::EnumExtractTagOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto enumVal = adaptor.getEnumVal();
    auto enumType = enumVal.getType();

    if (mlir::isa<mlir::IntegerType>(enumType)) {
      // All-unit-variant enum: the value IS the tag
      rewriter.replaceOp(op, enumVal);
    } else {
      auto tag =
          rewriter.create<mlir::LLVM::ExtractValueOp>(loc, enumVal, llvm::ArrayRef<int64_t>{0});
      rewriter.replaceOp(op, tag.getResult());
    }
    return mlir::success();
  }
};

/// Lower hew.enum_extract_payload -> extractvalue [field_index]
struct EnumExtractPayloadOpLowering : public mlir::OpConversionPattern<hew::EnumExtractPayloadOp> {
  using OpConversionPattern<hew::EnumExtractPayloadOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::EnumExtractPayloadOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto fieldIdx = static_cast<int64_t>(op.getFieldIndex());
    auto val = rewriter.create<mlir::LLVM::ExtractValueOp>(loc, adaptor.getEnumVal(),
                                                           llvm::ArrayRef<int64_t>{fieldIdx});
    rewriter.replaceOp(op, val.getResult());
    return mlir::success();
  }
};

/// Convert memref.alloca with Hew element types to lowered element types.
struct MemRefAllocaTypeConversion : public mlir::OpConversionPattern<mlir::memref::AllocaOp> {
  using OpConversionPattern<mlir::memref::AllocaOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(mlir::memref::AllocaOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto memrefType = op.getType();
    auto convertedElemType = getTypeConverter()->convertType(memrefType.getElementType());
    if (!convertedElemType || convertedElemType == memrefType.getElementType())
      return mlir::failure();
    auto newMemRefType = mlir::MemRefType::get(memrefType.getShape(), convertedElemType,
                                               memrefType.getLayout(), memrefType.getMemorySpace());
    rewriter.replaceOpWithNewOp<mlir::memref::AllocaOp>(op, newMemRefType);
    return mlir::success();
  }
};

/// Convert memref.load with Hew element types.
struct MemRefLoadTypeConversion : public mlir::OpConversionPattern<mlir::memref::LoadOp> {
  using OpConversionPattern<mlir::memref::LoadOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(mlir::memref::LoadOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto resultType = getTypeConverter()->convertType(op.getResult().getType());
    if (!resultType || resultType == op.getResult().getType())
      return mlir::failure();
    rewriter.replaceOpWithNewOp<mlir::memref::LoadOp>(op, adaptor.getMemref(),
                                                      adaptor.getIndices());
    return mlir::success();
  }
};

/// Convert memref.store with Hew element types.
struct MemRefStoreTypeConversion : public mlir::OpConversionPattern<mlir::memref::StoreOp> {
  using OpConversionPattern<mlir::memref::StoreOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(mlir::memref::StoreOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto valueType = op.getValueToStore().getType();
    auto convertedType = getTypeConverter()->convertType(valueType);
    if (!convertedType || convertedType == valueType)
      return mlir::failure();
    rewriter.replaceOpWithNewOp<mlir::memref::StoreOp>(op, adaptor.getValue(), adaptor.getMemref(),
                                                       adaptor.getIndices());
    return mlir::success();
  }
};

/// Lower hew.pack_args -> alloca + GEP + store + sizeof
struct PackArgsOpLowering : public mlir::OpConversionPattern<hew::PackArgsOp> {
  using OpConversionPattern<hew::PackArgsOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::PackArgsOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto module = op->getParentOfType<mlir::ModuleOp>();
    // Deep-copy owned values (strings, vecs) for actor isolation.
    auto clonedArgs = deepCopyOwnedArgs(rewriter, loc, module, op.getArgs(), adaptor.getArgs());
    auto [dataPtr, dataSize] = emitPackArgs(rewriter, loc, clonedArgs);
    rewriter.replaceOp(op, {dataPtr, dataSize});
    return mlir::success();
  }
};

/// Lower hew.actor_spawn -> alloca state + store init args + runtime call
struct ActorSpawnOpLowering : public mlir::OpConversionPattern<hew::ActorSpawnOp> {
  using OpConversionPattern<hew::ActorSpawnOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::ActorSpawnOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto *ctx = rewriter.getContext();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(ctx);
    auto i32Type = rewriter.getI32Type();
    auto sizeType = getSizeType(ctx, module);

    // Recover state struct type from the TypeAttr
    // ODS unwraps TypeAttr automatically: getStateType() returns mlir::Type
    auto stateType = op.getStateType();

    // 1. Allocate state struct on stack
    auto stateAlloca = emitEntryAlloca(rewriter, loc, stateType);

    // 2. Store init args into state struct fields.
    // Deep-copy owned pointer types (strings, vecs) so the spawned actor
    // owns independent copies that outlive the spawning scope.
    auto initArgs = adaptor.getInitArgs();
    auto origInitArgs = op.getInitArgs();
    auto clonedInitArgs = deepCopyOwnedArgs(rewriter, loc, module, origInitArgs, initArgs);
    for (auto [idx, argVal] : llvm::enumerate(clonedInitArgs)) {
      auto fieldPtr = rewriter.create<mlir::LLVM::GEPOp>(
          loc, ptrType, stateType, stateAlloca,
          llvm::ArrayRef<mlir::LLVM::GEPArg>{0, static_cast<int32_t>(idx)});
      rewriter.create<mlir::LLVM::StoreOp>(loc, argVal, fieldPtr);
    }

    // 2b. Call ActorName_init(state) if it exists in the module
    {
      std::string initName = op.getActorName().str() + "_init";
      if (module.lookupSymbol<mlir::func::FuncOp>(initName)) {
        auto initFuncType = rewriter.getFunctionType({ptrType}, {});
        getOrInsertFuncDecl(module, rewriter, initName, initFuncType);
        rewriter.create<mlir::func::CallOp>(loc, initName, mlir::TypeRange{},
                                            mlir::ValueRange{stateAlloca});
      }
    }

    // 3. Compute sizeof(state)
    auto stateSize = emitSizeOf(rewriter, loc, stateType);

    // 4. Get dispatch function pointer
    // ODS unwraps FlatSymbolRefAttr: getDispatchFn() returns StringRef
    auto dispatchName = op.getDispatchFn();
    auto dispatchFuncType = rewriter.getFunctionType({ptrType, i32Type, ptrType, sizeType}, {});
    getOrInsertFuncDecl(module, rewriter, dispatchName, dispatchFuncType);
    // Create a func-typed reference then cast to !llvm.ptr via
    // UnrealizedConversionCastOp. This bridge is needed because the dispatch
    // function is a func.func at this stage (FuncToLLVM hasn't run yet), but
    // hew_actor_spawn expects an !llvm.ptr. ReconcileUnrealizedCasts removes
    // the cast after FuncToLLVM converts func.func → llvm.func.
    auto funcRef = rewriter.create<mlir::func::ConstantOp>(
        loc, dispatchFuncType, mlir::SymbolRefAttr::get(rewriter.getContext(), dispatchName));
    auto dispatchPtr =
        rewriter.create<mlir::UnrealizedConversionCastOp>(loc, ptrType, funcRef.getResult())
            .getResult(0);

    // 5. Call runtime spawn
    mlir::Value result;

    // Check if this actor uses coalesce overflow policy (spawn via opts struct)
    if (op.getCoalesceKeyFn().has_value()) {
      // Use hew_actor_spawn_opts with full options struct
      // HewActorOpts: { ptr, usize, fn_ptr, i32, i32, fn_ptr, i32, i32 }
      auto optsStructType = mlir::LLVM::LLVMStructType::getLiteral(
          ctx, {ptrType, sizeType, ptrType, i32Type, i32Type, ptrType, i32Type, i32Type});

      auto optsAlloca = emitEntryAlloca(rewriter, loc, optsStructType);

      // Store fields into opts struct
      auto storeField = [&](unsigned idx, mlir::Value val) {
        auto gep = rewriter.create<mlir::LLVM::GEPOp>(
            loc, ptrType, optsStructType, optsAlloca,
            llvm::ArrayRef<mlir::LLVM::GEPArg>{0, static_cast<int32_t>(idx)});
        rewriter.create<mlir::LLVM::StoreOp>(loc, val, gep);
      };

      // init_state (ptr)
      storeField(0, stateAlloca);
      // state_size (usize/i64)
      storeField(1, stateSize);
      // dispatch (fn ptr)
      storeField(2, dispatchPtr);
      // mailbox_capacity (i32)
      auto capVal = rewriter.create<mlir::arith::ConstantIntOp>(
          loc, i32Type,
          op.getMailboxCapacity().has_value()
              ? static_cast<int64_t>(op.getMailboxCapacity().value())
              : -1LL);
      storeField(3, capVal);
      // overflow policy (i32) — map from spec encoding to runtime enum
      // Runtime: 0=DropNew, 1=DropOld, 2=Block, 3=Fail, 4=Coalesce
      int32_t runtimeOverflow = 4; // Coalesce (we only get here for coalesce)
      auto overflowVal = rewriter.create<mlir::arith::ConstantIntOp>(loc, i32Type, runtimeOverflow);
      storeField(4, overflowVal);
      // coalesce_key_fn (fn ptr)
      auto keyFnName = op.getCoalesceKeyFn().value();
      auto keyFnFuncType = rewriter.getFunctionType({i32Type, ptrType, sizeType}, {sizeType});
      getOrInsertFuncDecl(module, rewriter, keyFnName, keyFnFuncType);
      auto keyFuncRef = rewriter.create<mlir::func::ConstantOp>(
          loc, keyFnFuncType, mlir::SymbolRefAttr::get(rewriter.getContext(), keyFnName));
      auto keyFnPtr =
          rewriter.create<mlir::UnrealizedConversionCastOp>(loc, ptrType, keyFuncRef.getResult())
              .getResult(0);
      storeField(5, keyFnPtr);
      // coalesce_fallback (i32)
      // Runtime enum: Block=0, DropNew=1, DropOld=2, Fail=3, Coalesce=4
      int32_t runtimeFallback = 1; // DropNew (default)
      if (op.getCoalesceFallback().has_value()) {
        int32_t fb = static_cast<int32_t>(op.getCoalesceFallback().value());
        // Spec encoding → runtime encoding:
        //   1 (drop_new) → 1 (DropNew)
        //   2 (drop_old) → 2 (DropOld)
        //   3 (block)    → 0 (Block)
        //   4 (fail)     → 3 (Fail)
        switch (fb) {
        case 1:
          runtimeFallback = 1;
          break; // DropNew
        case 2:
          runtimeFallback = 2;
          break; // DropOld
        case 3:
          runtimeFallback = 0;
          break; // Block
        case 4:
          runtimeFallback = 3;
          break; // Fail
        default:
          runtimeFallback = 1;
          break; // DropNew
        }
      }
      auto fallbackVal = rewriter.create<mlir::arith::ConstantIntOp>(loc, i32Type, runtimeFallback);
      storeField(6, fallbackVal);
      // budget (i32) — 0 = default
      auto budgetVal = rewriter.create<mlir::arith::ConstantIntOp>(loc, i32Type, 0);
      storeField(7, budgetVal);

      auto spawnOptsFuncType = rewriter.getFunctionType({ptrType}, {ptrType});
      getOrInsertFuncDecl(module, rewriter, "hew_actor_spawn_opts", spawnOptsFuncType);
      auto call = rewriter.create<mlir::func::CallOp>(
          loc, "hew_actor_spawn_opts", mlir::TypeRange{ptrType}, mlir::ValueRange{optsAlloca});
      result = call.getResult(0);
    } else if (op.getMailboxCapacity().has_value()) {
      auto spawnFuncType =
          rewriter.getFunctionType({ptrType, sizeType, ptrType, i32Type}, {ptrType});
      getOrInsertFuncDecl(module, rewriter, "hew_actor_spawn_bounded", spawnFuncType);
      auto capVal = rewriter.create<mlir::arith::ConstantIntOp>(
          loc, i32Type, static_cast<int64_t>(op.getMailboxCapacity().value()));
      llvm::SmallVector<mlir::Value, 4> spawnArgs = {stateAlloca, stateSize, dispatchPtr, capVal};
      auto call = rewriter.create<mlir::func::CallOp>(loc, "hew_actor_spawn_bounded",
                                                      mlir::TypeRange{ptrType}, spawnArgs);
      result = call.getResult(0);
    } else {
      auto spawnFuncType = rewriter.getFunctionType({ptrType, sizeType, ptrType}, {ptrType});
      getOrInsertFuncDecl(module, rewriter, "hew_actor_spawn", spawnFuncType);
      llvm::SmallVector<mlir::Value, 3> spawnArgs = {stateAlloca, stateSize, dispatchPtr};
      auto call = rewriter.create<mlir::func::CallOp>(loc, "hew_actor_spawn",
                                                      mlir::TypeRange{ptrType}, spawnArgs);
      result = call.getResult(0);
    }

    rewriter.replaceOp(op, result);
    return mlir::success();
  }
};

/// Lower hew.actor_send -> pack args + func.call @hew_actor_send
struct ActorSendOpLowering : public mlir::OpConversionPattern<hew::ActorSendOp> {
  using OpConversionPattern<hew::ActorSendOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::ActorSendOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto *ctx = rewriter.getContext();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(ctx);
    auto i32Type = rewriter.getI32Type();
    auto sizeType = getSizeType(ctx, module);

    auto targetVal = adaptor.getTarget();
    auto msgTypeVal = rewriter.create<mlir::arith::ConstantIntOp>(
        loc, i32Type, static_cast<int64_t>(op.getMsgType()));

    // Deep-copy owned values (strings, vecs) so the receiver gets
    // independent copies that survive the sender's scope-exit drops.
    auto clonedArgs = deepCopyOwnedArgs(rewriter, loc, module, op.getArgs(), adaptor.getArgs());
    auto [dataPtr, dataSize] = emitPackArgs(rewriter, loc, clonedArgs);

    auto sendFuncType = rewriter.getFunctionType({ptrType, i32Type, ptrType, sizeType}, {});
    getOrInsertFuncDecl(module, rewriter, "hew_actor_send", sendFuncType);
    rewriter.create<mlir::func::CallOp>(loc, "hew_actor_send", mlir::TypeRange{},
                                        mlir::ValueRange{targetVal, msgTypeVal, dataPtr, dataSize});

    rewriter.eraseOp(op);
    return mlir::success();
  }
};

/// Lower hew.actor_ask -> pack args + func.call @hew_actor_ask (blocking)
struct ActorAskOpLowering : public mlir::OpConversionPattern<hew::ActorAskOp> {
  using OpConversionPattern<hew::ActorAskOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::ActorAskOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto *ctx = rewriter.getContext();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(ctx);
    auto i32Type = rewriter.getI32Type();
    auto sizeType = getSizeType(ctx, module);

    auto targetVal = adaptor.getTarget();
    auto msgTypeVal = rewriter.create<mlir::arith::ConstantIntOp>(
        loc, i32Type, static_cast<int64_t>(op.getMsgType()));

    // Deep-copy owned values (strings, vecs) for the same reason as actor_send.
    auto clonedArgs = deepCopyOwnedArgs(rewriter, loc, module, op.getArgs(), adaptor.getArgs());
    auto [dataPtr, dataSize] = emitPackArgs(rewriter, loc, clonedArgs);

    // Phase 1: blocking ask — hew_actor_ask returns void*
    auto askFuncType = rewriter.getFunctionType({ptrType, i32Type, ptrType, sizeType}, {ptrType});
    getOrInsertFuncDecl(module, rewriter, "hew_actor_ask", askFuncType);
    auto call = rewriter.create<mlir::func::CallOp>(
        loc, "hew_actor_ask", mlir::TypeRange{ptrType},
        mlir::ValueRange{targetVal, msgTypeVal, dataPtr, dataSize});
    auto replyPtr = call.getResult(0);

    // Load the result from the reply pointer — always load as ptr since
    // custom types like !hew.string_ref can't be loaded by LLVM directly
    auto resultType = op.getResult().getType();
    mlir::Value resultVal;
    if (resultType == ptrType || llvm::isa<mlir::LLVM::LLVMPointerType>(resultType)) {
      auto loaded = rewriter.create<mlir::LLVM::LoadOp>(loc, ptrType, replyPtr);
      resultVal = loaded.getResult();
    } else if (resultType == i32Type || resultType == rewriter.getI64Type() ||
               llvm::isa<mlir::IntegerType>(resultType) || llvm::isa<mlir::FloatType>(resultType) ||
               llvm::isa<mlir::LLVM::LLVMStructType>(resultType)) {
      auto loaded = rewriter.create<mlir::LLVM::LoadOp>(loc, resultType, replyPtr);
      resultVal = loaded.getResult();
    } else {
      // Custom types (e.g., !hew.string_ref): load as ptr, then cast
      auto loaded = rewriter.create<mlir::LLVM::LoadOp>(loc, ptrType, replyPtr);
      resultVal = rewriter
                      .create<mlir::UnrealizedConversionCastOp>(
                          loc, resultType, mlir::ValueRange{loaded.getResult()})
                      .getResult(0);
    }

    // Free the malloc'd reply buffer (allocated by hew_reply, returned by hew_actor_ask)
    auto freeFuncType = rewriter.getFunctionType({ptrType}, {});
    getOrInsertFuncDecl(module, rewriter, "free", freeFuncType);
    rewriter.create<mlir::func::CallOp>(loc, "free", mlir::TypeRange{}, mlir::ValueRange{replyPtr});

    rewriter.replaceOp(op, resultVal);
    return mlir::success();
  }
};

/// Lower hew.actor_stop -> func.call @hew_actor_stop
struct ActorStopOpLowering : public mlir::OpConversionPattern<hew::ActorStopOp> {
  using OpConversionPattern<hew::ActorStopOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::ActorStopOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());

    auto stopFuncType = rewriter.getFunctionType({ptrType}, {});
    getOrInsertFuncDecl(module, rewriter, "hew_actor_stop", stopFuncType);
    rewriter.create<mlir::func::CallOp>(loc, "hew_actor_stop", mlir::TypeRange{},
                                        mlir::ValueRange{adaptor.getTarget()});

    rewriter.eraseOp(op);
    return mlir::success();
  }
};

/// Lower hew.actor_close -> func.call @hew_actor_close
struct ActorCloseOpLowering : public mlir::OpConversionPattern<hew::ActorCloseOp> {
  using OpConversionPattern<hew::ActorCloseOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::ActorCloseOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());

    auto closeFuncType = rewriter.getFunctionType({ptrType}, {});
    getOrInsertFuncDecl(module, rewriter, "hew_actor_close", closeFuncType);
    rewriter.create<mlir::func::CallOp>(loc, "hew_actor_close", mlir::TypeRange{},
                                        mlir::ValueRange{adaptor.getTarget()});

    rewriter.eraseOp(op);
    return mlir::success();
  }
};

/// Lower hew.actor.self -> func.call @hew_actor_self
struct ActorSelfOpLowering : public mlir::OpConversionPattern<hew::ActorSelfOp> {
  using OpConversionPattern<hew::ActorSelfOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::ActorSelfOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());

    auto funcType = rewriter.getFunctionType({}, {ptrType});
    getOrInsertFuncDecl(module, rewriter, "hew_actor_self", funcType);
    auto call = rewriter.create<mlir::func::CallOp>(loc, "hew_actor_self", mlir::TypeRange{ptrType},
                                                    mlir::ValueRange{});
    rewriter.replaceOp(op, call.getResult(0));
    return mlir::success();
  }
};

/// Lower hew.actor.link -> func.call @hew_actor_link
struct ActorLinkOpLowering : public mlir::OpConversionPattern<hew::ActorLinkOp> {
  using OpConversionPattern<hew::ActorLinkOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::ActorLinkOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());

    auto funcType = rewriter.getFunctionType({ptrType, ptrType}, {});
    getOrInsertFuncDecl(module, rewriter, "hew_actor_link", funcType);
    rewriter.create<mlir::func::CallOp>(
        loc, "hew_actor_link", mlir::TypeRange{},
        mlir::ValueRange{adaptor.getSelfRef(), adaptor.getTarget()});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

/// Lower hew.actor.unlink -> func.call @hew_actor_unlink
struct ActorUnlinkOpLowering : public mlir::OpConversionPattern<hew::ActorUnlinkOp> {
  using OpConversionPattern<hew::ActorUnlinkOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::ActorUnlinkOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());

    auto funcType = rewriter.getFunctionType({ptrType, ptrType}, {});
    getOrInsertFuncDecl(module, rewriter, "hew_actor_unlink", funcType);
    rewriter.create<mlir::func::CallOp>(
        loc, "hew_actor_unlink", mlir::TypeRange{},
        mlir::ValueRange{adaptor.getSelfRef(), adaptor.getTarget()});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

/// Lower hew.actor.monitor -> func.call @hew_actor_monitor
struct ActorMonitorOpLowering : public mlir::OpConversionPattern<hew::ActorMonitorOp> {
  using OpConversionPattern<hew::ActorMonitorOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::ActorMonitorOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto i64Type = rewriter.getI64Type();

    auto funcType = rewriter.getFunctionType({ptrType, ptrType}, {i64Type});
    getOrInsertFuncDecl(module, rewriter, "hew_actor_monitor", funcType);
    auto call = rewriter.create<mlir::func::CallOp>(
        loc, "hew_actor_monitor", mlir::TypeRange{i64Type},
        mlir::ValueRange{adaptor.getSelfRef(), adaptor.getTarget()});
    rewriter.replaceOp(op, call.getResult(0));
    return mlir::success();
  }
};

/// Lower hew.actor.demonitor -> func.call @hew_actor_demonitor
struct ActorDemonitorOpLowering : public mlir::OpConversionPattern<hew::ActorDemonitorOp> {
  using OpConversionPattern<hew::ActorDemonitorOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::ActorDemonitorOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto i64Type = rewriter.getI64Type();

    auto funcType = rewriter.getFunctionType({i64Type}, {});
    getOrInsertFuncDecl(module, rewriter, "hew_actor_demonitor", funcType);
    rewriter.create<mlir::func::CallOp>(loc, "hew_actor_demonitor", mlir::TypeRange{},
                                        mlir::ValueRange{adaptor.getMonitorRef()});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

/// Lower hew.cooperate -> func.call @hew_actor_cooperate
struct CooperateOpLowering : public mlir::OpConversionPattern<hew::CooperateOp> {
  using OpConversionPattern<hew::CooperateOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::CooperateOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();

    auto funcType = rewriter.getFunctionType({}, {});
    getOrInsertFuncDecl(module, rewriter, "hew_actor_cooperate", funcType);
    rewriter.create<mlir::func::CallOp>(loc, "hew_actor_cooperate", mlir::TypeRange{},
                                        mlir::ValueRange{});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

/// Lower hew.sleep -> func.call @hew_sleep_ms
struct SleepOpLowering : public mlir::OpConversionPattern<hew::SleepOp> {
  using OpConversionPattern<hew::SleepOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::SleepOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto i32Type = rewriter.getI32Type();

    // hew_sleep_ms takes c_int (i32) — truncate if operand is i64
    auto funcType = rewriter.getFunctionType({i32Type}, {});
    getOrInsertFuncDecl(module, rewriter, "hew_sleep_ms", funcType);
    mlir::Value msVal = adaptor.getDurationMs();
    if (msVal.getType().isInteger(64)) {
      msVal = rewriter.create<mlir::arith::TruncIOp>(loc, i32Type, msVal);
    }
    rewriter.create<mlir::func::CallOp>(loc, "hew_sleep_ms", mlir::TypeRange{},
                                        mlir::ValueRange{msVal});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

/// Lower hew.panic -> func.call @hew_panic
struct PanicOpLowering : public mlir::OpConversionPattern<hew::PanicOp> {
  using OpConversionPattern<hew::PanicOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::PanicOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();

    auto funcType = rewriter.getFunctionType({}, {});
    getOrInsertFuncDecl(module, rewriter, "hew_panic", funcType);
    rewriter.create<mlir::func::CallOp>(loc, "hew_panic", mlir::TypeRange{}, mlir::ValueRange{});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

// ── Assert op lowerings ────────────────────────────────────────────────────

/// Lower hew.assert -> coerce condition to i64 + func.call @hew_assert
struct AssertOpLowering : public mlir::OpConversionPattern<hew::AssertOp> {
  using OpConversionPattern<hew::AssertOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::AssertOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto i64Type = rewriter.getI64Type();

    // Coerce the condition to i64
    mlir::Value cond = adaptor.getCondition();
    auto condType = cond.getType();
    if (condType.isInteger(1)) {
      cond = rewriter.create<mlir::arith::ExtUIOp>(loc, i64Type, cond);
    } else if (condType.isInteger(32)) {
      cond = rewriter.create<mlir::arith::ExtSIOp>(loc, i64Type, cond);
    }
    // else: already i64 (or compatible), pass through

    auto funcType = rewriter.getFunctionType({i64Type}, {});
    getOrInsertFuncDecl(module, rewriter, "hew_assert", funcType);
    rewriter.create<mlir::func::CallOp>(loc, "hew_assert", mlir::TypeRange{},
                                        mlir::ValueRange{cond});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

/// Lower hew.assert_eq -> type-polymorphic func.call @hew_assert_eq_*
struct AssertEqOpLowering : public mlir::OpConversionPattern<hew::AssertEqOp> {
  using OpConversionPattern<hew::AssertEqOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::AssertEqOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();

    mlir::Value left = adaptor.getLeft();
    mlir::Value right = adaptor.getRight();
    auto leftType = left.getType();

    std::string funcName = "hew_assert_eq_i64";
    mlir::FunctionType funcType;

    if (leftType.isF64()) {
      funcName = "hew_assert_eq_f64";
      funcType = rewriter.getFunctionType({rewriter.getF64Type(), rewriter.getF64Type()}, {});
    } else if (leftType.isInteger(1)) {
      funcName = "hew_assert_eq_bool";
      left = rewriter.create<mlir::arith::ExtUIOp>(loc, rewriter.getI32Type(), left);
      right = rewriter.create<mlir::arith::ExtUIOp>(loc, rewriter.getI32Type(), right);
      funcType = rewriter.getFunctionType({rewriter.getI32Type(), rewriter.getI32Type()}, {});
    } else if (mlir::isa<mlir::LLVM::LLVMPointerType>(leftType)) {
      // Covers both !hew.string_ref (already converted to !llvm.ptr)
      // and raw !llvm.ptr
      funcName = "hew_assert_eq_str";
      auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
      funcType = rewriter.getFunctionType({ptrType, ptrType}, {});
    } else if (leftType.isInteger(32)) {
      // Widen i32 to i64
      left = rewriter.create<mlir::arith::ExtSIOp>(loc, rewriter.getI64Type(), left);
      right = rewriter.create<mlir::arith::ExtSIOp>(loc, rewriter.getI64Type(), right);
      funcType = rewriter.getFunctionType({rewriter.getI64Type(), rewriter.getI64Type()}, {});
    } else {
      // Default: i64
      funcType = rewriter.getFunctionType({rewriter.getI64Type(), rewriter.getI64Type()}, {});
    }

    getOrInsertFuncDecl(module, rewriter, funcName, funcType);
    rewriter.create<mlir::func::CallOp>(loc, funcName, mlir::TypeRange{},
                                        mlir::ValueRange{left, right});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

/// Lower hew.assert_ne -> type-polymorphic func.call @hew_assert_ne_*
struct AssertNeOpLowering : public mlir::OpConversionPattern<hew::AssertNeOp> {
  using OpConversionPattern<hew::AssertNeOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::AssertNeOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();

    mlir::Value left = adaptor.getLeft();
    mlir::Value right = adaptor.getRight();
    auto leftType = left.getType();

    std::string funcName = "hew_assert_ne_i64";
    mlir::FunctionType funcType;

    if (leftType.isF64()) {
      funcName = "hew_assert_ne_f64";
      funcType = rewriter.getFunctionType({rewriter.getF64Type(), rewriter.getF64Type()}, {});
    } else if (leftType.isInteger(1)) {
      funcName = "hew_assert_ne_bool";
      left = rewriter.create<mlir::arith::ExtUIOp>(loc, rewriter.getI32Type(), left);
      right = rewriter.create<mlir::arith::ExtUIOp>(loc, rewriter.getI32Type(), right);
      funcType = rewriter.getFunctionType({rewriter.getI32Type(), rewriter.getI32Type()}, {});
    } else if (mlir::isa<mlir::LLVM::LLVMPointerType>(leftType)) {
      funcName = "hew_assert_ne_str";
      auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
      funcType = rewriter.getFunctionType({ptrType, ptrType}, {});
    } else if (leftType.isInteger(32)) {
      left = rewriter.create<mlir::arith::ExtSIOp>(loc, rewriter.getI64Type(), left);
      right = rewriter.create<mlir::arith::ExtSIOp>(loc, rewriter.getI64Type(), right);
      funcType = rewriter.getFunctionType({rewriter.getI64Type(), rewriter.getI64Type()}, {});
    } else {
      funcType = rewriter.getFunctionType({rewriter.getI64Type(), rewriter.getI64Type()}, {});
    }

    getOrInsertFuncDecl(module, rewriter, funcName, funcType);
    rewriter.create<mlir::func::CallOp>(loc, funcName, mlir::TypeRange{},
                                        mlir::ValueRange{left, right});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

// ── Supervisor op lowerings ────────────────────────────────────────────────

/// Lower hew.supervisor.new -> func.call @hew_supervisor_new
struct SupervisorNewOpLowering : public mlir::OpConversionPattern<hew::SupervisorNewOp> {
  using OpConversionPattern<hew::SupervisorNewOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::SupervisorNewOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto i32Type = rewriter.getI32Type();

    auto funcType = rewriter.getFunctionType({i32Type, i32Type, i32Type}, {ptrType});
    getOrInsertFuncDecl(module, rewriter, "hew_supervisor_new", funcType);
    auto call = rewriter.create<mlir::func::CallOp>(
        loc, "hew_supervisor_new", mlir::TypeRange{ptrType},
        mlir::ValueRange{adaptor.getStrategy(), adaptor.getMaxRestarts(), adaptor.getWindowSecs()});
    rewriter.replaceOp(op, call.getResult(0));
    return mlir::success();
  }
};

/// Lower hew.supervisor.start -> func.call @hew_supervisor_start
struct SupervisorStartOpLowering : public mlir::OpConversionPattern<hew::SupervisorStartOp> {
  using OpConversionPattern<hew::SupervisorStartOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::SupervisorStartOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto i32Type = rewriter.getI32Type();

    auto funcType = rewriter.getFunctionType({ptrType}, {i32Type});
    getOrInsertFuncDecl(module, rewriter, "hew_supervisor_start", funcType);
    auto call =
        rewriter.create<mlir::func::CallOp>(loc, "hew_supervisor_start", mlir::TypeRange{i32Type},
                                            mlir::ValueRange{adaptor.getSupervisor()});
    rewriter.replaceOp(op, call.getResult(0));
    return mlir::success();
  }
};

/// Lower hew.supervisor.stop -> func.call @hew_supervisor_stop
struct SupervisorStopOpLowering : public mlir::OpConversionPattern<hew::SupervisorStopOp> {
  using OpConversionPattern<hew::SupervisorStopOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::SupervisorStopOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());

    auto funcType = rewriter.getFunctionType({ptrType}, {});
    getOrInsertFuncDecl(module, rewriter, "hew_supervisor_stop", funcType);
    rewriter.create<mlir::func::CallOp>(loc, "hew_supervisor_stop", mlir::TypeRange{},
                                        mlir::ValueRange{adaptor.getSupervisor()});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

/// Lower hew.supervisor.add_child -> func.call @hew_supervisor_add_child_spec
struct SupervisorAddChildOpLowering : public mlir::OpConversionPattern<hew::SupervisorAddChildOp> {
  using OpConversionPattern<hew::SupervisorAddChildOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::SupervisorAddChildOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto i32Type = rewriter.getI32Type();

    auto funcType = rewriter.getFunctionType({ptrType, ptrType}, {i32Type});
    getOrInsertFuncDecl(module, rewriter, "hew_supervisor_add_child_spec", funcType);
    auto call = rewriter.create<mlir::func::CallOp>(
        loc, "hew_supervisor_add_child_spec", mlir::TypeRange{i32Type},
        mlir::ValueRange{adaptor.getSupervisor(), adaptor.getSpec()});
    rewriter.replaceOp(op, call.getResult(0));
    return mlir::success();
  }
};

/// Lower hew.supervisor.child_spec_create -> alloca + GEP/store for each field
struct ChildSpecCreateOpLowering : public mlir::OpConversionPattern<hew::ChildSpecCreateOp> {
  using OpConversionPattern<hew::ChildSpecCreateOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::ChildSpecCreateOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto ctx = rewriter.getContext();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(ctx);
    auto i64Type = rewriter.getI64Type();
    auto i32Type = rewriter.getI32Type();

    // Build the HewChildSpec struct type:
    // { ptr name, ptr init_state, i64 init_state_size,
    //   ptr dispatch, i32 restart_policy, i32 mailbox_capacity, i32 overflow }
    auto structType = mlir::LLVM::LLVMStructType::getLiteral(
        ctx, {ptrType, ptrType, i64Type, ptrType, i32Type, i32Type, i32Type});

    // Alloca the struct on the stack
    auto alloca = emitEntryAlloca(rewriter, loc, structType);

    // Store each field via GEP
    mlir::Value fields[] = {adaptor.getName(),          adaptor.getInitState(),
                            adaptor.getInitStateSize(), adaptor.getDispatch(),
                            adaptor.getRestartPolicy(), adaptor.getMailboxCapacity(),
                            adaptor.getOverflowPolicy()};
    for (int i = 0; i < 7; ++i) {
      auto gep = rewriter.create<mlir::LLVM::GEPOp>(loc, ptrType, structType, alloca,
                                                    llvm::ArrayRef<mlir::LLVM::GEPArg>{0, i});
      rewriter.create<mlir::LLVM::StoreOp>(loc, fields[i], gep);
    }

    rewriter.replaceOp(op, alloca);
    return mlir::success();
  }
};

/// Lower hew.supervisor.add_child_supervisor -> func.call
/// @hew_supervisor_add_child_supervisor_with_init
struct SupervisorAddChildSupervisorOpLowering
    : public mlir::OpConversionPattern<hew::SupervisorAddChildSupervisorOp> {
  using OpConversionPattern<hew::SupervisorAddChildSupervisorOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::SupervisorAddChildSupervisorOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto i32Type = rewriter.getI32Type();

    auto funcType = rewriter.getFunctionType({ptrType, ptrType, ptrType}, {i32Type});
    getOrInsertFuncDecl(module, rewriter, "hew_supervisor_add_child_supervisor_with_init",
                        funcType);
    auto call = rewriter.create<mlir::func::CallOp>(
        loc, "hew_supervisor_add_child_supervisor_with_init", mlir::TypeRange{i32Type},
        mlir::ValueRange{adaptor.getParent(), adaptor.getChild(), adaptor.getInitFn()});
    rewriter.replaceOp(op, call.getResult(0));
    return mlir::success();
  }
};

/// Lower hew.receive -> chained scf.if dispatch + data extraction + handler calls
struct ReceiveOpLowering : public mlir::OpConversionPattern<hew::ReceiveOp> {
  using OpConversionPattern<hew::ReceiveOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::ReceiveOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto *ctx = rewriter.getContext();
    auto i32Type = rewriter.getI32Type();
    auto sizeType = getSizeType(ctx, module);
    auto ptrType = mlir::LLVM::LLVMPointerType::get(ctx);

    auto stateVal = adaptor.getState();
    auto msgTypeVal = adaptor.getMsgType();
    auto dataVal = adaptor.getData();
    auto dataSizeVal = adaptor.getDataSize();

    auto handlers = op.getHandlers();

    // Handler parameter types are derived from the module symbol table.
    // This is idiomatic MLIR — symbol resolution is O(1) and avoids
    // duplicating type information in the op's attributes.
    for (auto [idx, handlerAttr] : llvm::enumerate(handlers)) {
      auto handlerRef = mlir::cast<mlir::FlatSymbolRefAttr>(handlerAttr);
      auto handlerName = handlerRef.getValue();

      // Look up handler function to get its argument types
      auto handlerFunc = module.lookupSymbol<mlir::func::FuncOp>(handlerName);
      if (!handlerFunc) {
        return op.emitOpError("handler function not found: ") << handlerName;
      }

      // Create condition: msg_type == idx
      auto msgIdx =
          rewriter.create<mlir::arith::ConstantIntOp>(loc, i32Type, static_cast<int64_t>(idx));
      auto cond = rewriter.create<mlir::arith::CmpIOp>(loc, mlir::arith::CmpIPredicate::eq,
                                                       msgTypeVal, msgIdx);

      auto ifOp = rewriter.create<mlir::scf::IfOp>(loc, cond, /*withElseRegion=*/false);
      rewriter.setInsertionPointToStart(&ifOp.getThenRegion().front());

      // Build call args: first arg is state pointer
      llvm::SmallVector<mlir::Value, 4> callArgs;
      callArgs.push_back(stateVal);

      // Remaining args are extracted from the data pointer
      auto handlerType = handlerFunc.getFunctionType();
      auto numHandlerArgs = handlerType.getNumInputs();

      if (numHandlerArgs > 1) {
        // Handler has message parameters (beyond the state pointer)
        auto numMsgParams = numHandlerArgs - 1;

        if (numMsgParams == 1) {
          // Single param: data points directly to the value
          auto paramType = handlerType.getInput(1);
          auto loaded = rewriter.create<mlir::LLVM::LoadOp>(loc, paramType, dataVal);
          callArgs.push_back(loaded);
        } else {
          // Multiple params: data points to a packed struct
          llvm::SmallVector<mlir::Type, 4> fieldTypes;
          for (unsigned pi = 1; pi < numHandlerArgs; ++pi) {
            fieldTypes.push_back(handlerType.getInput(pi));
          }
          auto packType = mlir::LLVM::LLVMStructType::getLiteral(rewriter.getContext(), fieldTypes);

          auto packed = rewriter.create<mlir::LLVM::LoadOp>(loc, packType, dataVal);
          for (unsigned pi = 0; pi < fieldTypes.size(); ++pi) {
            auto field = rewriter.create<mlir::LLVM::ExtractValueOp>(
                loc, packed, llvm::ArrayRef<int64_t>{static_cast<int64_t>(pi)});
            callArgs.push_back(field);
          }
        }
      }

      // Ensure handler function is declared
      getOrInsertFuncDecl(module, rewriter, handlerName, handlerType);

      bool hasReturnType = handlerType.getNumResults() > 0;
      if (hasReturnType) {
        // Call handler and capture return value
        auto callOp = rewriter.create<mlir::func::CallOp>(loc, handlerName,
                                                          handlerType.getResults(), callArgs);
        auto resultVal = callOp.getResult(0);
        auto resultType = resultVal.getType();

        // Compute expected data size for the handler's parameters
        mlir::Value expectedSize;
        if (numHandlerArgs <= 1) {
          expectedSize = rewriter.create<mlir::arith::ConstantIntOp>(loc, sizeType, 0);
        } else {
          auto numMsgParams = numHandlerArgs - 1;
          if (numMsgParams == 1) {
            expectedSize = emitSizeOf(rewriter, loc, handlerType.getInput(1));
          } else {
            llvm::SmallVector<mlir::Type, 4> ft;
            for (unsigned pi = 1; pi < numHandlerArgs; ++pi)
              ft.push_back(handlerType.getInput(pi));
            auto pt = mlir::LLVM::LLVMStructType::getLiteral(rewriter.getContext(), ft);
            expectedSize = emitSizeOf(rewriter, loc, pt);
          }
        }

        // Check if data_size > expectedSize → reply channel is present
        auto hasReply = rewriter.create<mlir::arith::CmpIOp>(loc, mlir::arith::CmpIPredicate::ugt,
                                                             dataSizeVal, expectedSize);

        auto replyIfOp = rewriter.create<mlir::scf::IfOp>(loc, hasReply, /*withElseRegion=*/false);
        rewriter.setInsertionPointToStart(&replyIfOp.getThenRegion().front());

        // Extract reply channel pointer from end of data:
        // offset = data_size - sizeof(ptr)
        auto ptrSizeVal = emitSizeOf(rewriter, loc, ptrType);
        auto offset = rewriter.create<mlir::arith::SubIOp>(loc, dataSizeVal, ptrSizeVal);
        auto i8Type = rewriter.getI8Type();
        auto replyChanAddr = rewriter.create<mlir::LLVM::GEPOp>(loc, ptrType, i8Type, dataVal,
                                                                mlir::ValueRange{offset});
        auto replyChan = rewriter.create<mlir::LLVM::LoadOp>(loc, ptrType, replyChanAddr);

        // Store result value to a temp alloca so we can pass its address
        auto one = rewriter.create<mlir::arith::ConstantIntOp>(loc, sizeType, 1);
        auto resultAlloca = rewriter.create<mlir::LLVM::AllocaOp>(loc, ptrType, resultType, one);
        rewriter.create<mlir::LLVM::StoreOp>(loc, resultVal, resultAlloca);

        // Call hew_reply(ch, &result, sizeof(result))
        auto resultSize = emitSizeOf(rewriter, loc, resultType);
        auto replyFuncType = rewriter.getFunctionType({ptrType, ptrType, sizeType}, {});
        getOrInsertFuncDecl(module, rewriter, "hew_reply", replyFuncType);
        rewriter.create<mlir::func::CallOp>(loc, "hew_reply", mlir::TypeRange{},
                                            mlir::ValueRange{replyChan, resultAlloca, resultSize});

        rewriter.setInsertionPointAfter(replyIfOp);
      } else {
        // Void handler — fire-and-forget (original behavior)
        rewriter.create<mlir::func::CallOp>(loc, handlerName, mlir::TypeRange{}, callArgs);
      }

      rewriter.setInsertionPointAfter(ifOp);
    }

    rewriter.eraseOp(op);
    return mlir::success();
  }
};

// ── Vec collection op lowerings ────────────────────────────────────────────

/// Helper: map an MLIR element type to the runtime function suffix.
static std::string vecElemSuffix(mlir::Type elemType) {
  if (elemType.isInteger(64))
    return "_i64";
  if (elemType.isF64())
    return "_f64";
  if (mlir::isa<hew::StringRefType>(elemType))
    return "_str";
  if (mlir::isa<hew::ActorRefType>(elemType) || mlir::isa<hew::TypedActorRefType>(elemType) ||
      mlir::isa<hew::HandleType>(elemType) || mlir::isa<mlir::LLVM::LLVMPointerType>(elemType))
    return "_ptr";
  if (mlir::isa<mlir::LLVM::LLVMStructType>(elemType))
    return "_generic";
  return "";
}

/// Helper: map element type for push/get/set/pop to the variant suffix
/// including ptr. Runtime uses explicit _i32 suffix for these ops.
static std::string vecElemSuffixWithPtr(mlir::Type elemType) {
  if (elemType.isInteger(64))
    return "_i64";
  if (elemType.isF64())
    return "_f64";
  if (mlir::isa<hew::StringRefType>(elemType))
    return "_str";
  if (mlir::isa<mlir::LLVM::LLVMPointerType>(elemType) || mlir::isa<hew::ActorRefType>(elemType) ||
      mlir::isa<hew::TypedActorRefType>(elemType) || mlir::isa<hew::HandleType>(elemType))
    return "_ptr";
  if (mlir::isa<mlir::LLVM::LLVMStructType>(elemType))
    return "_generic";
  if (elemType.isInteger(32))
    return "_i32";
  return "_i32"; // default fallback
}

struct VecNewOpLowering : public mlir::OpConversionPattern<hew::VecNewOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::VecNewOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());

    std::string suffix;
    if (auto vecTy = mlir::dyn_cast<hew::VecType>(op.getResult().getType()))
      suffix = vecElemSuffix(vecTy.getElementType());

    if (suffix == "_generic") {
      // For struct element types, create Vec with explicit element size.
      auto vecTy = mlir::dyn_cast<hew::VecType>(op.getResult().getType());
      auto elemType = vecTy.getElementType();
      auto convertedElem = getTypeConverter()->convertType(elemType);
      auto mod = op->getParentOfType<mlir::ModuleOp>();
      // Compute struct size with alignment padding
      uint64_t elemSize = 0;
      if (auto structTy = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(convertedElem)) {
        uint64_t maxAlign = 1;
        for (auto fieldTy : structTy.getBody()) {
          uint64_t fieldSize = 0, fieldAlign = 1;
          if (fieldTy.isInteger(32)) {
            fieldSize = 4;
            fieldAlign = 4;
          } else if (fieldTy.isInteger(64) || fieldTy.isF64()) {
            fieldSize = 8;
            fieldAlign = 8;
          } else if (fieldTy.isInteger(16)) {
            fieldSize = 2;
            fieldAlign = 2;
          } else if (fieldTy.isInteger(8) || fieldTy.isInteger(1)) {
            fieldSize = 1;
            fieldAlign = 1;
          } else {
            fieldSize = 8;
            fieldAlign = 8;
          } // pointer or other
          // Align current offset
          elemSize = (elemSize + fieldAlign - 1) & ~(fieldAlign - 1);
          elemSize += fieldSize;
          if (fieldAlign > maxAlign)
            maxAlign = fieldAlign;
        }
        // Final alignment to max field alignment
        elemSize = (elemSize + maxAlign - 1) & ~(maxAlign - 1);
      }
      if (elemSize == 0)
        elemSize = 8;
      auto i64Type = rewriter.getI64Type();
      auto sizeVal = rewriter.create<mlir::LLVM::ConstantOp>(
          loc, i64Type, rewriter.getI64IntegerAttr(static_cast<int64_t>(elemSize)));
      auto funcType = rewriter.getFunctionType({i64Type}, {ptrType});
      getOrInsertFuncDecl(mod, rewriter, "hew_vec_new_with_elem_size", funcType);
      auto call = rewriter.create<mlir::func::CallOp>(
          loc, "hew_vec_new_with_elem_size", mlir::TypeRange{ptrType}, mlir::ValueRange{sizeVal});
      rewriter.replaceOp(op, call.getResults());
    } else {
      std::string funcName = "hew_vec_new" + suffix;
      auto funcType = rewriter.getFunctionType({}, {ptrType});
      getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, funcName, funcType);
      auto call = rewriter.create<mlir::func::CallOp>(loc, funcName, mlir::TypeRange{ptrType},
                                                      mlir::ValueRange{});
      rewriter.replaceOp(op, call.getResults());
    }
    return mlir::success();
  }
};

struct VecPushOpLowering : public mlir::OpConversionPattern<hew::VecPushOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::VecPushOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto valType = adaptor.getValue().getType();

    std::string suffix = vecElemSuffixWithPtr(op.getValue().getType());
    if (suffix.empty())
      suffix = vecElemSuffixWithPtr(valType);

    if (suffix == "_generic") {
      // For struct elements: alloca + store + pass pointer to push_generic
      auto one = rewriter.create<mlir::LLVM::ConstantOp>(loc, rewriter.getI64Type(),
                                                         rewriter.getI64IntegerAttr(1));
      auto alloca = rewriter.create<mlir::LLVM::AllocaOp>(loc, ptrType, valType, one);
      rewriter.create<mlir::LLVM::StoreOp>(loc, adaptor.getValue(), alloca);
      auto funcType = rewriter.getFunctionType({ptrType, ptrType}, {});
      getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, "hew_vec_push_generic",
                          funcType);
      rewriter.create<mlir::func::CallOp>(loc, "hew_vec_push_generic", mlir::TypeRange{},
                                          mlir::ValueRange{adaptor.getVec(), alloca});
    } else {
      std::string funcName = "hew_vec_push" + suffix;
      auto funcType = rewriter.getFunctionType({ptrType, valType}, {});
      getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, funcName, funcType);
      rewriter.create<mlir::func::CallOp>(loc, funcName, mlir::TypeRange{},
                                          mlir::ValueRange{adaptor.getVec(), adaptor.getValue()});
    }
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

struct VecGetOpLowering : public mlir::OpConversionPattern<hew::VecGetOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::VecGetOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto resultType = getTypeConverter()->convertType(op.getResult().getType());

    std::string suffix = vecElemSuffixWithPtr(op.getResult().getType());
    if (suffix.empty())
      suffix = vecElemSuffixWithPtr(resultType);

    if (suffix == "_generic") {
      // For struct elements: get returns a pointer, then load the struct
      auto idxType = adaptor.getIndex().getType();
      auto funcType = rewriter.getFunctionType({ptrType, idxType}, {ptrType});
      getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, "hew_vec_get_generic",
                          funcType);
      auto call = rewriter.create<mlir::func::CallOp>(
          loc, "hew_vec_get_generic", mlir::TypeRange{ptrType},
          mlir::ValueRange{adaptor.getVec(), adaptor.getIndex()});
      auto loaded = rewriter.create<mlir::LLVM::LoadOp>(loc, resultType, call.getResult(0));
      rewriter.replaceOp(op, loaded.getResult());
    } else {
      std::string funcName = "hew_vec_get" + suffix;
      auto idxType = adaptor.getIndex().getType();
      auto funcType = rewriter.getFunctionType({ptrType, idxType}, {resultType});
      getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, funcName, funcType);
      auto call = rewriter.create<mlir::func::CallOp>(
          loc, funcName, mlir::TypeRange{resultType},
          mlir::ValueRange{adaptor.getVec(), adaptor.getIndex()});
      rewriter.replaceOp(op, call.getResults());
    }
    return mlir::success();
  }
};

struct VecSetOpLowering : public mlir::OpConversionPattern<hew::VecSetOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::VecSetOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto valType = adaptor.getValue().getType();

    std::string suffix = vecElemSuffixWithPtr(op.getValue().getType());
    if (suffix.empty())
      suffix = vecElemSuffixWithPtr(valType);

    if (suffix == "_generic") {
      // For struct elements: alloca + store + pass pointer to set_generic
      auto one = rewriter.create<mlir::LLVM::ConstantOp>(loc, rewriter.getI64Type(),
                                                         rewriter.getI64IntegerAttr(1));
      auto alloca = rewriter.create<mlir::LLVM::AllocaOp>(loc, ptrType, valType, one);
      rewriter.create<mlir::LLVM::StoreOp>(loc, adaptor.getValue(), alloca);
      auto idxType = adaptor.getIndex().getType();
      auto funcType = rewriter.getFunctionType({ptrType, idxType, ptrType}, {});
      getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, "hew_vec_set_generic",
                          funcType);
      rewriter.create<mlir::func::CallOp>(
          loc, "hew_vec_set_generic", mlir::TypeRange{},
          mlir::ValueRange{adaptor.getVec(), adaptor.getIndex(), alloca});
    } else {
      std::string funcName = "hew_vec_set" + suffix;
      auto idxType = adaptor.getIndex().getType();
      auto funcType = rewriter.getFunctionType({ptrType, idxType, valType}, {});
      getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, funcName, funcType);
      rewriter.create<mlir::func::CallOp>(
          loc, funcName, mlir::TypeRange{},
          mlir::ValueRange{adaptor.getVec(), adaptor.getIndex(), adaptor.getValue()});
    }
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

struct VecLenOpLowering : public mlir::OpConversionPattern<hew::VecLenOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::VecLenOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto resultType = getTypeConverter()->convertType(op.getResult().getType());
    auto funcType = rewriter.getFunctionType({ptrType}, {resultType});
    getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, "hew_vec_len", funcType);
    auto call = rewriter.create<mlir::func::CallOp>(loc, "hew_vec_len", mlir::TypeRange{resultType},
                                                    mlir::ValueRange{adaptor.getVec()});
    rewriter.replaceOp(op, call.getResults());
    return mlir::success();
  }
};

struct VecPopOpLowering : public mlir::OpConversionPattern<hew::VecPopOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::VecPopOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto resultType = getTypeConverter()->convertType(op.getResult().getType());

    std::string suffix = vecElemSuffixWithPtr(op.getResult().getType());
    if (suffix.empty())
      suffix = vecElemSuffixWithPtr(resultType);

    std::string funcName = "hew_vec_pop" + suffix;
    auto funcType = rewriter.getFunctionType({ptrType}, {resultType});
    getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, funcName, funcType);
    auto call = rewriter.create<mlir::func::CallOp>(loc, funcName, mlir::TypeRange{resultType},
                                                    mlir::ValueRange{adaptor.getVec()});
    rewriter.replaceOp(op, call.getResults());
    return mlir::success();
  }
};

struct VecRemoveOpLowering : public mlir::OpConversionPattern<hew::VecRemoveOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::VecRemoveOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto valType = adaptor.getValue().getType();

    std::string suffix = vecElemSuffixWithPtr(op.getValue().getType());
    if (suffix.empty())
      suffix = vecElemSuffixWithPtr(valType);

    std::string funcName = "hew_vec_remove" + suffix;
    auto funcType = rewriter.getFunctionType({ptrType, valType}, {});
    getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, funcName, funcType);
    rewriter.create<mlir::func::CallOp>(loc, funcName, mlir::TypeRange{},
                                        mlir::ValueRange{adaptor.getVec(), adaptor.getValue()});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

struct VecIsEmptyOpLowering : public mlir::OpConversionPattern<hew::VecIsEmptyOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::VecIsEmptyOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto i1Type = rewriter.getI1Type();
    auto funcType = rewriter.getFunctionType({ptrType}, {i1Type});
    getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, "hew_vec_is_empty",
                        funcType);
    auto call = rewriter.create<mlir::func::CallOp>(
        loc, "hew_vec_is_empty", mlir::TypeRange{i1Type}, mlir::ValueRange{adaptor.getVec()});
    rewriter.replaceOp(op, call.getResults());
    return mlir::success();
  }
};

struct VecClearOpLowering : public mlir::OpConversionPattern<hew::VecClearOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::VecClearOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto funcType = rewriter.getFunctionType({ptrType}, {});
    getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, "hew_vec_clear", funcType);
    rewriter.create<mlir::func::CallOp>(loc, "hew_vec_clear", mlir::TypeRange{},
                                        mlir::ValueRange{adaptor.getVec()});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

struct VecFreeOpLowering : public mlir::OpConversionPattern<hew::VecFreeOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::VecFreeOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto funcType = rewriter.getFunctionType({ptrType}, {});
    getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, "hew_vec_free", funcType);
    rewriter.create<mlir::func::CallOp>(loc, "hew_vec_free", mlir::TypeRange{},
                                        mlir::ValueRange{adaptor.getVec()});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

// ── HashMap collection op lowerings ────────────────────────────────────────

struct HashMapNewOpLowering : public mlir::OpConversionPattern<hew::HashMapNewOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::HashMapNewOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto funcType = rewriter.getFunctionType({}, {ptrType});
    getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, "hew_hashmap_new_impl",
                        funcType);
    auto call = rewriter.create<mlir::func::CallOp>(loc, "hew_hashmap_new_impl",
                                                    mlir::TypeRange{ptrType}, mlir::ValueRange{});
    rewriter.replaceOp(op, call.getResults());
    return mlir::success();
  }
};

struct HashMapInsertOpLowering : public mlir::OpConversionPattern<hew::HashMapInsertOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::HashMapInsertOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto *ctx = op.getContext();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(ctx);
    auto i32Type = rewriter.getI32Type();
    auto i64Type = rewriter.getI64Type();
    auto f64Type = rewriter.getF64Type();
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto valType = adaptor.getValue().getType();

    if (valType == i64Type) {
      // hew_hashmap_insert_i64(map, key, val)
      auto funcType = rewriter.getFunctionType({ptrType, ptrType, i64Type}, {});
      getOrInsertFuncDecl(module, rewriter, "hew_hashmap_insert_i64", funcType);
      rewriter.create<mlir::func::CallOp>(
          loc, "hew_hashmap_insert_i64", mlir::TypeRange{},
          mlir::ValueRange{adaptor.getMap(), adaptor.getKey(), adaptor.getValue()});
    } else if (valType == f64Type) {
      // hew_hashmap_insert_f64(map, key, val)
      auto funcType = rewriter.getFunctionType({ptrType, ptrType, f64Type}, {});
      getOrInsertFuncDecl(module, rewriter, "hew_hashmap_insert_f64", funcType);
      rewriter.create<mlir::func::CallOp>(
          loc, "hew_hashmap_insert_f64", mlir::TypeRange{},
          mlir::ValueRange{adaptor.getMap(), adaptor.getKey(), adaptor.getValue()});
    } else if (valType == ptrType) {
      // String value: hew_hashmap_insert_impl(map, key, 0, val_str)
      auto funcType = rewriter.getFunctionType({ptrType, ptrType, i32Type, ptrType}, {});
      getOrInsertFuncDecl(module, rewriter, "hew_hashmap_insert_impl", funcType);
      auto zero =
          rewriter.create<mlir::arith::ConstantOp>(loc, i32Type, rewriter.getI32IntegerAttr(0));
      rewriter.create<mlir::func::CallOp>(
          loc, "hew_hashmap_insert_impl", mlir::TypeRange{},
          mlir::ValueRange{adaptor.getMap(), adaptor.getKey(), zero, adaptor.getValue()});
    } else {
      // i32 (default): hew_hashmap_insert_impl(map, key, val_i32, null)
      auto funcType = rewriter.getFunctionType({ptrType, ptrType, i32Type, ptrType}, {});
      getOrInsertFuncDecl(module, rewriter, "hew_hashmap_insert_impl", funcType);
      auto nullStr = rewriter.create<mlir::LLVM::ZeroOp>(loc, ptrType);
      rewriter.create<mlir::func::CallOp>(
          loc, "hew_hashmap_insert_impl", mlir::TypeRange{},
          mlir::ValueRange{adaptor.getMap(), adaptor.getKey(), adaptor.getValue(), nullStr});
    }

    rewriter.eraseOp(op);
    return mlir::success();
  }
};

struct HashMapGetOpLowering : public mlir::OpConversionPattern<hew::HashMapGetOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::HashMapGetOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto *ctx = op.getContext();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(ctx);
    auto i64Type = rewriter.getI64Type();
    auto f64Type = rewriter.getF64Type();
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto resultType = getTypeConverter()->convertType(op.getResult().getType());

    std::string funcName;
    if (resultType == i64Type) {
      funcName = "hew_hashmap_get_i64";
    } else if (resultType == f64Type) {
      funcName = "hew_hashmap_get_f64";
    } else if (resultType == ptrType) {
      funcName = "hew_hashmap_get_str_impl";
    } else {
      funcName = "hew_hashmap_get_i32";
    }

    auto funcType = rewriter.getFunctionType({ptrType, ptrType}, {resultType});
    getOrInsertFuncDecl(module, rewriter, funcName, funcType);
    auto call =
        rewriter.create<mlir::func::CallOp>(loc, funcName, mlir::TypeRange{resultType},
                                            mlir::ValueRange{adaptor.getMap(), adaptor.getKey()});
    rewriter.replaceOp(op, call.getResults());
    return mlir::success();
  }
};

struct HashMapContainsKeyOpLowering : public mlir::OpConversionPattern<hew::HashMapContainsKeyOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::HashMapContainsKeyOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto keyType = adaptor.getKey().getType();
    auto i1Type = rewriter.getI1Type();
    auto funcType = rewriter.getFunctionType({ptrType, keyType}, {i1Type});
    getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, "hew_hashmap_contains_key",
                        funcType);
    auto call = rewriter.create<mlir::func::CallOp>(
        loc, "hew_hashmap_contains_key", mlir::TypeRange{i1Type},
        mlir::ValueRange{adaptor.getMap(), adaptor.getKey()});
    rewriter.replaceOp(op, call.getResult(0));
    return mlir::success();
  }
};

struct HashMapRemoveOpLowering : public mlir::OpConversionPattern<hew::HashMapRemoveOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::HashMapRemoveOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto i1Type = rewriter.getI1Type();
    // Runtime returns bool, but the op has no result — discard the return value
    auto funcType = rewriter.getFunctionType({ptrType, ptrType}, {i1Type});
    getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, "hew_hashmap_remove",
                        funcType);
    rewriter.create<mlir::func::CallOp>(loc, "hew_hashmap_remove", mlir::TypeRange{i1Type},
                                        mlir::ValueRange{adaptor.getMap(), adaptor.getKey()});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

struct HashMapLenOpLowering : public mlir::OpConversionPattern<hew::HashMapLenOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::HashMapLenOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto resultType = getTypeConverter()->convertType(op.getResult().getType());
    auto funcType = rewriter.getFunctionType({ptrType}, {resultType});
    getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, "hew_hashmap_len",
                        funcType);
    auto call = rewriter.create<mlir::func::CallOp>(
        loc, "hew_hashmap_len", mlir::TypeRange{resultType}, mlir::ValueRange{adaptor.getMap()});
    rewriter.replaceOp(op, call.getResults());
    return mlir::success();
  }
};

struct HashMapFreeOpLowering : public mlir::OpConversionPattern<hew::HashMapFreeOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::HashMapFreeOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto funcType = rewriter.getFunctionType({ptrType}, {});
    getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, "hew_hashmap_free_impl",
                        funcType);
    rewriter.create<mlir::func::CallOp>(loc, "hew_hashmap_free_impl", mlir::TypeRange{},
                                        mlir::ValueRange{adaptor.getMap()});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

struct HashMapKeysOpLowering : public mlir::OpConversionPattern<hew::HashMapKeysOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::HashMapKeysOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto funcType = rewriter.getFunctionType({ptrType}, {ptrType});
    getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, "hew_hashmap_keys",
                        funcType);
    auto call = rewriter.create<mlir::func::CallOp>(
        loc, "hew_hashmap_keys", mlir::TypeRange{ptrType}, mlir::ValueRange{adaptor.getMap()});
    rewriter.replaceOp(op, call.getResults());
    return mlir::success();
  }
};

// ── Generator op lowerings ─────────────────────────────────────────────────

/// Lower hew.gen.create -> func.call @hew_gen_ctx_create
struct GenCtxCreateOpLowering : public mlir::OpConversionPattern<hew::GenCtxCreateOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::GenCtxCreateOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto sizeType = getSizeType(rewriter.getContext(), module);

    auto funcType = rewriter.getFunctionType({ptrType, ptrType, sizeType}, {ptrType});
    getOrInsertFuncDecl(module, rewriter, "hew_gen_ctx_create", funcType);
    auto call = rewriter.create<mlir::func::CallOp>(
        loc, "hew_gen_ctx_create", mlir::TypeRange{ptrType},
        mlir::ValueRange{adaptor.getBodyFn(), adaptor.getArgsPtr(), adaptor.getArgsSize()});
    rewriter.replaceOp(op, call.getResults());
    return mlir::success();
  }
};

/// Lower hew.gen.next -> func.call @hew_gen_next
struct GenNextOpLowering : public mlir::OpConversionPattern<hew::GenNextOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::GenNextOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());

    auto funcType = rewriter.getFunctionType({ptrType, ptrType}, {ptrType});
    getOrInsertFuncDecl(module, rewriter, "hew_gen_next", funcType);
    auto call = rewriter.create<mlir::func::CallOp>(
        loc, "hew_gen_next", mlir::TypeRange{ptrType},
        mlir::ValueRange{adaptor.getCtx(), adaptor.getOutSizePtr()});
    rewriter.replaceOp(op, call.getResults());
    return mlir::success();
  }
};

/// Lower hew.gen.yield -> func.call @hew_gen_yield
struct GenYieldOpLowering : public mlir::OpConversionPattern<hew::GenYieldOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::GenYieldOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto sizeType = getSizeType(rewriter.getContext(), module);
    auto i1Type = rewriter.getI1Type();

    auto funcType = rewriter.getFunctionType({ptrType, ptrType, sizeType}, {i1Type});
    getOrInsertFuncDecl(module, rewriter, "hew_gen_yield", funcType);
    auto call = rewriter.create<mlir::func::CallOp>(
        loc, "hew_gen_yield", mlir::TypeRange{i1Type},
        mlir::ValueRange{adaptor.getCtx(), adaptor.getValuePtr(), adaptor.getValueSize()});
    rewriter.replaceOp(op, call.getResults());
    return mlir::success();
  }
};

/// Lower hew.gen.free -> func.call @hew_gen_free
struct GenFreeOpLowering : public mlir::OpConversionPattern<hew::GenFreeOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::GenFreeOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());

    auto funcType = rewriter.getFunctionType({ptrType}, {});
    getOrInsertFuncDecl(module, rewriter, "hew_gen_free", funcType);
    rewriter.create<mlir::func::CallOp>(loc, "hew_gen_free", mlir::TypeRange{},
                                        mlir::ValueRange{adaptor.getCtx()});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

/// Lower hew.gen.wrap_value -> llvm.undef + llvm.insertvalue(1,[0]) + llvm.insertvalue(val,[1])
struct GenWrapValueOpLowering : public mlir::OpConversionPattern<hew::GenWrapValueOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::GenWrapValueOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto wrapperType = op.getResult().getType();
    auto i8Type = rewriter.getI8Type();

    auto undef = rewriter.create<mlir::LLVM::UndefOp>(loc, wrapperType);
    auto oneI8 = rewriter.create<mlir::arith::ConstantIntOp>(loc, i8Type, 1);
    auto withTag =
        rewriter.create<mlir::LLVM::InsertValueOp>(loc, undef, oneI8, llvm::ArrayRef<int64_t>{0});
    auto withVal = rewriter.create<mlir::LLVM::InsertValueOp>(loc, withTag, adaptor.getValue(),
                                                              llvm::ArrayRef<int64_t>{1});
    rewriter.replaceOp(op, withVal.getResult());
    return mlir::success();
  }
};

/// Lower hew.gen.wrap_done -> llvm.undef + llvm.insertvalue(0,[0]) + llvm.insertvalue(zero,[1])
struct GenWrapDoneOpLowering : public mlir::OpConversionPattern<hew::GenWrapDoneOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::GenWrapDoneOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto wrapperType = op.getResult().getType();
    auto i8Type = rewriter.getI8Type();

    // Extract the value type from the wrapper struct's second field
    auto structType = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(wrapperType);
    if (!structType || structType.getBody().size() < 2)
      return mlir::failure();
    auto valueType = structType.getBody()[1];

    auto undef = rewriter.create<mlir::LLVM::UndefOp>(loc, wrapperType);
    auto zeroI8 = rewriter.create<mlir::arith::ConstantIntOp>(loc, i8Type, 0);
    auto withTag =
        rewriter.create<mlir::LLVM::InsertValueOp>(loc, undef, zeroI8, llvm::ArrayRef<int64_t>{0});

    // Create a zero/default value for the value slot
    mlir::Value defaultVal;
    if (mlir::isa<mlir::IntegerType>(valueType))
      defaultVal = rewriter.create<mlir::arith::ConstantIntOp>(loc, valueType, 0);
    else if (mlir::isa<mlir::FloatType>(valueType))
      defaultVal =
          rewriter.create<mlir::arith::ConstantOp>(loc, rewriter.getFloatAttr(valueType, 0.0));
    else if (mlir::isa<mlir::LLVM::LLVMPointerType>(valueType))
      defaultVal = rewriter.create<mlir::LLVM::ZeroOp>(loc, valueType);
    else
      defaultVal = rewriter.create<mlir::LLVM::UndefOp>(loc, valueType);

    auto withVal = rewriter.create<mlir::LLVM::InsertValueOp>(loc, withTag, defaultVal,
                                                              llvm::ArrayRef<int64_t>{1});
    rewriter.replaceOp(op, withVal.getResult());
    return mlir::success();
  }
};

// ── Regex op lowerings ─────────────────────────────────────────────────────

struct RegexNewOpLowering : public mlir::OpConversionPattern<hew::RegexNewOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::RegexNewOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto funcType = rewriter.getFunctionType({ptrType}, {ptrType});
    getOrInsertFuncDecl(module, rewriter, "hew_regex_new", funcType);
    auto call = rewriter.create<mlir::func::CallOp>(loc, "hew_regex_new", mlir::TypeRange{ptrType},
                                                    mlir::ValueRange{adaptor.getPattern()});
    rewriter.replaceOp(op, call.getResults());
    return mlir::success();
  }
};

struct RegexIsMatchOpLowering : public mlir::OpConversionPattern<hew::RegexIsMatchOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::RegexIsMatchOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto i1Type = rewriter.getI1Type();
    auto funcType = rewriter.getFunctionType({ptrType, ptrType}, {i1Type});
    getOrInsertFuncDecl(module, rewriter, "hew_regex_is_match", funcType);
    auto call = rewriter.create<mlir::func::CallOp>(
        loc, "hew_regex_is_match", mlir::TypeRange{i1Type},
        mlir::ValueRange{adaptor.getRegex(), adaptor.getText()});
    rewriter.replaceOp(op, call.getResults());
    return mlir::success();
  }
};

struct RegexFindOpLowering : public mlir::OpConversionPattern<hew::RegexFindOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::RegexFindOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto funcType = rewriter.getFunctionType({ptrType, ptrType}, {ptrType});
    getOrInsertFuncDecl(module, rewriter, "hew_regex_find", funcType);
    auto call = rewriter.create<mlir::func::CallOp>(
        loc, "hew_regex_find", mlir::TypeRange{ptrType},
        mlir::ValueRange{adaptor.getRegex(), adaptor.getText()});
    rewriter.replaceOp(op, call.getResults());
    return mlir::success();
  }
};

struct RegexReplaceOpLowering : public mlir::OpConversionPattern<hew::RegexReplaceOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::RegexReplaceOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto funcType = rewriter.getFunctionType({ptrType, ptrType, ptrType}, {ptrType});
    getOrInsertFuncDecl(module, rewriter, "hew_regex_replace", funcType);
    auto call = rewriter.create<mlir::func::CallOp>(
        loc, "hew_regex_replace", mlir::TypeRange{ptrType},
        mlir::ValueRange{adaptor.getRegex(), adaptor.getText(), adaptor.getReplacement()});
    rewriter.replaceOp(op, call.getResults());
    return mlir::success();
  }
};

struct RegexFreeOpLowering : public mlir::OpConversionPattern<hew::RegexFreeOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::RegexFreeOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto funcType = rewriter.getFunctionType({ptrType}, {});
    getOrInsertFuncDecl(module, rewriter, "hew_regex_free", funcType);
    rewriter.create<mlir::func::CallOp>(loc, "hew_regex_free", mlir::TypeRange{},
                                        mlir::ValueRange{adaptor.getRegex()});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

// ── Scheduler lifecycle op lowerings ───────────────────────────────────────

struct SchedInitOpLowering : public mlir::OpConversionPattern<hew::SchedInitOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::SchedInitOp op, OpAdaptor /*adaptor*/,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto funcType = rewriter.getFunctionType({}, {});
    getOrInsertFuncDecl(module, rewriter, "hew_sched_init", funcType);
    rewriter.create<mlir::func::CallOp>(loc, "hew_sched_init", mlir::TypeRange{},
                                        mlir::ValueRange{});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

struct SchedShutdownOpLowering : public mlir::OpConversionPattern<hew::SchedShutdownOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::SchedShutdownOp op, OpAdaptor /*adaptor*/,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto funcType = rewriter.getFunctionType({}, {});
    getOrInsertFuncDecl(module, rewriter, "hew_sched_shutdown", funcType);
    rewriter.create<mlir::func::CallOp>(loc, "hew_sched_shutdown", mlir::TypeRange{},
                                        mlir::ValueRange{});
    // After scheduler shutdown (workers joined), clean up remaining actors.
    getOrInsertFuncDecl(module, rewriter, "hew_runtime_cleanup", funcType);
    rewriter.create<mlir::func::CallOp>(loc, "hew_runtime_cleanup", mlir::TypeRange{},
                                        mlir::ValueRange{});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

// ── Arena allocation op lowering ───────────────────────────────────────────

struct ArenaMallocOpLowering : public mlir::OpConversionPattern<hew::ArenaMallocOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::ArenaMallocOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto sizeType = getSizeType(rewriter.getContext(), module);
    auto funcType = rewriter.getFunctionType({sizeType}, {ptrType});
    getOrInsertFuncDecl(module, rewriter, "hew_arena_malloc", funcType);
    auto call = rewriter.create<mlir::func::CallOp>(
        loc, "hew_arena_malloc", mlir::TypeRange{ptrType}, mlir::ValueRange{adaptor.getSize()});
    rewriter.replaceOp(op, call.getResults());
    return mlir::success();
  }
};

// ── Reference counting op lowerings ────────────────────────────────────────

struct RcNewOpLowering : public mlir::OpConversionPattern<hew::RcNewOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::RcNewOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto sizeType = getSizeType(rewriter.getContext(), module);
    auto funcType = rewriter.getFunctionType({ptrType, sizeType, ptrType}, {ptrType});
    getOrInsertFuncDecl(module, rewriter, "hew_rc_new", funcType);
    auto call = rewriter.create<mlir::func::CallOp>(
        loc, "hew_rc_new", mlir::TypeRange{ptrType},
        mlir::ValueRange{adaptor.getData(), adaptor.getSize(), adaptor.getDropFn()});
    rewriter.replaceOp(op, call.getResults());
    return mlir::success();
  }
};

struct RcCloneOpLowering : public mlir::OpConversionPattern<hew::RcCloneOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::RcCloneOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto funcType = rewriter.getFunctionType({ptrType}, {ptrType});
    getOrInsertFuncDecl(module, rewriter, "hew_rc_clone", funcType);
    auto call = rewriter.create<mlir::func::CallOp>(loc, "hew_rc_clone", mlir::TypeRange{ptrType},
                                                    mlir::ValueRange{adaptor.getPtr()});
    rewriter.replaceOp(op, call.getResults());
    return mlir::success();
  }
};

struct RcDropOpLowering : public mlir::OpConversionPattern<hew::RcDropOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::RcDropOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto funcType = rewriter.getFunctionType({ptrType}, {});
    getOrInsertFuncDecl(module, rewriter, "hew_rc_drop", funcType);
    rewriter.create<mlir::func::CallOp>(loc, "hew_rc_drop", mlir::TypeRange{},
                                        mlir::ValueRange{adaptor.getPtr()});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

// ── Drop op lowering ───────────────────────────────────────────────────────

struct DropOpLowering : public mlir::OpConversionPattern<hew::DropOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::DropOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto funcType = rewriter.getFunctionType({ptrType}, {});
    auto funcName = op.getDropFn().str();
    getOrInsertFuncDecl(module, rewriter, funcName, funcType);
    rewriter.create<mlir::func::CallOp>(loc, funcName, mlir::TypeRange{},
                                        mlir::ValueRange{adaptor.getValue()});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

// ── Scope op lowerings ─────────────────────────────────────────────────────

/// Lower hew.scope.create -> func.call @hew_scope_create + @hew_task_scope_new
struct ScopeCreateOpLowering : public mlir::OpConversionPattern<hew::ScopeCreateOp> {
  using OpConversionPattern<hew::ScopeCreateOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::ScopeCreateOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());

    auto scopeFuncType = rewriter.getFunctionType({}, {ptrType});
    getOrInsertFuncDecl(module, rewriter, "hew_scope_create", scopeFuncType);
    auto actorScope = rewriter.create<mlir::func::CallOp>(
        loc, "hew_scope_create", mlir::TypeRange{ptrType}, mlir::ValueRange{});

    getOrInsertFuncDecl(module, rewriter, "hew_task_scope_new", scopeFuncType);
    auto taskScope = rewriter.create<mlir::func::CallOp>(
        loc, "hew_task_scope_new", mlir::TypeRange{ptrType}, mlir::ValueRange{});

    rewriter.replaceOp(op, {actorScope.getResult(0), taskScope.getResult(0)});
    return mlir::success();
  }
};

/// Lower hew.scope.join -> func.call @hew_task_scope_join_all + @hew_scope_wait_all
struct ScopeJoinOpLowering : public mlir::OpConversionPattern<hew::ScopeJoinOp> {
  using OpConversionPattern<hew::ScopeJoinOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::ScopeJoinOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());

    auto voidPtrFuncType = rewriter.getFunctionType({ptrType}, {});
    getOrInsertFuncDecl(module, rewriter, "hew_task_scope_join_all", voidPtrFuncType);
    rewriter.create<mlir::func::CallOp>(loc, "hew_task_scope_join_all", mlir::TypeRange{},
                                        mlir::ValueRange{adaptor.getTaskScope()});

    getOrInsertFuncDecl(module, rewriter, "hew_scope_wait_all", voidPtrFuncType);
    rewriter.create<mlir::func::CallOp>(loc, "hew_scope_wait_all", mlir::TypeRange{},
                                        mlir::ValueRange{adaptor.getActorScope()});

    rewriter.eraseOp(op);
    return mlir::success();
  }
};

/// Lower hew.scope.destroy -> func.call @hew_task_scope_destroy + @hew_scope_free
struct ScopeDestroyOpLowering : public mlir::OpConversionPattern<hew::ScopeDestroyOp> {
  using OpConversionPattern<hew::ScopeDestroyOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::ScopeDestroyOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());

    auto voidPtrFuncType = rewriter.getFunctionType({ptrType}, {});
    getOrInsertFuncDecl(module, rewriter, "hew_task_scope_destroy", voidPtrFuncType);
    rewriter.create<mlir::func::CallOp>(loc, "hew_task_scope_destroy", mlir::TypeRange{},
                                        mlir::ValueRange{adaptor.getTaskScope()});

    getOrInsertFuncDecl(module, rewriter, "hew_scope_free", voidPtrFuncType);
    rewriter.create<mlir::func::CallOp>(loc, "hew_scope_free", mlir::TypeRange{},
                                        mlir::ValueRange{adaptor.getActorScope()});

    rewriter.eraseOp(op);
    return mlir::success();
  }
};

/// Lower hew.scope.launch -> hew_task_new + hew_task_scope_spawn + hew_task_spawn_thread
struct ScopeLaunchOpLowering : public mlir::OpConversionPattern<hew::ScopeLaunchOp> {
  using OpConversionPattern<hew::ScopeLaunchOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::ScopeLaunchOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());

    // hew_task_new() -> ptr
    auto taskNewFuncType = rewriter.getFunctionType({}, {ptrType});
    getOrInsertFuncDecl(module, rewriter, "hew_task_new", taskNewFuncType);
    auto taskPtr = rewriter.create<mlir::func::CallOp>(
        loc, "hew_task_new", mlir::TypeRange{ptrType}, mlir::ValueRange{});

    // hew_task_scope_spawn(scope, task) -> void
    auto spawnFuncType = rewriter.getFunctionType({ptrType, ptrType}, {});
    getOrInsertFuncDecl(module, rewriter, "hew_task_scope_spawn", spawnFuncType);
    rewriter.create<mlir::func::CallOp>(
        loc, "hew_task_scope_spawn", mlir::TypeRange{},
        mlir::ValueRange{adaptor.getTaskScope(), taskPtr.getResult(0)});

    // hew_task_spawn_thread(task, fn_ptr) -> void
    getOrInsertFuncDecl(module, rewriter, "hew_task_spawn_thread", spawnFuncType);
    rewriter.create<mlir::func::CallOp>(loc, "hew_task_spawn_thread", mlir::TypeRange{},
                                        mlir::ValueRange{taskPtr.getResult(0), adaptor.getFnPtr()});

    rewriter.replaceOp(op, taskPtr.getResult(0));
    return mlir::success();
  }
};

/// Lower hew.scope.await -> func.call @hew_task_await_blocking
struct ScopeAwaitOpLowering : public mlir::OpConversionPattern<hew::ScopeAwaitOp> {
  using OpConversionPattern<hew::ScopeAwaitOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::ScopeAwaitOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());

    auto funcType = rewriter.getFunctionType({ptrType}, {ptrType});
    getOrInsertFuncDecl(module, rewriter, "hew_task_await_blocking", funcType);
    auto call = rewriter.create<mlir::func::CallOp>(loc, "hew_task_await_blocking",
                                                    mlir::TypeRange{ptrType},
                                                    mlir::ValueRange{adaptor.getTask()});
    rewriter.replaceOp(op, call.getResult(0));
    return mlir::success();
  }
};

/// Lower hew.scope.cancel -> func.call @hew_task_scope_cancel
struct ScopeCancelOpLowering : public mlir::OpConversionPattern<hew::ScopeCancelOp> {
  using OpConversionPattern<hew::ScopeCancelOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::ScopeCancelOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());

    auto funcType = rewriter.getFunctionType({ptrType}, {});
    getOrInsertFuncDecl(module, rewriter, "hew_task_scope_cancel", funcType);
    rewriter.create<mlir::func::CallOp>(loc, "hew_task_scope_cancel", mlir::TypeRange{},
                                        mlir::ValueRange{adaptor.getTaskScope()});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

/// Lower hew.task.set_result -> func.call @hew_task_set_result
struct TaskSetResultOpLowering : public mlir::OpConversionPattern<hew::TaskSetResultOp> {
  using OpConversionPattern<hew::TaskSetResultOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::TaskSetResultOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto sizeType = getSizeType(rewriter.getContext(), module);

    auto funcType = rewriter.getFunctionType({ptrType, ptrType, sizeType}, {});
    getOrInsertFuncDecl(module, rewriter, "hew_task_set_result", funcType);
    rewriter.create<mlir::func::CallOp>(
        loc, "hew_task_set_result", mlir::TypeRange{},
        mlir::ValueRange{adaptor.getTask(), adaptor.getResultPtr(), adaptor.getSize()});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

/// Lower hew.task.complete -> func.call @hew_task_complete_threaded
struct TaskCompleteOpLowering : public mlir::OpConversionPattern<hew::TaskCompleteOp> {
  using OpConversionPattern<hew::TaskCompleteOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::TaskCompleteOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());

    auto funcType = rewriter.getFunctionType({ptrType}, {});
    getOrInsertFuncDecl(module, rewriter, "hew_task_complete_threaded", funcType);
    rewriter.create<mlir::func::CallOp>(loc, "hew_task_complete_threaded", mlir::TypeRange{},
                                        mlir::ValueRange{adaptor.getTask()});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

// ── Select op lowerings ────────────────────────────────────────────────────

/// Lower hew.select.create -> func.call @hew_reply_channel_new
struct SelectCreateOpLowering : public mlir::OpConversionPattern<hew::SelectCreateOp> {
  using OpConversionPattern<hew::SelectCreateOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::SelectCreateOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());

    auto funcType = rewriter.getFunctionType({}, {ptrType});
    getOrInsertFuncDecl(module, rewriter, "hew_reply_channel_new", funcType);
    auto call = rewriter.create<mlir::func::CallOp>(loc, "hew_reply_channel_new",
                                                    mlir::TypeRange{ptrType}, mlir::ValueRange{});
    rewriter.replaceOp(op, call.getResult(0));
    return mlir::success();
  }
};

/// Lower hew.select.add -> func.call @hew_actor_ask_with_channel
struct SelectAddOpLowering : public mlir::OpConversionPattern<hew::SelectAddOp> {
  using OpConversionPattern<hew::SelectAddOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::SelectAddOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto i32Type = rewriter.getI32Type();
    auto sizeType = getSizeType(rewriter.getContext(), module);

    auto funcType = rewriter.getFunctionType({ptrType, i32Type, ptrType, sizeType, ptrType}, {});
    getOrInsertFuncDecl(module, rewriter, "hew_actor_ask_with_channel", funcType);
    rewriter.create<mlir::func::CallOp>(
        loc, "hew_actor_ask_with_channel", mlir::TypeRange{},
        mlir::ValueRange{adaptor.getActor(), adaptor.getMsgType(), adaptor.getDataPtr(),
                         adaptor.getDataSize(), adaptor.getChannel()});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

/// Lower hew.select.first -> func.call @hew_select_first
struct SelectFirstOpLowering : public mlir::OpConversionPattern<hew::SelectFirstOp> {
  using OpConversionPattern<hew::SelectFirstOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::SelectFirstOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto i32Type = rewriter.getI32Type();

    auto funcType = rewriter.getFunctionType({ptrType, i32Type, i32Type}, {i32Type});
    getOrInsertFuncDecl(module, rewriter, "hew_select_first", funcType);
    auto call = rewriter.create<mlir::func::CallOp>(
        loc, "hew_select_first", mlir::TypeRange{i32Type},
        mlir::ValueRange{adaptor.getChannels(), adaptor.getCount(), adaptor.getTimeoutMs()});
    rewriter.replaceOp(op, call.getResult(0));
    return mlir::success();
  }
};

/// Lower hew.select.destroy -> func.call @hew_reply_channel_free
struct SelectDestroyOpLowering : public mlir::OpConversionPattern<hew::SelectDestroyOp> {
  using OpConversionPattern<hew::SelectDestroyOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::SelectDestroyOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());

    auto funcType = rewriter.getFunctionType({ptrType}, {});
    getOrInsertFuncDecl(module, rewriter, "hew_reply_channel_free", funcType);
    rewriter.create<mlir::func::CallOp>(loc, "hew_reply_channel_free", mlir::TypeRange{},
                                        mlir::ValueRange{adaptor.getChannel()});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

/// Lower hew.select.wait -> func.call @hew_reply_wait
struct SelectWaitOpLowering : public mlir::OpConversionPattern<hew::SelectWaitOp> {
  using OpConversionPattern<hew::SelectWaitOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::SelectWaitOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());

    auto funcType = rewriter.getFunctionType({ptrType}, {ptrType});
    getOrInsertFuncDecl(module, rewriter, "hew_reply_wait", funcType);
    auto call = rewriter.create<mlir::func::CallOp>(loc, "hew_reply_wait", mlir::TypeRange{ptrType},
                                                    mlir::ValueRange{adaptor.getChannel()});
    rewriter.replaceOp(op, call.getResult(0));
    return mlir::success();
  }
};

// ── Runtime call op lowering ───────────────────────────────────────────────

struct RuntimeCallOpLowering : public mlir::OpConversionPattern<hew::RuntimeCallOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::RuntimeCallOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto callee = op.getCallee();

    llvm::SmallVector<mlir::Type> argTypes;
    for (auto v : adaptor.getOperands())
      argTypes.push_back(v.getType());

    llvm::SmallVector<mlir::Type> resultTypes;
    if (op.getResult())
      resultTypes.push_back(getTypeConverter()->convertType(op.getResult().getType()));

    auto funcType = rewriter.getFunctionType(argTypes, resultTypes);
    getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, callee.str(), funcType);
    auto call =
        rewriter.create<mlir::func::CallOp>(loc, callee, resultTypes, adaptor.getOperands());

    if (resultTypes.empty())
      rewriter.eraseOp(op);
    else
      rewriter.replaceOp(op, call.getResults());
    return mlir::success();
  }
};

// ── Trait dispatch op lowering ─────────────────────────────────────────────
// ── Trait dispatch op lowering (vtable-based O(1) dispatch) ────────────────

struct TraitDispatchOpLowering : public mlir::OpConversionPattern<hew::TraitDispatchOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::TraitDispatchOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());

    auto dataPtr = adaptor.getDataPtr();
    auto vtablePtr = adaptor.getVtablePtr();
    auto methodIndex = op.getMethodIndex();

    bool hasResult = op.getNumResults() > 0;
    mlir::Type resultType;
    if (hasResult)
      resultType = getTypeConverter()->convertType(op.getResult().getType());

    // Collect converted extra args
    llvm::SmallVector<mlir::Value> extraArgs(adaptor.getExtraArgs().begin(),
                                             adaptor.getExtraArgs().end());

    // GEP into vtable to get function pointer at method_index
    auto i32Type = rewriter.getI32Type();
    auto indexVal = rewriter.create<mlir::LLVM::ConstantOp>(
        loc, i32Type, rewriter.getI32IntegerAttr(static_cast<int32_t>(methodIndex)));
    auto funcPtrPtr = rewriter.create<mlir::LLVM::GEPOp>(loc, ptrType, ptrType, vtablePtr,
                                                         mlir::ValueRange{indexVal});
    auto funcPtr = rewriter.create<mlir::LLVM::LoadOp>(loc, ptrType, funcPtrPtr);

    // Build call args: data_ptr + extra args
    llvm::SmallVector<mlir::Value> callArgs = {dataPtr};
    callArgs.append(extraArgs.begin(), extraArgs.end());

    // Build function type for the indirect call
    llvm::SmallVector<mlir::Type> callArgTypes = {ptrType};
    for (auto v : extraArgs)
      callArgTypes.push_back(v.getType());
    llvm::SmallVector<mlir::Type> callResultTypes;
    if (hasResult)
      callResultTypes.push_back(resultType);
    auto callFuncType = rewriter.getFunctionType(callArgTypes, callResultTypes);

    // Cast loaded function pointer to function type for func.call_indirect
    auto callable =
        rewriter.create<mlir::UnrealizedConversionCastOp>(loc, callFuncType, funcPtr.getResult())
            .getResult(0);

    // Indirect call through function pointer
    auto call = rewriter.create<mlir::func::CallIndirectOp>(loc, callable, callArgs);

    if (hasResult) {
      rewriter.replaceOp(op, call.getResults());
    } else {
      rewriter.eraseOp(op);
    }
    return mlir::success();
  }
};

// ── String & conversion op lowerings ──────────────────────────────────────

struct StringConcatOpLowering : public mlir::OpConversionPattern<hew::StringConcatOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::StringConcatOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto funcType = rewriter.getFunctionType({ptrType, ptrType}, {ptrType});
    getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, "hew_string_concat",
                        funcType);
    auto call =
        rewriter.create<mlir::func::CallOp>(loc, "hew_string_concat", mlir::TypeRange{ptrType},
                                            mlir::ValueRange{adaptor.getLhs(), adaptor.getRhs()});
    rewriter.replaceOp(op, call.getResults());
    return mlir::success();
  }
};

struct ToStringOpLowering : public mlir::OpConversionPattern<hew::ToStringOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::ToStringOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto origType = op.getValue().getType();

    // Select runtime function based on operand type, extending small
    // integers to match the runtime function's parameter width.
    std::string funcName;
    mlir::Value arg = adaptor.getValue();
    if (origType.isInteger(1)) {
      funcName = "hew_bool_to_string";
    } else if (origType.isInteger(8)) {
      // bytes/u8 values — extend to i32 for decimal conversion via hew_int_to_string.
      // Note: char is i32 in MLIR and hits the i32 branch directly.
      // Using hew_char_to_string here would emit the raw byte as a character
      // (e.g. 0 → NUL → empty string), which is wrong for numeric u8 values.
      funcName = "hew_int_to_string";
      arg = rewriter.create<mlir::arith::ExtSIOp>(loc, rewriter.getI32Type(), arg);
    } else if (origType.isInteger(16)) {
      // i16 — extend to i32
      funcName = "hew_int_to_string";
      arg = rewriter.create<mlir::arith::ExtSIOp>(loc, rewriter.getI32Type(), arg);
    } else if (origType.isInteger(32)) {
      funcName = "hew_int_to_string";
    } else if (origType.isInteger(64)) {
      funcName = "hew_i64_to_string";
    } else if (origType.isF64() || origType.isF32()) {
      funcName = "hew_float_to_string";
    } else if (mlir::isa<mlir::IntegerType>(origType)) {
      // Other int widths — extend to i64
      funcName = "hew_i64_to_string";
      arg = rewriter.create<mlir::arith::ExtSIOp>(loc, rewriter.getI64Type(), arg);
    } else {
      funcName = "hew_i64_to_string"; // fallback
    }

    auto funcType = rewriter.getFunctionType({arg.getType()}, {ptrType});
    getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, funcName, funcType);
    auto call = rewriter.create<mlir::func::CallOp>(loc, funcName, mlir::TypeRange{ptrType},
                                                    mlir::ValueRange{arg});
    rewriter.replaceOp(op, call.getResults());
    return mlir::success();
  }
};

struct StringMethodOpLowering : public mlir::OpConversionPattern<hew::StringMethodOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::StringMethodOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto methodName = op.getMethod().str();
    std::string funcName = "hew_string_" + methodName;

    // Build operand list: receiver + extra args
    llvm::SmallVector<mlir::Value> args;
    args.push_back(adaptor.getReceiver());
    for (auto v : adaptor.getExtraArgs())
      args.push_back(v);

    llvm::SmallVector<mlir::Type> argTypes;
    for (auto v : args)
      argTypes.push_back(v.getType());

    bool hasResult = op.getResult() != nullptr;
    llvm::SmallVector<mlir::Type> resultTypes;
    if (hasResult)
      resultTypes.push_back(getTypeConverter()->convertType(op.getResult().getType()));

    auto funcType = rewriter.getFunctionType(argTypes, resultTypes);
    getOrInsertFuncDecl(module, rewriter, funcName, funcType);
    auto call = rewriter.create<mlir::func::CallOp>(loc, funcName, resultTypes, args);

    if (hasResult)
      rewriter.replaceOp(op, call.getResults());
    else
      rewriter.eraseOp(op);
    return mlir::success();
  }
};

// ── Tuple operations ────────────────────────────────────────────────

struct TupleCreateOpLowering : public mlir::OpConversionPattern<hew::TupleCreateOp> {
  using OpConversionPattern::OpConversionPattern;
  mlir::LogicalResult matchAndRewrite(hew::TupleCreateOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto resultType = getTypeConverter()->convertType(op.getResult().getType());
    auto structType = mlir::cast<mlir::LLVM::LLVMStructType>(resultType);
    mlir::Value result = rewriter.create<mlir::LLVM::UndefOp>(loc, structType);
    for (auto [idx, elem] : llvm::enumerate(adaptor.getElements())) {
      result = rewriter.create<mlir::LLVM::InsertValueOp>(loc, result, elem, idx);
    }
    rewriter.replaceOp(op, result);
    return mlir::success();
  }
};

struct TupleExtractOpLowering : public mlir::OpConversionPattern<hew::TupleExtractOp> {
  using OpConversionPattern::OpConversionPattern;
  mlir::LogicalResult matchAndRewrite(hew::TupleExtractOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto idx = op.getIndex();
    auto result = rewriter.create<mlir::LLVM::ExtractValueOp>(loc, adaptor.getTuple(), idx);
    rewriter.replaceOp(op, result.getResult());
    return mlir::success();
  }
};

// ── Array operations ────────────────────────────────────────────────

struct ArrayCreateOpLowering : public mlir::OpConversionPattern<hew::ArrayCreateOp> {
  using OpConversionPattern::OpConversionPattern;
  mlir::LogicalResult matchAndRewrite(hew::ArrayCreateOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto resultType = getTypeConverter()->convertType(op.getResult().getType());
    auto arrayType = mlir::cast<mlir::LLVM::LLVMArrayType>(resultType);
    mlir::Value result = rewriter.create<mlir::LLVM::UndefOp>(loc, arrayType);
    for (auto [idx, elem] : llvm::enumerate(adaptor.getElements())) {
      result = rewriter.create<mlir::LLVM::InsertValueOp>(loc, result, elem, idx);
    }
    rewriter.replaceOp(op, result);
    return mlir::success();
  }
};

struct ArrayExtractOpLowering : public mlir::OpConversionPattern<hew::ArrayExtractOp> {
  using OpConversionPattern::OpConversionPattern;
  mlir::LogicalResult matchAndRewrite(hew::ArrayExtractOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto idx = op.getIndex();
    auto result = rewriter.create<mlir::LLVM::ExtractValueOp>(loc, adaptor.getArray(), idx);
    rewriter.replaceOp(op, result.getResult());
    return mlir::success();
  }
};

// ── Trait object operations ─────────────────────────────────────────

struct TraitObjectCreateOpLowering : public mlir::OpConversionPattern<hew::TraitObjectCreateOp> {
  using OpConversionPattern::OpConversionPattern;
  mlir::LogicalResult matchAndRewrite(hew::TraitObjectCreateOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto resultType = getTypeConverter()->convertType(op.getResult().getType());
    auto structType = mlir::cast<mlir::LLVM::LLVMStructType>(resultType);
    mlir::Value result = rewriter.create<mlir::LLVM::UndefOp>(loc, structType);
    result = rewriter.create<mlir::LLVM::InsertValueOp>(loc, result, adaptor.getData(), 0);
    result = rewriter.create<mlir::LLVM::InsertValueOp>(loc, result, adaptor.getVtablePtr(), 1);
    rewriter.replaceOp(op, result);
    return mlir::success();
  }
};

struct TraitObjectDataOpLowering : public mlir::OpConversionPattern<hew::TraitObjectDataOp> {
  using OpConversionPattern::OpConversionPattern;
  mlir::LogicalResult matchAndRewrite(hew::TraitObjectDataOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto result =
        rewriter.create<mlir::LLVM::ExtractValueOp>(op.getLoc(), adaptor.getTraitObject(), 0);
    rewriter.replaceOp(op, result.getResult());
    return mlir::success();
  }
};

struct TraitObjectTagOpLowering : public mlir::OpConversionPattern<hew::TraitObjectTagOp> {
  using OpConversionPattern::OpConversionPattern;
  mlir::LogicalResult matchAndRewrite(hew::TraitObjectTagOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto result =
        rewriter.create<mlir::LLVM::ExtractValueOp>(op.getLoc(), adaptor.getTraitObject(), 1);
    rewriter.replaceOp(op, result.getResult());
    return mlir::success();
  }
};

// ── Closure operations ──────────────────────────────────────────────

struct ClosureCreateOpLowering : public mlir::OpConversionPattern<hew::ClosureCreateOp> {
  using OpConversionPattern::OpConversionPattern;
  mlir::LogicalResult matchAndRewrite(hew::ClosureCreateOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto resultType = getTypeConverter()->convertType(op.getResult().getType());
    auto structType = mlir::cast<mlir::LLVM::LLVMStructType>(resultType);
    mlir::Value result = rewriter.create<mlir::LLVM::UndefOp>(loc, structType);
    result = rewriter.create<mlir::LLVM::InsertValueOp>(loc, result, adaptor.getFnPtr(), 0);
    result = rewriter.create<mlir::LLVM::InsertValueOp>(loc, result, adaptor.getEnvPtr(), 1);
    rewriter.replaceOp(op, result);
    return mlir::success();
  }
};

struct ClosureGetFnOpLowering : public mlir::OpConversionPattern<hew::ClosureGetFnOp> {
  using OpConversionPattern::OpConversionPattern;
  mlir::LogicalResult matchAndRewrite(hew::ClosureGetFnOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto result = rewriter.create<mlir::LLVM::ExtractValueOp>(op.getLoc(), adaptor.getClosure(), 0);
    rewriter.replaceOp(op, result.getResult());
    return mlir::success();
  }
};

struct ClosureGetEnvOpLowering : public mlir::OpConversionPattern<hew::ClosureGetEnvOp> {
  using OpConversionPattern::OpConversionPattern;
  mlir::LogicalResult matchAndRewrite(hew::ClosureGetEnvOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto result = rewriter.create<mlir::LLVM::ExtractValueOp>(op.getLoc(), adaptor.getClosure(), 1);
    rewriter.replaceOp(op, result.getResult());
    return mlir::success();
  }
};

// ── hew.func_ptr lowering ──────────────────────────────────────────────────
// Lowers to llvm.mlir.addressof, which directly gets the function's address
// as !llvm.ptr.  The referenced function must already exist in the module
// (either as a func.func or llvm.func — at this stage it's func.func,
// and FuncToLLVM will convert it to llvm.func later).  We emit a
// func.constant + the type converter's materialization path so that
// FuncToLLVM can resolve it.

struct FuncPtrOpLowering : public mlir::OpConversionPattern<hew::FuncPtrOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::FuncPtrOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());

    auto funcName = op.getFuncName();

    // Look up the function to get its type
    auto funcOp = module.lookupSymbol<mlir::func::FuncOp>(funcName);
    if (!funcOp) {
      // Function might not be declared yet — create a minimal declaration
      return mlir::failure();
    }

    // Create func.constant to get a typed function reference, then
    // use the type converter's materialization to bridge to !llvm.ptr.
    // FuncToLLVM + ReconcileUnrealizedCasts will resolve this cleanly.
    auto funcRef = rewriter.create<mlir::func::ConstantOp>(
        loc, funcOp.getFunctionType(), mlir::SymbolRefAttr::get(rewriter.getContext(), funcName));
    // Bridge func type -> !llvm.ptr via UnrealizedConversionCast.
    // This is the standard MLIR pattern: the FuncToLLVM pass + reconcile
    // pass will eliminate these casts.
    auto castOp =
        rewriter.create<mlir::UnrealizedConversionCastOp>(loc, ptrType, funcRef.getResult());
    rewriter.replaceOp(op, castOp.getResult(0));
    return mlir::success();
  }
};

// ── hew.bitcast lowering ───────────────────────────────────────────────────
// For most casts (Hew type <-> !llvm.ptr), this is an identity operation
// since both types convert to !llvm.ptr.  For cross-dialect bridges
// (!llvm.ptr <-> FunctionType), we emit an UnrealizedConversionCastOp
// that gets resolved by FuncToLLVM + ReconcileUnrealizedCasts.

struct BitcastOpLowering : public mlir::OpConversionPattern<hew::BitcastOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::BitcastOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto convertedResultType = getTypeConverter()->convertType(op.getResult().getType());
    // If the type converter doesn't know about the result type (e.g.
    // FunctionType), fall back to the original type.
    if (!convertedResultType)
      convertedResultType = op.getResult().getType();

    auto inputVal = adaptor.getInput();

    // If the converted types match, this is a pure identity cast.
    if (inputVal.getType() == convertedResultType) {
      rewriter.replaceOp(op, inputVal);
      return mlir::success();
    }

    // Types differ after conversion (e.g. !llvm.ptr vs FunctionType).
    // Emit an UnrealizedConversionCastOp to bridge between dialects.
    // FuncToLLVM + ReconcileUnrealizedCasts will resolve these.
    auto castOp = rewriter.create<mlir::UnrealizedConversionCastOp>(op.getLoc(),
                                                                    convertedResultType, inputVal);
    rewriter.replaceOp(op, castOp.getResult(0));
    return mlir::success();
  }
};

} // anonymous namespace

static mlir::LogicalResult failOnUnreconciledCasts(mlir::ModuleOp module, llvm::StringRef stage) {
  bool foundUnreconciledCast = false;
  module.walk([&](mlir::UnrealizedConversionCastOp castOp) {
    foundUnreconciledCast = true;
    castOp.emitError() << "unreconciled unrealized_conversion_cast remained " << stage;
  });
  if (!foundUnreconciledCast)
    return mlir::success();

  llvm::errs() << "Error: unreconciled unrealized_conversion_cast remained " << stage << '\n';
  return mlir::failure();
}

// ============================================================================
// WASM target validation
// ============================================================================

/// Walk the module and reject ops that require OS threads.  Actor spawn/send/ask
/// are allowed — only supervision, scoped concurrency, link/monitor, and select
/// are rejected.
static mlir::LogicalResult validateWasmUnsupportedOps(mlir::ModuleOp module) {
  bool failed = false;

  struct UnsupportedOp {
    llvm::StringRef opName;
    llvm::StringRef reason;
  };

  static const UnsupportedOp unsupported[] = {
      // Supervision
      {"hew.supervisor.new", "supervision trees require OS threads (native target only)"},
      {"hew.supervisor.start", "supervision trees require OS threads (native target only)"},
      {"hew.supervisor.stop", "supervision trees require OS threads (native target only)"},
      {"hew.supervisor.add_child", "supervision trees require OS threads (native target only)"},
      {"hew.supervisor.add_child_supervisor",
       "supervision trees require OS threads (native target only)"},
      {"hew.supervisor.child_spec_create",
       "supervision trees require OS threads (native target only)"},
      // Link / Monitor
      {"hew.actor.link", "link/monitor requires OS threads (native target only)"},
      {"hew.actor.unlink", "link/monitor requires OS threads (native target only)"},
      {"hew.actor.monitor", "link/monitor requires OS threads (native target only)"},
      {"hew.actor.demonitor", "link/monitor requires OS threads (native target only)"},
      // Scoped concurrency
      {"hew.scope.create", "structured concurrency requires OS threads (native target only)"},
      {"hew.scope.join", "structured concurrency requires OS threads (native target only)"},
      {"hew.scope.destroy", "structured concurrency requires OS threads (native target only)"},
      {"hew.scope.launch", "structured concurrency requires OS threads (native target only)"},
      {"hew.scope.await", "structured concurrency requires OS threads (native target only)"},
      {"hew.scope.cancel", "structured concurrency requires OS threads (native target only)"},

      // Tasks
      {"hew.task.set_result", "structured concurrency requires OS threads (native target only)"},
      {"hew.task.complete", "structured concurrency requires OS threads (native target only)"},
      // Select
      {"hew.select.create", "select requires OS threads (native target only)"},
      {"hew.select.add", "select requires OS threads (native target only)"},
      {"hew.select.first", "select requires OS threads (native target only)"},
      {"hew.select.destroy", "select requires OS threads (native target only)"},
      {"hew.select.wait", "select requires OS threads (native target only)"},
  };

  module.walk([&](mlir::Operation *op) {
    auto name = op->getName().getStringRef();
    for (const auto &entry : unsupported) {
      if (name == entry.opName) {
        op->emitError() << "operation '" << name << "' is not supported on WASM targets\n"
                        << "  note: " << entry.reason << "\n"
                        << "  note: actors, send, and ask/await work on WASM "
                           "without this feature";
        failed = true;
        return;
      }
    }
  });

  return failed ? mlir::failure() : mlir::success();
}

// ============================================================================
// Codegen implementation
// ============================================================================

Codegen::Codegen(mlir::MLIRContext &context) : context(context) {}

// ── Step 1: Lower hew dialect ops to standard ops ──────────────────────────

mlir::LogicalResult Codegen::lowerHewDialect(mlir::ModuleOp module) {
  // Type converter: map Hew custom types to standard/LLVM types
  mlir::TypeConverter typeConverter;
  typeConverter.addConversion([](mlir::Type type) { return type; });
  typeConverter.addConversion([](hew::ActorRefType type) -> mlir::Type {
    return mlir::LLVM::LLVMPointerType::get(type.getContext());
  });
  typeConverter.addConversion([](hew::StringRefType type) -> mlir::Type {
    return mlir::LLVM::LLVMPointerType::get(type.getContext());
  });
  typeConverter.addConversion([](hew::TypedActorRefType type) -> mlir::Type {
    return mlir::LLVM::LLVMPointerType::get(type.getContext());
  });
  typeConverter.addConversion([](hew::VecType type) -> mlir::Type {
    return mlir::LLVM::LLVMPointerType::get(type.getContext());
  });
  typeConverter.addConversion([](hew::HashMapType type) -> mlir::Type {
    return mlir::LLVM::LLVMPointerType::get(type.getContext());
  });
  typeConverter.addConversion([](hew::HandleType type) -> mlir::Type {
    return mlir::LLVM::LLVMPointerType::get(type.getContext());
  });
  typeConverter.addConversion(
      [&typeConverter](hew::HewTupleType type) -> std::optional<mlir::Type> {
        llvm::SmallVector<mlir::Type, 4> convertedTypes;
        for (auto elemType : type.getElementTypes()) {
          auto converted = typeConverter.convertType(elemType);
          if (!converted)
            return std::nullopt;
          convertedTypes.push_back(converted);
        }
        return mlir::LLVM::LLVMStructType::getLiteral(type.getContext(), convertedTypes);
      });
  typeConverter.addConversion(
      [&typeConverter](hew::HewArrayType type) -> std::optional<mlir::Type> {
        auto elemType = typeConverter.convertType(type.getElementType());
        if (!elemType)
          return std::nullopt;
        return mlir::LLVM::LLVMArrayType::get(elemType, type.getSize());
      });
  typeConverter.addConversion([](hew::HewTraitObjectType type) -> mlir::Type {
    auto *ctx = type.getContext();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(ctx);
    return mlir::LLVM::LLVMStructType::getLiteral(ctx, {ptrType, ptrType});
  });
  typeConverter.addConversion([](hew::ClosureType type) -> mlir::Type {
    auto *ctx = type.getContext();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(ctx);
    return mlir::LLVM::LLVMStructType::getLiteral(ctx, {ptrType, ptrType});
  });
  typeConverter.addConversion([&](mlir::MemRefType type) -> std::optional<mlir::Type> {
    auto elemType = typeConverter.convertType(type.getElementType());
    if (!elemType || elemType == type.getElementType())
      return std::nullopt; // unchanged
    return mlir::MemRefType::get(type.getShape(), elemType, type.getLayout(),
                                 type.getMemorySpace());
  });
  typeConverter.addConversion(
      [&typeConverter](hew::OptionEnumType type) -> std::optional<mlir::Type> {
        auto inner = typeConverter.convertType(type.getInnerType());
        if (!inner)
          return std::nullopt;
        return mlir::LLVM::LLVMStructType::getLiteral(
            type.getContext(), {mlir::IntegerType::get(type.getContext(), 32), inner});
      });
  typeConverter.addConversion(
      [&typeConverter](hew::ResultEnumType type) -> std::optional<mlir::Type> {
        auto ok = typeConverter.convertType(type.getOkType());
        auto err = typeConverter.convertType(type.getErrType());
        if (!ok || !err)
          return std::nullopt;
        return mlir::LLVM::LLVMStructType::getLiteral(
            type.getContext(), {mlir::IntegerType::get(type.getContext(), 32), ok, err});
      });
  // Materializations: only allow boundary casts that map through the type
  // converter (plus explicit func ref <-> !llvm.ptr bridging).
  auto canMaterializeBoundaryCast = [&typeConverter](mlir::Type resultType, mlir::Type inputType) {
    if (resultType == inputType)
      return true;

    auto convertedInputType = typeConverter.convertType(inputType);
    if (convertedInputType && convertedInputType == resultType)
      return true;

    auto convertedResultType = typeConverter.convertType(resultType);
    if (convertedResultType && convertedResultType == inputType)
      return true;

    if (convertedInputType && convertedResultType && convertedInputType == convertedResultType)
      return true;

    return (llvm::isa<mlir::LLVM::LLVMPointerType>(resultType) &&
            llvm::isa<mlir::FunctionType>(inputType)) ||
           (llvm::isa<mlir::FunctionType>(resultType) &&
            llvm::isa<mlir::LLVM::LLVMPointerType>(inputType));
  };

  auto materializeBoundaryCast = [&](mlir::OpBuilder &builder, mlir::Type resultType,
                                     mlir::ValueRange inputs, mlir::Location loc) -> mlir::Value {
    if (inputs.size() != 1)
      return mlir::Value();

    auto input = inputs.front();
    if (input.getType() == resultType)
      return input;
    if (!canMaterializeBoundaryCast(resultType, input.getType()))
      return mlir::Value();

    return builder.create<mlir::UnrealizedConversionCastOp>(loc, resultType, input).getResult(0);
  };

  typeConverter.addSourceMaterialization(materializeBoundaryCast);
  typeConverter.addTargetMaterialization(materializeBoundaryCast);

  mlir::ConversionTarget target(context);

  // Mark hew dialect ops as illegal (they must be converted)
  target.addIllegalDialect<hew::HewDialect>();
  // VtableRefOp is lowered in a separate pass after FuncToLLVM
  target.addLegalOp<hew::VtableRefOp>();

  // Mark everything else as legal
  target.addLegalDialect<mlir::arith::ArithDialect>();
  target.addLegalDialect<mlir::cf::ControlFlowDialect>();
  target.addLegalDialect<mlir::LLVM::LLVMDialect>();
  target.addLegalOp<mlir::ModuleOp>();
  target.addLegalOp<mlir::UnrealizedConversionCastOp>();

  // Dialects that may carry Hew types: legal only when all types are converted
  auto isLegalOp = [&](mlir::Operation *op) { return typeConverter.isLegal(op); };
  target.addDynamicallyLegalDialect<mlir::func::FuncDialect, mlir::scf::SCFDialect,
                                    mlir::memref::MemRefDialect>(isLegalOp);
  // func.func needs signature check too
  target.addDynamicallyLegalOp<mlir::func::FuncOp>([&](mlir::func::FuncOp op) {
    return typeConverter.isSignatureLegal(op.getFunctionType()) &&
           typeConverter.isLegal(&op.getBody());
  });

  mlir::RewritePatternSet patterns(&context);

  // Add our custom lowering patterns for hew ops
  patterns.add<PrintOpLowering>(typeConverter, &context);
  patterns.add<ConstantOpLowering>(typeConverter, &context);
  patterns.add<GlobalStringOpLowering>(typeConverter, &context);
  patterns.add<CastOpLowering>(typeConverter, &context);
  patterns.add<StructInitOpLowering>(typeConverter, &context);
  patterns.add<FieldGetOpLowering>(typeConverter, &context);
  patterns.add<FieldSetOpLowering>(typeConverter, &context);
  // Semantic ops
  patterns.add<SizeOfOpLowering>(typeConverter, &context);
  patterns.add<EnumConstructOpLowering>(typeConverter, &context);
  patterns.add<EnumExtractTagOpLowering>(typeConverter, &context);
  patterns.add<EnumExtractPayloadOpLowering>(typeConverter, &context);
  patterns.add<PackArgsOpLowering>(typeConverter, &context);
  // Actor ops (Phase 1)
  patterns.add<ActorSpawnOpLowering>(typeConverter, &context);
  patterns.add<ActorSendOpLowering>(typeConverter, &context);
  patterns.add<ActorAskOpLowering>(typeConverter, &context);
  patterns.add<ActorStopOpLowering>(typeConverter, &context);
  patterns.add<ActorCloseOpLowering>(typeConverter, &context);
  patterns.add<ActorSelfOpLowering>(typeConverter, &context);
  patterns.add<ActorLinkOpLowering>(typeConverter, &context);
  patterns.add<ActorUnlinkOpLowering>(typeConverter, &context);
  patterns.add<ActorMonitorOpLowering>(typeConverter, &context);
  patterns.add<ActorDemonitorOpLowering>(typeConverter, &context);
  patterns.add<CooperateOpLowering>(typeConverter, &context);
  patterns.add<SleepOpLowering>(typeConverter, &context);
  patterns.add<PanicOpLowering>(typeConverter, &context);
  patterns.add<ReceiveOpLowering>(typeConverter, &context);
  // Assert ops
  patterns.add<AssertOpLowering>(typeConverter, &context);
  patterns.add<AssertEqOpLowering>(typeConverter, &context);
  patterns.add<AssertNeOpLowering>(typeConverter, &context);
  // Supervisor ops
  patterns.add<SupervisorNewOpLowering>(typeConverter, &context);
  patterns.add<SupervisorStartOpLowering>(typeConverter, &context);
  patterns.add<SupervisorStopOpLowering>(typeConverter, &context);
  patterns.add<SupervisorAddChildOpLowering>(typeConverter, &context);
  patterns.add<ChildSpecCreateOpLowering>(typeConverter, &context);
  patterns.add<SupervisorAddChildSupervisorOpLowering>(typeConverter, &context);
  // Vec ops
  patterns.add<VecNewOpLowering>(typeConverter, &context);
  patterns.add<VecPushOpLowering>(typeConverter, &context);
  patterns.add<VecGetOpLowering>(typeConverter, &context);
  patterns.add<VecSetOpLowering>(typeConverter, &context);
  patterns.add<VecLenOpLowering>(typeConverter, &context);
  patterns.add<VecPopOpLowering>(typeConverter, &context);
  patterns.add<VecRemoveOpLowering>(typeConverter, &context);
  patterns.add<VecIsEmptyOpLowering>(typeConverter, &context);
  patterns.add<VecClearOpLowering>(typeConverter, &context);
  patterns.add<VecFreeOpLowering>(typeConverter, &context);
  // HashMap ops
  patterns.add<HashMapNewOpLowering>(typeConverter, &context);
  patterns.add<HashMapInsertOpLowering>(typeConverter, &context);
  patterns.add<HashMapGetOpLowering>(typeConverter, &context);
  patterns.add<HashMapContainsKeyOpLowering>(typeConverter, &context);
  patterns.add<HashMapRemoveOpLowering>(typeConverter, &context);
  patterns.add<HashMapLenOpLowering>(typeConverter, &context);
  patterns.add<HashMapFreeOpLowering>(typeConverter, &context);
  patterns.add<HashMapKeysOpLowering>(typeConverter, &context);
  // Generator ops
  patterns.add<GenCtxCreateOpLowering>(typeConverter, &context);
  patterns.add<GenNextOpLowering>(typeConverter, &context);
  patterns.add<GenYieldOpLowering>(typeConverter, &context);
  patterns.add<GenFreeOpLowering>(typeConverter, &context);
  patterns.add<GenWrapValueOpLowering>(typeConverter, &context);
  patterns.add<GenWrapDoneOpLowering>(typeConverter, &context);
  // Regex ops
  patterns.add<RegexNewOpLowering>(typeConverter, &context);
  patterns.add<RegexIsMatchOpLowering>(typeConverter, &context);
  patterns.add<RegexFindOpLowering>(typeConverter, &context);
  patterns.add<RegexReplaceOpLowering>(typeConverter, &context);
  patterns.add<RegexFreeOpLowering>(typeConverter, &context);
  // Scheduler lifecycle ops
  patterns.add<SchedInitOpLowering>(typeConverter, &context);
  patterns.add<SchedShutdownOpLowering>(typeConverter, &context);
  // Arena allocation
  patterns.add<ArenaMallocOpLowering>(typeConverter, &context);
  // Reference counting ops
  patterns.add<RcNewOpLowering>(typeConverter, &context);
  patterns.add<RcCloneOpLowering>(typeConverter, &context);
  patterns.add<RcDropOpLowering>(typeConverter, &context);
  // Drop op (generic resource cleanup)
  patterns.add<DropOpLowering>(typeConverter, &context);
  // Scope ops
  patterns.add<ScopeCreateOpLowering>(typeConverter, &context);
  patterns.add<ScopeJoinOpLowering>(typeConverter, &context);
  patterns.add<ScopeDestroyOpLowering>(typeConverter, &context);
  patterns.add<ScopeLaunchOpLowering>(typeConverter, &context);
  patterns.add<ScopeAwaitOpLowering>(typeConverter, &context);
  patterns.add<ScopeCancelOpLowering>(typeConverter, &context);

  patterns.add<TaskSetResultOpLowering>(typeConverter, &context);
  patterns.add<TaskCompleteOpLowering>(typeConverter, &context);
  // Select ops
  patterns.add<SelectCreateOpLowering>(typeConverter, &context);
  patterns.add<SelectAddOpLowering>(typeConverter, &context);
  patterns.add<SelectFirstOpLowering>(typeConverter, &context);
  patterns.add<SelectDestroyOpLowering>(typeConverter, &context);
  patterns.add<SelectWaitOpLowering>(typeConverter, &context);
  // Runtime call
  patterns.add<RuntimeCallOpLowering>(typeConverter, &context);
  // Trait dispatch
  patterns.add<TraitDispatchOpLowering>(typeConverter, &context);
  // String & conversion ops
  patterns.add<StringConcatOpLowering>(typeConverter, &context);
  patterns.add<ToStringOpLowering>(typeConverter, &context);
  patterns.add<StringMethodOpLowering>(typeConverter, &context);
  // Tuple, array, trait object ops
  patterns.add<TupleCreateOpLowering>(typeConverter, &context);
  patterns.add<TupleExtractOpLowering>(typeConverter, &context);
  patterns.add<ArrayCreateOpLowering>(typeConverter, &context);
  patterns.add<ArrayExtractOpLowering>(typeConverter, &context);
  patterns.add<TraitObjectCreateOpLowering>(typeConverter, &context);
  patterns.add<TraitObjectDataOpLowering>(typeConverter, &context);
  patterns.add<TraitObjectTagOpLowering>(typeConverter, &context);
  // VtableRefOp is lowered by VtableGlobalPass after FuncToLLVM
  // Closure ops
  patterns.add<ClosureCreateOpLowering>(typeConverter, &context);
  patterns.add<ClosureGetFnOpLowering>(typeConverter, &context);
  patterns.add<ClosureGetEnvOpLowering>(typeConverter, &context);
  // Type bridge ops
  patterns.add<FuncPtrOpLowering>(typeConverter, &context);
  patterns.add<BitcastOpLowering>(typeConverter, &context);

  // Function signature conversion (converts !hew.result/option in func types)
  mlir::populateFunctionOpInterfaceTypeConversionPattern<mlir::func::FuncOp>(patterns,
                                                                             typeConverter);
  mlir::populateReturnOpTypeConversionPattern(patterns, typeConverter);
  mlir::populateCallOpTypeConversionPattern(patterns, typeConverter);

  // SCF structural type conversion (converts !hew.result/option in scf.if/yield)
  mlir::scf::populateSCFStructuralTypeConversionsAndLegality(typeConverter, patterns, target);

  // MemRef type conversion (converts memref<!hew.result<...>> element types)
  patterns.add<MemRefAllocaTypeConversion>(typeConverter, &context);
  patterns.add<MemRefLoadTypeConversion>(typeConverter, &context);
  patterns.add<MemRefStoreTypeConversion>(typeConverter, &context);

  if (mlir::failed(mlir::applyPartialConversion(module, target, std::move(patterns)))) {
    llvm::errs() << "Error: failed to lower Hew dialect ops\n";
    return mlir::failure();
  }

  return mlir::success();
}

// ── InternalLinkagePass — hide non-main functions from dynamic symbol table ─
//
// Stdlib wrapper modules (e.g. std/fs.hew) define short-named helpers like
// `read` and `write` that shadow POSIX libc symbols.  Giving every non-main
// function definition internal linkage prevents these wrappers from being
// exported, which avoids crashes when the runtime calls libc::write() etc.

struct InternalLinkagePass
    : public mlir::PassWrapper<InternalLinkagePass, mlir::OperationPass<mlir::ModuleOp>> {
  MLIR_DEFINE_EXPLICIT_INTERNAL_INLINE_TYPE_ID(InternalLinkagePass)

  void runOnOperation() override {
    getOperation()->walk([](mlir::LLVM::LLVMFuncOp funcOp) {
      if (funcOp.isExternal())
        return;
      if (funcOp.getName() == "main" ||
          funcOp.getVisibility() == mlir::SymbolTable::Visibility::Public)
        return;
      funcOp.setLinkage(mlir::LLVM::Linkage::Internal);
    });
  }

  llvm::StringRef getArgument() const override { return "set-internal-linkage"; }
  llvm::StringRef getDescription() const override {
    return "Set internal linkage on non-main function definitions";
  }
};

// ── SetTailCallsPass — transfer hew.tail_call to LLVM tail call kind ───────

struct SetTailCallsPass
    : public mlir::PassWrapper<SetTailCallsPass, mlir::OperationPass<mlir::ModuleOp>> {
  MLIR_DEFINE_EXPLICIT_INTERNAL_INLINE_TYPE_ID(SetTailCallsPass)

  void runOnOperation() override {
    getOperation()->walk([](mlir::LLVM::CallOp callOp) {
      if (callOp->hasAttr("hew.tail_call")) {
        callOp.setTailCallKind(mlir::LLVM::tailcallkind::TailCallKind::Tail);
        callOp->removeAttr("hew.tail_call");
      }
    });
  }

  llvm::StringRef getArgument() const override { return "set-tail-calls"; }
  llvm::StringRef getDescription() const override {
    return "Set tail call attributes on marked LLVM calls";
  }
};

// ── DevirtualizeTraitDispatchPass — direct-call when vtable is static ───────
//
// After canonicalization, trait_object.data/tag fold through
// trait_object.create, so a TraitDispatchOp's vtable_ptr operand often
// points directly at the VtableRefOp that carries the function-name list.
// When that's the case the concrete type is statically known and we can
// replace the indirect dispatch with a direct func.call to the shim.

struct DevirtualizeTraitDispatchPass
    : public mlir::PassWrapper<DevirtualizeTraitDispatchPass, mlir::OperationPass<mlir::ModuleOp>> {
  MLIR_DEFINE_EXPLICIT_INTERNAL_INLINE_TYPE_ID(DevirtualizeTraitDispatchPass)

  void runOnOperation() override {
    auto module = getOperation();

    // Collect ops first to avoid modifying while walking.
    llvm::SmallVector<hew::TraitDispatchOp> worklist;
    module.walk([&](hew::TraitDispatchOp op) { worklist.push_back(op); });

    for (auto op : worklist) {
      auto vtableRef = op.getVtablePtr().getDefiningOp<hew::VtableRefOp>();
      if (!vtableRef)
        continue; // non-devirtualizable (vtable from param, phi, etc.)

      auto functions = vtableRef.getFunctions();
      auto methodIndex = static_cast<size_t>(op.getMethodIndex());
      if (methodIndex >= functions.size())
        continue;

      auto funcName = mlir::cast<mlir::StringAttr>(functions[methodIndex]).getValue();

      // Verify the target function exists in the module.
      if (!module.lookupSymbol<mlir::func::FuncOp>(funcName))
        continue;

      mlir::OpBuilder builder(op);
      auto loc = op.getLoc();

      // Build call args: data_ptr + extra_args (same convention as shim).
      llvm::SmallVector<mlir::Value> callArgs = {op.getDataPtr()};
      for (auto arg : op.getExtraArgs())
        callArgs.push_back(arg);

      llvm::SmallVector<mlir::Type> resultTypes;
      if (op.getNumResults() > 0)
        resultTypes.push_back(op.getResult().getType());

      auto call = builder.create<mlir::func::CallOp>(loc, funcName, resultTypes, callArgs);

      if (op.getNumResults() > 0)
        op.replaceAllUsesWith(call.getResults());
      op.erase();
    }
  }

  llvm::StringRef getArgument() const override { return "devirtualize-trait-dispatch"; }
  llvm::StringRef getDescription() const override {
    return "Replace trait dispatch with direct calls when vtable is statically "
           "known";
  }
};

// ── StackPromoteDynCoercionPass — arena.malloc → alloca for confined ptrs ──
//
// When an ArenaMallocOp result never escapes the current function (only used
// by stores-into, devirtualized shim calls, or confined trait objects), the
// heap allocation can be replaced by a stack alloca.

struct StackPromoteDynCoercionPass
    : public mlir::PassWrapper<StackPromoteDynCoercionPass, mlir::OperationPass<mlir::ModuleOp>> {
  MLIR_DEFINE_EXPLICIT_INTERNAL_INLINE_TYPE_ID(StackPromoteDynCoercionPass)

  /// Return true if every user of \p val is safe for stack promotion.
  static bool isConfined(mlir::Value val) {
    for (auto *user : val.getUsers()) {
      if (auto storeOp = mlir::dyn_cast<mlir::LLVM::StoreOp>(user)) {
        // Safe only when our value is the *address* operand (operand 1),
        // not the value being stored (operand 0).
        if (storeOp.getAddr() != val)
          return false;
        continue;
      }
      if (auto callOp = mlir::dyn_cast<mlir::func::CallOp>(user)) {
        // Devirtualized shim calls: the shim loads from the pointer and
        // does not escape it.
        if (callOp.getCallee().starts_with("__dyn.") && callOp.getOperands().front() == val)
          continue;
        return false;
      }
      if (auto createOp = mlir::dyn_cast<hew::TraitObjectCreateOp>(user)) {
        // The trait object itself must also be confined.
        if (!isTraitObjectConfined(createOp.getResult()))
          return false;
        continue;
      }
      // Any other use is a potential escape.
      return false;
    }
    return true;
  }

  /// Return true if a trait object value is only used by extractors or
  /// dispatches within the same function.
  static bool isTraitObjectConfined(mlir::Value traitObj) {
    for (auto *user : traitObj.getUsers()) {
      if (mlir::isa<hew::TraitObjectDataOp>(user) || mlir::isa<hew::TraitObjectTagOp>(user) ||
          mlir::isa<hew::TraitDispatchOp>(user))
        continue;
      return false;
    }
    return true;
  }

  void runOnOperation() override {
    auto module = getOperation();

    llvm::SmallVector<hew::ArenaMallocOp> worklist;
    module.walk([&](hew::ArenaMallocOp op) { worklist.push_back(op); });

    for (auto op : worklist) {
      if (!isConfined(op.getResult()))
        continue;

      // We need the SizeOfOp to know the element type for the alloca.
      auto sizeOfOp = op.getSize().getDefiningOp<hew::SizeOfOp>();
      if (!sizeOfOp)
        continue;

      auto measuredType = sizeOfOp.getMeasuredType();
      auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
      auto loc = op.getLoc();

      // Insert alloca in the function entry block.
      auto parentFunc = op->getParentOfType<mlir::func::FuncOp>();
      if (!parentFunc)
        continue;
      auto &entryBlock = parentFunc.getBody().front();
      mlir::OpBuilder allocBuilder(&entryBlock, entryBlock.begin());
      auto one = allocBuilder.create<mlir::LLVM::ConstantOp>(loc, allocBuilder.getI64Type(),
                                                             allocBuilder.getI64IntegerAttr(1));
      auto alloca = allocBuilder.create<mlir::LLVM::AllocaOp>(loc, ptrType, measuredType, one);

      op.getResult().replaceAllUsesWith(alloca.getResult());
      op.erase();

      // DCE the SizeOfOp if it has no remaining users.
      if (sizeOfOp->use_empty())
        sizeOfOp.erase();
    }
  }

  llvm::StringRef getArgument() const override { return "stack-promote-dyn-coercion"; }
  llvm::StringRef getDescription() const override {
    return "Replace arena.malloc with alloca when the pointer is confined";
  }
};

// ── VtableGlobalPass — lower VtableRefOp to vtable globals + addressof ─────
//
// Runs after FuncToLLVM so that llvm.mlir.addressof can reference llvm.func
// symbols for vtable entries.

struct VtableGlobalPass
    : public mlir::PassWrapper<VtableGlobalPass, mlir::OperationPass<mlir::ModuleOp>> {
  MLIR_DEFINE_EXPLICIT_INTERNAL_INLINE_TYPE_ID(VtableGlobalPass)

  void runOnOperation() override {
    auto module = getOperation();
    auto *ctx = &getContext();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(ctx);

    // Track which vtable globals have been created
    llvm::DenseSet<llvm::StringRef> createdVtables;

    module.walk([&](hew::VtableRefOp op) {
      mlir::OpBuilder builder(op);
      auto loc = op.getLoc();
      auto vtableName = op.getVtableName();
      auto functions = op.getFunctions();

      // Create the vtable global if it doesn't exist yet
      if (!module.lookupSymbol<mlir::LLVM::GlobalOp>(vtableName) &&
          !createdVtables.contains(vtableName)) {
        auto arrayType = mlir::LLVM::LLVMArrayType::get(ptrType, functions.size());

        mlir::OpBuilder moduleBuilder(ctx);
        moduleBuilder.setInsertionPointToStart(module.getBody());
        auto globalOp = moduleBuilder.create<mlir::LLVM::GlobalOp>(
            loc, arrayType, /*isConstant=*/true, mlir::LLVM::Linkage::Internal, vtableName.str(),
            mlir::Attribute{});

        auto *initBlock = moduleBuilder.createBlock(&globalOp.getInitializerRegion());
        moduleBuilder.setInsertionPointToStart(initBlock);

        mlir::Value arr = moduleBuilder.create<mlir::LLVM::UndefOp>(loc, arrayType);
        for (size_t i = 0; i < functions.size(); ++i) {
          auto funcName = mlir::cast<mlir::StringAttr>(functions[i]).getValue().str();
          auto funcAddr = moduleBuilder.create<mlir::LLVM::AddressOfOp>(loc, ptrType, funcName);
          arr = moduleBuilder.create<mlir::LLVM::InsertValueOp>(loc, arr, funcAddr, i);
        }
        moduleBuilder.create<mlir::LLVM::ReturnOp>(loc, arr);
        createdVtables.insert(vtableName);
      }

      // Replace VtableRefOp with llvm.mlir.addressof
      auto vtableAddr = builder.create<mlir::LLVM::AddressOfOp>(loc, ptrType, vtableName.str());
      op.replaceAllUsesWith(vtableAddr.getResult());
      op.erase();
    });
  }

  llvm::StringRef getArgument() const override { return "vtable-global"; }
  llvm::StringRef getDescription() const override {
    return "Lower VtableRefOp to vtable globals and addressof";
  }
};

// ── Step 2: Lower standard dialects to LLVM dialect ────────────────────────

mlir::LogicalResult Codegen::lowerToLLVMDialect(mlir::ModuleOp module) {
  mlir::PassManager pm(&context);

  // Optimization passes — fold constants, eliminate redundant loads, simplify
  pm.addPass(mlir::createCanonicalizerPass());
  pm.addPass(mlir::createCSEPass());

  // SCF → ControlFlow (scf.if/while/for → cf.br/cf.cond_br)
  pm.addPass(mlir::createSCFToControlFlowPass());
  // Func → LLVM
  pm.addPass(mlir::createConvertFuncToLLVMPass());
  // Lower VtableRefOp → vtable globals + llvm.mlir.addressof
  // (must run after FuncToLLVM so addressof can reference llvm.func)
  pm.addPass(std::make_unique<VtableGlobalPass>());
  // Hide non-main functions to avoid shadowing libc symbols
  pm.addPass(std::make_unique<InternalLinkagePass>());
  // Transfer hew.tail_call attributes to LLVM::CallOp tail call kind
  pm.addPass(std::make_unique<SetTailCallsPass>());
  // Arith → LLVM
  pm.addPass(mlir::createArithToLLVMConversionPass());
  // ControlFlow → LLVM
  pm.addPass(mlir::createConvertControlFlowToLLVMPass());
  // MemRef → LLVM
  pm.addPass(mlir::createFinalizeMemRefToLLVMConversionPass());
  // Clean up unrealized casts from type converter boundaries
  pm.addPass(mlir::createReconcileUnrealizedCastsPass());

  // Post-lowering cleanup — simplify LLVM dialect IR
  pm.addPass(mlir::createCanonicalizerPass());

  if (mlir::failed(pm.run(module))) {
    llvm::errs() << "Error: failed to lower to LLVM dialect\n";
    return mlir::failure();
  }

  return mlir::success();
}

// ── Step 3 & 4: Translate to LLVM IR ──────────────────────────────────────

std::unique_ptr<llvm::Module> Codegen::lowerToLLVMIR(mlir::ModuleOp module,
                                                     llvm::LLVMContext &llvmContext,
                                                     bool skipOptimization) {
  // Verify module before any lowering — catches malformed IR from MLIRGen
  if (mlir::failed(mlir::verify(module))) {
    llvm::errs() << "Error: module verification failed before lowering\n";
    module.dump();
    return nullptr;
  }

  // Pre-lowering: canonicalize → devirtualize → canonicalize → stack-promote
  {
    mlir::PassManager prePM(&context);
    prePM.addPass(mlir::createCanonicalizerPass());
    prePM.addPass(std::make_unique<DevirtualizeTraitDispatchPass>());
    prePM.addPass(mlir::createCanonicalizerPass());
    prePM.addPass(std::make_unique<StackPromoteDynCoercionPass>());
    if (mlir::failed(prePM.run(module))) {
      llvm::errs() << "Error: pre-lowering optimization pipeline failed\n";
      return nullptr;
    }
  }

  // Step 1: Lower hew ops
  if (mlir::failed(lowerHewDialect(module))) {
    llvm::errs() << "Error: Hew dialect lowering failed\n";
    return nullptr;
  }

  // Verify after hew lowering
  if (mlir::failed(mlir::verify(module))) {
    llvm::errs() << "Error: module verification failed after Hew lowering\n";
    module.dump();
    return nullptr;
  }

  // Step 2: Lower to LLVM dialect
  if (mlir::failed(lowerToLLVMDialect(module))) {
    llvm::errs() << "Error: LLVM dialect lowering failed\n";
    return nullptr;
  }

  // Fail fast at the translation boundary if cast reconciliation missed
  // any unrealized conversion cast.
  if (mlir::failed(failOnUnreconciledCasts(module, "after reconcile pass"))) {
    module.dump();
    return nullptr;
  }

  // Verify after full lowering
  if (mlir::failed(mlir::verify(module))) {
    llvm::errs() << "Error: module verification failed after LLVM lowering\n";
    module.dump();
    return nullptr;
  }

  // Register dialect translations
  mlir::registerLLVMDialectTranslation(context);
  mlir::registerBuiltinDialectTranslation(context);

  // Translate to LLVM IR
  auto llvmModule = mlir::translateModuleToLLVMIR(module, llvmContext);
  if (!llvmModule) {
    llvm::errs() << "Error: MLIR to LLVM IR translation failed\n";
    return nullptr;
  }

  // Run LLVM optimization passes (skip when debug_info is set for debugger use).
  if (!skipOptimization) {
    llvm::LoopAnalysisManager LAM;
    llvm::FunctionAnalysisManager FAM;
    llvm::CGSCCAnalysisManager CGAM;
    llvm::ModuleAnalysisManager MAM;

    llvm::PassBuilder PB;
    PB.registerModuleAnalyses(MAM);
    PB.registerCGSCCAnalyses(CGAM);
    PB.registerFunctionAnalyses(FAM);
    PB.registerLoopAnalyses(LAM);
    PB.crossRegisterProxies(LAM, FAM, CGAM, MAM);

    auto MPM = PB.buildPerModuleDefaultPipeline(llvm::OptimizationLevel::O3);
    MPM.run(*llvmModule, MAM);
  }

  return llvmModule;
}

// ── Step 4: Emit object file via LLVM TargetMachine ───────────────────────

int Codegen::emitObjectFile(llvm::Module &module, const std::string &path,
                            const std::string &targetTripleStr) {
  // Initialize ALL targets for cross-compilation support
  llvm::InitializeAllTargets();
  llvm::InitializeAllTargetInfos();
  llvm::InitializeAllTargetMCs();
  llvm::InitializeAllAsmPrinters();
  llvm::InitializeAllAsmParsers();

  // Use specified target or default to host
  llvm::Triple targetTriple(targetTripleStr.empty() ? llvm::sys::getDefaultTargetTriple()
                                                    : targetTripleStr);
  module.setTargetTriple(targetTriple);

  std::string error;
  auto *target = llvm::TargetRegistry::lookupTarget(targetTriple.str(), error);
  if (!target) {
    llvm::errs() << "Error: could not look up target: " << error << "\n";
    return 1;
  }

  llvm::TargetOptions opt;
  opt.FunctionSections = true;
  opt.DataSections = true;
  auto targetMachine =
      target->createTargetMachine(targetTriple, "generic", "", opt, llvm::Reloc::PIC_);
  module.setDataLayout(targetMachine->createDataLayout());

  std::error_code ec;
  llvm::raw_fd_ostream dest(path, ec, llvm::sys::fs::OF_None);
  if (ec) {
    llvm::errs() << "Error opening output file " << path << ": " << ec.message() << "\n";
    return 1;
  }

  llvm::legacy::PassManager pass;
  if (targetMachine->addPassesToEmitFile(pass, dest, nullptr, llvm::CodeGenFileType::ObjectFile)) {
    llvm::errs() << "Error: target machine cannot emit object file\n";
    return 1;
  }

  pass.run(module);
  dest.flush();
  return 0;
}

// ── Step 5: Link with runtime ─────────────────────────────────────────────

int Codegen::linkExecutable(const std::string &objectPath, const std::string &outputPath,
                            const std::string &runtimeLibDir, const std::string &targetTripleStr) {
  llvm::Triple targetTriple(targetTripleStr.empty() ? llvm::sys::getDefaultTargetTriple()
                                                    : targetTripleStr);
  llvm::Triple hostTriple(llvm::sys::getDefaultTargetTriple());

  // When cross-compiling, try <triple>-gcc as the linker
#ifdef _WIN32
  std::string linkerName = "clang";
#else
  std::string linkerName = "cc";
#endif
  bool crossCompiling = (targetTriple.str() != hostTriple.str());
  if (crossCompiling) {
    linkerName = targetTriple.str() + "-gcc";
  }

  auto ccOrErr = llvm::sys::findProgramByName(linkerName);
  if (!ccOrErr) {
    if (crossCompiling) {
      llvm::errs() << "Error: cross-linker '" << linkerName << "' not found in PATH.\n"
                   << "Hint: install a cross-compilation toolchain, or use "
                   << "--emit-obj to produce an object file for manual linking.\n";
    } else {
#ifdef _WIN32
      llvm::errs() << "Error: could not find 'clang' linker in PATH\n";
#else
      llvm::errs() << "Error: could not find 'cc' linker in PATH\n";
#endif
    }
    return 1;
  }
  std::string ccPath = ccOrErr.get();

  // Find the Rust runtime library
  std::string rtLibPath;
  if (!runtimeLibDir.empty()) {
#ifdef _WIN32
    auto candidate = std::filesystem::path(runtimeLibDir) / "hew_runtime.lib";
#else
    auto candidate = std::filesystem::path(runtimeLibDir) / "libhew_runtime.a";
#endif
    if (std::filesystem::exists(candidate))
      rtLibPath = candidate.string();
  }

  // Build argument list
  llvm::SmallVector<llvm::StringRef, 16> args;
  args.push_back(ccPath);

  // Use lld if available — significantly faster than GNU ld for large archives
  std::string fuseLinker;
  if (!crossCompiling) {
#ifdef _WIN32
    if (llvm::sys::findProgramByName("lld-link"))
      fuseLinker = "-fuse-ld=lld-link";
#else
    if (llvm::sys::findProgramByName("ld.lld"))
      fuseLinker = "-fuse-ld=lld";
    else if (llvm::sys::findProgramByName("ld.mold"))
      fuseLinker = "-fuse-ld=mold";
#endif
  }
  if (!fuseLinker.empty())
    args.push_back(fuseLinker);

  args.push_back(objectPath);
  if (!rtLibPath.empty()) {
    args.push_back(rtLibPath);
  }

  // Dead-code elimination: discard unreferenced sections from the archive
  if (targetTriple.isOSBinFormatELF()) {
    args.push_back("-Wl,--gc-sections");
    args.push_back("-Wl,--strip-all");
  } else if (targetTriple.isOSDarwin()) {
    args.push_back("-Wl,-dead_strip");
    args.push_back("-Wl,-x");
  } else if (targetTriple.isOSWindows()) {
    args.push_back("-Wl,/OPT:REF");
  }

  // Add platform-specific system libraries
  if (targetTriple.isOSLinux()) {
    args.push_back("-lpthread");
    args.push_back("-lm");
    args.push_back("-ldl");
    args.push_back("-lrt");
  } else if (targetTriple.isOSDarwin()) {
    args.push_back("-lpthread");
    args.push_back("-lm");
    args.push_back("-framework");
    args.push_back("CoreFoundation");
    args.push_back("-framework");
    args.push_back("Security");
  } else if (targetTriple.isOSWindows()) {
    args.push_back("-lws2_32");
    args.push_back("-luserenv");
    args.push_back("-lbcrypt");
    args.push_back("-lntdll");
    args.push_back("-ladvapi32");
  } else {
    // Generic Unix-like fallback
    args.push_back("-lpthread");
    args.push_back("-lm");
  }

  args.push_back("-o");
  args.push_back(outputPath);

  std::string errMsg;
  int ret = llvm::sys::ExecuteAndWait(ccPath, args, /*Env=*/std::nullopt,
                                      /*Redirects=*/{}, /*SecondsToWait=*/0,
                                      /*MemoryLimit=*/0, &errMsg);
  if (ret != 0) {
    llvm::errs() << "Error: linking failed";
    if (!errMsg.empty())
      llvm::errs() << ": " << errMsg;
    llvm::errs() << "\n";
    return 1;
  }

  return 0;
}

// ── Full pipeline ─────────────────────────────────────────────────────────

int Codegen::compile(mlir::ModuleOp module, const CodegenOptions &opts) {
  llvm::LLVMContext llvmContext;

  // Set pointer width so lowering patterns emit correct size_t type.
  // wasm32 uses 32-bit pointers/sizes; default is 64-bit.
  int ptrWidth = 64;
  if (opts.target_triple.find("wasm32") != std::string::npos) {
    ptrWidth = 32;
  }
  module->setAttr("hew.ptr_width",
                  mlir::IntegerAttr::get(mlir::IntegerType::get(&context, 32), ptrWidth));

  // Set the LLVM data layout on the MLIR module BEFORE lowering so that
  // sizeof computations (GEP-null trick) use correct pointer sizes for the
  // target.  Without this, cross-compilation to wasm32 treats pointers as
  // 8 bytes instead of 4.
  {
    llvm::InitializeAllTargets();
    llvm::InitializeAllTargetInfos();
    llvm::InitializeAllTargetMCs();
    llvm::Triple triple(opts.target_triple.empty() ? llvm::sys::getDefaultTargetTriple()
                                                   : opts.target_triple);
    std::string error;
    auto *target = llvm::TargetRegistry::lookupTarget(triple.str(), error);
    if (target) {
      llvm::TargetOptions tOpts;
      auto tm = target->createTargetMachine(triple, "generic", "", tOpts, llvm::Reloc::PIC_);
      if (tm) {
        auto dl = tm->createDataLayout().getStringRepresentation();
        module->setAttr(mlir::LLVM::LLVMDialect::getDataLayoutAttrName(),
                        mlir::StringAttr::get(&context, dl));
      }
    }
  }

  // Reject unsupported ops early when targeting WASM.
  if (opts.target_triple.find("wasm") != std::string::npos) {
    if (mlir::failed(validateWasmUnsupportedOps(module)))
      return 1;
  }

  auto llvmModule = lowerToLLVMIR(module, llvmContext, opts.debug_info);
  if (!llvmModule)
    return 1;

  // WASM targets: create `__original_main` wrapper that calls the user's
  // `main` and returns i32 (WASI convention). Hew's `main` may return i64
  // (Hew `int`), void, or i32 — the wrapper normalizes to i32.
  if (opts.target_triple.find("wasm") != std::string::npos) {
    if (auto *mainFn = llvmModule->getFunction("main")) {
      auto *i32Ty = llvm::Type::getInt32Ty(llvmContext);
      auto *wrapperTy = llvm::FunctionType::get(i32Ty, false);
      auto *wrapper = llvm::Function::Create(wrapperTy, llvm::Function::ExternalLinkage,
                                             "__original_main", llvmModule.get());
      auto *bb = llvm::BasicBlock::Create(llvmContext, "entry", wrapper);
      llvm::IRBuilder<> builder(bb);
      auto *call = builder.CreateCall(mainFn);
      auto *retTy = mainFn->getReturnType();
      if (retTy->isVoidTy()) {
        builder.CreateRet(llvm::ConstantInt::get(i32Ty, 0));
      } else if (retTy == i32Ty) {
        builder.CreateRet(call);
      } else {
        // i64 → i32 truncation
        builder.CreateRet(builder.CreateTrunc(call, i32Ty));
      }
      mainFn->setLinkage(llvm::Function::InternalLinkage);
    }
  }

  // If --emit-llvm, just print LLVM IR
  if (opts.emit_llvm_ir) {
    llvmModule->print(llvm::outs(), nullptr);
    return 0;
  }

  // Determine output path
  std::string outputPath = opts.output_path;
  if (outputPath.empty())
    outputPath = "a.out";

  // When emitting only the object file, write directly to the output path.
  // Otherwise, use a temporary .o alongside the final executable path.
  std::string objectPath = opts.emit_object ? outputPath : outputPath + ".o";

  // Emit object file
  int ret = emitObjectFile(*llvmModule, objectPath, opts.target_triple);
  if (ret != 0)
    return ret;

  if (opts.emit_object) {
    // Just the object file, no linking
    return 0;
  }

  // Link
  ret = linkExecutable(objectPath, outputPath, opts.runtime_lib_dir, opts.target_triple);

  // Clean up the object file (unless --emit-obj)
  if (ret == 0 && !opts.emit_object) {
    std::filesystem::remove(objectPath);
  }

  return ret;
}
