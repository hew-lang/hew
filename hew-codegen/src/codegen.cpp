//===- codegen.cpp - Hew MLIR-to-native codegen pipeline -------------------===//
//
// Lowers Hew MLIR modules through the standard pipeline:
//   hew dialect -> func/arith/llvm ops
//   SCF -> ControlFlow -> LLVM dialect
//   LLVM dialect -> LLVM IR -> object file -> linked executable
//
//===----------------------------------------------------------------------===//

#include "hew/codegen.h"
#include "hew/debug_info.h"
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
#include "mlir/Conversion/MathToLLVM/MathToLLVM.h"
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
#include "llvm/IR/IntrinsicInst.h"
#include "llvm/IR/Intrinsics.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/LegacyPassManager.h"
#include "llvm/MC/TargetRegistry.h"
#include "llvm/Passes/PassBuilder.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/FileSystem.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Support/TargetSelect.h"
#include "llvm/Target/TargetMachine.h"
#include "llvm/Target/TargetOptions.h"
#include "llvm/TargetParser/Host.h"
#include "llvm/Transforms/Coroutines/CoroCleanup.h"
#include "llvm/Transforms/Coroutines/CoroEarly.h"
#include "llvm/Transforms/Coroutines/CoroElide.h"
#include "llvm/Transforms/Coroutines/CoroSplit.h"
#include <climits>
#include <iostream>
#include <stdexcept>
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

/// Return true when the module targets a 64-bit platform (native).
/// Inline Vec/string lowering assumes 64-bit struct layout and must be
/// skipped on WASM32.
static bool isNative64(mlir::ModuleOp module) {
  if (auto attr = module->getAttrOfType<mlir::IntegerAttr>("hew.ptr_width"))
    return attr.getInt() == 64;
  return true;
}

/// Get or declare an external function using func.func (used before
/// the full LLVM lowering, so we can use func.call).
mlir::func::FuncOp getOrInsertFuncDecl(mlir::ModuleOp module, mlir::OpBuilder &builder,
                                       llvm::StringRef name, mlir::FunctionType funcType) {
  if (auto func = module.lookupSymbol<mlir::func::FuncOp>(name))
    return func;

  auto savedIP = builder.saveInsertionPoint();
  builder.setInsertionPointToStart(module.getBody());

  auto funcOp = mlir::func::FuncOp::create(builder, builder.getUnknownLoc(), name, funcType);
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
        inputVal = mlir::arith::ExtUIOp::create(rewriter, loc, rewriter.getI32Type(), inputVal);
      else
        inputVal = mlir::arith::ExtSIOp::create(rewriter, loc, rewriter.getI32Type(), inputVal);
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
    } else if (auto floatType = mlir::dyn_cast<mlir::FloatType>(inputType);
               floatType && floatType.getWidth() == 32) {
      // f32: Promote to f64 for printing
      inputVal = mlir::arith::ExtFOp::create(rewriter, loc, rewriter.getF64Type(), inputVal);
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
      return op->emitError("unsupported type for print");
    }

    getOrInsertFuncDecl(module, rewriter, funcName, funcType);
    mlir::func::CallOp::create(rewriter, loc, funcName, mlir::TypeRange{},
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
          mlir::LLVM::GlobalOp::create(rewriter, loc, arrayType, /*isConstant=*/true,
                                       mlir::LLVM::Linkage::Internal, symName,
                                       rewriter.getStringAttr(std::string(strValue) + '\0'));
          rewriter.restoreInsertionPoint(savedIP);
        }
      }

      auto addrOp = mlir::LLVM::AddressOfOp::create(rewriter, loc, ptrType, symName);
      rewriter.replaceOp(op, addrOp.getResult());
      return mlir::success();
    }

    // Integer constant
    if (auto intAttr = mlir::dyn_cast<mlir::IntegerAttr>(value)) {
      auto newOp = mlir::arith::ConstantOp::create(rewriter, loc, intAttr);
      rewriter.replaceOp(op, newOp.getResult());
      return mlir::success();
    }

    // Float constant
    if (auto floatAttr = mlir::dyn_cast<mlir::FloatAttr>(value)) {
      auto newOp = mlir::arith::ConstantOp::create(rewriter, loc, floatAttr);
      rewriter.replaceOp(op, newOp.getResult());
      return mlir::success();
    }

    // Bool constant (stored as IntegerAttr with i1 type)
    if (auto boolAttr = mlir::dyn_cast<mlir::BoolAttr>(value)) {
      auto intAttr = rewriter.getIntegerAttr(rewriter.getI1Type(), boolAttr.getValue() ? 1 : 0);
      auto newOp = mlir::arith::ConstantOp::create(rewriter, loc, intAttr);
      rewriter.replaceOp(op, newOp.getResult());
      return mlir::success();
    }

    op.emitError() << "unsupported constant type: " << value;
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

    bool srcIsFloat = mlir::isa<mlir::FloatType>(inputType);
    bool dstIsFloat = mlir::isa<mlir::FloatType>(resultType);

    // Int to float
    if (inputType.isIntOrIndex() && dstIsFloat) {
      if (isUnsigned)
        rewriter.replaceOpWithNewOp<mlir::arith::UIToFPOp>(op, resultType, inputVal);
      else
        rewriter.replaceOpWithNewOp<mlir::arith::SIToFPOp>(op, resultType, inputVal);
      return mlir::success();
    }

    // Float to int
    if (srcIsFloat && resultType.isIntOrIndex()) {
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

    // Float to float conversion (f32 ↔ f64)
    if (srcIsFloat && dstIsFloat) {
      auto srcWidth = mlir::cast<mlir::FloatType>(inputType).getWidth();
      auto dstWidth = mlir::cast<mlir::FloatType>(resultType).getWidth();
      if (srcWidth < dstWidth) {
        // f32 → f64: extend
        rewriter.replaceOpWithNewOp<mlir::arith::ExtFOp>(op, resultType, inputVal);
        return mlir::success();
      }
      if (srcWidth > dstWidth) {
        // f64 → f32: truncate
        rewriter.replaceOpWithNewOp<mlir::arith::TruncFOp>(op, resultType, inputVal);
        return mlir::success();
      }
      // Same width: just replace
      rewriter.replaceOp(op, inputVal);
      return mlir::success();
    }

    op.emitError() << "unsupported cast: " << inputType << " to " << resultType;
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
    mlir::Value result = mlir::LLVM::UndefOp::create(rewriter, loc, structType);

    // Insert each field value
    auto fields = adaptor.getFields();
    for (auto [idx, fieldVal] : llvm::enumerate(fields)) {
      result = mlir::LLVM::InsertValueOp::create(
          rewriter, loc, result, fieldVal, llvm::ArrayRef<int64_t>{static_cast<int64_t>(idx)});
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

    auto result = mlir::LLVM::ExtractValueOp::create(
        rewriter, loc, adaptor.getStructVal(),
        llvm::ArrayRef<int64_t>{static_cast<int64_t>(fieldIndex)});

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

    auto result = mlir::LLVM::InsertValueOp::create(
        rewriter, loc, adaptor.getStructVal(), adaptor.getValue(),
        llvm::ArrayRef<int64_t>{static_cast<int64_t>(fieldIndex)});

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
  auto nullPtr = mlir::LLVM::ZeroOp::create(rewriter, loc, ptrType);
  auto sizeGep = mlir::LLVM::GEPOp::create(rewriter, loc, ptrType, structType, nullPtr,
                                           llvm::ArrayRef<mlir::LLVM::GEPArg>{1});
  return mlir::LLVM::PtrToIntOp::create(rewriter, loc, sizeType, sizeGep);
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
    auto one = mlir::arith::ConstantIntOp::create(rewriter, loc, sizeType, 1);
    return mlir::LLVM::AllocaOp::create(rewriter, loc, ptrType, elemType, one).getResult();
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
      auto cloned = mlir::func::CallOp::create(rewriter, loc, "strdup", mlir::TypeRange{ptrType},
                                               mlir::ValueRange{convArg});
      result.push_back(cloned.getResult(0));
    } else if (mlir::isa<hew::VecType>(origType)) {
      auto ft = rewriter.getFunctionType({ptrType}, {ptrType});
      getOrInsertFuncDecl(module, rewriter, "hew_vec_clone", ft);
      auto cloned = mlir::func::CallOp::create(rewriter, loc, "hew_vec_clone",
                                               mlir::TypeRange{ptrType}, mlir::ValueRange{convArg});
      result.push_back(cloned.getResult(0));
    } else if (mlir::isa<hew::HashMapType>(origType)) {
      auto ft = rewriter.getFunctionType({ptrType}, {ptrType});
      getOrInsertFuncDecl(module, rewriter, "hew_hashmap_clone_impl", ft);
      auto cloned = mlir::func::CallOp::create(rewriter, loc, "hew_hashmap_clone_impl",
                                               mlir::TypeRange{ptrType}, mlir::ValueRange{convArg});
      result.push_back(cloned.getResult(0));
    } else if (mlir::isa<hew::ClosureType>(origType)) {
      auto closureType = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(convArg.getType());
      if (!closureType || closureType.getBody().size() != 2) {
        result.push_back(convArg);
        continue;
      }
      auto fnPtr = mlir::LLVM::ExtractValueOp::create(
                       rewriter, loc, convArg, llvm::ArrayRef<int64_t>{static_cast<int64_t>(0)})
                       .getResult();
      auto envPtr = mlir::LLVM::ExtractValueOp::create(
                        rewriter, loc, convArg, llvm::ArrayRef<int64_t>{static_cast<int64_t>(1)})
                        .getResult();
      auto ft = rewriter.getFunctionType({ptrType}, {ptrType});
      getOrInsertFuncDecl(module, rewriter, "hew_rc_clone", ft);
      auto clonedEnv = mlir::func::CallOp::create(
          rewriter, loc, "hew_rc_clone", mlir::TypeRange{ptrType}, mlir::ValueRange{envPtr});
      mlir::Value rebuilt = mlir::LLVM::UndefOp::create(rewriter, loc, closureType);
      rebuilt = mlir::LLVM::InsertValueOp::create(rewriter, loc, rebuilt, fnPtr,
                                                  llvm::ArrayRef<int64_t>{static_cast<int64_t>(0)});
      rebuilt = mlir::LLVM::InsertValueOp::create(rewriter, loc, rebuilt, clonedEnv.getResult(0),
                                                  llvm::ArrayRef<int64_t>{static_cast<int64_t>(1)});
      result.push_back(rebuilt);
    } else if (mlir::isa<hew::HandleType>(origType)) {
      // Handle types are opaque pointers to Rust-owned state (WebSocket
      // connections, HTTP servers, etc.). They cannot be cloned — ownership
      // must transfer to the actor. We pass the pointer as-is and null out
      // the original to prevent the spawning scope's Drop from freeing it.
      result.push_back(convArg);
      // Null out the source: find the alloca/variable backing this value
      // and store null into it so the destructor becomes a no-op.
      if (auto loadOp = convArg.getDefiningOp<mlir::LLVM::LoadOp>()) {
        auto nullVal = mlir::LLVM::ZeroOp::create(rewriter, loc, convArg.getType());
        mlir::LLVM::StoreOp::create(rewriter, loc, nullVal, loadOp.getAddr());
      }
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
    auto nullPtr = mlir::LLVM::ZeroOp::create(rewriter, loc, ptrType);
    auto zero = mlir::arith::ConstantIntOp::create(rewriter, loc, sizeType, 0);
    return {nullPtr, zero};
  }

  if (args.size() == 1) {
    auto argType = args[0].getType();
    auto alloca = emitEntryAlloca(rewriter, loc, argType);
    mlir::LLVM::StoreOp::create(rewriter, loc, args[0], alloca);
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
    auto fieldPtr =
        mlir::LLVM::GEPOp::create(rewriter, loc, ptrType, packType, alloca,
                                  llvm::ArrayRef<mlir::LLVM::GEPArg>{0, static_cast<int32_t>(idx)});
    mlir::LLVM::StoreOp::create(rewriter, loc, val, fieldPtr);
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
      auto tagVal = mlir::arith::ConstantIntOp::create(rewriter, loc, resultType, variantIdx);
      rewriter.replaceOp(op, tagVal.getResult());
      return mlir::success();
    }

    // Struct-based enum: undef + insertvalue(tag at 0) + insertvalue(payloads)
    mlir::Value result = mlir::LLVM::UndefOp::create(rewriter, loc, resultType);
    auto i32Type = rewriter.getI32Type();
    auto tagVal = mlir::arith::ConstantIntOp::create(rewriter, loc, i32Type, variantIdx);
    result = mlir::LLVM::InsertValueOp::create(rewriter, loc, result, tagVal,
                                               llvm::ArrayRef<int64_t>{0});

    auto payloads = adaptor.getPayloads();
    auto positions = op.getPayloadPositions();
    for (auto [i, payload] : llvm::enumerate(payloads)) {
      int64_t pos;
      if (positions) {
        pos = mlir::cast<mlir::IntegerAttr>((*positions)[i]).getInt();
      } else {
        pos = static_cast<int64_t>(i) + 1;
      }
      result = mlir::LLVM::InsertValueOp::create(rewriter, loc, result, payload,
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
          mlir::LLVM::ExtractValueOp::create(rewriter, loc, enumVal, llvm::ArrayRef<int64_t>{0});
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
    auto val = mlir::LLVM::ExtractValueOp::create(rewriter, loc, adaptor.getEnumVal(),
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
      auto fieldPtr = mlir::LLVM::GEPOp::create(
          rewriter, loc, ptrType, stateType, stateAlloca,
          llvm::ArrayRef<mlir::LLVM::GEPArg>{0, static_cast<int32_t>(idx)});
      mlir::LLVM::StoreOp::create(rewriter, loc, argVal, fieldPtr);
    }

    // 2b. Call ActorName_init(state) if it exists in the module
    {
      std::string initName = op.getActorName().str() + "_init";
      if (module.lookupSymbol<mlir::func::FuncOp>(initName)) {
        auto initFuncType = rewriter.getFunctionType({ptrType}, {});
        getOrInsertFuncDecl(module, rewriter, initName, initFuncType);
        mlir::func::CallOp::create(rewriter, loc, initName, mlir::TypeRange{},
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
    auto funcRef = mlir::func::ConstantOp::create(
        rewriter, loc, dispatchFuncType,
        mlir::SymbolRefAttr::get(rewriter.getContext(), dispatchName));
    auto dispatchPtr =
        mlir::UnrealizedConversionCastOp::create(rewriter, loc, ptrType, funcRef.getResult())
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
        auto gep = mlir::LLVM::GEPOp::create(
            rewriter, loc, ptrType, optsStructType, optsAlloca,
            llvm::ArrayRef<mlir::LLVM::GEPArg>{0, static_cast<int32_t>(idx)});
        mlir::LLVM::StoreOp::create(rewriter, loc, val, gep);
      };

      // init_state (ptr)
      storeField(0, stateAlloca);
      // state_size (usize/i64)
      storeField(1, stateSize);
      // dispatch (fn ptr)
      storeField(2, dispatchPtr);
      // mailbox_capacity (i32)
      auto capVal = mlir::arith::ConstantIntOp::create(
          rewriter, loc, i32Type,
          op.getMailboxCapacity().has_value()
              ? static_cast<int64_t>(op.getMailboxCapacity().value())
              : -1LL);
      storeField(3, capVal);
      // overflow policy (i32) — map from spec encoding to runtime enum
      // Runtime: 0=DropNew, 1=DropOld, 2=Block, 3=Fail, 4=Coalesce
      int32_t runtimeOverflow = 4; // Coalesce (we only get here for coalesce)
      auto overflowVal =
          mlir::arith::ConstantIntOp::create(rewriter, loc, i32Type, runtimeOverflow);
      storeField(4, overflowVal);
      // coalesce_key_fn (fn ptr)
      auto keyFnName = op.getCoalesceKeyFn().value();
      auto keyFnFuncType = rewriter.getFunctionType({i32Type, ptrType, sizeType}, {sizeType});
      getOrInsertFuncDecl(module, rewriter, keyFnName, keyFnFuncType);
      auto keyFuncRef = mlir::func::ConstantOp::create(
          rewriter, loc, keyFnFuncType, mlir::SymbolRefAttr::get(rewriter.getContext(), keyFnName));
      auto keyFnPtr =
          mlir::UnrealizedConversionCastOp::create(rewriter, loc, ptrType, keyFuncRef.getResult())
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
      auto fallbackVal =
          mlir::arith::ConstantIntOp::create(rewriter, loc, i32Type, runtimeFallback);
      storeField(6, fallbackVal);
      // budget (i32) — 0 = default
      auto budgetVal = mlir::arith::ConstantIntOp::create(rewriter, loc, i32Type, 0);
      storeField(7, budgetVal);

      auto spawnOptsFuncType = rewriter.getFunctionType({ptrType}, {ptrType});
      getOrInsertFuncDecl(module, rewriter, "hew_actor_spawn_opts", spawnOptsFuncType);
      auto call =
          mlir::func::CallOp::create(rewriter, loc, "hew_actor_spawn_opts",
                                     mlir::TypeRange{ptrType}, mlir::ValueRange{optsAlloca});
      result = call.getResult(0);
    } else if (op.getMailboxCapacity().has_value()) {
      auto spawnFuncType =
          rewriter.getFunctionType({ptrType, sizeType, ptrType, i32Type}, {ptrType});
      getOrInsertFuncDecl(module, rewriter, "hew_actor_spawn_bounded", spawnFuncType);
      auto capVal = mlir::arith::ConstantIntOp::create(
          rewriter, loc, i32Type, static_cast<int64_t>(op.getMailboxCapacity().value()));
      llvm::SmallVector<mlir::Value, 4> spawnArgs = {stateAlloca, stateSize, dispatchPtr, capVal};
      auto call = mlir::func::CallOp::create(rewriter, loc, "hew_actor_spawn_bounded",
                                             mlir::TypeRange{ptrType}, spawnArgs);
      result = call.getResult(0);
    } else {
      auto spawnFuncType = rewriter.getFunctionType({ptrType, sizeType, ptrType}, {ptrType});
      getOrInsertFuncDecl(module, rewriter, "hew_actor_spawn", spawnFuncType);
      llvm::SmallVector<mlir::Value, 3> spawnArgs = {stateAlloca, stateSize, dispatchPtr};
      auto call = mlir::func::CallOp::create(rewriter, loc, "hew_actor_spawn",
                                             mlir::TypeRange{ptrType}, spawnArgs);
      result = call.getResult(0);
    }

    // Register terminate function if the actor has one.
    {
      std::string terminateName = op.getActorName().str() + "_terminate";
      if (module.lookupSymbol<mlir::func::FuncOp>(terminateName)) {
        auto terminateFuncType = rewriter.getFunctionType({ptrType}, {});
        getOrInsertFuncDecl(module, rewriter, terminateName, terminateFuncType);
        auto terminateFuncRef = mlir::func::ConstantOp::create(
            rewriter, loc, terminateFuncType,
            mlir::SymbolRefAttr::get(rewriter.getContext(), terminateName));
        auto terminatePtr =
            mlir::UnrealizedConversionCastOp::create(rewriter, loc, ptrType,
                                                     terminateFuncRef.getResult())
                .getResult(0);

        auto setTerminateFuncType = rewriter.getFunctionType({ptrType, ptrType}, {});
        getOrInsertFuncDecl(module, rewriter, "hew_actor_set_terminate", setTerminateFuncType);
        mlir::func::CallOp::create(rewriter, loc, "hew_actor_set_terminate", mlir::TypeRange{},
                                   mlir::ValueRange{result, terminatePtr});
      }
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
    auto i64Type = rewriter.getI64Type();
    auto sizeType = getSizeType(ctx, module);

    auto targetVal = adaptor.getTarget();
    auto msgTypeVal = mlir::arith::ConstantIntOp::create(rewriter, loc, i32Type,
                                                         static_cast<int64_t>(op.getMsgType()));

    // Deep-copy owned values (strings, vecs) so the receiver gets
    // independent copies that survive the sender's scope-exit drops.
    auto clonedArgs = deepCopyOwnedArgs(rewriter, loc, module, op.getArgs(), adaptor.getArgs());
    auto [dataPtr, dataSize] = emitPackArgs(rewriter, loc, clonedArgs);

    // Remote dispatch: if the target is an i64 PID (from Node::lookup),
    // route through hew_actor_send_by_id which handles both local and
    // remote delivery transparently.
    if (targetVal.getType() == i64Type) {
      auto sendFuncType =
          rewriter.getFunctionType({i64Type, i32Type, ptrType, sizeType}, {i32Type});
      getOrInsertFuncDecl(module, rewriter, "hew_actor_send_by_id", sendFuncType);
      mlir::func::CallOp::create(rewriter, loc, "hew_actor_send_by_id", mlir::TypeRange{i32Type},
                                 mlir::ValueRange{targetVal, msgTypeVal, dataPtr, dataSize});
    } else {
      // Local dispatch: direct pointer to actor struct.
      auto sendFuncType = rewriter.getFunctionType({ptrType, i32Type, ptrType, sizeType}, {});
      getOrInsertFuncDecl(module, rewriter, "hew_actor_send", sendFuncType);
      mlir::func::CallOp::create(rewriter, loc, "hew_actor_send", mlir::TypeRange{},
                                 mlir::ValueRange{targetVal, msgTypeVal, dataPtr, dataSize});
    }

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
    auto i64Type = rewriter.getI64Type();
    auto msgTypeVal = mlir::arith::ConstantIntOp::create(rewriter, loc, i32Type,
                                                         static_cast<int64_t>(op.getMsgType()));

    // Deep-copy owned values (strings, vecs) for the same reason as actor_send.
    auto clonedArgs = deepCopyOwnedArgs(rewriter, loc, module, op.getArgs(), adaptor.getArgs());
    auto [dataPtr, dataSize] = emitPackArgs(rewriter, loc, clonedArgs);

    // Remote actors (i64 PID from Node::lookup): distributed ask via
    // hew_node_api_ask, which sends the message with a request_id over
    // the mesh and blocks until the reply arrives.
    if (targetVal.getType() == i64Type) {
      auto resultType = op.getResult().getType();
      auto zeroReplySize = mlir::arith::ConstantIntOp::create(rewriter, loc, sizeType, 0);
      auto loadType = resultType;
      auto isDirectReplyLoadType = [&](mlir::Type type) {
        return llvm::isa<mlir::LLVM::LLVMPointerType, mlir::IntegerType, mlir::FloatType,
                         mlir::LLVM::LLVMStructType>(type);
      };
      mlir::Value replySize = zeroReplySize;
      if (!llvm::isa<mlir::NoneType>(resultType)) {
        loadType = getTypeConverter()->convertType(resultType);
        if (!loadType)
          loadType = resultType;
        auto replyLoadType = isDirectReplyLoadType(loadType) ? loadType : ptrType;
        replySize = emitSizeOf(rewriter, loc, replyLoadType);
      }

      auto askFuncType =
          rewriter.getFunctionType({i64Type, i32Type, ptrType, sizeType, sizeType}, {ptrType});
      getOrInsertFuncDecl(module, rewriter, "hew_node_api_ask", askFuncType);
      auto call = mlir::func::CallOp::create(rewriter, loc, "hew_node_api_ask",
                                             mlir::TypeRange{ptrType},
                                             mlir::ValueRange{targetVal, msgTypeVal, dataPtr,
                                                              dataSize, replySize});
      auto replyPtr = call.getResult(0);

      // Void-return handler: free the reply buffer and erase the op.
      // free(null) is a safe no-op if the ask failed.
      if (llvm::isa<mlir::NoneType>(resultType)) {
        auto freeFuncType = rewriter.getFunctionType({ptrType}, {});
        getOrInsertFuncDecl(module, rewriter, "free", freeFuncType);
        mlir::func::CallOp::create(rewriter, loc, "free", mlir::TypeRange{},
                                   mlir::ValueRange{replyPtr});
        rewriter.eraseOp(op);
        return mlir::success();
      }

      // Convert Hew dialect types (e.g., !hew.result, !hew.option) to their
      // LLVM representation so the load uses a concrete LLVM type.
      // Non-void: load the result from the reply pointer.
      // hew_node_api_ask guarantees a non-null return for non-void asks
      // (returns a zeroed allocation on timeout/failure).
      mlir::Value resultVal;
      if (llvm::isa<mlir::LLVM::LLVMPointerType>(loadType)) {
        auto loaded = mlir::LLVM::LoadOp::create(rewriter, loc, ptrType, replyPtr);
        resultVal = loaded.getResult();
      } else if (isDirectReplyLoadType(loadType)) {
        auto loaded = mlir::LLVM::LoadOp::create(rewriter, loc, loadType, replyPtr);
        resultVal = loaded.getResult();
      } else {
        auto loaded = mlir::LLVM::LoadOp::create(rewriter, loc, ptrType, replyPtr);
        resultVal = mlir::UnrealizedConversionCastOp::create(rewriter, loc, loadType,
                                                             mlir::ValueRange{loaded.getResult()})
                        .getResult(0);
      }

      // Free the malloc'd reply buffer.
      auto freeFuncType = rewriter.getFunctionType({ptrType}, {});
      getOrInsertFuncDecl(module, rewriter, "free", freeFuncType);
      mlir::func::CallOp::create(rewriter, loc, "free", mlir::TypeRange{},
                                 mlir::ValueRange{replyPtr});

      rewriter.replaceOp(op, resultVal);
      return mlir::success();
    }

    // Local actor: blocking ask via hew_actor_ask (returns void*)
    auto askFuncType = rewriter.getFunctionType({ptrType, i32Type, ptrType, sizeType}, {ptrType});
    getOrInsertFuncDecl(module, rewriter, "hew_actor_ask", askFuncType);
    auto call =
        mlir::func::CallOp::create(rewriter, loc, "hew_actor_ask", mlir::TypeRange{ptrType},
                                   mlir::ValueRange{targetVal, msgTypeVal, dataPtr, dataSize});
    auto replyPtr = call.getResult(0);

    // Load the result from the reply pointer — convert Hew dialect types
    // (e.g., !hew.result, !hew.option) to LLVM before loading.
    auto resultType = op.getResult().getType();

    // Void-return handler: hew_actor_ask returns null (hew_reply was called with null/0).
    // free(null) is a safe no-op. Erase the op — no result value to produce.
    if (llvm::isa<mlir::NoneType>(resultType)) {
      auto freeFuncType = rewriter.getFunctionType({ptrType}, {});
      getOrInsertFuncDecl(module, rewriter, "free", freeFuncType);
      mlir::func::CallOp::create(rewriter, loc, "free", mlir::TypeRange{},
                                 mlir::ValueRange{replyPtr});
      rewriter.eraseOp(op);
      return mlir::success();
    }

    auto loadType = getTypeConverter()->convertType(resultType);
    if (!loadType)
      loadType = resultType;

    mlir::Value resultVal;
    if (loadType == ptrType || llvm::isa<mlir::LLVM::LLVMPointerType>(loadType)) {
      auto loaded = mlir::LLVM::LoadOp::create(rewriter, loc, ptrType, replyPtr);
      resultVal = loaded.getResult();
    } else if (loadType == i32Type || loadType == rewriter.getI64Type() ||
               llvm::isa<mlir::IntegerType>(loadType) || llvm::isa<mlir::FloatType>(loadType) ||
               llvm::isa<mlir::LLVM::LLVMStructType>(loadType)) {
      auto loaded = mlir::LLVM::LoadOp::create(rewriter, loc, loadType, replyPtr);
      resultVal = loaded.getResult();
    } else {
      // Custom types (e.g., !hew.string_ref): load as ptr, then cast
      auto loaded = mlir::LLVM::LoadOp::create(rewriter, loc, ptrType, replyPtr);
      resultVal = mlir::UnrealizedConversionCastOp::create(rewriter, loc, loadType,
                                                           mlir::ValueRange{loaded.getResult()})
                      .getResult(0);
    }

    // Free the malloc'd reply buffer (allocated by hew_reply, returned by hew_actor_ask)
    auto freeFuncType = rewriter.getFunctionType({ptrType}, {});
    getOrInsertFuncDecl(module, rewriter, "free", freeFuncType);
    mlir::func::CallOp::create(rewriter, loc, "free", mlir::TypeRange{},
                               mlir::ValueRange{replyPtr});

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
    mlir::func::CallOp::create(rewriter, loc, "hew_actor_stop", mlir::TypeRange{},
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
    mlir::func::CallOp::create(rewriter, loc, "hew_actor_close", mlir::TypeRange{},
                               mlir::ValueRange{adaptor.getTarget()});

    rewriter.eraseOp(op);
    return mlir::success();
  }
};

/// Lower hew.actor_await -> func.call @hew_actor_await
struct ActorAwaitOpLowering : public mlir::OpConversionPattern<hew::ActorAwaitOp> {
  using OpConversionPattern<hew::ActorAwaitOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::ActorAwaitOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
    auto i32Type = rewriter.getI32Type();

    auto awaitFuncType = rewriter.getFunctionType({ptrType}, {i32Type});
    getOrInsertFuncDecl(module, rewriter, "hew_actor_await", awaitFuncType);
    auto call = rewriter.create<mlir::func::CallOp>(
        loc, "hew_actor_await", mlir::TypeRange{i32Type}, mlir::ValueRange{adaptor.getTarget()});

    rewriter.replaceOp(op, call.getResults());
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
    auto call = mlir::func::CallOp::create(rewriter, loc, "hew_actor_self",
                                           mlir::TypeRange{ptrType}, mlir::ValueRange{});
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
    mlir::func::CallOp::create(rewriter, loc, "hew_actor_link", mlir::TypeRange{},
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
    mlir::func::CallOp::create(rewriter, loc, "hew_actor_unlink", mlir::TypeRange{},
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
    auto call =
        mlir::func::CallOp::create(rewriter, loc, "hew_actor_monitor", mlir::TypeRange{i64Type},
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
    mlir::func::CallOp::create(rewriter, loc, "hew_actor_demonitor", mlir::TypeRange{},
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
    mlir::func::CallOp::create(rewriter, loc, "hew_actor_cooperate", mlir::TypeRange{},
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
      auto i64Type = rewriter.getI64Type();
      auto maxI32 = mlir::arith::ConstantIntOp::create(rewriter, loc, i64Type, (int64_t)INT32_MAX);
      auto cmp = mlir::arith::CmpIOp::create(rewriter, loc, mlir::arith::CmpIPredicate::sgt, msVal,
                                             maxI32);
      msVal = mlir::arith::SelectOp::create(rewriter, loc, cmp, maxI32, msVal);
      msVal = mlir::arith::TruncIOp::create(rewriter, loc, i32Type, msVal);
    }
    mlir::func::CallOp::create(rewriter, loc, "hew_sleep_ms", mlir::TypeRange{},
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
    mlir::func::CallOp::create(rewriter, loc, "hew_panic", mlir::TypeRange{}, mlir::ValueRange{});
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
      cond = mlir::arith::ExtUIOp::create(rewriter, loc, i64Type, cond);
    } else if (condType.isInteger(8) || condType.isInteger(16)) {
      cond = mlir::arith::ExtSIOp::create(rewriter, loc, i64Type, cond);
    } else if (condType.isInteger(32)) {
      cond = mlir::arith::ExtSIOp::create(rewriter, loc, i64Type, cond);
    }
    // else: already i64 (or compatible), pass through

    auto funcType = rewriter.getFunctionType({i64Type}, {});
    getOrInsertFuncDecl(module, rewriter, "hew_assert", funcType);
    mlir::func::CallOp::create(rewriter, loc, "hew_assert", mlir::TypeRange{},
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
      left = mlir::arith::ExtUIOp::create(rewriter, loc, rewriter.getI32Type(), left);
      right = mlir::arith::ExtUIOp::create(rewriter, loc, rewriter.getI32Type(), right);
      funcType = rewriter.getFunctionType({rewriter.getI32Type(), rewriter.getI32Type()}, {});
    } else if (mlir::isa<mlir::LLVM::LLVMPointerType>(leftType)) {
      // Covers both !hew.string_ref (already converted to !llvm.ptr)
      // and raw !llvm.ptr
      funcName = "hew_assert_eq_str";
      auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
      funcType = rewriter.getFunctionType({ptrType, ptrType}, {});
    } else if (leftType.isInteger(8) || leftType.isInteger(16)) {
      left = mlir::arith::ExtSIOp::create(rewriter, loc, rewriter.getI64Type(), left);
      right = mlir::arith::ExtSIOp::create(rewriter, loc, rewriter.getI64Type(), right);
      funcType = rewriter.getFunctionType({rewriter.getI64Type(), rewriter.getI64Type()}, {});
    } else if (leftType.isF32()) {
      funcName = "hew_assert_eq_f64";
      left = mlir::arith::ExtFOp::create(rewriter, loc, rewriter.getF64Type(), left);
      right = mlir::arith::ExtFOp::create(rewriter, loc, rewriter.getF64Type(), right);
      funcType = rewriter.getFunctionType({rewriter.getF64Type(), rewriter.getF64Type()}, {});
    } else if (leftType.isInteger(32)) {
      // Widen i32 to i64
      left = mlir::arith::ExtSIOp::create(rewriter, loc, rewriter.getI64Type(), left);
      right = mlir::arith::ExtSIOp::create(rewriter, loc, rewriter.getI64Type(), right);
      funcType = rewriter.getFunctionType({rewriter.getI64Type(), rewriter.getI64Type()}, {});
    } else if (leftType.isInteger(64) || mlir::isa<mlir::IndexType>(leftType)) {
      // i64 or index: pass through directly
      funcType = rewriter.getFunctionType({rewriter.getI64Type(), rewriter.getI64Type()}, {});
    } else {
      return op->emitError("unsupported type for assert_eq");
    }

    getOrInsertFuncDecl(module, rewriter, funcName, funcType);
    mlir::func::CallOp::create(rewriter, loc, funcName, mlir::TypeRange{},
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
      left = mlir::arith::ExtUIOp::create(rewriter, loc, rewriter.getI32Type(), left);
      right = mlir::arith::ExtUIOp::create(rewriter, loc, rewriter.getI32Type(), right);
      funcType = rewriter.getFunctionType({rewriter.getI32Type(), rewriter.getI32Type()}, {});
    } else if (mlir::isa<mlir::LLVM::LLVMPointerType>(leftType)) {
      funcName = "hew_assert_ne_str";
      auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());
      funcType = rewriter.getFunctionType({ptrType, ptrType}, {});
    } else if (leftType.isInteger(8) || leftType.isInteger(16)) {
      left = mlir::arith::ExtSIOp::create(rewriter, loc, rewriter.getI64Type(), left);
      right = mlir::arith::ExtSIOp::create(rewriter, loc, rewriter.getI64Type(), right);
      funcType = rewriter.getFunctionType({rewriter.getI64Type(), rewriter.getI64Type()}, {});
    } else if (leftType.isF32()) {
      funcName = "hew_assert_ne_f64";
      left = mlir::arith::ExtFOp::create(rewriter, loc, rewriter.getF64Type(), left);
      right = mlir::arith::ExtFOp::create(rewriter, loc, rewriter.getF64Type(), right);
      funcType = rewriter.getFunctionType({rewriter.getF64Type(), rewriter.getF64Type()}, {});
    } else if (leftType.isInteger(32)) {
      left = mlir::arith::ExtSIOp::create(rewriter, loc, rewriter.getI64Type(), left);
      right = mlir::arith::ExtSIOp::create(rewriter, loc, rewriter.getI64Type(), right);
      funcType = rewriter.getFunctionType({rewriter.getI64Type(), rewriter.getI64Type()}, {});
    } else if (leftType.isInteger(64) || mlir::isa<mlir::IndexType>(leftType)) {
      funcType = rewriter.getFunctionType({rewriter.getI64Type(), rewriter.getI64Type()}, {});
    } else {
      return op->emitError("unsupported type for assert_ne");
    }

    getOrInsertFuncDecl(module, rewriter, funcName, funcType);
    mlir::func::CallOp::create(rewriter, loc, funcName, mlir::TypeRange{},
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
    auto call = mlir::func::CallOp::create(
        rewriter, loc, "hew_supervisor_new", mlir::TypeRange{ptrType},
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
        mlir::func::CallOp::create(rewriter, loc, "hew_supervisor_start", mlir::TypeRange{i32Type},
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
    mlir::func::CallOp::create(rewriter, loc, "hew_supervisor_stop", mlir::TypeRange{},
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
    auto call = mlir::func::CallOp::create(
        rewriter, loc, "hew_supervisor_add_child_spec", mlir::TypeRange{i32Type},
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
      auto gep = mlir::LLVM::GEPOp::create(rewriter, loc, ptrType, structType, alloca,
                                           llvm::ArrayRef<mlir::LLVM::GEPArg>{0, i});
      mlir::LLVM::StoreOp::create(rewriter, loc, fields[i], gep);
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
    auto call = mlir::func::CallOp::create(
        rewriter, loc, "hew_supervisor_add_child_supervisor_with_init", mlir::TypeRange{i32Type},
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
          mlir::arith::ConstantIntOp::create(rewriter, loc, i32Type, static_cast<int64_t>(idx));
      auto cond = mlir::arith::CmpIOp::create(rewriter, loc, mlir::arith::CmpIPredicate::eq,
                                              msgTypeVal, msgIdx);

      auto ifOp = mlir::scf::IfOp::create(rewriter, loc, cond, /*withElseRegion=*/false);
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
          auto loaded = mlir::LLVM::LoadOp::create(rewriter, loc, paramType, dataVal);
          callArgs.push_back(loaded);
        } else {
          // Multiple params: data points to a packed struct
          llvm::SmallVector<mlir::Type, 4> fieldTypes;
          for (unsigned pi = 1; pi < numHandlerArgs; ++pi) {
            fieldTypes.push_back(handlerType.getInput(pi));
          }
          auto packType = mlir::LLVM::LLVMStructType::getLiteral(rewriter.getContext(), fieldTypes);

          auto packed = mlir::LLVM::LoadOp::create(rewriter, loc, packType, dataVal);
          for (unsigned pi = 0; pi < fieldTypes.size(); ++pi) {
            auto field = mlir::LLVM::ExtractValueOp::create(
                rewriter, loc, packed, llvm::ArrayRef<int64_t>{static_cast<int64_t>(pi)});
            callArgs.push_back(field);
          }
        }
      }

      // Ensure handler function is declared
      getOrInsertFuncDecl(module, rewriter, handlerName, handlerType);

      // Get the reply channel from the scheduler's thread-local (set from
      // HewMsgNode.reply_channel before dispatch). This replaces the old
      // convention of embedding the reply channel pointer in the data buffer.
      auto getReplyFuncType = rewriter.getFunctionType({}, {ptrType});
      getOrInsertFuncDecl(module, rewriter, "hew_get_reply_channel", getReplyFuncType);

      bool hasReturnType = handlerType.getNumResults() > 0;
      if (hasReturnType) {
        // Call handler and capture return value
        auto callOp = mlir::func::CallOp::create(rewriter, loc, handlerName,
                                                 handlerType.getResults(), callArgs);
        auto resultVal = callOp.getResult(0);
        auto resultType = resultVal.getType();

        // Check if a reply channel is present (non-null).
        auto replyChan = mlir::func::CallOp::create(rewriter, loc, "hew_get_reply_channel",
                                                     mlir::TypeRange{ptrType}, mlir::ValueRange{})
                             .getResult(0);
        auto nullPtr = mlir::LLVM::ZeroOp::create(rewriter, loc, ptrType);
        auto hasReply = mlir::LLVM::ICmpOp::create(rewriter, loc, mlir::LLVM::ICmpPredicate::ne,
                                                    replyChan, nullPtr);

        auto replyIfOp = mlir::scf::IfOp::create(rewriter, loc, hasReply, /*withElseRegion=*/false);
        rewriter.setInsertionPointToStart(&replyIfOp.getThenRegion().front());

        // Store result value to a temp alloca so we can pass its address
        auto one = mlir::arith::ConstantIntOp::create(rewriter, loc, sizeType, 1);
        auto resultAlloca = mlir::LLVM::AllocaOp::create(rewriter, loc, ptrType, resultType, one);
        mlir::LLVM::StoreOp::create(rewriter, loc, resultVal, resultAlloca);

        // Call hew_reply(ch, &result, sizeof(result))
        auto resultSize = emitSizeOf(rewriter, loc, resultType);
        auto replyFuncType = rewriter.getFunctionType({ptrType, ptrType, sizeType}, {});
        getOrInsertFuncDecl(module, rewriter, "hew_reply", replyFuncType);
        mlir::func::CallOp::create(rewriter, loc, "hew_reply", mlir::TypeRange{},
                                   mlir::ValueRange{replyChan, resultAlloca, resultSize});

        rewriter.setInsertionPointAfter(replyIfOp);
      } else {
        // Void handler — call the handler (no return value)
        mlir::func::CallOp::create(rewriter, loc, handlerName, mlir::TypeRange{}, callArgs);

        // If a reply channel was set (caller used `await c.handler()`),
        // signal completion with an empty reply so the caller is unblocked.
        auto replyChan = mlir::func::CallOp::create(rewriter, loc, "hew_get_reply_channel",
                                                     mlir::TypeRange{ptrType}, mlir::ValueRange{})
                             .getResult(0);
        auto nullPtr = mlir::LLVM::ZeroOp::create(rewriter, loc, ptrType);
        auto hasReply = mlir::LLVM::ICmpOp::create(rewriter, loc, mlir::LLVM::ICmpPredicate::ne,
                                                    replyChan, nullPtr);
        auto replyIfOp = mlir::scf::IfOp::create(rewriter, loc, hasReply, /*withElseRegion=*/false);
        rewriter.setInsertionPointToStart(&replyIfOp.getThenRegion().front());

        // Call hew_reply(ch, null, 0) — signal completion with no value
        auto replyFuncType = rewriter.getFunctionType({ptrType, ptrType, sizeType}, {});
        getOrInsertFuncDecl(module, rewriter, "hew_reply", replyFuncType);
        auto voidNullPtr = mlir::LLVM::ZeroOp::create(rewriter, loc, ptrType);
        auto voidZeroSize = mlir::arith::ConstantIntOp::create(rewriter, loc, sizeType, 0);
        mlir::func::CallOp::create(rewriter, loc, "hew_reply", mlir::TypeRange{},
                                   mlir::ValueRange{replyChan, voidNullPtr, voidZeroSize});

        rewriter.setInsertionPointAfter(replyIfOp);
      }

      rewriter.setInsertionPointAfter(ifOp);
    }

    rewriter.eraseOp(op);
    return mlir::success();
  }
};

// ── Vec collection op lowerings ────────────────────────────────────────────

/// Helper: map an MLIR element type to the runtime function suffix.
/// Returns the explicit suffix for every type — callers that need the
/// unsuffixed variant (e.g. hew_vec_new for i32) must fixup afterwards.
static std::string vecElemSuffix(mlir::Type elemType) {
  if (elemType.isInteger(64))
    return "_i64";
  if (elemType.isF64())
    return "_f64";
  if (mlir::isa<hew::StringRefType>(elemType))
    return "_str";
  if (mlir::isa<mlir::LLVM::LLVMPointerType>(elemType) || mlir::isa<hew::ActorRefType>(elemType) ||
      mlir::isa<hew::TypedActorRefType>(elemType) || mlir::isa<hew::HandleType>(elemType) ||
      mlir::isa<hew::VecType>(elemType) || mlir::isa<hew::HashMapType>(elemType))
    return "_ptr";
  if (mlir::isa<mlir::LLVM::LLVMStructType>(elemType))
    return "_generic";
  if (elemType.isF32())
    return "_f64"; // f32 promoted to f64 for Vec storage (runtime has no _f32 variant)
  if (elemType.isInteger(32))
    return "_i32";
  if (elemType.isInteger(1))
    return "_i32"; // bool (i1) stored as i32 in Vec
  if (elemType.isInteger(8))
    return "_i32"; // i8/u8/byte stored as i32 in Vec
  if (elemType.isInteger(16))
    return "_i32"; // i16/u16 stored as i32 in Vec
  llvm_unreachable("vecElemSuffix: unrecognized element type");
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

    // hew_vec_new (unsuffixed) is the i32 constructor
    if (suffix == "_i32")
      suffix = "";

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
          } else if (auto ft = mlir::dyn_cast<mlir::FloatType>(fieldTy);
                     ft && ft.getWidth() == 32) {
            fieldSize = 4;
            fieldAlign = 4;
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
      auto sizeVal = mlir::LLVM::ConstantOp::create(
          rewriter, loc, i64Type, rewriter.getI64IntegerAttr(static_cast<int64_t>(elemSize)));
      auto funcType = rewriter.getFunctionType({i64Type}, {ptrType});
      getOrInsertFuncDecl(mod, rewriter, "hew_vec_new_with_elem_size", funcType);
      auto call = mlir::func::CallOp::create(rewriter, loc, "hew_vec_new_with_elem_size",
                                             mlir::TypeRange{ptrType}, mlir::ValueRange{sizeVal});
      rewriter.replaceOp(op, call.getResults());
    } else {
      std::string funcName = "hew_vec_new" + suffix;
      auto funcType = rewriter.getFunctionType({}, {ptrType});
      getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, funcName, funcType);
      auto call = mlir::func::CallOp::create(rewriter, loc, funcName, mlir::TypeRange{ptrType},
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

    std::string suffix = vecElemSuffix(op.getValue().getType());
    if (suffix.empty())
      suffix = vecElemSuffix(valType);

    if (suffix == "_generic") {
      // For struct elements: alloca + store + pass pointer to push_generic
      auto one = mlir::LLVM::ConstantOp::create(rewriter, loc, rewriter.getI64Type(),
                                                rewriter.getI64IntegerAttr(1));
      auto alloca = mlir::LLVM::AllocaOp::create(rewriter, loc, ptrType, valType, one);
      mlir::LLVM::StoreOp::create(rewriter, loc, adaptor.getValue(), alloca);
      auto funcType = rewriter.getFunctionType({ptrType, ptrType}, {});
      getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, "hew_vec_push_generic",
                          funcType);
      mlir::func::CallOp::create(rewriter, loc, "hew_vec_push_generic", mlir::TypeRange{},
                                 mlir::ValueRange{adaptor.getVec(), alloca});
    } else if ((suffix == "_i64" || suffix == "_i32" || suffix == "_f64") &&
               isNative64(op->getParentOfType<mlir::ModuleOp>())) {
      auto i64Type = rewriter.getI64Type();
      auto vecPtr = adaptor.getVec();
      auto value = adaptor.getValue();

      // Promote f32 to f64 for Vec storage (runtime uses f64 slots)
      if (suffix == "_f64" && valType.isF32()) {
        value = mlir::arith::ExtFOp::create(rewriter, loc, rewriter.getF64Type(), value);
        valType = rewriter.getF64Type();
      }

      // Widen narrow int types (i1/i8/i16) to i32 for correct GEP stride
      if (suffix == "_i32" && valType != rewriter.getI32Type()) {
        if (valType.isInteger(1) || valType.isInteger(8))
          value = mlir::arith::ExtUIOp::create(rewriter, loc, rewriter.getI32Type(), value);
        else
          value = mlir::arith::ExtSIOp::create(rewriter, loc, rewriter.getI32Type(), value);
        valType = rewriter.getI32Type();
      }

      auto vecStructType = mlir::LLVM::LLVMStructType::getLiteral(
          op.getContext(), {ptrType, i64Type, i64Type, i64Type, rewriter.getI32Type()});

      // Load len (field 1) and cap (field 2)
      auto lenFieldPtr = mlir::LLVM::GEPOp::create(
          rewriter, loc, ptrType, vecStructType, vecPtr,
          llvm::ArrayRef<mlir::LLVM::GEPArg>{mlir::LLVM::GEPArg(0), mlir::LLVM::GEPArg(1)});
      auto len = mlir::LLVM::LoadOp::create(rewriter, loc, i64Type, lenFieldPtr);

      auto capFieldPtr = mlir::LLVM::GEPOp::create(
          rewriter, loc, ptrType, vecStructType, vecPtr,
          llvm::ArrayRef<mlir::LLVM::GEPArg>{mlir::LLVM::GEPArg(0), mlir::LLVM::GEPArg(2)});
      auto cap = mlir::LLVM::LoadOp::create(rewriter, loc, i64Type, capFieldPtr);

      // needs_grow = len >= cap
      auto needsGrow =
          mlir::arith::CmpIOp::create(rewriter, loc, mlir::arith::CmpIPredicate::uge, len, cap);

      // Declare the runtime push function for the slow path
      std::string funcName = "hew_vec_push" + suffix;
      auto funcType = rewriter.getFunctionType({ptrType, valType}, {});
      getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, funcName, funcType);

      mlir::scf::IfOp::create(
          rewriter, loc, needsGrow,
          // Then: slow path — call runtime to grow and push
          [&](mlir::OpBuilder &b, mlir::Location l) {
            mlir::func::CallOp::create(b, l, funcName, mlir::TypeRange{},
                                       mlir::ValueRange{vecPtr, value});
            mlir::scf::YieldOp::create(b, l);
          },
          // Else: fast path — store directly at data[len], bump len
          [&](mlir::OpBuilder &b, mlir::Location l) {
            auto dataFieldPtr = mlir::LLVM::GEPOp::create(
                b, l, ptrType, vecStructType, vecPtr,
                llvm::ArrayRef<mlir::LLVM::GEPArg>{mlir::LLVM::GEPArg(0), mlir::LLVM::GEPArg(0)});
            auto dataPtr = mlir::LLVM::LoadOp::create(b, l, ptrType, dataFieldPtr);

            auto elemPtr = mlir::LLVM::GEPOp::create(
                b, l, ptrType, valType, dataPtr,
                mlir::ArrayRef<mlir::LLVM::GEPArg>{mlir::LLVM::GEPArg(len)});
            mlir::LLVM::StoreOp::create(b, l, value, elemPtr);

            auto one = mlir::LLVM::ConstantOp::create(b, l, i64Type, b.getI64IntegerAttr(1));
            auto newLen = mlir::arith::AddIOp::create(b, l, len, one);
            mlir::LLVM::StoreOp::create(b, l, newLen, lenFieldPtr);
            mlir::scf::YieldOp::create(b, l);
          });
    } else {
      std::string funcName = "hew_vec_push" + suffix;
      auto pushVal = adaptor.getValue();
      auto pushType = valType;
      // Promote f32→f64 or widen narrow ints for runtime call
      if (suffix == "_f64" && valType.isF32()) {
        pushVal = mlir::arith::ExtFOp::create(rewriter, loc, rewriter.getF64Type(), pushVal);
        pushType = rewriter.getF64Type();
      } else if (suffix == "_i32" && !valType.isInteger(32)) {
        if (valType.isInteger(1) || valType.isInteger(8))
          pushVal = mlir::arith::ExtUIOp::create(rewriter, loc, rewriter.getI32Type(), pushVal);
        else
          pushVal = mlir::arith::ExtSIOp::create(rewriter, loc, rewriter.getI32Type(), pushVal);
        pushType = rewriter.getI32Type();
      }
      auto funcType = rewriter.getFunctionType({ptrType, pushType}, {});
      getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, funcName, funcType);
      mlir::func::CallOp::create(rewriter, loc, funcName, mlir::TypeRange{},
                                 mlir::ValueRange{adaptor.getVec(), pushVal});
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

    std::string suffix = vecElemSuffix(op.getResult().getType());
    if (suffix.empty())
      suffix = vecElemSuffix(resultType);

    // Inline lowering for primitive types: load data/len from HewVec struct,
    // bounds-check, then GEP+load. Avoids runtime function call overhead.
    // HewVec layout (repr(C), 64-bit): { ptr data, i64 len, i64 cap, i64 elem_size, i32 elem_kind }
    if ((suffix == "_i64" || suffix == "_i32" || suffix == "_f64") &&
        isNative64(op->getParentOfType<mlir::ModuleOp>())) {
      auto i64Type = rewriter.getI64Type();
      auto vecPtr = adaptor.getVec();
      auto index = adaptor.getIndex();

      // Define HewVec struct type: { ptr, i64, i64, i64, i32 }
      auto vecStructType = mlir::LLVM::LLVMStructType::getLiteral(
          op.getContext(), {ptrType, i64Type, i64Type, i64Type, rewriter.getI32Type()});

      // Load len field (struct field index 1)
      auto lenFieldPtr = mlir::LLVM::GEPOp::create(
          rewriter, loc, ptrType, vecStructType, vecPtr,
          llvm::ArrayRef<mlir::LLVM::GEPArg>{mlir::LLVM::GEPArg(0), mlir::LLVM::GEPArg(1)});
      auto len = mlir::LLVM::LoadOp::create(rewriter, loc, i64Type, lenFieldPtr);

      // Bounds check: if index >= len, call abort
      auto oob =
          mlir::arith::CmpIOp::create(rewriter, loc, mlir::arith::CmpIPredicate::uge, index, len);

      // Declare the OOB abort function
      auto abortFuncType = rewriter.getFunctionType({i64Type, i64Type}, {});
      getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, "hew_vec_abort_oob",
                          abortFuncType);

      // Use scf.if for the bounds check (abort terminates; yield is for IR validity)
      mlir::scf::IfOp::create(
          rewriter, loc, oob,
          [&](mlir::OpBuilder &b, mlir::Location l) {
            mlir::func::CallOp::create(b, l, "hew_vec_abort_oob", mlir::TypeRange{},
                                       mlir::ValueRange{index, len});
            mlir::scf::YieldOp::create(b, l);
          },
          nullptr);

      // Load data pointer (struct field 0) and GEP to element
      auto dataFieldPtr = mlir::LLVM::GEPOp::create(
          rewriter, loc, ptrType, vecStructType, vecPtr,
          llvm::ArrayRef<mlir::LLVM::GEPArg>{mlir::LLVM::GEPArg(0), mlir::LLVM::GEPArg(0)});
      auto dataPtr = mlir::LLVM::LoadOp::create(rewriter, loc, ptrType, dataFieldPtr);

      // GEP to element and load (use f64 stride for f32, i32 stride for narrow int types)
      mlir::Type elemStorageType = resultType;
      if (suffix == "_f64" && resultType.isF32())
        elemStorageType = rewriter.getF64Type();
      else if (suffix == "_i32" && !resultType.isInteger(32))
        elemStorageType = rewriter.getI32Type();
      auto elemPtr =
          mlir::LLVM::GEPOp::create(rewriter, loc, ptrType, elemStorageType, dataPtr,
                                    mlir::ArrayRef<mlir::LLVM::GEPArg>{mlir::LLVM::GEPArg(index)});
      auto loaded = mlir::LLVM::LoadOp::create(rewriter, loc, elemStorageType, elemPtr);
      mlir::Value result = loaded.getResult();
      if (suffix == "_f64" && resultType.isF32())
        result = mlir::arith::TruncFOp::create(rewriter, loc, resultType, result);
      else if (elemStorageType != resultType)
        result = mlir::arith::TruncIOp::create(rewriter, loc, resultType, result);
      rewriter.replaceOp(op, result);
    } else if (suffix == "_generic") {
      // For struct elements: get returns a pointer, then load the struct
      auto idxType = adaptor.getIndex().getType();
      auto funcType = rewriter.getFunctionType({ptrType, idxType}, {ptrType});
      getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, "hew_vec_get_generic",
                          funcType);
      auto call =
          mlir::func::CallOp::create(rewriter, loc, "hew_vec_get_generic", mlir::TypeRange{ptrType},
                                     mlir::ValueRange{adaptor.getVec(), adaptor.getIndex()});
      auto loaded = mlir::LLVM::LoadOp::create(rewriter, loc, resultType, call.getResult(0));
      rewriter.replaceOp(op, loaded.getResult());
    } else {
      std::string funcName = "hew_vec_get" + suffix;
      auto idxType = adaptor.getIndex().getType();
      mlir::Type callResultType = resultType;
      if (suffix == "_f64" && resultType.isF32())
        callResultType = rewriter.getF64Type();
      if (suffix == "_i32" && !resultType.isInteger(32))
        callResultType = rewriter.getI32Type();
      auto funcType = rewriter.getFunctionType({ptrType, idxType}, {callResultType});
      getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, funcName, funcType);
      auto call =
          mlir::func::CallOp::create(rewriter, loc, funcName, mlir::TypeRange{callResultType},
                                     mlir::ValueRange{adaptor.getVec(), adaptor.getIndex()});
      mlir::Value getResult = call.getResult(0);
      if (callResultType != resultType) {
        if (callResultType.isF64() && resultType.isF32())
          getResult = mlir::arith::TruncFOp::create(rewriter, loc, resultType, getResult);
        else
          getResult = mlir::arith::TruncIOp::create(rewriter, loc, resultType, getResult);
      }
      rewriter.replaceOp(op, getResult);
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

    std::string suffix = vecElemSuffix(op.getValue().getType());
    if (suffix.empty())
      suffix = vecElemSuffix(valType);

    // Inline lowering for primitive types: bounds-check then GEP+store.
    if ((suffix == "_i64" || suffix == "_i32" || suffix == "_f64") &&
        isNative64(op->getParentOfType<mlir::ModuleOp>())) {
      auto i64Type = rewriter.getI64Type();
      auto vecPtr = adaptor.getVec();
      auto index = adaptor.getIndex();
      auto value = adaptor.getValue();

      // Promote f32 to f64 for Vec storage (runtime uses f64 slots)
      if (suffix == "_f64" && valType.isF32()) {
        value = mlir::arith::ExtFOp::create(rewriter, loc, rewriter.getF64Type(), value);
        valType = rewriter.getF64Type();
      }

      // Widen narrow int types (i1/i8/i16) to i32 for correct GEP stride
      if (suffix == "_i32" && valType != rewriter.getI32Type()) {
        if (valType.isInteger(1) || valType.isInteger(8))
          value = mlir::arith::ExtUIOp::create(rewriter, loc, rewriter.getI32Type(), value);
        else
          value = mlir::arith::ExtSIOp::create(rewriter, loc, rewriter.getI32Type(), value);
        valType = rewriter.getI32Type();
      }

      auto vecStructType = mlir::LLVM::LLVMStructType::getLiteral(
          op.getContext(), {ptrType, i64Type, i64Type, i64Type, rewriter.getI32Type()});

      // Load len field (struct field 1)
      auto lenFieldPtr = mlir::LLVM::GEPOp::create(
          rewriter, loc, ptrType, vecStructType, vecPtr,
          llvm::ArrayRef<mlir::LLVM::GEPArg>{mlir::LLVM::GEPArg(0), mlir::LLVM::GEPArg(1)});
      auto len = mlir::LLVM::LoadOp::create(rewriter, loc, i64Type, lenFieldPtr);

      // Bounds check using scf.if (stays in structured control flow)
      auto oob =
          mlir::arith::CmpIOp::create(rewriter, loc, mlir::arith::CmpIPredicate::uge, index, len);

      auto abortFuncType = rewriter.getFunctionType({i64Type, i64Type}, {});
      getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, "hew_vec_abort_oob",
                          abortFuncType);

      mlir::scf::IfOp::create(
          rewriter, loc, oob,
          [&](mlir::OpBuilder &b, mlir::Location l) {
            mlir::func::CallOp::create(b, l, "hew_vec_abort_oob", mlir::TypeRange{},
                                       mlir::ValueRange{index, len});
            mlir::scf::YieldOp::create(b, l);
          },
          nullptr);

      // Store value at element
      auto dataFieldPtr = mlir::LLVM::GEPOp::create(
          rewriter, loc, ptrType, vecStructType, vecPtr,
          llvm::ArrayRef<mlir::LLVM::GEPArg>{mlir::LLVM::GEPArg(0), mlir::LLVM::GEPArg(0)});
      auto dataPtr = mlir::LLVM::LoadOp::create(rewriter, loc, ptrType, dataFieldPtr);

      auto elemPtr =
          mlir::LLVM::GEPOp::create(rewriter, loc, ptrType, valType, dataPtr,
                                    mlir::ArrayRef<mlir::LLVM::GEPArg>{mlir::LLVM::GEPArg(index)});
      mlir::LLVM::StoreOp::create(rewriter, loc, value, elemPtr);
    } else if (suffix == "_generic") {
      // For struct elements: alloca + store + pass pointer to set_generic
      auto one = mlir::LLVM::ConstantOp::create(rewriter, loc, rewriter.getI64Type(),
                                                rewriter.getI64IntegerAttr(1));
      auto alloca = mlir::LLVM::AllocaOp::create(rewriter, loc, ptrType, valType, one);
      mlir::LLVM::StoreOp::create(rewriter, loc, adaptor.getValue(), alloca);
      auto idxType = adaptor.getIndex().getType();
      auto funcType = rewriter.getFunctionType({ptrType, idxType, ptrType}, {});
      getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, "hew_vec_set_generic",
                          funcType);
      mlir::func::CallOp::create(rewriter, loc, "hew_vec_set_generic", mlir::TypeRange{},
                                 mlir::ValueRange{adaptor.getVec(), adaptor.getIndex(), alloca});
    } else {
      std::string funcName = "hew_vec_set" + suffix;
      auto idxType = adaptor.getIndex().getType();
      auto setVal = adaptor.getValue();
      auto setType = valType;
      if (suffix == "_f64" && valType.isF32()) {
        setVal = mlir::arith::ExtFOp::create(rewriter, loc, rewriter.getF64Type(), setVal);
        setType = rewriter.getF64Type();
      } else if (suffix == "_i32" && !valType.isInteger(32)) {
        if (valType.isInteger(1) || valType.isInteger(8))
          setVal = mlir::arith::ExtUIOp::create(rewriter, loc, rewriter.getI32Type(), setVal);
        else
          setVal = mlir::arith::ExtSIOp::create(rewriter, loc, rewriter.getI32Type(), setVal);
        setType = rewriter.getI32Type();
      }
      auto funcType = rewriter.getFunctionType({ptrType, idxType, setType}, {});
      getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, funcName, funcType);
      mlir::func::CallOp::create(rewriter, loc, funcName, mlir::TypeRange{},
                                 mlir::ValueRange{adaptor.getVec(), adaptor.getIndex(), setVal});
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
    auto i64Type = rewriter.getI64Type();

    if (isNative64(op->getParentOfType<mlir::ModuleOp>())) {
      // Inline: load len directly from HewVec struct field 1
      // HewVec layout (repr(C), 64-bit): { ptr, i64, i64, i64, i32 }
      auto vecStructType = mlir::LLVM::LLVMStructType::getLiteral(
          op.getContext(), {ptrType, i64Type, i64Type, i64Type, rewriter.getI32Type()});
      auto lenFieldPtr = mlir::LLVM::GEPOp::create(
          rewriter, loc, ptrType, vecStructType, adaptor.getVec(),
          llvm::ArrayRef<mlir::LLVM::GEPArg>{mlir::LLVM::GEPArg(0), mlir::LLVM::GEPArg(1)});
      auto len = mlir::LLVM::LoadOp::create(rewriter, loc, i64Type, lenFieldPtr);
      rewriter.replaceOp(op, len.getResult());
    } else {
      // WASM: call runtime function (struct layout differs on 32-bit)
      auto funcType = rewriter.getFunctionType({ptrType}, {i64Type});
      getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, "hew_vec_len", funcType);
      auto call = mlir::func::CallOp::create(rewriter, loc, "hew_vec_len", mlir::TypeRange{i64Type},
                                             mlir::ValueRange{adaptor.getVec()});
      rewriter.replaceOp(op, call.getResults());
    }
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

    std::string suffix = vecElemSuffix(op.getResult().getType());
    if (suffix.empty())
      suffix = vecElemSuffix(resultType);

    std::string funcName = "hew_vec_pop" + suffix;
    mlir::Type callResultType = resultType;
    if (suffix == "_f64" && resultType.isF32())
      callResultType = rewriter.getF64Type();
    if (suffix == "_i32" && !resultType.isInteger(32))
      callResultType = rewriter.getI32Type();
    auto funcType = rewriter.getFunctionType({ptrType}, {callResultType});
    getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, funcName, funcType);
    auto call = mlir::func::CallOp::create(rewriter, loc, funcName, mlir::TypeRange{callResultType},
                                           mlir::ValueRange{adaptor.getVec()});
    mlir::Value popResult = call.getResult(0);
    if (callResultType != resultType) {
      if (callResultType.isF64() && resultType.isF32())
        popResult = mlir::arith::TruncFOp::create(rewriter, loc, resultType, popResult);
      else
        popResult = mlir::arith::TruncIOp::create(rewriter, loc, resultType, popResult);
    }
    rewriter.replaceOp(op, popResult);
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
    mlir::Value val = adaptor.getValue();

    std::string suffix = vecElemSuffix(op.getValue().getType());
    if (suffix.empty())
      suffix = vecElemSuffix(valType);

    // Promote narrow types to match runtime function signatures
    mlir::Type callType = valType;
    if (auto intTy = mlir::dyn_cast<mlir::IntegerType>(valType)) {
      unsigned w = intTy.getWidth();
      if (w < 32) {
        callType = rewriter.getI32Type();
        if (w == 1 || w == 8)
          val = mlir::arith::ExtUIOp::create(rewriter, loc, callType, val);
        else
          val = mlir::arith::ExtSIOp::create(rewriter, loc, callType, val);
      }
    } else if (auto fTy = mlir::dyn_cast<mlir::FloatType>(valType)) {
      if (fTy.getWidth() == 32) {
        callType = rewriter.getF64Type();
        val = mlir::arith::ExtFOp::create(rewriter, loc, callType, val);
      }
    }

    std::string funcName = "hew_vec_remove" + suffix;
    auto funcType = rewriter.getFunctionType({ptrType, callType}, {});
    getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, funcName, funcType);
    mlir::func::CallOp::create(rewriter, loc, funcName, mlir::TypeRange{},
                               mlir::ValueRange{adaptor.getVec(), val});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

struct VecRemoveAtOpLowering : public mlir::OpConversionPattern<hew::VecRemoveAtOp> {
  using OpConversionPattern::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::VecRemoveAtOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto i64Type = rewriter.getI64Type();

    std::string funcName = "hew_vec_remove_at";
    auto funcType = rewriter.getFunctionType({ptrType, i64Type}, {});
    getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, funcName, funcType);
    mlir::func::CallOp::create(rewriter, loc, funcName, mlir::TypeRange{},
                               mlir::ValueRange{adaptor.getVec(), adaptor.getIndex()});
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
    auto call =
        mlir::func::CallOp::create(rewriter, loc, "hew_vec_is_empty", mlir::TypeRange{i1Type},
                                   mlir::ValueRange{adaptor.getVec()});
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
    mlir::func::CallOp::create(rewriter, loc, "hew_vec_clear", mlir::TypeRange{},
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
    mlir::func::CallOp::create(rewriter, loc, "hew_vec_free", mlir::TypeRange{},
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
    auto call = mlir::func::CallOp::create(rewriter, loc, "hew_hashmap_new_impl",
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
      mlir::func::CallOp::create(
          rewriter, loc, "hew_hashmap_insert_i64", mlir::TypeRange{},
          mlir::ValueRange{adaptor.getMap(), adaptor.getKey(), adaptor.getValue()});
    } else if (valType == f64Type) {
      // hew_hashmap_insert_f64(map, key, val)
      auto funcType = rewriter.getFunctionType({ptrType, ptrType, f64Type}, {});
      getOrInsertFuncDecl(module, rewriter, "hew_hashmap_insert_f64", funcType);
      mlir::func::CallOp::create(
          rewriter, loc, "hew_hashmap_insert_f64", mlir::TypeRange{},
          mlir::ValueRange{adaptor.getMap(), adaptor.getKey(), adaptor.getValue()});
    } else if (auto fTy = mlir::dyn_cast<mlir::FloatType>(valType); fTy && fTy.getWidth() == 32) {
      // f32 → promote to f64, then insert_f64
      auto funcType = rewriter.getFunctionType({ptrType, ptrType, f64Type}, {});
      getOrInsertFuncDecl(module, rewriter, "hew_hashmap_insert_f64", funcType);
      auto promoted = mlir::arith::ExtFOp::create(rewriter, loc, f64Type, adaptor.getValue());
      mlir::func::CallOp::create(rewriter, loc, "hew_hashmap_insert_f64", mlir::TypeRange{},
                                 mlir::ValueRange{adaptor.getMap(), adaptor.getKey(), promoted});
    } else if (valType == ptrType) {
      // String value: hew_hashmap_insert_impl(map, key, 0, val_str)
      auto funcType = rewriter.getFunctionType({ptrType, ptrType, i32Type, ptrType}, {});
      getOrInsertFuncDecl(module, rewriter, "hew_hashmap_insert_impl", funcType);
      auto zero =
          mlir::arith::ConstantOp::create(rewriter, loc, i32Type, rewriter.getI32IntegerAttr(0));
      mlir::func::CallOp::create(
          rewriter, loc, "hew_hashmap_insert_impl", mlir::TypeRange{},
          mlir::ValueRange{adaptor.getMap(), adaptor.getKey(), zero, adaptor.getValue()});
    } else {
      // Integer types (i32, i1, i8, i16): promote to i32 then use insert_impl
      mlir::Value val = adaptor.getValue();
      if (auto intTy = mlir::dyn_cast<mlir::IntegerType>(valType)) {
        if (intTy.getWidth() < 32) {
          if (intTy.getWidth() == 1 || intTy.getWidth() == 8)
            val = mlir::arith::ExtUIOp::create(rewriter, loc, i32Type, val);
          else
            val = mlir::arith::ExtSIOp::create(rewriter, loc, i32Type, val);
        }
      }
      auto funcType = rewriter.getFunctionType({ptrType, ptrType, i32Type, ptrType}, {});
      getOrInsertFuncDecl(module, rewriter, "hew_hashmap_insert_impl", funcType);
      auto nullStr = mlir::LLVM::ZeroOp::create(rewriter, loc, ptrType);
      mlir::func::CallOp::create(
          rewriter, loc, "hew_hashmap_insert_impl", mlir::TypeRange{},
          mlir::ValueRange{adaptor.getMap(), adaptor.getKey(), val, nullStr});
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
    mlir::Type callReturnType = resultType;

    if (resultType == i64Type) {
      funcName = "hew_hashmap_get_i64";
    } else if (resultType == f64Type) {
      funcName = "hew_hashmap_get_f64";
    } else if (resultType == ptrType) {
      funcName = "hew_hashmap_get_str_impl";
    } else if (auto fTy = mlir::dyn_cast<mlir::FloatType>(resultType);
               fTy && fTy.getWidth() == 32) {
      // f32: call get_f64, then TruncFOp
      funcName = "hew_hashmap_get_f64";
      callReturnType = f64Type;
    } else if (auto intTy = mlir::dyn_cast<mlir::IntegerType>(resultType);
               intTy && intTy.getWidth() < 32) {
      // i1/i8/i16: call get_i32, then TruncIOp
      funcName = "hew_hashmap_get_i32";
      callReturnType = rewriter.getI32Type();
    } else {
      funcName = "hew_hashmap_get_i32";
      callReturnType = rewriter.getI32Type();
    }

    auto funcType = rewriter.getFunctionType({ptrType, ptrType}, {callReturnType});
    getOrInsertFuncDecl(module, rewriter, funcName, funcType);
    auto call = mlir::func::CallOp::create(rewriter, loc, funcName, mlir::TypeRange{callReturnType},
                                           mlir::ValueRange{adaptor.getMap(), adaptor.getKey()});

    if (callReturnType != resultType) {
      mlir::Value result = call.getResult(0);
      if (mlir::isa<mlir::FloatType>(resultType)) {
        result = mlir::arith::TruncFOp::create(rewriter, loc, resultType, result);
      } else {
        result = mlir::arith::TruncIOp::create(rewriter, loc, resultType, result);
      }
      rewriter.replaceOp(op, result);
    } else {
      rewriter.replaceOp(op, call.getResults());
    }
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
    auto call = mlir::func::CallOp::create(rewriter, loc, "hew_hashmap_contains_key",
                                           mlir::TypeRange{i1Type},
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
    mlir::func::CallOp::create(rewriter, loc, "hew_hashmap_remove", mlir::TypeRange{i1Type},
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
    auto call =
        mlir::func::CallOp::create(rewriter, loc, "hew_hashmap_len", mlir::TypeRange{resultType},
                                   mlir::ValueRange{adaptor.getMap()});
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
    mlir::func::CallOp::create(rewriter, loc, "hew_hashmap_free_impl", mlir::TypeRange{},
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
    auto call =
        mlir::func::CallOp::create(rewriter, loc, "hew_hashmap_keys", mlir::TypeRange{ptrType},
                                   mlir::ValueRange{adaptor.getMap()});
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
    auto call = mlir::func::CallOp::create(
        rewriter, loc, "hew_gen_ctx_create", mlir::TypeRange{ptrType},
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
    auto call =
        mlir::func::CallOp::create(rewriter, loc, "hew_gen_next", mlir::TypeRange{ptrType},
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
    auto call = mlir::func::CallOp::create(
        rewriter, loc, "hew_gen_yield", mlir::TypeRange{i1Type},
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
    mlir::func::CallOp::create(rewriter, loc, "hew_gen_free", mlir::TypeRange{},
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

    auto undef = mlir::LLVM::UndefOp::create(rewriter, loc, wrapperType);
    auto oneI8 = mlir::arith::ConstantIntOp::create(rewriter, loc, i8Type, 1);
    auto withTag =
        mlir::LLVM::InsertValueOp::create(rewriter, loc, undef, oneI8, llvm::ArrayRef<int64_t>{0});
    auto withVal = mlir::LLVM::InsertValueOp::create(rewriter, loc, withTag, adaptor.getValue(),
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

    auto undef = mlir::LLVM::UndefOp::create(rewriter, loc, wrapperType);
    auto zeroI8 = mlir::arith::ConstantIntOp::create(rewriter, loc, i8Type, 0);
    auto withTag =
        mlir::LLVM::InsertValueOp::create(rewriter, loc, undef, zeroI8, llvm::ArrayRef<int64_t>{0});

    // Create a zero/default value for the value slot
    mlir::Value defaultVal;
    if (mlir::isa<mlir::IntegerType>(valueType))
      defaultVal = mlir::arith::ConstantIntOp::create(rewriter, loc, valueType, 0);
    else if (mlir::isa<mlir::FloatType>(valueType))
      defaultVal =
          mlir::arith::ConstantOp::create(rewriter, loc, rewriter.getFloatAttr(valueType, 0.0));
    else if (mlir::isa<mlir::LLVM::LLVMPointerType>(valueType))
      defaultVal = mlir::LLVM::ZeroOp::create(rewriter, loc, valueType);
    else
      defaultVal = mlir::LLVM::UndefOp::create(rewriter, loc, valueType);

    auto withVal = mlir::LLVM::InsertValueOp::create(rewriter, loc, withTag, defaultVal,
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
    auto call = mlir::func::CallOp::create(rewriter, loc, "hew_regex_new", mlir::TypeRange{ptrType},
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
    auto call =
        mlir::func::CallOp::create(rewriter, loc, "hew_regex_is_match", mlir::TypeRange{i1Type},
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
    auto call =
        mlir::func::CallOp::create(rewriter, loc, "hew_regex_find", mlir::TypeRange{ptrType},
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
    auto call = mlir::func::CallOp::create(
        rewriter, loc, "hew_regex_replace", mlir::TypeRange{ptrType},
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
    mlir::func::CallOp::create(rewriter, loc, "hew_regex_free", mlir::TypeRange{},
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
    auto i32Type = rewriter.getI32Type();
    auto funcType = rewriter.getFunctionType({}, {i32Type});
    getOrInsertFuncDecl(module, rewriter, "hew_sched_init", funcType);
    mlir::func::CallOp::create(rewriter, loc, "hew_sched_init", mlir::TypeRange{i32Type},
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
    mlir::func::CallOp::create(rewriter, loc, "hew_sched_shutdown", mlir::TypeRange{},
                               mlir::ValueRange{});
    // After scheduler shutdown (workers joined), clean up remaining actors.
    getOrInsertFuncDecl(module, rewriter, "hew_runtime_cleanup", funcType);
    mlir::func::CallOp::create(rewriter, loc, "hew_runtime_cleanup", mlir::TypeRange{},
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
    auto call =
        mlir::func::CallOp::create(rewriter, loc, "hew_arena_malloc", mlir::TypeRange{ptrType},
                                   mlir::ValueRange{adaptor.getSize()});
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
    auto call = mlir::func::CallOp::create(
        rewriter, loc, "hew_rc_new", mlir::TypeRange{ptrType},
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
    auto call = mlir::func::CallOp::create(rewriter, loc, "hew_rc_clone", mlir::TypeRange{ptrType},
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
    mlir::func::CallOp::create(rewriter, loc, "hew_rc_drop", mlir::TypeRange{},
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
    auto funcName = op.getDropFn().str();
    auto argVal = adaptor.getValue();

    // User-defined drop functions take the struct type directly.
    // Look up the existing declaration to get the correct signature.
    if (op.getIsUserDrop()) {
      if (auto existingFunc = module.lookupSymbol<mlir::func::FuncOp>(funcName)) {
        mlir::func::CallOp::create(rewriter, loc, funcName, mlir::TypeRange{},
                                    mlir::ValueRange{argVal});
        rewriter.eraseOp(op);
        return mlir::success();
      }
    }

    // Runtime drops take !llvm.ptr.
    auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());
    auto funcType = rewriter.getFunctionType({ptrType}, {});
    getOrInsertFuncDecl(module, rewriter, funcName, funcType);
    mlir::func::CallOp::create(rewriter, loc, funcName, mlir::TypeRange{},
                               mlir::ValueRange{argVal});
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
    auto actorScope = mlir::func::CallOp::create(rewriter, loc, "hew_scope_create",
                                                 mlir::TypeRange{ptrType}, mlir::ValueRange{});

    getOrInsertFuncDecl(module, rewriter, "hew_task_scope_new", scopeFuncType);
    auto taskScope = mlir::func::CallOp::create(rewriter, loc, "hew_task_scope_new",
                                                mlir::TypeRange{ptrType}, mlir::ValueRange{});

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
    mlir::func::CallOp::create(rewriter, loc, "hew_task_scope_join_all", mlir::TypeRange{},
                               mlir::ValueRange{adaptor.getTaskScope()});

    getOrInsertFuncDecl(module, rewriter, "hew_scope_wait_all", voidPtrFuncType);
    mlir::func::CallOp::create(rewriter, loc, "hew_scope_wait_all", mlir::TypeRange{},
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
    mlir::func::CallOp::create(rewriter, loc, "hew_task_scope_destroy", mlir::TypeRange{},
                               mlir::ValueRange{adaptor.getTaskScope()});

    getOrInsertFuncDecl(module, rewriter, "hew_scope_free", voidPtrFuncType);
    mlir::func::CallOp::create(rewriter, loc, "hew_scope_free", mlir::TypeRange{},
                               mlir::ValueRange{adaptor.getActorScope()});

    rewriter.eraseOp(op);
    return mlir::success();
  }
};

/// Lower hew.scope.launch -> hew_task_new + hew_task_scope_spawn + hew_task_set_env +
/// hew_task_spawn_thread
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
    auto taskPtr = mlir::func::CallOp::create(rewriter, loc, "hew_task_new",
                                              mlir::TypeRange{ptrType}, mlir::ValueRange{});

    // hew_task_scope_spawn(scope, task) -> void
    auto spawnFuncType = rewriter.getFunctionType({ptrType, ptrType}, {});
    getOrInsertFuncDecl(module, rewriter, "hew_task_scope_spawn", spawnFuncType);
    mlir::func::CallOp::create(rewriter, loc, "hew_task_scope_spawn", mlir::TypeRange{},
                               mlir::ValueRange{adaptor.getTaskScope(), taskPtr.getResult(0)});

    // hew_task_set_env(task, env_ptr) -> void
    auto setEnvFuncType = rewriter.getFunctionType({ptrType, ptrType}, {});
    getOrInsertFuncDecl(module, rewriter, "hew_task_set_env", setEnvFuncType);
    mlir::func::CallOp::create(rewriter, loc, "hew_task_set_env", mlir::TypeRange{},
                               mlir::ValueRange{taskPtr.getResult(0), adaptor.getEnvPtr()});

    // hew_task_spawn_thread(task, fn_ptr) -> void
    getOrInsertFuncDecl(module, rewriter, "hew_task_spawn_thread", spawnFuncType);
    mlir::func::CallOp::create(rewriter, loc, "hew_task_spawn_thread", mlir::TypeRange{},
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
    auto call =
        mlir::func::CallOp::create(rewriter, loc, "hew_task_await_blocking",
                                   mlir::TypeRange{ptrType}, mlir::ValueRange{adaptor.getTask()});
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
    mlir::func::CallOp::create(rewriter, loc, "hew_task_scope_cancel", mlir::TypeRange{},
                               mlir::ValueRange{adaptor.getTaskScope()});
    rewriter.eraseOp(op);
    return mlir::success();
  }
};

struct TaskGetEnvOpLowering : public mlir::OpConversionPattern<hew::TaskGetEnvOp> {
  using OpConversionPattern<hew::TaskGetEnvOp>::OpConversionPattern;

  mlir::LogicalResult matchAndRewrite(hew::TaskGetEnvOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto module = op->getParentOfType<mlir::ModuleOp>();
    auto loc = op.getLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(rewriter.getContext());

    auto funcType = rewriter.getFunctionType({ptrType}, {ptrType});
    getOrInsertFuncDecl(module, rewriter, "hew_task_get_env", funcType);
    auto call =
        mlir::func::CallOp::create(rewriter, loc, "hew_task_get_env", mlir::TypeRange{ptrType},
                                   mlir::ValueRange{adaptor.getTask()});
    rewriter.replaceOp(op, call.getResult(0));
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
    mlir::func::CallOp::create(
        rewriter, loc, "hew_task_set_result", mlir::TypeRange{},
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
    mlir::func::CallOp::create(rewriter, loc, "hew_task_complete_threaded", mlir::TypeRange{},
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
    auto call = mlir::func::CallOp::create(rewriter, loc, "hew_reply_channel_new",
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

    auto funcType =
        rewriter.getFunctionType({ptrType, i32Type, ptrType, sizeType, ptrType}, {i32Type});
    getOrInsertFuncDecl(module, rewriter, "hew_actor_ask_with_channel", funcType);
    auto call = mlir::func::CallOp::create(
        rewriter, loc, "hew_actor_ask_with_channel", mlir::TypeRange{i32Type},
        mlir::ValueRange{adaptor.getActor(), adaptor.getMsgType(), adaptor.getDataPtr(),
                         adaptor.getDataSize(), adaptor.getChannel()});
    rewriter.replaceOp(op, call.getResult(0));
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
    auto call = mlir::func::CallOp::create(
        rewriter, loc, "hew_select_first", mlir::TypeRange{i32Type},
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
    mlir::func::CallOp::create(rewriter, loc, "hew_reply_channel_free", mlir::TypeRange{},
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
    auto call =
        mlir::func::CallOp::create(rewriter, loc, "hew_reply_wait", mlir::TypeRange{ptrType},
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
        mlir::func::CallOp::create(rewriter, loc, callee, resultTypes, adaptor.getOperands());

    if (resultTypes.empty())
      rewriter.eraseOp(op);
    else
      rewriter.replaceOp(op, call.getResults());
    return mlir::success();
  }
};

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
    auto indexVal = mlir::LLVM::ConstantOp::create(
        rewriter, loc, i32Type, rewriter.getI32IntegerAttr(static_cast<int32_t>(methodIndex)));
    auto funcPtrPtr = mlir::LLVM::GEPOp::create(rewriter, loc, ptrType, ptrType, vtablePtr,
                                                mlir::ValueRange{indexVal});
    auto funcPtr = mlir::LLVM::LoadOp::create(rewriter, loc, ptrType, funcPtrPtr);

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
        mlir::UnrealizedConversionCastOp::create(rewriter, loc, callFuncType, funcPtr.getResult())
            .getResult(0);

    // Indirect call through function pointer
    auto call = mlir::func::CallIndirectOp::create(rewriter, loc, callable, callArgs);

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
        mlir::func::CallOp::create(rewriter, loc, "hew_string_concat", mlir::TypeRange{ptrType},
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
    bool isUnsigned = op->hasAttrOfType<mlir::BoolAttr>("is_unsigned") &&
                      op->getAttrOfType<mlir::BoolAttr>("is_unsigned").getValue();

    // Select runtime function based on operand type, extending small
    // integers to match the runtime function's parameter width.
    // Use zero-extension for unsigned source types.
    std::string funcName;
    mlir::Value arg = adaptor.getValue();
    if (origType.isInteger(1)) {
      funcName = "hew_bool_to_string";
    } else if (origType.isInteger(8) || origType.isInteger(16)) {
      // Sub-i32 values — extend to i32 for decimal conversion.
      // Note: char is i32 in MLIR and hits the i32 branch directly.
      funcName = isUnsigned ? "hew_uint_to_string" : "hew_int_to_string";
      if (isUnsigned)
        arg = mlir::arith::ExtUIOp::create(rewriter, loc, rewriter.getI32Type(), arg);
      else
        arg = mlir::arith::ExtSIOp::create(rewriter, loc, rewriter.getI32Type(), arg);
    } else if (origType.isInteger(32)) {
      funcName = isUnsigned ? "hew_uint_to_string" : "hew_int_to_string";
    } else if (origType.isInteger(64)) {
      funcName = isUnsigned ? "hew_u64_to_string" : "hew_i64_to_string";
    } else if (origType.isF64() || origType.isF32()) {
      funcName = "hew_float_to_string";
      if (origType.isF32())
        arg = mlir::arith::ExtFOp::create(rewriter, loc, rewriter.getF64Type(), arg);
    } else if (mlir::isa<mlir::IntegerType>(origType)) {
      // Other int widths — extend to i64
      funcName = isUnsigned ? "hew_u64_to_string" : "hew_i64_to_string";
      if (isUnsigned)
        arg = mlir::arith::ExtUIOp::create(rewriter, loc, rewriter.getI64Type(), arg);
      else
        arg = mlir::arith::ExtSIOp::create(rewriter, loc, rewriter.getI64Type(), arg);
    } else {
      op.emitError() << "ToStringOp: cannot convert type to string: " << origType;
      return mlir::failure();
    }

    auto funcType = rewriter.getFunctionType({arg.getType()}, {ptrType});
    getOrInsertFuncDecl(op->getParentOfType<mlir::ModuleOp>(), rewriter, funcName, funcType);
    auto call = mlir::func::CallOp::create(rewriter, loc, funcName, mlir::TypeRange{ptrType},
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

    // Inline lowering for char_at: strlen + bounds check + GEP + byte load.
    // Avoids a full C-ABI call per character access.
    // Only inline on 64-bit targets; WASM32 has different size_t/strlen types.
    if (methodName == "char_at" && isNative64(module)) {
      auto i8Type = rewriter.getI8Type();
      auto i32Type = rewriter.getI32Type();
      auto i64Type = rewriter.getI64Type();
      auto ptrType = mlir::LLVM::LLVMPointerType::get(op.getContext());

      auto strPtr = adaptor.getReceiver();
      auto index = adaptor.getExtraArgs()[0];

      // Zero-extend index to i64 for comparison with strlen result.
      // Use ExtUIOp (not ExtSIOp) to avoid misinterpreting large positive indices
      // (e.g., u32 >= 2^31) as negative values. Negative signed indices become
      // large unsigned values (e.g., -1 → 4294967295), which uge correctly catches.
      auto index64 = mlir::arith::ExtUIOp::create(rewriter, loc, i64Type, index);

      // Call strlen to get byte length
      auto strlenType = rewriter.getFunctionType({ptrType}, {i64Type});
      getOrInsertFuncDecl(module, rewriter, "strlen", strlenType);
      auto lenCall = mlir::func::CallOp::create(rewriter, loc, "strlen", mlir::TypeRange{i64Type},
                                                mlir::ValueRange{strPtr});
      auto len = lenCall.getResult(0);

      // Bounds check: if index >= len, abort (uge catches negative indices too)
      auto oob =
          mlir::arith::CmpIOp::create(rewriter, loc, mlir::arith::CmpIPredicate::uge, index64, len);

      auto abortFuncType = rewriter.getFunctionType({i64Type, i64Type}, {});
      getOrInsertFuncDecl(module, rewriter, "hew_string_abort_oob", abortFuncType);

      mlir::scf::IfOp::create(
          rewriter, loc, oob,
          [&](mlir::OpBuilder &b, mlir::Location l) {
            mlir::func::CallOp::create(b, l, "hew_string_abort_oob", mlir::TypeRange{},
                                       mlir::ValueRange{index64, len});
            mlir::scf::YieldOp::create(b, l);
          },
          nullptr);

      // GEP to byte at index, load i8, zero-extend to i32
      auto elemPtr = mlir::LLVM::GEPOp::create(
          rewriter, loc, ptrType, i8Type, strPtr,
          mlir::ArrayRef<mlir::LLVM::GEPArg>{mlir::LLVM::GEPArg(index64)});
      auto loaded = mlir::LLVM::LoadOp::create(rewriter, loc, i8Type, elemPtr);
      auto result = mlir::arith::ExtUIOp::create(rewriter, loc, i32Type, loaded);
      rewriter.replaceOp(op, result.getResult());
      return mlir::success();
    }

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
    auto call = mlir::func::CallOp::create(rewriter, loc, funcName, resultTypes, args);

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
    mlir::Value result = mlir::LLVM::UndefOp::create(rewriter, loc, structType);
    for (auto [idx, elem] : llvm::enumerate(adaptor.getElements())) {
      result = mlir::LLVM::InsertValueOp::create(
          rewriter, loc, result, elem, llvm::ArrayRef<int64_t>{static_cast<int64_t>(idx)});
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
    auto result = mlir::LLVM::ExtractValueOp::create(
        rewriter, loc, adaptor.getTuple(), llvm::ArrayRef<int64_t>{static_cast<int64_t>(idx)});
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
    mlir::Value result = mlir::LLVM::UndefOp::create(rewriter, loc, arrayType);
    for (auto [idx, elem] : llvm::enumerate(adaptor.getElements())) {
      result = mlir::LLVM::InsertValueOp::create(
          rewriter, loc, result, elem, llvm::ArrayRef<int64_t>{static_cast<int64_t>(idx)});
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
    auto result = mlir::LLVM::ExtractValueOp::create(
        rewriter, loc, adaptor.getArray(), llvm::ArrayRef<int64_t>{static_cast<int64_t>(idx)});
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
    mlir::Value result = mlir::LLVM::UndefOp::create(rewriter, loc, structType);
    result = mlir::LLVM::InsertValueOp::create(rewriter, loc, result, adaptor.getData(),
                                               llvm::ArrayRef<int64_t>{static_cast<int64_t>(0)});
    result = mlir::LLVM::InsertValueOp::create(rewriter, loc, result, adaptor.getVtablePtr(),
                                               llvm::ArrayRef<int64_t>{static_cast<int64_t>(1)});
    rewriter.replaceOp(op, result);
    return mlir::success();
  }
};

struct TraitObjectDataOpLowering : public mlir::OpConversionPattern<hew::TraitObjectDataOp> {
  using OpConversionPattern::OpConversionPattern;
  mlir::LogicalResult matchAndRewrite(hew::TraitObjectDataOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto result =
        mlir::LLVM::ExtractValueOp::create(rewriter, op.getLoc(), adaptor.getTraitObject(),
                                           llvm::ArrayRef<int64_t>{static_cast<int64_t>(0)});
    rewriter.replaceOp(op, result.getResult());
    return mlir::success();
  }
};

struct TraitObjectTagOpLowering : public mlir::OpConversionPattern<hew::TraitObjectTagOp> {
  using OpConversionPattern::OpConversionPattern;
  mlir::LogicalResult matchAndRewrite(hew::TraitObjectTagOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto result =
        mlir::LLVM::ExtractValueOp::create(rewriter, op.getLoc(), adaptor.getTraitObject(),
                                           llvm::ArrayRef<int64_t>{static_cast<int64_t>(1)});
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
    mlir::Value result = mlir::LLVM::UndefOp::create(rewriter, loc, structType);
    result = mlir::LLVM::InsertValueOp::create(rewriter, loc, result, adaptor.getFnPtr(),
                                               llvm::ArrayRef<int64_t>{static_cast<int64_t>(0)});
    result = mlir::LLVM::InsertValueOp::create(rewriter, loc, result, adaptor.getEnvPtr(),
                                               llvm::ArrayRef<int64_t>{static_cast<int64_t>(1)});
    rewriter.replaceOp(op, result);
    return mlir::success();
  }
};

struct ClosureGetFnOpLowering : public mlir::OpConversionPattern<hew::ClosureGetFnOp> {
  using OpConversionPattern::OpConversionPattern;
  mlir::LogicalResult matchAndRewrite(hew::ClosureGetFnOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto result =
        mlir::LLVM::ExtractValueOp::create(rewriter, op.getLoc(), adaptor.getClosure(),
                                           llvm::ArrayRef<int64_t>{static_cast<int64_t>(0)});
    rewriter.replaceOp(op, result.getResult());
    return mlir::success();
  }
};

struct ClosureGetEnvOpLowering : public mlir::OpConversionPattern<hew::ClosureGetEnvOp> {
  using OpConversionPattern::OpConversionPattern;
  mlir::LogicalResult matchAndRewrite(hew::ClosureGetEnvOp op, OpAdaptor adaptor,
                                      mlir::ConversionPatternRewriter &rewriter) const override {
    auto result =
        mlir::LLVM::ExtractValueOp::create(rewriter, op.getLoc(), adaptor.getClosure(),
                                           llvm::ArrayRef<int64_t>{static_cast<int64_t>(1)});
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
      op.emitError() << "runtime function not found: " << funcName;
      return mlir::failure();
    }

    // Create func.constant to get a typed function reference, then
    // use the type converter's materialization to bridge to !llvm.ptr.
    // FuncToLLVM + ReconcileUnrealizedCasts will resolve this cleanly.
    //
    // Use the type converter to build the FunctionType so it matches the
    // function's signature after partial conversion (e.g. !hew.string_ref
    // becomes !llvm.ptr).
    auto *tc = getTypeConverter();
    auto origFuncType = funcOp.getFunctionType();
    llvm::SmallVector<mlir::Type> convertedInputs, convertedResults;
    for (auto t : origFuncType.getInputs()) {
      auto ct = tc->convertType(t);
      convertedInputs.push_back(ct ? ct : t);
    }
    for (auto t : origFuncType.getResults()) {
      auto ct = tc->convertType(t);
      convertedResults.push_back(ct ? ct : t);
    }
    auto convertedFuncType = mlir::FunctionType::get(
        rewriter.getContext(), convertedInputs, convertedResults);
    auto funcRef =
        mlir::func::ConstantOp::create(rewriter, loc, convertedFuncType,
                                       mlir::SymbolRefAttr::get(rewriter.getContext(), funcName));
    // Bridge func type -> !llvm.ptr via UnrealizedConversionCast.
    // This is the standard MLIR pattern: the FuncToLLVM pass + reconcile
    // pass will eliminate these casts.
    auto castOp =
        mlir::UnrealizedConversionCastOp::create(rewriter, loc, ptrType, funcRef.getResult());
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
    auto castOp = mlir::UnrealizedConversionCastOp::create(rewriter, op.getLoc(),
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

static void dumpModuleAtFailure(mlir::ModuleOp module, llvm::StringRef stage) {
  llvm::errs() << "MLIR module dump (" << stage << "):\n";
  module.print(llvm::errs());
  llvm::errs() << '\n';
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
    llvm::StringRef group;
    llvm::StringRef detail;
  };

  static const UnsupportedOp unsupported[] = {
      // Supervision
      {"hew.supervisor.new", "supervision tree",
       "supervision trees require OS threads for restart strategies"},
      {"hew.supervisor.start", "supervision tree",
       "supervision trees require OS threads for restart strategies"},
      {"hew.supervisor.stop", "supervision tree",
       "supervision trees require OS threads for restart strategies"},
      {"hew.supervisor.add_child", "supervision tree",
       "supervision trees require OS threads for restart strategies"},
      {"hew.supervisor.add_child_supervisor", "supervision tree",
       "supervision trees require OS threads for restart strategies"},
      {"hew.supervisor.child_spec_create", "supervision tree",
       "supervision trees require OS threads for restart strategies"},
      // Link / Monitor
      {"hew.actor.link", "link/monitor",
       "link/monitor fault propagation requires OS threads to watch peers"},
      {"hew.actor.unlink", "link/monitor",
       "link/monitor fault propagation requires OS threads to watch peers"},
      {"hew.actor.monitor", "link/monitor",
       "link/monitor fault propagation requires OS threads to watch peers"},
      {"hew.actor.demonitor", "link/monitor",
       "link/monitor fault propagation requires OS threads to watch peers"},
      // Scoped concurrency
      {"hew.scope.create", "structured concurrency",
       "structured concurrency scopes require OS threads for scheduling"},
      {"hew.scope.join", "structured concurrency",
       "structured concurrency scopes require OS threads for scheduling"},
      {"hew.scope.destroy", "structured concurrency",
       "structured concurrency scopes require OS threads for scheduling"},
      {"hew.scope.launch", "structured concurrency",
       "structured concurrency scopes require OS threads for scheduling"},
      {"hew.scope.await", "structured concurrency",
       "structured concurrency scopes require OS threads for scheduling"},
      {"hew.scope.cancel", "structured concurrency",
       "structured concurrency scopes require OS threads for scheduling"},

      // Tasks
      {"hew.task.set_result", "task",
       "task completion APIs require OS threads to drive child scopes"},
      {"hew.task.complete", "task",
       "task completion APIs require OS threads to drive child scopes"},
      // Select
      {"hew.select.create", "select",
       "select waits on multiple mailboxes using OS threads for blocking"},
      {"hew.select.add", "select",
       "select waits on multiple mailboxes using OS threads for blocking"},
      {"hew.select.first", "select",
       "select waits on multiple mailboxes using OS threads for blocking"},
      {"hew.select.destroy", "select",
       "select waits on multiple mailboxes using OS threads for blocking"},
      {"hew.select.wait", "select",
       "select waits on multiple mailboxes using OS threads for blocking"},
  };

  module.walk([&](mlir::Operation *op) {
    auto name = op->getName().getStringRef();
    for (const auto &entry : unsupported) {
      if (name == entry.opName) {
        op->emitError()
            << "operation '" << name << "' is part of the " << entry.group
            << " APIs and is not supported on WASM32 — " << entry.detail << "\n"
            << "  help: Consider using basic actors (spawn/send/ask) which work on WASM "
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
  // LLVMStructType may embed Hew dialect types (e.g. !hew.string_ref) when
  // used for user-defined enum layouts.  Recursively convert body elements.
  typeConverter.addConversion([&typeConverter](
                                  mlir::LLVM::LLVMStructType type) -> std::optional<mlir::Type> {
    if (type.isIdentified())
      return mlir::Type(type);
    auto body = type.getBody();
    bool anyChanged = false;
    llvm::SmallVector<mlir::Type, 4> converted;
    for (auto elem : body) {
      auto c = typeConverter.convertType(elem);
      if (!c)
        return std::nullopt;
      converted.push_back(c);
      if (c != elem)
        anyChanged = true;
    }
    if (!anyChanged)
      return mlir::Type(type);
    return mlir::LLVM::LLVMStructType::getLiteral(type.getContext(), converted, type.isPacked());
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

    return mlir::UnrealizedConversionCastOp::create(builder, loc, resultType, input).getResult(0);
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
  patterns.add<ActorAwaitOpLowering>(typeConverter, &context);
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
  patterns.add<VecRemoveAtOpLowering>(typeConverter, &context);
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

  patterns.add<TaskGetEnvOpLowering>(typeConverter, &context);
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

      auto call = mlir::func::CallOp::create(builder, loc, funcName, resultTypes, callArgs);

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
      auto one = mlir::LLVM::ConstantOp::create(allocBuilder, loc, allocBuilder.getI64Type(),
                                                allocBuilder.getI64IntegerAttr(1));
      auto alloca = mlir::LLVM::AllocaOp::create(allocBuilder, loc, ptrType, measuredType, one);

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
        auto globalOp = mlir::LLVM::GlobalOp::create(
            moduleBuilder, loc, arrayType, /*isConstant=*/true, mlir::LLVM::Linkage::Internal,
            vtableName.str(), mlir::Attribute{});

        auto *initBlock = moduleBuilder.createBlock(&globalOp.getInitializerRegion());
        moduleBuilder.setInsertionPointToStart(initBlock);

        mlir::Value arr = mlir::LLVM::UndefOp::create(moduleBuilder, loc, arrayType);
        for (size_t i = 0; i < functions.size(); ++i) {
          auto funcName = mlir::cast<mlir::StringAttr>(functions[i]).getValue().str();
          auto funcAddr = mlir::LLVM::AddressOfOp::create(moduleBuilder, loc, ptrType, funcName);
          arr = mlir::LLVM::InsertValueOp::create(moduleBuilder, loc, arr, funcAddr,
                                                  llvm::ArrayRef<int64_t>{static_cast<int64_t>(i)});
        }
        mlir::LLVM::ReturnOp::create(moduleBuilder, loc, arr);
        createdVtables.insert(vtableName);
      }

      // Replace VtableRefOp with llvm.mlir.addressof
      auto vtableAddr = mlir::LLVM::AddressOfOp::create(builder, loc, ptrType, vtableName.str());
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

  pm.addPass(mlir::createConvertMathToLLVMPass());
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

// ── Coroutine generator transformation ────────────────────────────────────
//
// Transforms __coro body functions and their init/next/done stubs into proper
// LLVM coroutine-based generators.  This runs after MLIR → LLVM IR translation
// and before the LLVM optimization pipeline (which includes CoroSplit).
//
// For each generator function `foo`:
//   foo__coro(params..., ptr promise) → void  — body with suspend markers
//   foo(params...)                    → ptr   — init stub (returns null)
//   foo__next(ptr)                    → T     — next stub (returns default)
//   foo__done(ptr)                    → i1    — done stub (returns false)
//
// After transformation:
//   foo__coro — proper coroutine with presplitcoroutine attribute
//   foo       — ramp: calls foo__coro to create coroutine handle
//   foo__next — resume coroutine, read promise
//   foo__done — call coro.done
//
static void transformCoroutineGenerators(llvm::Module &M) {
  // Find all __coro body functions
  llvm::SmallVector<llvm::Function *, 4> coroFunctions;
  for (auto &F : M) {
    if (F.getName().ends_with("__coro") && !F.isDeclaration())
      coroFunctions.push_back(&F);
  }

  if (coroFunctions.empty())
    return;

  auto &Ctx = M.getContext();
  auto *PtrTy = llvm::PointerType::get(Ctx, 0);
  auto *I8Ty = llvm::Type::getInt8Ty(Ctx);
  auto *I32Ty = llvm::Type::getInt32Ty(Ctx);
  auto *I1Ty = llvm::Type::getInt1Ty(Ctx);
  auto *VoidTy = llvm::Type::getVoidTy(Ctx);

  // Determine size_t type from the target's pointer width.
  // On wasm32 this is i32; on x86_64/aarch64 it is i64.
  unsigned ptrBits = M.getDataLayout().getPointerSizeInBits();
  auto *SizeTy = llvm::IntegerType::get(Ctx, ptrBits);

  // Declare coroutine intrinsics
  auto *CoroIdFn = llvm::Intrinsic::getOrInsertDeclaration(&M, llvm::Intrinsic::coro_id);
  auto *CoroAllocFn = llvm::Intrinsic::getOrInsertDeclaration(&M, llvm::Intrinsic::coro_alloc);
  auto *CoroSizeFn =
      llvm::Intrinsic::getOrInsertDeclaration(&M, llvm::Intrinsic::coro_size, {SizeTy});
  auto *CoroBeginFn = llvm::Intrinsic::getOrInsertDeclaration(&M, llvm::Intrinsic::coro_begin);
  auto *CoroSuspendFn = llvm::Intrinsic::getOrInsertDeclaration(&M, llvm::Intrinsic::coro_suspend);
  auto *CoroEndFn = llvm::Intrinsic::getOrInsertDeclaration(&M, llvm::Intrinsic::coro_end);
  auto *CoroFreeFn = llvm::Intrinsic::getOrInsertDeclaration(&M, llvm::Intrinsic::coro_free);
  auto *CoroResumeFn = llvm::Intrinsic::getOrInsertDeclaration(&M, llvm::Intrinsic::coro_resume);
  auto *CoroDoneFn = llvm::Intrinsic::getOrInsertDeclaration(&M, llvm::Intrinsic::coro_done);
  auto *CoroPromiseFn = llvm::Intrinsic::getOrInsertDeclaration(&M, llvm::Intrinsic::coro_promise);

  // Ensure malloc and free are declared with target-correct size_t.
  auto *MallocFn = M.getFunction("malloc");
  if (!MallocFn) {
    auto *MallocTy = llvm::FunctionType::get(PtrTy, {SizeTy}, false);
    MallocFn = llvm::Function::Create(MallocTy, llvm::Function::ExternalLinkage, "malloc", &M);
  }
  auto *FreeFn = M.getFunction("free");
  if (!FreeFn) {
    auto *FreeTy = llvm::FunctionType::get(VoidTy, {PtrTy}, false);
    FreeFn = llvm::Function::Create(FreeTy, llvm::Function::ExternalLinkage, "free", &M);
  }

  // Remove the marker function declaration
  if (auto *marker = M.getFunction("__hew_coro_suspend"))
    marker->setLinkage(llvm::Function::InternalLinkage);

  for (auto *CoroBodyFn : coroFunctions) {
    std::string coroName = CoroBodyFn->getName().str();
    // Extract base name: "foo__coro" → "foo"
    std::string baseName = coroName.substr(0, coroName.size() - 6); // strip "__coro"

    auto *InitFn = M.getFunction(baseName);
    auto *NextFn = M.getFunction(baseName + "__next");
    auto *DoneFn = M.getFunction(baseName + "__done");

    if (!InitFn || !NextFn || !DoneFn) {
      throw std::runtime_error("missing generator stubs for '" + baseName +
                               "' — coroutine IR generation is incomplete");
    }

    // Determine the yield type from the __next function's return type
    auto *YieldTy = NextFn->getReturnType();

    // Create a promise struct type: { YieldTy }
    auto *PromiseStructTy = llvm::StructType::get(Ctx, llvm::ArrayRef<llvm::Type *>{YieldTy});

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    // Transform the __coro body function into a proper LLVM coroutine.
    //
    // The body was generated with:
    //   - An alloca for promise (the last argument is the promise ptr)
    //   - Calls to __hew_coro_suspend at yield points
    //
    // We need to:
    //   1. Create the coroutine entry (coro.id, coro.alloc, coro.begin)
    //   2. Replace __hew_coro_suspend calls with coro.suspend + switch
    //   3. Add final suspend and cleanup blocks
    //   4. Mark with presplitcoroutine attribute
    //
    // Instead of modifying the existing function in-place (which is very
    // complex due to CFG manipulation), we build a NEW coroutine function
    // that calls the body inline.  But that's also complex...
    //
    // SIMPLEST APPROACH: Build the entire coroutine from scratch at the
    // LLVM IR level, replacing the stub functions.
    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━

    // Step 1: Collect the suspend marker calls in the coro body
    llvm::SmallVector<llvm::CallInst *, 8> suspendCalls;
    for (auto &BB : *CoroBodyFn) {
      for (auto &I : BB) {
        if (auto *CI = llvm::dyn_cast<llvm::CallInst>(&I)) {
          auto *Callee = CI->getCalledFunction();
          if (Callee && Callee->getName() == "__hew_coro_suspend")
            suspendCalls.push_back(CI);
        }
      }
    }

    // Step 2: Transform the __coro function into a proper coroutine
    // We do this by:
    // a) Creating a new entry block with coro preamble
    // b) Splitting at each suspend call to insert coro.suspend
    // c) Adding cleanup and suspend blocks

    // Rename the old entry block
    auto &OldEntry = CoroBodyFn->getEntryBlock();

    // Create new blocks
    auto *NewEntry = llvm::BasicBlock::Create(Ctx, "coro.entry", CoroBodyFn, &OldEntry);
    auto *DynAlloc = llvm::BasicBlock::Create(Ctx, "coro.dyn.alloc", CoroBodyFn, &OldEntry);
    auto *Begin = llvm::BasicBlock::Create(Ctx, "coro.begin", CoroBodyFn, &OldEntry);
    auto *Cleanup = llvm::BasicBlock::Create(Ctx, "coro.cleanup", CoroBodyFn);
    auto *Suspend = llvm::BasicBlock::Create(Ctx, "coro.suspend", CoroBodyFn);
    auto *FinalSuspend = llvm::BasicBlock::Create(Ctx, "coro.final", CoroBodyFn);

    // The promise is an alloca in the new entry block
    llvm::IRBuilder<> IRB(NewEntry);
    auto *PromiseAlloca = IRB.CreateAlloca(PromiseStructTy, nullptr, "promise");
    PromiseAlloca->setAlignment(llvm::Align(8));

    // Get the promise value ptr (field 0)
    auto *PromiseValPtr = IRB.CreateStructGEP(PromiseStructTy, PromiseAlloca, 0, "promise.val.ptr");

    // coro.id
    auto *NullPtr = llvm::ConstantPointerNull::get(PtrTy);
    auto *CoroId = IRB.CreateCall(
        CoroIdFn, {llvm::ConstantInt::get(I32Ty, 0), PromiseAlloca, NullPtr, NullPtr}, "coro.id");

    // coro.alloc
    auto *NeedAlloc = IRB.CreateCall(CoroAllocFn, {CoroId}, "need.alloc");
    IRB.CreateCondBr(NeedAlloc, DynAlloc, Begin);

    // dyn.alloc block
    IRB.SetInsertPoint(DynAlloc);
    auto *CoroSize = IRB.CreateCall(CoroSizeFn, {}, "coro.size");
    auto *Alloc = IRB.CreateCall(MallocFn, {CoroSize}, "coro.alloc");
    IRB.CreateBr(Begin);

    // begin block
    IRB.SetInsertPoint(Begin);
    auto *PhiMem = IRB.CreatePHI(PtrTy, 2, "phi.mem");
    PhiMem->addIncoming(NullPtr, NewEntry);
    PhiMem->addIncoming(Alloc, DynAlloc);
    auto *Hdl = IRB.CreateCall(CoroBeginFn, {CoroId, PhiMem}, "coro.hdl");

    // Replace the promise ptr argument with our actual promise value ptr.
    // The last argument of the coro function was the promise pointer.
    auto *PromiseArg = CoroBodyFn->getArg(CoroBodyFn->arg_size() - 1);
    PromiseArg->replaceAllUsesWith(PromiseValPtr);

    // NO initial suspend — the ramp function runs the body until the first
    // yield, storing the first value in the promise.  __next() reads the
    // promise (getting the current value) THEN resumes (advancing to the
    // next yield or to the end of the function).
    IRB.CreateBr(&OldEntry);

    // Replace all return instructions in the body with branches to the
    // final suspend block
    llvm::SmallVector<llvm::ReturnInst *, 4> returns;
    for (auto &BB : *CoroBodyFn) {
      if (&BB == Cleanup || &BB == Suspend || &BB == FinalSuspend)
        continue;
      if (auto *RI = llvm::dyn_cast<llvm::ReturnInst>(BB.getTerminator()))
        returns.push_back(RI);
    }
    for (auto *RI : returns) {
      IRB.SetInsertPoint(RI);
      IRB.CreateBr(FinalSuspend);
      RI->eraseFromParent();
    }

    // Replace each __hew_coro_suspend call with coro.suspend + switch
    for (auto *CI : suspendCalls) {
      auto *ParentBB = CI->getParent();

      // Split the block after the suspend call.  Everything after the call
      // goes into the "resume" block.
      auto *ResumeBlock = ParentBB->splitBasicBlock(CI->getNextNode(), "coro.resume");

      // Remove the unconditional branch that splitBasicBlock created
      ParentBB->getTerminator()->eraseFromParent();

      // Insert coro.suspend at the end of ParentBB
      IRB.SetInsertPoint(ParentBB);
      auto *SuspResult = IRB.CreateCall(
          CoroSuspendFn, {llvm::ConstantTokenNone::get(Ctx), llvm::ConstantInt::get(I1Ty, 0)},
          "susp");
      auto *SW = IRB.CreateSwitch(SuspResult, Suspend, 2);
      SW->addCase(llvm::ConstantInt::get(I8Ty, 0), ResumeBlock);
      SW->addCase(llvm::ConstantInt::get(I8Ty, 1), Cleanup);

      // Remove the original marker call
      CI->eraseFromParent();
    }

    // Final suspend block
    IRB.SetInsertPoint(FinalSuspend);
    auto *FinalResult = IRB.CreateCall(
        CoroSuspendFn, {llvm::ConstantTokenNone::get(Ctx), llvm::ConstantInt::get(I1Ty, 1)},
        "final.susp");
    auto *FinalSW = IRB.CreateSwitch(FinalResult, Suspend, 2);
    FinalSW->addCase(llvm::ConstantInt::get(I8Ty, 0), Suspend); // final resume → suspend
    FinalSW->addCase(llvm::ConstantInt::get(I8Ty, 1), Cleanup);

    // Cleanup block
    IRB.SetInsertPoint(Cleanup);
    auto *FreeMem = IRB.CreateCall(CoroFreeFn, {CoroId, Hdl}, "coro.free.mem");
    IRB.CreateCall(FreeFn, {FreeMem});
    IRB.CreateBr(Suspend);

    // Suspend block (final)
    IRB.SetInsertPoint(Suspend);
    IRB.CreateCall(CoroEndFn,
                   {Hdl, llvm::ConstantInt::get(I1Ty, 0), llvm::ConstantTokenNone::get(Ctx)});
    IRB.CreateRet(Hdl);

    // Change the return type of the coro function to ptr (it returns the handle)
    // We need to create a new function with the right return type
    // Actually, we can change the return type by creating a new function.
    // But modifying in-place is tricky... Let's use a workaround:
    // Change the function type to return ptr instead of void.

    // Actually, the LLVM coroutine framework expects the ramp function to
    // return ptr (the handle).  But the __coro function returns void in
    // MLIR.  We've already replaced the returns with branches to final
    // suspend, and the suspend block now returns the handle.
    // BUT the function signature still says void return.  We need to fix that.

    // Create a new function with the correct type
    llvm::SmallVector<llvm::Type *, 4> CoroParamTys;
    for (auto &Arg : CoroBodyFn->args())
      CoroParamTys.push_back(Arg.getType());
    auto *NewCoroTy = llvm::FunctionType::get(PtrTy, CoroParamTys, false);
    auto *NewCoroFn =
        llvm::Function::Create(NewCoroTy, CoroBodyFn->getLinkage(), coroName + ".tmp", &M);
    NewCoroFn->setPresplitCoroutine();

    // Splice all blocks from old to new
    NewCoroFn->splice(NewCoroFn->begin(), CoroBodyFn);

    // Update argument references
    for (auto [OldArg, NewArg] : llvm::zip(CoroBodyFn->args(), NewCoroFn->args())) {
      OldArg.replaceAllUsesWith(&NewArg);
      NewArg.setName(OldArg.getName());
    }

    // Remove old function, rename new one
    std::string origCoroName = CoroBodyFn->getName().str();
    CoroBodyFn->replaceAllUsesWith(NewCoroFn);
    CoroBodyFn->eraseFromParent();
    NewCoroFn->setName(origCoroName);

    auto *CoroFn = NewCoroFn;

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    // Rewrite the init function: call the coroutine ramp, return handle
    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    {
      // Delete the stub body
      InitFn->deleteBody();
      auto *Entry = llvm::BasicBlock::Create(Ctx, "entry", InitFn);
      IRB.SetInsertPoint(Entry);

      // Call the coroutine ramp function (which is the __coro function)
      // Pass through the init function's parameters + a dummy promise ptr
      // (the coroutine creates its own promise via alloca)
      llvm::SmallVector<llvm::Value *, 4> Args;
      for (auto &Arg : InitFn->args())
        Args.push_back(&Arg);
      // The last argument is the promise ptr — pass null since the coro
      // function creates its own alloca for the promise
      Args.push_back(llvm::ConstantPointerNull::get(PtrTy));

      auto *Handle = IRB.CreateCall(CoroFn, Args, "gen.hdl");
      IRB.CreateRet(Handle);
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    // Rewrite __next: read promise value, then resume
    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    //
    // The coroutine stores the yielded value in the promise BEFORE
    // suspending.  __next reads the current value, then resumes the
    // coroutine to advance to the next yield (or to final suspend if
    // the generator is exhausted).
    {
      NextFn->deleteBody();
      auto *Entry = llvm::BasicBlock::Create(Ctx, "entry", NextFn);
      IRB.SetInsertPoint(Entry);

      auto *Handle = NextFn->getArg(0);

      // Read the promise value (stored before the current suspend)
      // coro.promise returns a ptr to the promise alloca
      // Use 8-byte alignment (standard for 64-bit types like i64, f64, ptr)
      auto *Promise = IRB.CreateCall(
          CoroPromiseFn,
          {Handle, llvm::ConstantInt::get(I32Ty, 8), llvm::ConstantInt::get(I1Ty, 0)},
          "promise.ptr");
      auto *Val = IRB.CreateLoad(YieldTy, Promise, "yield.val");

      // Resume the coroutine to advance to the next yield
      IRB.CreateCall(CoroResumeFn, {Handle});

      IRB.CreateRet(Val);
    }

    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    // Rewrite __done: call coro.done
    // ━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━
    {
      DoneFn->deleteBody();
      auto *Entry = llvm::BasicBlock::Create(Ctx, "entry", DoneFn);
      IRB.SetInsertPoint(Entry);

      auto *Handle = DoneFn->getArg(0);
      auto *Done = IRB.CreateCall(CoroDoneFn, {Handle}, "done");
      IRB.CreateRet(Done);
    }
  }

  // Remove the suspend marker function if it exists
  if (auto *marker = M.getFunction("__hew_coro_suspend")) {
    if (marker->use_empty())
      marker->eraseFromParent();
  }
}

// ── Step 3 & 4: Translate to LLVM IR ──────────────────────────────────────

std::unique_ptr<llvm::Module> Codegen::lowerToLLVMIR(mlir::ModuleOp module,
                                                     llvm::LLVMContext &llvmContext,
                                                     bool skipOptimization) {
  // Verify module before any lowering — catches malformed IR from MLIRGen
  if (mlir::failed(mlir::verify(module))) {
    llvm::errs() << "Error: module verification failed before lowering\n";
    dumpModuleAtFailure(module, "before lowering");
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
    dumpModuleAtFailure(module, "after Hew lowering");
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
    dumpModuleAtFailure(module, "after reconcile pass");
    return nullptr;
  }

  // Verify after full lowering
  if (mlir::failed(mlir::verify(module))) {
    llvm::errs() << "Error: module verification failed after LLVM lowering\n";
    dumpModuleAtFailure(module, "after LLVM lowering");
    return nullptr;
  }

  // Register dialect translations
  mlir::registerLLVMDialectTranslation(context);
  mlir::registerBuiltinDialectTranslation(context);

  // Translate to LLVM IR
  auto llvmModule = mlir::translateModuleToLLVMIR(module, llvmContext);
  if (!llvmModule) {
    llvm::errs() << "Error: MLIR to LLVM IR translation failed\n";
    dumpModuleAtFailure(module, "before LLVM IR translation");
    return nullptr;
  }

  // Transform coroutine-based generators: replace marker calls with actual
  // LLVM coroutine intrinsics.  This must run BEFORE the LLVM optimization
  // pipeline because CoroSplit (part of the default O3 pipeline) expects
  // the presplitcoroutine attribute and proper coro.suspend calls.
  transformCoroutineGenerators(*llvmModule);

  // Run coroutine passes unconditionally when generators are present.
  // These must run even in debug mode (skipOptimization) because LLVM
  // cannot select coroutine intrinsics without lowering them first.
  {
    bool hasCoroutines = false;
    for (auto &F : *llvmModule) {
      if (F.isPresplitCoroutine()) {
        hasCoroutines = true;
        break;
      }
    }
    if (hasCoroutines) {
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

      llvm::ModulePassManager MPM;
      MPM.addPass(llvm::CoroEarlyPass());
      llvm::CGSCCPassManager CGPM;
      CGPM.addPass(llvm::CoroSplitPass());
      MPM.addPass(llvm::createModuleToPostOrderCGSCCPassAdaptor(std::move(CGPM)));
      MPM.addPass(llvm::createModuleToFunctionPassAdaptor(llvm::CoroElidePass()));
      MPM.addPass(llvm::CoroCleanupPass());
      MPM.run(*llvmModule, MAM);
    }
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
  auto *target = llvm::TargetRegistry::lookupTarget(targetTriple, error);
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

// ── Full pipeline ─────────────────────────────────────────────────────────

std::unique_ptr<llvm::Module> Codegen::buildLLVMModule(mlir::ModuleOp module,
                                                       const CodegenOptions &opts,
                                                       llvm::LLVMContext &llvmContext) {
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
    auto *target = llvm::TargetRegistry::lookupTarget(triple, error);
    if (!target)
      throw std::runtime_error("cannot find target for triple '" + triple.str() +
                               "': " + error);
    llvm::TargetOptions tOpts;
    auto tm = target->createTargetMachine(triple, "generic", "", tOpts, llvm::Reloc::PIC_);
    if (!tm)
      throw std::runtime_error("cannot create target machine for triple '" +
                               triple.str() + "'");
    auto dl = tm->createDataLayout().getStringRepresentation();
    module->setAttr(mlir::LLVM::LLVMDialect::getDataLayoutAttrName(),
                    mlir::StringAttr::get(&context, dl));
  }

  // Reject unsupported ops early when targeting WASM.
  if (opts.target_triple.find("wasm") != std::string::npos) {
    if (mlir::failed(validateWasmUnsupportedOps(module)))
      return nullptr;
  }

  auto llvmModule = lowerToLLVMIR(module, llvmContext, opts.debug_info);
  if (!llvmModule)
    return nullptr;

  // Emit DWARF debug info when in debug mode and source path is available.
  // This wraps the raw debug locations (from MLIR FileLineColLoc) in proper
  // DI metadata (DICompileUnit, DISubprogram) so LLVM emits .debug_info.
  if (opts.debug_info && !opts.source_path.empty()) {
    hew::emitDebugInfo(*llvmModule, opts.source_path, opts.line_map);
  }

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

  return llvmModule;
}

int Codegen::compile(mlir::ModuleOp module, const CodegenOptions &opts) {
  llvm::LLVMContext llvmContext;
  auto llvmModule = buildLLVMModule(module, opts, llvmContext);
  if (!llvmModule)
    return 1;

  // If --emit-llvm, just print LLVM IR
  if (opts.emit_llvm_ir) {
    llvmModule->print(llvm::outs(), nullptr);
    return 0;
  }

  // Determine output path
  std::string outputPath = opts.output_path;
  if (outputPath.empty())
    outputPath = "output.o";

  // Emit object file — linking is handled by the Rust CLI (hew-cli/src/link.rs)
  return emitObjectFile(*llvmModule, outputPath, opts.target_triple);
}
