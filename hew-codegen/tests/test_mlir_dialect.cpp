//===- test_mlir_dialect.cpp - Tests for the Hew MLIR dialect -------------===//
//
// Verifies that the Hew MLIR dialect can be loaded and its operations
// can be created and verified.
//
//===----------------------------------------------------------------------===//

#include "hew/mlir/HewDialect.h"
#include "hew/mlir/HewOps.h"
#include "hew/mlir/HewTypes.h"

#include "mlir/Dialect/Func/IR/FuncOps.h"
#include "mlir/Dialect/LLVMIR/LLVMDialect.h"
#include "mlir/Dialect/LLVMIR/LLVMTypes.h"
#include "mlir/Interfaces/SideEffectInterfaces.h"
#include "mlir/IR/Builders.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/Diagnostics.h"
#include "mlir/IR/MLIRContext.h"
#include "mlir/IR/Verifier.h"
#include "mlir/Transforms/GreedyPatternRewriteDriver.h"

#include <cassert>
#include <cstdio>

static int tests_run = 0;
static int tests_passed = 0;

#define TEST(name)                                                                                 \
  do {                                                                                             \
    tests_run++;                                                                                   \
    printf("  test %s ... ", #name);                                                               \
  } while (0)

#define PASS()                                                                                     \
  do {                                                                                             \
    tests_passed++;                                                                                \
    printf("ok\n");                                                                                \
  } while (0)

#define FAIL(msg)                                                                                  \
  do {                                                                                             \
    printf("FAILED: %s\n", msg);                                                                   \
  } while (0)

//===----------------------------------------------------------------------===//
// Test: Load dialect into context
//===----------------------------------------------------------------------===//

static void test_dialect_loads() {
  TEST(dialect_loads);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();

  // Check that the dialect is registered
  auto *dialect = ctx.getLoadedDialect<hew::HewDialect>();
  if (!dialect) {
    FAIL("Hew dialect not loaded");
    return;
  }
  if (dialect->getNamespace() != "hew") {
    FAIL("Unexpected dialect namespace");
    return;
  }
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: Create hew.constant with integer value
//===----------------------------------------------------------------------===//

static void test_constant_i64() {
  TEST(constant_i64);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  // Create a module to hold the operation
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  // Create a function to hold the constant
  auto funcType = builder.getFunctionType({}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  // Create hew.constant 42 : i64
  auto i64Type = builder.getI64Type();
  auto constOp = builder.create<hew::ConstantOp>(loc, i64Type, int64_t(42));

  if (!constOp) {
    FAIL("Failed to create ConstantOp");
    module->destroy();
    return;
  }

  // Check result type
  if (constOp.getType() != i64Type) {
    FAIL("Unexpected result type");
    module->destroy();
    return;
  }

  // Check value attribute
  auto valAttr = llvm::dyn_cast<mlir::IntegerAttr>(constOp.getValue());
  if (!valAttr || valAttr.getInt() != 42) {
    FAIL("Unexpected value attribute");
    module->destroy();
    return;
  }

  // Add return terminator
  builder.create<mlir::func::ReturnOp>(loc);

  // Verify the module
  if (mlir::failed(mlir::verify(module))) {
    FAIL("Module verification failed");
    module->destroy();
    return;
  }

  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: Create hew.constant with float value
//===----------------------------------------------------------------------===//

static void test_constant_f64() {
  TEST(constant_f64);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto funcType = builder.getFunctionType({}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto f64Type = builder.getF64Type();
  auto constOp = builder.create<hew::ConstantOp>(loc, f64Type, 3.14);

  if (!constOp) {
    FAIL("Failed to create ConstantOp");
    module->destroy();
    return;
  }

  auto valAttr = llvm::dyn_cast<mlir::FloatAttr>(constOp.getValue());
  if (!valAttr) {
    FAIL("Value is not a FloatAttr");
    module->destroy();
    return;
  }

  builder.create<mlir::func::ReturnOp>(loc);

  if (mlir::failed(mlir::verify(module))) {
    FAIL("Module verification failed");
    module->destroy();
    return;
  }

  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: Create hew.constant with bool value
//===----------------------------------------------------------------------===//

static void test_constant_bool() {
  TEST(constant_bool);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto funcType = builder.getFunctionType({}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto constOp = builder.create<hew::ConstantOp>(loc, true);

  if (!constOp) {
    FAIL("Failed to create ConstantOp");
    module->destroy();
    return;
  }

  if (constOp.getType() != builder.getI1Type()) {
    FAIL("Bool constant should have i1 type");
    module->destroy();
    return;
  }

  builder.create<mlir::func::ReturnOp>(loc);

  if (mlir::failed(mlir::verify(module))) {
    FAIL("Module verification failed");
    module->destroy();
    return;
  }

  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: Create hew.global_string
//===----------------------------------------------------------------------===//

static void test_global_string() {
  TEST(global_string);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto gsOp = builder.create<hew::GlobalStringOp>(loc, builder.getStringAttr("str0"),
                                                  builder.getStringAttr("hello, world"));

  if (!gsOp) {
    FAIL("Failed to create GlobalStringOp");
    module->destroy();
    return;
  }

  if (gsOp.getSymName() != "str0") {
    FAIL("Unexpected symbol name");
    module->destroy();
    return;
  }

  if (gsOp.getValue() != "hello, world") {
    FAIL("Unexpected string value");
    module->destroy();
    return;
  }

  if (mlir::failed(mlir::verify(module))) {
    FAIL("Module verification failed");
    module->destroy();
    return;
  }

  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: Create hew.print
//===----------------------------------------------------------------------===//

static void test_print_op() {
  TEST(print_op);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto funcType = builder.getFunctionType({i64Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto arg = entryBlock->getArgument(0);
  builder.create<hew::PrintOp>(loc, arg, /*newline=*/builder.getBoolAttr(true));

  builder.create<mlir::func::ReturnOp>(loc);

  if (mlir::failed(mlir::verify(module))) {
    FAIL("Module verification failed");
    module->destroy();
    return;
  }

  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: hew.print reports write side effects
//===----------------------------------------------------------------------===//

static void test_print_op_effects() {
  TEST(print_op_effects);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto funcType = builder.getFunctionType({i64Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto printOp =
      builder.create<hew::PrintOp>(loc, entryBlock->getArgument(0), builder.getBoolAttr(true));

  auto effects = mlir::dyn_cast<mlir::MemoryEffectOpInterface>(printOp.getOperation());
  if (!effects || !effects.hasEffect<mlir::MemoryEffects::Write>() || effects.hasNoEffect()) {
    FAIL("PrintOp should report write memory effects");
    module->destroy();
    return;
  }

  builder.create<mlir::func::ReturnOp>(loc);

  if (mlir::failed(mlir::verify(module))) {
    FAIL("Module verification failed");
    module->destroy();
    return;
  }

  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: Create hew.cast
//===----------------------------------------------------------------------===//

static void test_cast_op() {
  TEST(cast_op);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i32Type = builder.getI32Type();
  auto f64Type = builder.getF64Type();
  auto funcType = builder.getFunctionType({i32Type}, {f64Type});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto arg = entryBlock->getArgument(0);
  auto castOp = builder.create<hew::CastOp>(loc, f64Type, arg);

  if (!castOp) {
    FAIL("Failed to create CastOp");
    module->destroy();
    return;
  }

  if (castOp.getType() != f64Type) {
    FAIL("Unexpected result type");
    module->destroy();
    return;
  }

  builder.create<mlir::func::ReturnOp>(loc, mlir::ValueRange{castOp.getResult()});

  if (mlir::failed(mlir::verify(module))) {
    FAIL("Module verification failed");
    module->destroy();
    return;
  }

  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: Create hew.struct_init and hew.field_get
//===----------------------------------------------------------------------===//

static void test_struct_ops() {
  TEST(struct_init_and_field_get);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto funcType = builder.getFunctionType({i64Type, i64Type}, {i64Type});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto x = entryBlock->getArgument(0);
  auto y = entryBlock->getArgument(1);

  // Use i64 as struct type for simplicity (real lowering would use LLVM struct types)
  // For now, we just test op creation and verifier
  auto structType = mlir::LLVM::LLVMStructType::getLiteral(&ctx, {i64Type, i64Type});

  // Create hew.struct_init
  auto fieldNames = builder.getStrArrayAttr({"x", "y"});
  auto structName = builder.getStringAttr("Point");
  auto structInit = builder.create<hew::StructInitOp>(loc, structType, mlir::ValueRange{x, y},
                                                      fieldNames, structName);

  if (!structInit) {
    FAIL("Failed to create StructInitOp");
    module->destroy();
    return;
  }

  // Create hew.field_get to extract field "x" (index 0)
  auto fieldGet =
      builder.create<hew::FieldGetOp>(loc, i64Type, structInit.getResult(),
                                      builder.getStringAttr("x"), builder.getI64IntegerAttr(0));

  if (!fieldGet) {
    FAIL("Failed to create FieldGetOp");
    module->destroy();
    return;
  }

  // Create hew.field_set to produce new struct with field "x" replaced
  auto newX = builder.create<hew::ConstantOp>(loc, i64Type, int64_t(99));
  auto fieldSet =
      builder.create<hew::FieldSetOp>(loc, structType, structInit.getResult(), newX.getResult(),
                                      builder.getStringAttr("x"), builder.getI64IntegerAttr(0));

  if (!fieldSet) {
    FAIL("Failed to create FieldSetOp");
    module->destroy();
    return;
  }

  // Extract from the updated struct to verify it chains
  auto fieldGet2 = builder.create<hew::FieldGetOp>(
      loc, i64Type, fieldSet.getResult(), builder.getStringAttr("x"), builder.getI64IntegerAttr(0));

  builder.create<mlir::func::ReturnOp>(loc, mlir::ValueRange{fieldGet2.getResult()});

  if (mlir::failed(mlir::verify(module))) {
    FAIL("Module verification failed");
    module->destroy();
    return;
  }

  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: FieldGetOp folds through struct_init/field_set producers
//===----------------------------------------------------------------------===//

static void test_field_get_fold() {
  TEST(field_get_fold);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto structType = mlir::LLVM::LLVMStructType::getLiteral(&ctx, {i64Type, i64Type});
  auto funcType = builder.getFunctionType({i64Type, i64Type}, {i64Type});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto x = entryBlock->getArgument(0);
  auto y = entryBlock->getArgument(1);
  auto structInit = builder.create<hew::StructInitOp>(loc, structType, mlir::ValueRange{x, y},
                                                      builder.getStrArrayAttr({"x", "y"}),
                                                      builder.getStringAttr("Point"));

  auto getY =
      builder.create<hew::FieldGetOp>(loc, i64Type, structInit.getResult(),
                                      builder.getStringAttr("y"), builder.getI64IntegerAttr(1));
  auto foldFromInit = getY.fold(hew::FieldGetOp::FoldAdaptor({}, getY));
  auto foldedY = llvm::dyn_cast<mlir::Value>(foldFromInit);
  if (!foldedY || foldedY != y) {
    FAIL("FieldGetOp should fold to struct_init operand");
    module->destroy();
    return;
  }

  auto replacement = builder.create<hew::ConstantOp>(loc, i64Type, int64_t(99));
  auto setX = builder.create<hew::FieldSetOp>(loc, structType, structInit.getResult(),
                                              replacement.getResult(), builder.getStringAttr("x"),
                                              builder.getI64IntegerAttr(0));
  auto getX = builder.create<hew::FieldGetOp>(
      loc, i64Type, setX.getResult(), builder.getStringAttr("x"), builder.getI64IntegerAttr(0));
  auto foldFromSet = getX.fold(hew::FieldGetOp::FoldAdaptor({}, getX));
  auto foldedX = llvm::dyn_cast<mlir::Value>(foldFromSet);
  if (!foldedX || foldedX != replacement.getResult()) {
    FAIL("FieldGetOp should fold to matching field_set value");
    module->destroy();
    return;
  }

  builder.create<mlir::func::ReturnOp>(loc, mlir::ValueRange{getX.getResult()});

  if (mlir::failed(mlir::verify(module))) {
    FAIL("Module verification failed");
    module->destroy();
    return;
  }

  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: Module dump (print MLIR IR)
//===----------------------------------------------------------------------===//

static void test_module_dump() {
  TEST(module_dump);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  // Add a global string
  builder.create<hew::GlobalStringOp>(loc, builder.getStringAttr("greeting"),
                                      builder.getStringAttr("hello from hew!"));

  // Add a function with a constant and print
  auto i64Type = builder.getI64Type();
  auto funcType = builder.getFunctionType({}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "main", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto constVal = builder.create<hew::ConstantOp>(loc, i64Type, int64_t(42));
  builder.create<hew::PrintOp>(loc, constVal.getResult(), builder.getBoolAttr(true));
  builder.create<mlir::func::ReturnOp>(loc);

  // Verify
  if (mlir::failed(mlir::verify(module))) {
    FAIL("Module verification failed");
    module->destroy();
    return;
  }

  // Dump to check it looks right (visual check)
  printf("\n--- MLIR Module Dump ---\n");
  module->dump();
  printf("--- End Dump ---\n");

  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: Verifier rejects StructInitOp with wrong field count
//===----------------------------------------------------------------------===//

static void test_struct_init_verifier_field_count() {
  TEST(struct_init_verifier_field_count);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  // Struct has 2 fields but we provide 3 operands
  auto structType = mlir::LLVM::LLVMStructType::getLiteral(&ctx, {i64Type, i64Type});
  auto funcType = builder.getFunctionType({i64Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto x = entryBlock->getArgument(0);
  // 3 operands for a 2-field struct — should fail verification
  builder.create<hew::StructInitOp>(loc, structType, mlir::ValueRange{x, x, x},
                                    builder.getStrArrayAttr({"a", "b", "c"}),
                                    builder.getStringAttr("Bad"));
  builder.create<mlir::func::ReturnOp>(loc);

  // Suppress diagnostic output for expected errors
  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject struct_init with mismatched field count");
    module->destroy();
    return;
  }

  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: Verifier rejects FieldGetOp with out-of-bounds index
//===----------------------------------------------------------------------===//

static void test_field_get_verifier_bounds() {
  TEST(field_get_verifier_bounds);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto structType = mlir::LLVM::LLVMStructType::getLiteral(&ctx, {i64Type, i64Type});
  auto funcType = builder.getFunctionType({}, {i64Type});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto x = builder.create<hew::ConstantOp>(loc, i64Type, int64_t(1));
  auto y = builder.create<hew::ConstantOp>(loc, i64Type, int64_t(2));
  auto s = builder.create<hew::StructInitOp>(loc, structType, mlir::ValueRange{x, y},
                                             builder.getStrArrayAttr({"x", "y"}),
                                             builder.getStringAttr("P"));
  // Index 5 is out of bounds for a 2-field struct
  auto fg = builder.create<hew::FieldGetOp>(loc, i64Type, s.getResult(), builder.getStringAttr("z"),
                                            builder.getI64IntegerAttr(5));
  builder.create<mlir::func::ReturnOp>(loc, mlir::ValueRange{fg.getResult()});

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject field_get with out-of-bounds index");
    module->destroy();
    return;
  }

  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: Verifier rejects StructInitOp with mismatched field type
//===----------------------------------------------------------------------===//

static void test_struct_init_verifier_field_type() {
  TEST(struct_init_verifier_field_type);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto f64Type = builder.getF64Type();
  // Struct expects (i64, i64) but we provide (i64, f64)
  auto structType = mlir::LLVM::LLVMStructType::getLiteral(&ctx, {i64Type, i64Type});
  auto funcType = builder.getFunctionType({}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto x = builder.create<hew::ConstantOp>(loc, i64Type, int64_t(1));
  auto y = builder.create<hew::ConstantOp>(loc, f64Type, 3.14);
  builder.create<hew::StructInitOp>(loc, structType, mlir::ValueRange{x, y},
                                    builder.getStrArrayAttr({"a", "b"}),
                                    builder.getStringAttr("Bad"));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject struct_init with mismatched field type");
    module->destroy();
    return;
  }

  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: TupleExtractOp folds through tuple.create
//===----------------------------------------------------------------------===//

static void test_tuple_extract_fold() {
  TEST(tuple_extract_fold);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto f64Type = builder.getF64Type();
  auto tupleType = hew::HewTupleType::get(&ctx, {i64Type, f64Type, i64Type});
  auto funcType = builder.getFunctionType({i64Type, f64Type, i64Type}, {f64Type});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto a = entryBlock->getArgument(0);
  auto b = entryBlock->getArgument(1);
  auto c = entryBlock->getArgument(2);

  auto tupleCreate = builder.create<hew::TupleCreateOp>(loc, tupleType, mlir::ValueRange{a, b, c});

  // Extract element 1 (the f64)
  auto extract = builder.create<hew::TupleExtractOp>(loc, f64Type, tupleCreate.getResult(),
                                                     builder.getI64IntegerAttr(1));

  // Test the fold
  auto foldResult = extract.fold(hew::TupleExtractOp::FoldAdaptor({}, extract));
  auto foldedVal = llvm::dyn_cast<mlir::Value>(foldResult);
  if (!foldedVal || foldedVal != b) {
    FAIL("TupleExtractOp should fold to the corresponding tuple.create element");
    module->destroy();
    return;
  }

  builder.create<mlir::func::ReturnOp>(loc, mlir::ValueRange{extract.getResult()});

  if (mlir::failed(mlir::verify(module))) {
    FAIL("Module verification failed");
    module->destroy();
    return;
  }

  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: ArrayExtractOp folds through array.create
//===----------------------------------------------------------------------===//

static void test_array_extract_fold() {
  TEST(array_extract_fold);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto arrayType = hew::HewArrayType::get(&ctx, i64Type, 3);
  auto funcType = builder.getFunctionType({i64Type, i64Type, i64Type}, {i64Type});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto a = entryBlock->getArgument(0);
  auto b = entryBlock->getArgument(1);
  auto c = entryBlock->getArgument(2);

  auto arrCreate = builder.create<hew::ArrayCreateOp>(loc, arrayType, mlir::ValueRange{a, b, c});

  // Extract element 2 (the third i64)
  auto extract = builder.create<hew::ArrayExtractOp>(loc, i64Type, arrCreate.getResult(),
                                                     builder.getI64IntegerAttr(2));

  auto foldResult = extract.fold(hew::ArrayExtractOp::FoldAdaptor({}, extract));
  auto foldedVal = llvm::dyn_cast<mlir::Value>(foldResult);
  if (!foldedVal || foldedVal != c) {
    FAIL("ArrayExtractOp should fold to the corresponding array.create element");
    module->destroy();
    return;
  }

  builder.create<mlir::func::ReturnOp>(loc, mlir::ValueRange{extract.getResult()});

  if (mlir::failed(mlir::verify(module))) {
    FAIL("Module verification failed");
    module->destroy();
    return;
  }

  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: EnumExtractTagOp folds through enum_construct (constant fold)
//===----------------------------------------------------------------------===//

static void test_enum_extract_tag_fold() {
  TEST(enum_extract_tag_fold);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();
  // Enum as LLVM struct: { tag: i32, payload: i64 }
  auto enumType = mlir::LLVM::LLVMStructType::getLiteral(&ctx, {i32Type, i64Type});
  auto funcType = builder.getFunctionType({i64Type}, {i32Type});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto payload = entryBlock->getArgument(0);

  // Construct enum with variant_index=2
  auto enumConstruct = builder.create<hew::EnumConstructOp>(
      loc, enumType, uint32_t(2), "MyEnum", mlir::ValueRange{payload},
      /*payload_positions=*/mlir::ArrayAttr());

  auto extractTag = builder.create<hew::EnumExtractTagOp>(loc, i32Type, enumConstruct.getResult());

  // Fold should produce a constant IntegerAttr(2)
  auto foldResult = extractTag.fold(hew::EnumExtractTagOp::FoldAdaptor({}, extractTag));
  auto foldedAttr = llvm::dyn_cast<mlir::Attribute>(foldResult);
  if (!foldedAttr) {
    FAIL("EnumExtractTagOp should constant-fold to IntegerAttr");
    module->destroy();
    return;
  }
  auto intAttr = llvm::dyn_cast<mlir::IntegerAttr>(foldedAttr);
  if (!intAttr || intAttr.getInt() != 2) {
    FAIL("Folded tag should be 2");
    module->destroy();
    return;
  }

  builder.create<mlir::func::ReturnOp>(loc, mlir::ValueRange{extractTag.getResult()});

  if (mlir::failed(mlir::verify(module))) {
    FAIL("Module verification failed");
    module->destroy();
    return;
  }

  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: ClosureGetFnOp/ClosureGetEnvOp fold through closure.create
//===----------------------------------------------------------------------===//

static void test_closure_fold() {
  TEST(closure_get_fn_env_fold);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto ptrType = mlir::LLVM::LLVMPointerType::get(&ctx);
  auto i64Type = builder.getI64Type();
  auto closureType = hew::ClosureType::get(&ctx, {i64Type}, i64Type);
  auto funcType = builder.getFunctionType({ptrType, ptrType}, {ptrType});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto fnPtr = entryBlock->getArgument(0);
  auto envPtr = entryBlock->getArgument(1);

  auto closureCreate = builder.create<hew::ClosureCreateOp>(loc, closureType, fnPtr, envPtr);

  auto getFn = builder.create<hew::ClosureGetFnOp>(loc, ptrType, closureCreate.getResult());
  auto getEnv = builder.create<hew::ClosureGetEnvOp>(loc, ptrType, closureCreate.getResult());

  // Test GetFn fold
  auto fnFold = getFn.fold(hew::ClosureGetFnOp::FoldAdaptor({}, getFn));
  auto foldedFn = llvm::dyn_cast<mlir::Value>(fnFold);
  if (!foldedFn || foldedFn != fnPtr) {
    FAIL("ClosureGetFnOp should fold to fn_ptr operand");
    module->destroy();
    return;
  }

  // Test GetEnv fold
  auto envFold = getEnv.fold(hew::ClosureGetEnvOp::FoldAdaptor({}, getEnv));
  auto foldedEnv = llvm::dyn_cast<mlir::Value>(envFold);
  if (!foldedEnv || foldedEnv != envPtr) {
    FAIL("ClosureGetEnvOp should fold to env_ptr operand");
    module->destroy();
    return;
  }

  builder.create<mlir::func::ReturnOp>(loc, mlir::ValueRange{getFn.getResult()});

  if (mlir::failed(mlir::verify(module))) {
    FAIL("Module verification failed");
    module->destroy();
    return;
  }

  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: TraitObjectDataOp/TraitObjectTagOp fold through trait_object.create
//===----------------------------------------------------------------------===//

static void test_trait_object_fold() {
  TEST(trait_object_data_tag_fold);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto ptrType = mlir::LLVM::LLVMPointerType::get(&ctx);
  auto traitObjType = hew::HewTraitObjectType::get(&ctx, "TestTrait");
  auto funcType = builder.getFunctionType({ptrType, ptrType}, {ptrType});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto data = entryBlock->getArgument(0);
  auto vtablePtr = entryBlock->getArgument(1);

  auto create = builder.create<hew::TraitObjectCreateOp>(loc, traitObjType, data, vtablePtr);

  auto getData = builder.create<hew::TraitObjectDataOp>(loc, ptrType, create.getResult());
  auto getTag = builder.create<hew::TraitObjectTagOp>(loc, ptrType, create.getResult());

  auto dataFold = getData.fold(hew::TraitObjectDataOp::FoldAdaptor({}, getData));
  auto foldedData = llvm::dyn_cast<mlir::Value>(dataFold);
  if (!foldedData || foldedData != data) {
    FAIL("TraitObjectDataOp should fold to data operand");
    module->destroy();
    return;
  }

  auto tagFold = getTag.fold(hew::TraitObjectTagOp::FoldAdaptor({}, getTag));
  auto foldedTag = llvm::dyn_cast<mlir::Value>(tagFold);
  if (!foldedTag || foldedTag != vtablePtr) {
    FAIL("TraitObjectTagOp should fold to vtable_ptr operand");
    module->destroy();
    return;
  }

  builder.create<mlir::func::ReturnOp>(loc, mlir::ValueRange{getData.getResult()});

  if (mlir::failed(mlir::verify(module))) {
    FAIL("Module verification failed");
    module->destroy();
    return;
  }

  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: Collection ops report correct memory effects
//===----------------------------------------------------------------------===//

static void test_collection_effects() {
  TEST(collection_memory_effects);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto vecType = hew::VecType::get(&ctx, i64Type);
  auto funcType = builder.getFunctionType({vecType, i64Type}, {i64Type});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto vec = entryBlock->getArgument(0);
  auto val = entryBlock->getArgument(1);

  // VecNewOp — should have MemWrite (allocation)
  auto vecNew = builder.create<hew::VecNewOp>(loc, vecType);
  auto vecNewEffects = mlir::dyn_cast<mlir::MemoryEffectOpInterface>(vecNew.getOperation());
  if (!vecNewEffects || !vecNewEffects.hasEffect<mlir::MemoryEffects::Write>()) {
    FAIL("VecNewOp should have MemWrite effect");
    module->destroy();
    return;
  }
  // VecNewOp should NOT be Pure
  if (vecNewEffects.hasNoEffect()) {
    FAIL("VecNewOp should NOT be pure");
    module->destroy();
    return;
  }

  // VecPushOp — should have both MemRead and MemWrite
  auto pushOp = builder.create<hew::VecPushOp>(loc, vec, val);
  auto pushEffects = mlir::dyn_cast<mlir::MemoryEffectOpInterface>(pushOp.getOperation());
  if (!pushEffects || !pushEffects.hasEffect<mlir::MemoryEffects::Write>() ||
      !pushEffects.hasEffect<mlir::MemoryEffects::Read>()) {
    FAIL("VecPushOp should have both MemRead and MemWrite");
    module->destroy();
    return;
  }

  // VecLenOp — should have MemRead only
  auto lenOp = builder.create<hew::VecLenOp>(loc, i64Type, vec);
  auto lenEffects = mlir::dyn_cast<mlir::MemoryEffectOpInterface>(lenOp.getOperation());
  if (!lenEffects || !lenEffects.hasEffect<mlir::MemoryEffects::Read>()) {
    FAIL("VecLenOp should have MemRead effect");
    module->destroy();
    return;
  }
  if (lenEffects.hasEffect<mlir::MemoryEffects::Write>()) {
    FAIL("VecLenOp should NOT have MemWrite");
    module->destroy();
    return;
  }

  // VecGetOp — should have MemRead only
  auto getOp = builder.create<hew::VecGetOp>(loc, i64Type, vec, val);
  auto getEffects = mlir::dyn_cast<mlir::MemoryEffectOpInterface>(getOp.getOperation());
  if (!getEffects || !getEffects.hasEffect<mlir::MemoryEffects::Read>()) {
    FAIL("VecGetOp should have MemRead effect");
    module->destroy();
    return;
  }
  if (getEffects.hasEffect<mlir::MemoryEffects::Write>()) {
    FAIL("VecGetOp should NOT have MemWrite");
    module->destroy();
    return;
  }

  builder.create<mlir::func::ReturnOp>(loc, mlir::ValueRange{lenOp.getResult()});
  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: Pure ops report no side effects (enable CSE/DCE)
//===----------------------------------------------------------------------===//

static void test_pure_ops_no_effects() {
  TEST(pure_ops_no_effects);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto f64Type = builder.getF64Type();
  auto structType = mlir::LLVM::LLVMStructType::getLiteral(&ctx, {i64Type, i64Type});
  auto tupleType = mlir::LLVM::LLVMStructType::getLiteral(&ctx, {i64Type, f64Type});
  auto funcType = builder.getFunctionType({i64Type, f64Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto x = entryBlock->getArgument(0);
  auto y = entryBlock->getArgument(1);

  // ConstantOp — Pure
  auto constOp = builder.create<hew::ConstantOp>(loc, i64Type, int64_t(42));
  auto constEffects = mlir::dyn_cast<mlir::MemoryEffectOpInterface>(constOp.getOperation());
  if (!constEffects || !constEffects.hasNoEffect()) {
    FAIL("ConstantOp should be Pure (no effects)");
    module->destroy();
    return;
  }

  // CastOp — Pure
  auto castOp = builder.create<hew::CastOp>(loc, f64Type, x);
  auto castEffects = mlir::dyn_cast<mlir::MemoryEffectOpInterface>(castOp.getOperation());
  if (!castEffects || !castEffects.hasNoEffect()) {
    FAIL("CastOp should be Pure (no effects)");
    module->destroy();
    return;
  }

  // StructInitOp — Pure
  auto structInit = builder.create<hew::StructInitOp>(loc, structType, mlir::ValueRange{x, x},
                                                      builder.getStrArrayAttr({"a", "b"}),
                                                      builder.getStringAttr("S"));
  auto structEffects = mlir::dyn_cast<mlir::MemoryEffectOpInterface>(structInit.getOperation());
  if (!structEffects || !structEffects.hasNoEffect()) {
    FAIL("StructInitOp should be Pure (no effects)");
    module->destroy();
    return;
  }

  // TupleCreateOp — Pure
  auto tupleCreate = builder.create<hew::TupleCreateOp>(loc, tupleType, mlir::ValueRange{x, y});
  auto tupleEffects = mlir::dyn_cast<mlir::MemoryEffectOpInterface>(tupleCreate.getOperation());
  if (!tupleEffects || !tupleEffects.hasNoEffect()) {
    FAIL("TupleCreateOp should be Pure (no effects)");
    module->destroy();
    return;
  }

  builder.create<mlir::func::ReturnOp>(loc);
  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: CastOp chain canonicalization
//===----------------------------------------------------------------------===//

static void test_cast_chain_canonicalization() {
  TEST(cast_chain_canonicalization);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();
  auto f64Type = builder.getF64Type();
  auto funcType = builder.getFunctionType({i32Type}, {f64Type});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  // Build: cast(cast(arg : i32, i64), f64)
  auto arg = entryBlock->getArgument(0);
  auto cast1 = builder.create<hew::CastOp>(loc, i64Type, arg);
  auto cast2 = builder.create<hew::CastOp>(loc, f64Type, cast1.getResult());
  builder.create<mlir::func::ReturnOp>(loc, mlir::ValueRange{cast2.getResult()});

  // Run canonicalization patterns
  mlir::RewritePatternSet patterns(&ctx);
  hew::CastOp::getCanonicalizationPatterns(patterns, &ctx);

  mlir::GreedyRewriteConfig config;
  config.setMaxIterations(10);
  (void)mlir::applyPatternsGreedily(func, std::move(patterns), config);

  // After canonicalization the chain should be collapsed:
  // the single remaining cast should take arg (i32) → f64 directly.
  bool found = false;
  func.walk([&](hew::CastOp op) {
    if (op.getInput() == arg && op.getResult().getType() == f64Type)
      found = true;
  });

  if (!found) {
    FAIL("Cast chain was not collapsed to cast(arg, f64)");
    module->destroy();
    return;
  }

  // Ensure the intermediate cast (i32 → i64) was eliminated
  int castCount = 0;
  func.walk([&](hew::CastOp) { castCount++; });
  if (castCount != 1) {
    FAIL("Expected exactly 1 CastOp after canonicalization");
    module->destroy();
    return;
  }

  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: Dead Vec pair elimination (vec.new → vec.free with no other uses)
//===----------------------------------------------------------------------===//

static void test_dead_vec_elimination() {
  TEST(dead_vec_elimination);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i32Type = builder.getI32Type();
  auto vecType = hew::VecType::get(&ctx, i32Type);
  auto funcType = builder.getFunctionType({}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto vecNew = builder.create<hew::VecNewOp>(loc, vecType);
  builder.create<hew::VecFreeOp>(loc, vecNew.getResult());
  builder.create<mlir::func::ReturnOp>(loc);

  // Run canonicalization
  mlir::RewritePatternSet patterns(&ctx);
  hew::VecFreeOp::getCanonicalizationPatterns(patterns, &ctx);

  mlir::GreedyRewriteConfig config;
  config.setMaxIterations(10);
  (void)mlir::applyPatternsGreedily(func, std::move(patterns), config);

  // Both vec.new and vec.free should be eliminated
  int vecNewCount = 0, vecFreeCount = 0;
  func.walk([&](hew::VecNewOp) { vecNewCount++; });
  func.walk([&](hew::VecFreeOp) { vecFreeCount++; });

  if (vecNewCount != 0 || vecFreeCount != 0) {
    FAIL("Dead vec.new/vec.free pair should be eliminated");
    module->destroy();
    return;
  }

  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: Dead HashMap pair elimination (hashmap.new → hashmap.free, no uses)
//===----------------------------------------------------------------------===//

static void test_dead_hashmap_elimination() {
  TEST(dead_hashmap_elimination);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i32Type = builder.getI32Type();
  auto mapType = hew::HashMapType::get(&ctx, i32Type, i32Type);
  auto funcType = builder.getFunctionType({}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto mapNew = builder.create<hew::HashMapNewOp>(loc, mapType);
  builder.create<hew::HashMapFreeOp>(loc, mapNew.getResult());
  builder.create<mlir::func::ReturnOp>(loc);

  // Run canonicalization
  mlir::RewritePatternSet patterns(&ctx);
  hew::HashMapFreeOp::getCanonicalizationPatterns(patterns, &ctx);

  mlir::GreedyRewriteConfig config;
  config.setMaxIterations(10);
  (void)mlir::applyPatternsGreedily(func, std::move(patterns), config);

  // Both hashmap.new and hashmap.free should be eliminated
  int mapNewCount = 0, mapFreeCount = 0;
  func.walk([&](hew::HashMapNewOp) { mapNewCount++; });
  func.walk([&](hew::HashMapFreeOp) { mapFreeCount++; });

  if (mapNewCount != 0 || mapFreeCount != 0) {
    FAIL("Dead hashmap.new/hashmap.free pair should be eliminated");
    module->destroy();
    return;
  }

  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: StringConcatOp identity elimination (concat with "")
//===----------------------------------------------------------------------===//

static void test_string_concat_identity_canonicalization() {
  TEST(string_concat_identity_canonicalization);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  // Create global strings: one non-empty, one empty
  builder.create<hew::GlobalStringOp>(loc, builder.getStringAttr("str_hello"),
                                      builder.getStringAttr("hello"));
  builder.create<hew::GlobalStringOp>(loc, builder.getStringAttr("str_empty"),
                                      builder.getStringAttr(""));

  auto strRefType = hew::StringRefType::get(&ctx);
  auto funcType = builder.getFunctionType({}, {strRefType});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  // %hello = hew.constant "str_hello" : !hew.string_ref
  auto hello = builder.create<hew::ConstantOp>(loc, strRefType, builder.getStringAttr("str_hello"));
  // %empty = hew.constant "str_empty" : !hew.string_ref
  auto empty = builder.create<hew::ConstantOp>(loc, strRefType, builder.getStringAttr("str_empty"));

  // %result = hew.string_concat %hello, %empty
  auto concat =
      builder.create<hew::StringConcatOp>(loc, strRefType, hello.getResult(), empty.getResult());

  builder.create<mlir::func::ReturnOp>(loc, mlir::ValueRange{concat.getResult()});

  // Run canonicalization
  mlir::RewritePatternSet patterns(&ctx);
  hew::StringConcatOp::getCanonicalizationPatterns(patterns, &ctx);

  mlir::GreedyRewriteConfig config;
  config.setMaxIterations(10);
  (void)mlir::applyPatternsGreedily(func, std::move(patterns), config);

  // The concat should be eliminated — no StringConcatOp remaining
  int concatCount = 0;
  func.walk([&](hew::StringConcatOp) { concatCount++; });

  if (concatCount != 0) {
    FAIL("concat(x, \"\") should be eliminated by canonicalizer");
    module->destroy();
    return;
  }

  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: StringConcatOp identity elimination — LHS empty
//===----------------------------------------------------------------------===//

static void test_string_concat_identity_lhs_empty() {
  TEST(string_concat_identity_lhs_empty);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  builder.create<hew::GlobalStringOp>(loc, builder.getStringAttr("str_hello"),
                                      builder.getStringAttr("hello"));
  builder.create<hew::GlobalStringOp>(loc, builder.getStringAttr("str_empty"),
                                      builder.getStringAttr(""));

  auto strRefType = hew::StringRefType::get(&ctx);
  auto funcType = builder.getFunctionType({}, {strRefType});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto empty = builder.create<hew::ConstantOp>(loc, strRefType, builder.getStringAttr("str_empty"));
  auto hello = builder.create<hew::ConstantOp>(loc, strRefType, builder.getStringAttr("str_hello"));

  // concat("", hello) — LHS is empty
  auto concat =
      builder.create<hew::StringConcatOp>(loc, strRefType, empty.getResult(), hello.getResult());
  builder.create<mlir::func::ReturnOp>(loc, mlir::ValueRange{concat.getResult()});

  mlir::RewritePatternSet patterns(&ctx);
  hew::StringConcatOp::getCanonicalizationPatterns(patterns, &ctx);

  mlir::GreedyRewriteConfig config;
  config.setMaxIterations(10);
  (void)mlir::applyPatternsGreedily(func, std::move(patterns), config);

  int concatCount = 0;
  func.walk([&](hew::StringConcatOp) { concatCount++; });

  if (concatCount != 0) {
    FAIL("concat(\"\", x) should be eliminated by canonicalizer");
    module->destroy();
    return;
  }

  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: CastOp identity fold — cast(x:i32, i32) → x
//===----------------------------------------------------------------------===//

static void test_cast_identity_fold() {
  TEST(cast_identity_fold);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i32Type = builder.getI32Type();
  auto funcType = builder.getFunctionType({i32Type}, {i32Type});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto arg = entryBlock->getArgument(0);
  auto cast = builder.create<hew::CastOp>(loc, i32Type, arg);

  auto foldResult = cast.fold(hew::CastOp::FoldAdaptor({}, cast));
  auto foldedVal = llvm::dyn_cast<mlir::Value>(foldResult);
  if (!foldedVal || foldedVal != arg) {
    FAIL("Identity cast should fold to input value");
    module->destroy();
    return;
  }

  builder.create<mlir::func::ReturnOp>(loc, mlir::ValueRange{cast.getResult()});
  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: CastOp constant fold — int→int, int→float, float→int
//===----------------------------------------------------------------------===//

static void test_cast_constant_fold() {
  TEST(cast_constant_fold);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();
  auto f64Type = builder.getF64Type();
  auto funcType = builder.getFunctionType({}, {i64Type});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  // Test 1: int→wider int (42:i32 → i64)
  auto const42 = builder.create<hew::ConstantOp>(loc, i32Type, int64_t(42));
  auto castToI64 = builder.create<hew::CastOp>(loc, i64Type, const42.getResult());

  mlir::Attribute i32Attr = mlir::IntegerAttr::get(i32Type, 42);
  llvm::ArrayRef<mlir::Attribute> constOperands(i32Attr);
  auto foldResult1 = castToI64.fold(hew::CastOp::FoldAdaptor(constOperands, castToI64));
  auto foldedAttr1 = llvm::dyn_cast<mlir::Attribute>(foldResult1);
  if (!foldedAttr1) {
    FAIL("cast(42:i32, i64) should constant-fold");
    module->destroy();
    return;
  }
  auto intAttr1 = llvm::dyn_cast<mlir::IntegerAttr>(foldedAttr1);
  if (!intAttr1 || intAttr1.getInt() != 42 || intAttr1.getType() != i64Type) {
    FAIL("cast(42:i32, i64) should fold to 42:i64");
    module->destroy();
    return;
  }

  // Test 2: int→float (42:i32 → f64)
  auto castToF64 = builder.create<hew::CastOp>(loc, f64Type, const42.getResult());
  auto foldResult2 = castToF64.fold(hew::CastOp::FoldAdaptor(constOperands, castToF64));
  auto foldedAttr2 = llvm::dyn_cast<mlir::Attribute>(foldResult2);
  if (!foldedAttr2) {
    FAIL("cast(42:i32, f64) should constant-fold");
    module->destroy();
    return;
  }
  auto floatAttr = llvm::dyn_cast<mlir::FloatAttr>(foldedAttr2);
  if (!floatAttr || floatAttr.getValueAsDouble() != 42.0) {
    FAIL("cast(42:i32, f64) should fold to 42.0:f64");
    module->destroy();
    return;
  }

  // Test 3: float→int (3.14:f64 → i32)
  auto const3_14 = builder.create<hew::ConstantOp>(loc, f64Type, 3.14);
  auto castToI32 = builder.create<hew::CastOp>(loc, i32Type, const3_14.getResult());
  mlir::Attribute f64Attr = mlir::FloatAttr::get(f64Type, 3.14);
  llvm::ArrayRef<mlir::Attribute> floatOperands(f64Attr);
  auto foldResult3 = castToI32.fold(hew::CastOp::FoldAdaptor(floatOperands, castToI32));
  auto foldedAttr3 = llvm::dyn_cast<mlir::Attribute>(foldResult3);
  if (!foldedAttr3) {
    FAIL("cast(3.14:f64, i32) should constant-fold");
    module->destroy();
    return;
  }
  auto intAttr3 = llvm::dyn_cast<mlir::IntegerAttr>(foldedAttr3);
  if (!intAttr3 || intAttr3.getInt() != 3) {
    FAIL("cast(3.14:f64, i32) should fold to 3:i32");
    module->destroy();
    return;
  }

  builder.create<mlir::func::ReturnOp>(loc, mlir::ValueRange{castToI64.getResult()});
  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: EnumExtractPayloadOp fold through enum.construct
//===----------------------------------------------------------------------===//

static void test_enum_extract_payload_fold() {
  TEST(enum_extract_payload_fold);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();
  // Enum as LLVM struct: { tag: i32, payload0: i64 }
  auto enumType = mlir::LLVM::LLVMStructType::getLiteral(&ctx, {i32Type, i64Type});
  auto funcType = builder.getFunctionType({i64Type}, {i64Type});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto payload = entryBlock->getArgument(0);

  // Construct enum with variant_index=1, payload at position 1
  auto positionsAttr = builder.getArrayAttr({builder.getI64IntegerAttr(1)});
  auto enumConstruct = builder.create<hew::EnumConstructOp>(
      loc, enumType, uint32_t(1), "MyEnum", mlir::ValueRange{payload}, positionsAttr);

  // Extract payload at field_index=1
  auto extractPayload = builder.create<hew::EnumExtractPayloadOp>(
      loc, i64Type, enumConstruct.getResult(), builder.getI64IntegerAttr(1));

  auto foldResult = extractPayload.fold(hew::EnumExtractPayloadOp::FoldAdaptor({}, extractPayload));
  auto foldedVal = llvm::dyn_cast<mlir::Value>(foldResult);
  if (!foldedVal || foldedVal != payload) {
    FAIL("EnumExtractPayloadOp should fold to the payload operand");
    module->destroy();
    return;
  }

  builder.create<mlir::func::ReturnOp>(loc, mlir::ValueRange{extractPayload.getResult()});

  if (mlir::failed(mlir::verify(module))) {
    FAIL("Module verification failed");
    module->destroy();
    return;
  }

  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: Dead Vec NOT eliminated when vec has other uses
//===----------------------------------------------------------------------===//

static void test_dead_vec_not_eliminated_with_uses() {
  TEST(dead_vec_not_eliminated_with_uses);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i32Type = builder.getI32Type();
  auto vecType = hew::VecType::get(&ctx, i32Type);
  auto funcType = builder.getFunctionType({i32Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto vecNew = builder.create<hew::VecNewOp>(loc, vecType);
  // Push adds an extra use — vec.new no longer has exactly one use
  builder.create<hew::VecPushOp>(loc, vecNew.getResult(), entryBlock->getArgument(0));
  builder.create<hew::VecFreeOp>(loc, vecNew.getResult());
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::RewritePatternSet patterns(&ctx);
  hew::VecFreeOp::getCanonicalizationPatterns(patterns, &ctx);

  mlir::GreedyRewriteConfig config;
  config.setMaxIterations(10);
  (void)mlir::applyPatternsGreedily(func, std::move(patterns), config);

  // vec.new and vec.free should still be present (vec has other uses)
  int vecNewCount = 0, vecFreeCount = 0;
  func.walk([&](hew::VecNewOp) { vecNewCount++; });
  func.walk([&](hew::VecFreeOp) { vecFreeCount++; });

  if (vecNewCount != 1 || vecFreeCount != 1) {
    FAIL("Vec with uses should NOT be eliminated");
    module->destroy();
    return;
  }

  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Test: Dead HashMap NOT eliminated when map has other uses
//===----------------------------------------------------------------------===//

static void test_dead_hashmap_not_eliminated_with_uses() {
  TEST(dead_hashmap_not_eliminated_with_uses);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();

  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i32Type = builder.getI32Type();
  auto mapType = hew::HashMapType::get(&ctx, i32Type, i32Type);
  auto funcType = builder.getFunctionType({i32Type, i32Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *entryBlock = func.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto mapNew = builder.create<hew::HashMapNewOp>(loc, mapType);
  // Insert adds an extra use — hashmap.new no longer has exactly one use
  builder.create<hew::HashMapInsertOp>(loc, mapNew.getResult(), entryBlock->getArgument(0),
                                       entryBlock->getArgument(1));
  builder.create<hew::HashMapFreeOp>(loc, mapNew.getResult());
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::RewritePatternSet patterns(&ctx);
  hew::HashMapFreeOp::getCanonicalizationPatterns(patterns, &ctx);

  mlir::GreedyRewriteConfig config;
  config.setMaxIterations(10);
  (void)mlir::applyPatternsGreedily(func, std::move(patterns), config);

  // hashmap.new and hashmap.free should still be present
  int mapNewCount = 0, mapFreeCount = 0;
  func.walk([&](hew::HashMapNewOp) { mapNewCount++; });
  func.walk([&](hew::HashMapFreeOp) { mapFreeCount++; });

  if (mapNewCount != 1 || mapFreeCount != 1) {
    FAIL("HashMap with uses should NOT be eliminated");
    module->destroy();
    return;
  }

  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Verifier negative tests — Collection ops
//===----------------------------------------------------------------------===//

static void test_vec_remove_verifier_wrong_vec_type() {
  TEST(vec_remove_verifier_wrong_vec_type);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto funcType = builder.getFunctionType({i64Type, i64Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  // Use i64 instead of !hew.vec<T> for the vec operand
  builder.create<hew::VecRemoveOp>(loc, block->getArgument(0), block->getArgument(1));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject vec.remove with non-vec operand");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_vec_remove_verifier_wrong_elem_type() {
  TEST(vec_remove_verifier_wrong_elem_type);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto f64Type = builder.getF64Type();
  auto vecType = hew::VecType::get(&ctx, i64Type);
  auto funcType = builder.getFunctionType({vecType, f64Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  // Vec<i64> but value is f64
  builder.create<hew::VecRemoveOp>(loc, block->getArgument(0), block->getArgument(1));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject vec.remove with wrong element type");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_vec_push_verifier_wrong_elem_type() {
  TEST(vec_push_verifier_wrong_elem_type);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto f64Type = builder.getF64Type();
  auto vecType = hew::VecType::get(&ctx, i64Type);
  auto funcType = builder.getFunctionType({vecType, f64Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  // Vec<i64> but pushing f64
  builder.create<hew::VecPushOp>(loc, block->getArgument(0), block->getArgument(1));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject vec.push with wrong element type");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_vec_get_verifier_wrong_vec_type() {
  TEST(vec_get_verifier_wrong_vec_type);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto funcType = builder.getFunctionType({i64Type, i64Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  // Using i64 as vec operand
  builder.create<hew::VecGetOp>(loc, i64Type, block->getArgument(0), block->getArgument(1));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject vec.get with non-vec operand");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_vec_get_verifier_wrong_result_type() {
  TEST(vec_get_verifier_wrong_result_type);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto f64Type = builder.getF64Type();
  auto vecType = hew::VecType::get(&ctx, i64Type);
  auto funcType = builder.getFunctionType({vecType, i64Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  // Vec<i64> but result is f64
  builder.create<hew::VecGetOp>(loc, f64Type, block->getArgument(0), block->getArgument(1));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject vec.get with wrong result type");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_vec_set_verifier_wrong_elem_type() {
  TEST(vec_set_verifier_wrong_elem_type);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto f64Type = builder.getF64Type();
  auto vecType = hew::VecType::get(&ctx, i64Type);
  auto funcType = builder.getFunctionType({vecType, i64Type, f64Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  // Vec<i64>, index i64, but value is f64
  builder.create<hew::VecSetOp>(loc, block->getArgument(0), block->getArgument(1),
                                block->getArgument(2));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject vec.set with wrong element type");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_vec_pop_verifier_wrong_vec_type() {
  TEST(vec_pop_verifier_wrong_vec_type);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto funcType = builder.getFunctionType({i64Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  // Using i64 as vec operand
  builder.create<hew::VecPopOp>(loc, i64Type, block->getArgument(0));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject vec.pop with non-vec operand");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_vec_pop_verifier_wrong_result_type() {
  TEST(vec_pop_verifier_wrong_result_type);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto f64Type = builder.getF64Type();
  auto vecType = hew::VecType::get(&ctx, i64Type);
  auto funcType = builder.getFunctionType({vecType}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  // Vec<i64> but result is f64
  builder.create<hew::VecPopOp>(loc, f64Type, block->getArgument(0));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject vec.pop with wrong result type");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_hashmap_insert_verifier_wrong_key_type() {
  TEST(hashmap_insert_verifier_wrong_key_type);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();
  auto mapType = hew::HashMapType::get(&ctx, i32Type, i64Type);
  // key is i64 instead of i32
  auto funcType = builder.getFunctionType({mapType, i64Type, i64Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  builder.create<hew::HashMapInsertOp>(loc, block->getArgument(0), block->getArgument(1),
                                       block->getArgument(2));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject hashmap.insert with wrong key type");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_hashmap_insert_verifier_wrong_value_type() {
  TEST(hashmap_insert_verifier_wrong_value_type);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();
  auto f64Type = builder.getF64Type();
  auto mapType = hew::HashMapType::get(&ctx, i32Type, i64Type);
  // value is f64 instead of i64
  auto funcType = builder.getFunctionType({mapType, i32Type, f64Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  builder.create<hew::HashMapInsertOp>(loc, block->getArgument(0), block->getArgument(1),
                                       block->getArgument(2));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject hashmap.insert with wrong value type");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_hashmap_get_verifier_wrong_key_type() {
  TEST(hashmap_get_verifier_wrong_key_type);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();
  auto mapType = hew::HashMapType::get(&ctx, i32Type, i64Type);
  // key is i64 instead of i32
  auto funcType = builder.getFunctionType({mapType, i64Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  builder.create<hew::HashMapGetOp>(loc, i64Type, block->getArgument(0), block->getArgument(1));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject hashmap.get with wrong key type");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_hashmap_contains_key_verifier_wrong_key_type() {
  TEST(hashmap_contains_key_verifier_wrong_key_type);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();
  auto mapType = hew::HashMapType::get(&ctx, i32Type, i64Type);
  // key is i64 instead of i32
  auto funcType = builder.getFunctionType({mapType, i64Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  builder.create<hew::HashMapContainsKeyOp>(loc, i32Type, block->getArgument(0),
                                            block->getArgument(1));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject hashmap.contains_key with wrong key type");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_hashmap_remove_verifier_wrong_key_type() {
  TEST(hashmap_remove_verifier_wrong_key_type);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();
  auto mapType = hew::HashMapType::get(&ctx, i32Type, i64Type);
  // key is i64 instead of i32
  auto funcType = builder.getFunctionType({mapType, i64Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  builder.create<hew::HashMapRemoveOp>(loc, block->getArgument(0), block->getArgument(1));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject hashmap.remove with wrong key type");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_hashmap_keys_verifier_wrong_result_type() {
  TEST(hashmap_keys_verifier_wrong_result_type);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();
  auto mapType = hew::HashMapType::get(&ctx, i32Type, i64Type);
  auto funcType = builder.getFunctionType({mapType}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  // Result is i64 instead of !hew.vec<i32>
  builder.create<hew::HashMapKeysOp>(loc, i64Type, block->getArgument(0));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject hashmap.keys with non-vec result type");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_hashmap_len_verifier_wrong_operand_type() {
  TEST(hashmap_len_verifier_wrong_operand_type);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto funcType = builder.getFunctionType({i64Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  // Using i64 instead of !hew.hashmap<K,V>
  builder.create<hew::HashMapLenOp>(loc, i64Type, block->getArgument(0));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject hashmap.len with non-hashmap operand");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Verifier negative tests — Structural ops
//===----------------------------------------------------------------------===//

static void test_tuple_create_verifier_elem_count_mismatch() {
  TEST(tuple_create_verifier_elem_count_mismatch);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto f64Type = builder.getF64Type();
  // Tuple expects 2 elements but we provide 1
  auto tupleType = hew::HewTupleType::get(&ctx, {i64Type, f64Type});
  auto funcType = builder.getFunctionType({i64Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  builder.create<hew::TupleCreateOp>(loc, tupleType, mlir::ValueRange{block->getArgument(0)});
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject tuple.create with wrong element count");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_tuple_extract_verifier_out_of_bounds() {
  TEST(tuple_extract_verifier_out_of_bounds);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto tupleType = hew::HewTupleType::get(&ctx, {i64Type, i64Type});
  auto funcType = builder.getFunctionType({tupleType}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  // Index 5 is out of bounds for 2-element tuple
  builder.create<hew::TupleExtractOp>(loc, i64Type, block->getArgument(0),
                                      builder.getI64IntegerAttr(5));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject tuple.extract with out-of-bounds index");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_array_create_verifier_mixed_types() {
  TEST(array_create_verifier_mixed_types);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto f64Type = builder.getF64Type();
  // Array<i64, 2> but we provide (i64, f64)
  auto arrayType = hew::HewArrayType::get(&ctx, i64Type, 2);
  auto funcType = builder.getFunctionType({i64Type, f64Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  builder.create<hew::ArrayCreateOp>(
      loc, arrayType, mlir::ValueRange{block->getArgument(0), block->getArgument(1)});
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject array.create with mixed element types");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_array_extract_verifier_out_of_bounds() {
  TEST(array_extract_verifier_out_of_bounds);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto arrayType = hew::HewArrayType::get(&ctx, i64Type, 3);
  auto funcType = builder.getFunctionType({arrayType}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  // Index 10 is out of bounds for array of size 3
  builder.create<hew::ArrayExtractOp>(loc, i64Type, block->getArgument(0),
                                      builder.getI64IntegerAttr(10));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject array.extract with out-of-bounds index");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_trait_object_create_verifier_wrong_data_type() {
  TEST(trait_object_create_verifier_wrong_data_type);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto ptrType = mlir::LLVM::LLVMPointerType::get(&ctx);
  auto i64Type = builder.getI64Type();
  auto traitObjType = hew::HewTraitObjectType::get(&ctx, "TestTrait");
  // data is i64 instead of !llvm.ptr
  auto funcType = builder.getFunctionType({i64Type, ptrType}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  builder.create<hew::TraitObjectCreateOp>(loc, traitObjType, block->getArgument(0),
                                           block->getArgument(1));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject trait_object.create with non-ptr data");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_trait_object_data_verifier_wrong_operand_type() {
  TEST(trait_object_data_verifier_wrong_operand_type);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&ctx);
  auto funcType = builder.getFunctionType({i64Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  // Using i64 instead of !hew.trait_object
  builder.create<hew::TraitObjectDataOp>(loc, ptrType, block->getArgument(0));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject trait_object.data with non-trait_object operand");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_trait_object_tag_verifier_wrong_operand_type() {
  TEST(trait_object_tag_verifier_wrong_operand_type);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto ptrType = mlir::LLVM::LLVMPointerType::get(&ctx);
  auto i64Type = builder.getI64Type();
  auto funcType = builder.getFunctionType({i64Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  // Using i64 instead of !hew.trait_object
  builder.create<hew::TraitObjectTagOp>(loc, ptrType, block->getArgument(0));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject trait_object.tag with non-trait_object operand");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_closure_create_verifier_wrong_fn_ptr_type() {
  TEST(closure_create_verifier_wrong_fn_ptr_type);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&ctx);
  auto closureType = hew::ClosureType::get(&ctx, {i64Type}, i64Type);
  // fn_ptr is i64 instead of !llvm.ptr
  auto funcType = builder.getFunctionType({i64Type, ptrType}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  builder.create<hew::ClosureCreateOp>(loc, closureType, block->getArgument(0),
                                       block->getArgument(1));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject closure.create with non-ptr fn_ptr");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_closure_get_fn_verifier_wrong_operand_type() {
  TEST(closure_get_fn_verifier_wrong_operand_type);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&ctx);
  auto funcType = builder.getFunctionType({i64Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  // Using i64 instead of !hew.closure
  builder.create<hew::ClosureGetFnOp>(loc, ptrType, block->getArgument(0));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject closure.get_fn with non-closure operand");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_closure_get_env_verifier_wrong_operand_type() {
  TEST(closure_get_env_verifier_wrong_operand_type);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&ctx);
  auto funcType = builder.getFunctionType({i64Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  // Using i64 instead of !hew.closure
  builder.create<hew::ClosureGetEnvOp>(loc, ptrType, block->getArgument(0));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject closure.get_env with non-closure operand");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Verifier negative tests — Generator / Assert / String ops
//===----------------------------------------------------------------------===//

static void test_gen_wrap_value_verifier_wrong_result_type() {
  TEST(gen_wrap_value_verifier_wrong_result_type);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto funcType = builder.getFunctionType({i64Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  // Result is i64 instead of an LLVM struct type
  builder.create<hew::GenWrapValueOp>(loc, i64Type, block->getArgument(0));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject gen.wrap_value with non-struct result");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_gen_wrap_value_verifier_wrong_value_type() {
  TEST(gen_wrap_value_verifier_wrong_value_type);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i8Type = builder.getI8Type();
  auto i64Type = builder.getI64Type();
  auto f64Type = builder.getF64Type();
  // Struct is {i8, i64} but value is f64
  auto wrapperType = mlir::LLVM::LLVMStructType::getLiteral(&ctx, {i8Type, i64Type});
  auto funcType = builder.getFunctionType({f64Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  builder.create<hew::GenWrapValueOp>(loc, wrapperType, block->getArgument(0));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject gen.wrap_value with wrong value type");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_gen_wrap_done_verifier_wrong_result_type() {
  TEST(gen_wrap_done_verifier_wrong_result_type);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto funcType = builder.getFunctionType({}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  // Result is i64 instead of an LLVM struct type
  builder.create<hew::GenWrapDoneOp>(loc, i64Type);
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject gen.wrap_done with non-struct result");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_gen_wrap_done_verifier_wrong_tag_type() {
  TEST(gen_wrap_done_verifier_wrong_tag_type);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();
  ctx.loadDialect<mlir::LLVM::LLVMDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();
  // First field should be i8 but is i32
  auto wrapperType = mlir::LLVM::LLVMStructType::getLiteral(&ctx, {i32Type, i64Type});
  auto funcType = builder.getFunctionType({}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  builder.create<hew::GenWrapDoneOp>(loc, wrapperType);
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject gen.wrap_done with wrong tag type (i32 instead of i8)");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_assert_eq_verifier_type_mismatch() {
  TEST(assert_eq_verifier_type_mismatch);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto f64Type = builder.getF64Type();
  auto funcType = builder.getFunctionType({i64Type, f64Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  // left is i64, right is f64 — type mismatch
  builder.create<hew::AssertEqOp>(loc, block->getArgument(0), block->getArgument(1));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject assert_eq with mismatched operand types");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_assert_ne_verifier_type_mismatch() {
  TEST(assert_ne_verifier_type_mismatch);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto f64Type = builder.getF64Type();
  auto funcType = builder.getFunctionType({i64Type, f64Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  // left is i64, right is f64 — type mismatch
  builder.create<hew::AssertNeOp>(loc, block->getArgument(0), block->getArgument(1));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject assert_ne with mismatched operand types");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_string_method_verifier_empty_method() {
  TEST(string_method_verifier_empty_method);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto strRefType = hew::StringRefType::get(&ctx);
  auto funcType = builder.getFunctionType({strRefType}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  // Empty method name should fail
  builder.create<hew::StringMethodOp>(loc, mlir::TypeRange{strRefType}, builder.getStringAttr(""),
                                      block->getArgument(0), mlir::ValueRange{});
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject string_method with empty method name");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

static void test_vec_len_verifier_wrong_vec_type() {
  TEST(vec_len_verifier_wrong_vec_type);

  mlir::MLIRContext ctx;
  ctx.loadDialect<hew::HewDialect>();
  ctx.loadDialect<mlir::func::FuncDialect>();

  mlir::OpBuilder builder(&ctx);
  auto loc = builder.getUnknownLoc();
  auto module = mlir::ModuleOp::create(loc);
  builder.setInsertionPointToStart(module.getBody());

  auto i64Type = builder.getI64Type();
  auto funcType = builder.getFunctionType({i64Type}, {});
  auto func = builder.create<mlir::func::FuncOp>(loc, "test_fn", funcType);
  auto *block = func.addEntryBlock();
  builder.setInsertionPointToStart(block);

  // Using i64 instead of !hew.vec<T>
  builder.create<hew::VecLenOp>(loc, i64Type, block->getArgument(0));
  builder.create<mlir::func::ReturnOp>(loc);

  mlir::ScopedDiagnosticHandler handler(&ctx, [](mlir::Diagnostic &) { return mlir::success(); });
  if (mlir::succeeded(mlir::verify(module))) {
    FAIL("Should reject vec.len with non-vec operand");
    module->destroy();
    return;
  }
  module->destroy();
  PASS();
}

//===----------------------------------------------------------------------===//
// Main
//===----------------------------------------------------------------------===//

int main() {
  printf("=== Hew MLIR Dialect Tests ===\n");

  test_dialect_loads();
  test_constant_i64();
  test_constant_f64();
  test_constant_bool();
  test_global_string();
  test_print_op();
  test_print_op_effects();
  test_cast_op();
  test_struct_ops();
  test_field_get_fold();
  test_module_dump();

  // Verifier negative tests
  test_struct_init_verifier_field_count();
  test_field_get_verifier_bounds();
  test_struct_init_verifier_field_type();

  // Folder tests
  test_tuple_extract_fold();
  test_array_extract_fold();
  test_enum_extract_tag_fold();
  test_closure_fold();
  test_trait_object_fold();

  // Side-effect tests
  test_collection_effects();
  test_pure_ops_no_effects();

  // Canonicalization tests
  test_cast_chain_canonicalization();
  test_dead_vec_elimination();
  test_dead_hashmap_elimination();
  test_string_concat_identity_canonicalization();
  test_string_concat_identity_lhs_empty();
  test_cast_identity_fold();
  test_cast_constant_fold();
  test_enum_extract_payload_fold();
  test_dead_vec_not_eliminated_with_uses();
  test_dead_hashmap_not_eliminated_with_uses();

  // Verifier negative tests — Collection ops
  test_vec_remove_verifier_wrong_vec_type();
  test_vec_remove_verifier_wrong_elem_type();
  test_vec_push_verifier_wrong_elem_type();
  test_vec_get_verifier_wrong_vec_type();
  test_vec_get_verifier_wrong_result_type();
  test_vec_set_verifier_wrong_elem_type();
  test_vec_pop_verifier_wrong_vec_type();
  test_vec_pop_verifier_wrong_result_type();
  test_vec_len_verifier_wrong_vec_type();
  test_hashmap_insert_verifier_wrong_key_type();
  test_hashmap_insert_verifier_wrong_value_type();
  test_hashmap_get_verifier_wrong_key_type();
  test_hashmap_contains_key_verifier_wrong_key_type();
  test_hashmap_remove_verifier_wrong_key_type();
  test_hashmap_keys_verifier_wrong_result_type();
  test_hashmap_len_verifier_wrong_operand_type();

  // Verifier negative tests — Structural ops
  test_tuple_create_verifier_elem_count_mismatch();
  test_tuple_extract_verifier_out_of_bounds();
  test_array_create_verifier_mixed_types();
  test_array_extract_verifier_out_of_bounds();
  test_trait_object_create_verifier_wrong_data_type();
  test_trait_object_data_verifier_wrong_operand_type();
  test_trait_object_tag_verifier_wrong_operand_type();
  test_closure_create_verifier_wrong_fn_ptr_type();
  test_closure_get_fn_verifier_wrong_operand_type();
  test_closure_get_env_verifier_wrong_operand_type();

  // Verifier negative tests — Generator / Assert / String ops
  test_gen_wrap_value_verifier_wrong_result_type();
  test_gen_wrap_value_verifier_wrong_value_type();
  test_gen_wrap_done_verifier_wrong_result_type();
  test_gen_wrap_done_verifier_wrong_tag_type();
  test_assert_eq_verifier_type_mismatch();
  test_assert_ne_verifier_type_mismatch();
  test_string_method_verifier_empty_method();

  printf("\n%d/%d tests passed.\n", tests_passed, tests_run);
  return (tests_passed == tests_run) ? 0 : 1;
}
