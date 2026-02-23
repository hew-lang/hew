//===- HewDialect.cpp - Hew MLIR dialect implementation -------------------===//
//
// Implements the Hew MLIR dialect.
//
//===----------------------------------------------------------------------===//

#include "hew/mlir/HewDialect.h"
#include "hew/mlir/HewOps.h"
#include "hew/mlir/HewTypes.h"

#include "mlir/IR/Builders.h"
#include "mlir/IR/DialectImplementation.h"
#include "llvm/ADT/TypeSwitch.h"

using namespace mlir;

//===----------------------------------------------------------------------===//
// Pull in auto-generated dialect and type implementations
//===----------------------------------------------------------------------===//

#include "hew/mlir/HewOpsDialect.cpp.inc"

#define GET_TYPEDEF_CLASSES
#include "hew/mlir/HewTypes.cpp.inc"

//===----------------------------------------------------------------------===//
// Hew dialect initialization
//===----------------------------------------------------------------------===//

void hew::HewDialect::initialize() {
  addOperations<
#define GET_OP_LIST
#include "hew/mlir/HewOps.cpp.inc"
      >();
  addTypes<
#define GET_TYPEDEF_LIST
#include "hew/mlir/HewTypes.cpp.inc"
      >();
}
