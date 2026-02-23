//===- HewOps.h - Hew MLIR dialect operations -----------------*- C++ -*-===//
//
// Declaration of the Hew MLIR dialect operations.
//
//===----------------------------------------------------------------------===//

#ifndef HEW_MLIR_HEWOPS_H
#define HEW_MLIR_HEWOPS_H

#include "hew/mlir/HewTypes.h"
#include "mlir/Bytecode/BytecodeOpInterface.h"
#include "mlir/Interfaces/SideEffectInterfaces.h"
#include "mlir/IR/BuiltinTypes.h"
#include "mlir/IR/Dialect.h"
#include "mlir/IR/OpDefinition.h"
#include "mlir/IR/SymbolTable.h"

#define GET_OP_CLASSES
#include "hew/mlir/HewOps.h.inc"

#endif // HEW_MLIR_HEWOPS_H
