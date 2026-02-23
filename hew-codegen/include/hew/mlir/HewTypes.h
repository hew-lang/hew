//===- HewTypes.h - Hew MLIR dialect types ---------------------*- C++ -*-===//
//
// Declaration of Hew dialect custom types.
//
//===----------------------------------------------------------------------===//

#ifndef HEW_MLIR_HEWTYPES_H
#define HEW_MLIR_HEWTYPES_H

#include "mlir/IR/BuiltinTypes.h"
#include "mlir/IR/Types.h"

#define GET_TYPEDEF_CLASSES
#include "hew/mlir/HewTypes.h.inc"

#endif // HEW_MLIR_HEWTYPES_H
