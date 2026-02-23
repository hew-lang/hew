//===- HewDialect.td - Hew dialect definition -------------*- tablegen -*-===//
//
// Defines the Hew MLIR dialect.
//
//===----------------------------------------------------------------------===//

#ifndef HEW_DIALECT
#define HEW_DIALECT

include "mlir/IR/OpBase.td"

def Hew_Dialect : Dialect {
  let name = "hew";
  let summary = "The Hew programming language dialect.";
  let description = [{
    The Hew dialect defines operations for the Hew programming language,
    a statically-typed, actor-oriented language targeting concurrent and
    distributed systems. This dialect captures Hew-specific constructs
    that don't map directly to existing MLIR dialects (arith, scf, func,
    memref).
  }];
  let cppNamespace = "::hew";
  let useDefaultTypePrinterParser = 1;
}

// Base class for all Hew dialect operations.
class Hew_Op<string mnemonic, list<Trait> traits = []> :
    Op<Hew_Dialect, mnemonic, traits>;

#endif // HEW_DIALECT
