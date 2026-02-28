//===- MLIRGenHelpers.h - Shared helpers for MLIRGen split -------*- C++ -*-===//
//
// Static helper functions used across MLIRGen translation units.
//
//===----------------------------------------------------------------------===//

#ifndef HEW_MLIR_MLIRGEN_HELPERS_H
#define HEW_MLIR_MLIRGEN_HELPERS_H

#include "hew/ast_types.h"
#include "hew/mlir/HewOps.h"
#include "hew/mlir/HewTypes.h"

#include <functional>

#include "mlir/Dialect/Arith/IR/Arith.h"
#include "mlir/Dialect/Func/IR/FuncOps.h"
#include "mlir/Dialect/LLVMIR/LLVMDialect.h"
#include "mlir/Dialect/SCF/IR/SCF.h"
#include "mlir/IR/Builders.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/BuiltinTypes.h"
#include "mlir/IR/Value.h"

#include "llvm/ADT/StringRef.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

namespace hew {

/// Normalize an AST element type name to the canonical form used in
/// collectionVarTypes strings (e.g. "Vec<i64>").  The language aliases
/// "int" → i64 and "float" → f64, so we must unify them.
inline std::string normalizeElemTypeName(const std::string &name) {
  if (name == "int")
    return "i64";
  if (name == "float")
    return "f64";
  return name;
}

// ── TypeExpr→string helpers ─────────────────────────────────────────────
// These extract type classification strings from enriched AST type
// annotations (stmt.ty), replacing the manual inference heuristics that
// previously pattern-matched function names and MLIR types.

/// Extract a collection type string from a TypeExpr.
/// Returns "Vec<elem>", "HashMap<key,val>", "bytes", or "" (not a collection).
inline std::string typeExprToCollectionString(
    const ast::TypeExpr &te,
    const std::function<std::string(const std::string &)> &resolveAlias = nullptr) {
  auto *named = std::get_if<ast::TypeNamed>(&te.kind);
  if (!named)
    return "";
  auto resolve = [&](const std::string &n) -> std::string {
    return resolveAlias ? resolveAlias(n) : n;
  };
  auto resolved = resolve(named->name);
  if (resolved == "bytes")
    return "bytes";
  if (resolved == "Vec") {
    if (!named->type_args || named->type_args->empty())
      return "";
    const auto &arg = (*named->type_args)[0].value;
    auto *en = std::get_if<ast::TypeNamed>(&arg.kind);
    if (!en)
      return "";
    std::string elemType = normalizeElemTypeName(resolve(en->name));
    // Handle nested generics like ActorRef<T>
    if (en->type_args && !en->type_args->empty()) {
      elemType += "<";
      if (auto *inner = std::get_if<ast::TypeNamed>(&(*en->type_args)[0].value.kind))
        elemType += resolve(inner->name);
      else
        return "";
      elemType += ">";
    }
    return "Vec<" + elemType + ">";
  }
  if (resolved == "HashMap") {
    if (!named->type_args || named->type_args->size() < 2)
      return "";
    auto *k = std::get_if<ast::TypeNamed>(&(*named->type_args)[0].value.kind);
    auto *v = std::get_if<ast::TypeNamed>(&(*named->type_args)[1].value.kind);
    if (!k || !v)
      return "";
    std::string keyType = resolve(k->name);
    std::string valType = resolve(v->name);
    return "HashMap<" + keyType + "," + valType + ">";
  }
  return "";
}

/// Extract a handle type string from a TypeExpr.
/// Returns "http.Server", "net.Listener", "regex.Pattern", "process.Child",
/// etc., or "" if not a handle type.
inline std::string typeExprToHandleString(const ast::TypeExpr &te) {
  auto *named = std::get_if<ast::TypeNamed>(&te.kind);
  if (!named)
    return "";
  // Handle types use module-qualified names in the type checker
  static const std::string handleTypes[] = {
      "http.Server",   "http.Request",  "net.Listener",  "net.Connection", "regex.Pattern",
      "process.Child", "json.Value",    "csv.Table",     "toml.Value",     "yaml.Value",
      "sqlite.Db",     "sqlite.Result", "postgres.Conn", "postgres.Result"};
  for (const auto &ht : handleTypes) {
    if (named->name == ht)
      return ht;
  }
  return "";
}

/// Extract stream kind from a TypeExpr.
/// Returns "Stream", "Sink", or "" if not a stream type.
inline std::string typeExprStreamKind(const ast::TypeExpr &te) {
  auto *named = std::get_if<ast::TypeNamed>(&te.kind);
  if (!named)
    return "";
  if (named->name == "Stream")
    return "Stream";
  if (named->name == "Sink")
    return "Sink";
  return "";
}

/// Extract actor type name from a TypeExpr like ActorRef<MyActor>.
/// Returns the actor name or "" if not an actor reference.
inline std::string typeExprToActorName(const ast::TypeExpr &te) {
  auto *named = std::get_if<ast::TypeNamed>(&te.kind);
  if (!named)
    return "";
  if (named->name == "ActorRef" && named->type_args && !named->type_args->empty()) {
    if (auto *inner = std::get_if<ast::TypeNamed>(&(*named->type_args)[0].value.kind))
      return inner->name;
  }
  return "";
}

/// Extract trait name from a dyn Trait TypeExpr.
/// Returns the trait name or "" if not a trait object.
inline std::string typeExprTraitName(const ast::TypeExpr &te) {
  auto *traitObj = std::get_if<ast::TypeTraitObject>(&te.kind);
  if (!traitObj || traitObj->bounds.empty())
    return "";
  return traitObj->bounds[0].name;
}

/// Returns true if the MLIR type represents a pointer-like value — either
/// \c !llvm.ptr or any Hew dialect type that lowers to a pointer.
inline bool isPointerLikeType(mlir::Type type) {
  return mlir::isa<mlir::LLVM::LLVMPointerType, hew::ActorRefType, hew::TypedActorRefType,
                   hew::StringRefType, hew::VecType, hew::HashMapType, hew::HandleType>(type);
}

/// Convert a Hew dialect type to its LLVM storage type.  Pointer-like Hew
/// types become \c !llvm.ptr; struct-like types (ClosureType, TraitObjectType)
/// become their LLVM struct equivalent.  All other types are returned unchanged.
/// Note: HewTupleType and HewArrayType are NOT handled here — they would need
/// recursive conversion which requires a TypeConverter.
inline mlir::Type toLLVMStorageType(mlir::Type type) {
  if (mlir::isa<hew::ActorRefType, hew::TypedActorRefType, hew::StringRefType, hew::VecType,
                hew::HashMapType, hew::HandleType>(type))
    return mlir::LLVM::LLVMPointerType::get(type.getContext());
  // ClosureType lowers to !llvm.struct<(ptr, ptr)>
  if (mlir::isa<hew::ClosureType>(type)) {
    auto *ctx = type.getContext();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(ctx);
    return mlir::LLVM::LLVMStructType::getLiteral(ctx, {ptrType, ptrType});
  }
  return type;
}

/// Create an integer constant with a given type.
inline mlir::Value createIntConstant(mlir::OpBuilder &builder, mlir::Location loc, mlir::Type type,
                                     int64_t value) {
  return builder.create<mlir::arith::ConstantIntOp>(loc, type, value);
}

/// Return the struct field index where an enum payload is stored.
/// Built-in Result<T,E> uses per-variant slots (Ok -> 1, Err -> 2),
/// while Option<T> and user-defined enums use union-style slots starting at 1.
inline int64_t enumPayloadFieldIndex(llvm::StringRef enumName, int32_t variantIndex,
                                     int64_t payloadOrdinal = 0) {
  if (enumName == "__Result")
    return static_cast<int64_t>(variantIndex) + 1 + payloadOrdinal;
  return 1 + payloadOrdinal;
}

/// Create a type-appropriate zero/default value (works for int, float, struct).
inline mlir::Value createDefaultValue(mlir::OpBuilder &builder, mlir::Location loc,
                                      mlir::Type type) {
  if (mlir::isa<mlir::IntegerType>(type))
    return builder.create<mlir::arith::ConstantIntOp>(loc, type, 0);
  if (mlir::isa<mlir::FloatType>(type))
    return builder.create<mlir::arith::ConstantOp>(loc, builder.getFloatAttr(type, 0.0));
  if (mlir::isa<mlir::LLVM::LLVMPointerType>(type))
    return builder.create<mlir::LLVM::ZeroOp>(loc, type);
  // Hew pointer-like types: create a null pointer then cast to the Hew type
  if (mlir::isa<hew::StringRefType, hew::ActorRefType, hew::TypedActorRefType, hew::VecType,
                hew::HashMapType, hew::HandleType>(type)) {
    auto ptrType = mlir::LLVM::LLVMPointerType::get(type.getContext());
    auto zero = builder.create<mlir::LLVM::ZeroOp>(loc, ptrType);
    return builder.create<hew::BitcastOp>(loc, type, zero);
  }
  if (mlir::isa<mlir::LLVM::LLVMStructType>(type) || mlir::isa<mlir::LLVM::LLVMArrayType>(type))
    return builder.create<mlir::LLVM::UndefOp>(loc, type);
  // Hew tuple/array types: create via dialect ops with default elements
  if (auto tupleType = mlir::dyn_cast<hew::HewTupleType>(type)) {
    llvm::SmallVector<mlir::Value, 4> elements;
    for (auto elemType : tupleType.getElementTypes())
      elements.push_back(createDefaultValue(builder, loc, elemType));
    return builder.create<hew::TupleCreateOp>(loc, tupleType, elements);
  }
  if (auto arrayType = mlir::dyn_cast<hew::HewArrayType>(type)) {
    llvm::SmallVector<mlir::Value, 8> elements;
    for (int64_t i = 0; i < arrayType.getSize(); ++i)
      elements.push_back(createDefaultValue(builder, loc, arrayType.getElementType()));
    return builder.create<hew::ArrayCreateOp>(loc, arrayType, elements);
  }
  if (mlir::isa<hew::HewTraitObjectType>(type)) {
    auto ptrType = mlir::LLVM::LLVMPointerType::get(type.getContext());
    auto nullPtr = builder.create<mlir::LLVM::ZeroOp>(loc, ptrType);
    auto nullVtable = builder.create<mlir::LLVM::ZeroOp>(loc, ptrType);
    return builder.create<hew::TraitObjectCreateOp>(loc, type, nullPtr, nullVtable);
  }
  if (mlir::isa<hew::ClosureType>(type)) {
    auto ptrType = mlir::LLVM::LLVMPointerType::get(type.getContext());
    auto nullFn = builder.create<mlir::LLVM::ZeroOp>(loc, ptrType);
    auto nullEnv = builder.create<mlir::LLVM::ZeroOp>(loc, ptrType);
    return builder.create<hew::ClosureCreateOp>(loc, type, nullFn, nullEnv);
  }
  // Hew dialect enum types: produce a default-valued enum via hew.enum_construct
  if (auto opt = mlir::dyn_cast<hew::OptionEnumType>(type)) {
    // Default Option: None (variant 0, no payloads)
    return builder.create<hew::EnumConstructOp>(loc, type, /*variant_index=*/0,
                                                builder.getStringAttr("Option"), mlir::ValueRange{},
                                                /*payload_positions=*/nullptr);
  }
  if (auto res = mlir::dyn_cast<hew::ResultEnumType>(type)) {
    // Default Result: Ok(default_ok_value)
    auto okDefault = createDefaultValue(builder, loc, res.getOkType());
    return builder.create<hew::EnumConstructOp>(
        loc, type, /*variant_index=*/0, builder.getStringAttr("__Result"),
        mlir::ValueRange{okDefault}, /*payload_positions=*/nullptr);
  }
  // No valid default — this indicates a type we haven't handled.
  llvm::errs() << "MLIRGen: no default value for type: " << type << "\n";
  llvm::report_fatal_error("createDefaultValue: unhandled type");
}

/// Check if a statement might contain a return (recursively).
inline bool stmtMightContainReturn(const ast::Stmt &s) {
  return std::holds_alternative<ast::StmtReturn>(s.kind) ||
         std::holds_alternative<ast::StmtIf>(s.kind) ||
         std::holds_alternative<ast::StmtIfLet>(s.kind) ||
         std::holds_alternative<ast::StmtWhile>(s.kind) ||
         std::holds_alternative<ast::StmtFor>(s.kind) ||
         std::holds_alternative<ast::StmtLoop>(s.kind) ||
         std::holds_alternative<ast::StmtMatch>(s.kind);
}

/// Check if a statement might contain break or continue.
inline bool stmtMightContainBreakOrContinue(const ast::Stmt &s) {
  return std::holds_alternative<ast::StmtBreak>(s.kind) ||
         std::holds_alternative<ast::StmtContinue>(s.kind) ||
         std::holds_alternative<ast::StmtIf>(s.kind) ||
         std::holds_alternative<ast::StmtIfLet>(s.kind) ||
         std::holds_alternative<ast::StmtMatch>(s.kind) ||
         std::holds_alternative<ast::StmtLoop>(s.kind) ||
         std::holds_alternative<ast::StmtWhile>(s.kind) ||
         std::holds_alternative<ast::StmtFor>(s.kind);
}

/// Check if a block has a "real" terminator (not just an auto-inserted
/// scf.yield with no operands).
inline bool hasRealTerminator(mlir::Block *block) {
  if (!block || block->empty())
    return false;
  auto &lastOp = block->back();
  if (!lastOp.hasTrait<mlir::OpTrait::IsTerminator>())
    return false;
  // An empty scf.yield is auto-inserted — not a "real" terminator.
  if (auto yieldOp = mlir::dyn_cast<mlir::scf::YieldOp>(lastOp))
    return yieldOp.getNumOperands() > 0;
  return true;
}

} // namespace hew

#endif // HEW_MLIR_MLIRGEN_HELPERS_H
