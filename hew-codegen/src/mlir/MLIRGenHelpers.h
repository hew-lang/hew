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

#include <unordered_set>

#include "mlir/Dialect/Arith/IR/Arith.h"
#include "mlir/Dialect/LLVMIR/LLVMDialect.h"
#include "mlir/Dialect/SCF/IR/SCF.h"
#include "mlir/IR/Builders.h"
#include "mlir/IR/BuiltinTypes.h"
#include "mlir/IR/Value.h"

#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSwitch.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

namespace hew {

/// Returns true if the type is valid (non-null and not NoneType).
/// convertType() returns NoneType on failure; callers should check this
/// before using the result to prevent silent propagation of invalid types.
inline bool isValidType(mlir::Type type) {
  return type && !mlir::isa<mlir::NoneType>(type);
}

enum class PrimitiveTypeKind {
  Unknown,
  I8,
  I16,
  I32,
  I64,
  U8,
  U16,
  U32,
  U64,
  F32,
  F64,
  Bool,
  Char,
  String,
  Bytes,
  Duration,
};

inline PrimitiveTypeKind primitiveTypeKind(llvm::StringRef name) {
  return llvm::StringSwitch<PrimitiveTypeKind>(name)
      .Case("i8", PrimitiveTypeKind::I8)
      .Case("i16", PrimitiveTypeKind::I16)
      .Case("i32", PrimitiveTypeKind::I32)
      .Cases({"i64", "int"}, PrimitiveTypeKind::I64)
      .Cases({"Int", "isize"}, PrimitiveTypeKind::I64)
      .Case("u8", PrimitiveTypeKind::U8)
      .Case("byte", PrimitiveTypeKind::U8)
      .Case("u16", PrimitiveTypeKind::U16)
      .Case("u32", PrimitiveTypeKind::U32)
      .Cases({"u64", "uint"}, PrimitiveTypeKind::U64)
      .Case("usize", PrimitiveTypeKind::U64)
      .Case("f32", PrimitiveTypeKind::F32)
      .Cases({"f64", "float"}, PrimitiveTypeKind::F64)
      .Case("Float", PrimitiveTypeKind::F64)
      .Cases({"bool", "Bool"}, PrimitiveTypeKind::Bool)
      .Cases({"char", "Char"}, PrimitiveTypeKind::Char)
      .Cases({"string", "String"}, PrimitiveTypeKind::String)
      .Case("str", PrimitiveTypeKind::String)
      .Cases({"bytes", "Bytes"}, PrimitiveTypeKind::Bytes)
      .Cases({"duration", "Duration"}, PrimitiveTypeKind::Duration)
      .Default(PrimitiveTypeKind::Unknown);
}

inline llvm::StringRef canonicalPrimitiveTypeName(llvm::StringRef name) {
  switch (primitiveTypeKind(name)) {
  case PrimitiveTypeKind::I8:
    return "i8";
  case PrimitiveTypeKind::I16:
    return "i16";
  case PrimitiveTypeKind::I32:
    return "i32";
  case PrimitiveTypeKind::I64:
    return "i64";
  case PrimitiveTypeKind::U8:
    return "u8";
  case PrimitiveTypeKind::U16:
    return "u16";
  case PrimitiveTypeKind::U32:
    return "u32";
  case PrimitiveTypeKind::U64:
    return "u64";
  case PrimitiveTypeKind::F32:
    return "f32";
  case PrimitiveTypeKind::F64:
    return "f64";
  case PrimitiveTypeKind::Bool:
    return "bool";
  case PrimitiveTypeKind::Char:
    return "char";
  case PrimitiveTypeKind::String:
    return "string";
  case PrimitiveTypeKind::Bytes:
    return "bytes";
  case PrimitiveTypeKind::Duration:
    return "duration";
  case PrimitiveTypeKind::Unknown:
    return {};
  }
  return {};
}

/// Normalize an AST element type name to canonical form (e.g. "Vec<i64>").
inline std::string normalizeElemTypeName(const std::string &name) {
  if (auto canonical = canonicalPrimitiveTypeName(name); !canonical.empty())
    return canonical.str();
  return name;
}

// ── TypeExpr→string helpers ─────────────────────────────────────────────
// These extract type classification strings from enriched AST type
// annotations (stmt.ty), replacing the manual inference heuristics that
// previously pattern-matched function names and MLIR types.

using ResolveTypeAliasExprFn = llvm::function_ref<const ast::TypeExpr *(llvm::StringRef)>;

inline llvm::StringRef canonicalResolvedTypeName(llvm::StringRef name) {
  if (name == "ActorStream")
    return "Stream";
  if (name == "stream.Stream")
    return "Stream";
  if (name == "stream.Sink")
    return "Sink";
  if (name == "channel.Sender")
    return "Sender";
  if (name == "channel.Receiver")
    return "Receiver";
  if (auto canonical = canonicalPrimitiveTypeName(name); !canonical.empty())
    return canonical;
  return name;
}

inline const ast::TypeExpr *resolveAliasedTypeExpr(const ast::TypeExpr &te,
                                                   ResolveTypeAliasExprFn resolveAlias = nullptr) {
  if (!resolveAlias)
    return &te;

  const ast::TypeExpr *current = &te;
  std::unordered_set<std::string> seen;
  while (auto *named = std::get_if<ast::TypeNamed>(&current->kind)) {
    if (!seen.insert(named->name).second)
      break;
    auto *alias = resolveAlias(named->name);
    if (!alias)
      break;
    current = alias;
  }
  return current;
}

struct ResolvedTypeClassifier {
  const ast::TypeExpr *resolvedType = nullptr;
  const ast::TypeNamed *named = nullptr;
  std::string canonicalName;

  [[nodiscard]] bool isStream() const { return canonicalName == "Stream"; }
  [[nodiscard]] bool isSink() const { return canonicalName == "Sink"; }
  [[nodiscard]] bool isSender() const { return canonicalName == "Sender"; }
  [[nodiscard]] bool isReceiver() const { return canonicalName == "Receiver"; }
  [[nodiscard]] bool isActorRef() const { return canonicalName == "ActorRef"; }
};

inline ResolvedTypeClassifier classifyResolvedType(const ast::TypeExpr &te,
                                                   ResolveTypeAliasExprFn resolveAlias = nullptr) {
  const ast::TypeExpr *resolved = resolveAliasedTypeExpr(te, resolveAlias);
  auto *named = std::get_if<ast::TypeNamed>(&resolved->kind);
  return {
      resolved,
      named,
      named ? canonicalResolvedTypeName(named->name).str() : std::string{},
  };
}

inline std::string resolvedTypeExprString(const ast::TypeExpr &te,
                                          ResolveTypeAliasExprFn resolveAlias = nullptr) {
  auto classified = classifyResolvedType(te, resolveAlias);
  if (!classified.named)
    return "";

  std::string typeName = normalizeElemTypeName(classified.canonicalName);
  if (!classified.named->type_args || classified.named->type_args->empty())
    return typeName;

  typeName += "<";
  for (size_t i = 0; i < classified.named->type_args->size(); ++i) {
    if (i > 0)
      typeName += ",";
    auto argType = resolvedTypeExprString((*classified.named->type_args)[i].value, resolveAlias);
    if (argType.empty())
      return "";
    typeName += argType;
  }
  typeName += ">";
  return typeName;
}

/// Extract a collection type string from a TypeExpr.
/// Returns "Vec<elem>", "HashMap<key,val>", "bytes", or "" (not a collection).
inline std::string typeExprToCollectionString(const ast::TypeExpr &te,
                                              ResolveTypeAliasExprFn resolveAlias = nullptr) {
  auto classified = classifyResolvedType(te, resolveAlias);
  if (!classified.named)
    return "";

  if (classified.canonicalName == "bytes")
    return "bytes";
  if (classified.canonicalName == "Vec") {
    if (!classified.named->type_args || classified.named->type_args->empty())
      return "";
    auto elemType = resolvedTypeExprString((*classified.named->type_args)[0].value, resolveAlias);
    return elemType.empty() ? "" : "Vec<" + elemType + ">";
  }
  if (classified.canonicalName == "HashMap") {
    if (!classified.named->type_args || classified.named->type_args->size() < 2)
      return "";
    auto keyType = resolvedTypeExprString((*classified.named->type_args)[0].value, resolveAlias);
    auto valType = resolvedTypeExprString((*classified.named->type_args)[1].value, resolveAlias);
    if (keyType.empty() || valType.empty())
      return "";
    return "HashMap<" + keyType + "," + valType + ">";
  }
  return "";
}

/// Extract a handle type string from a TypeExpr.
/// Returns the handle type name (e.g. "http.Server", "regex.Pattern") or ""
/// if the type is not in the metadata-driven known-handle set.
///
/// \p knownHandles must be the set built from program.handle_types by
/// MLIRGen::generate() — NOT a hardcoded list — so that the lookup stays in
/// sync with the Rust type-checker's handle-type registry.
inline std::string typeExprToHandleString(const ast::TypeExpr &te,
                                          const std::unordered_set<std::string> &knownHandles,
                                          ResolveTypeAliasExprFn resolveAlias = nullptr) {
  auto classified = classifyResolvedType(te, resolveAlias);
  if (!classified.named)
    return "";
  if (knownHandles.count(classified.named->name))
    return classified.named->name;
  if (knownHandles.count(classified.canonicalName))
    return classified.canonicalName;
  if (classified.isStream() && knownHandles.count("stream.Stream"))
    return "stream.Stream";
  if (classified.isSink() && knownHandles.count("stream.Sink"))
    return "stream.Sink";
  if (classified.isSender() && knownHandles.count("channel.Sender"))
    return "channel.Sender";
  if (classified.isReceiver() && knownHandles.count("channel.Receiver"))
    return "channel.Receiver";
  return "";
}

/// Extract stream kind from a TypeExpr.
/// Returns "Stream", "Sink", or "" if not a stream type.
inline std::string typeExprStreamKind(const ast::TypeExpr &te,
                                      ResolveTypeAliasExprFn resolveAlias = nullptr) {
  auto classified = classifyResolvedType(te, resolveAlias);
  if (classified.isStream())
    return "Stream";
  if (classified.isSink())
    return "Sink";
  return "";
}

/// Extract the element type name from a Stream<T> or Sink<T> TypeExpr.
/// Returns "bytes", "string", or "" if the element type is absent or unknown.
inline std::string typeExprStreamElement(const ast::TypeExpr &te,
                                         ResolveTypeAliasExprFn resolveAlias = nullptr) {
  auto classified = classifyResolvedType(te, resolveAlias);
  if (!classified.named || !classified.named->type_args || classified.named->type_args->empty())
    return "";
  auto inner = classifyResolvedType((*classified.named->type_args)[0].value, resolveAlias);
  if (!inner.named)
    return "";
  if (inner.canonicalName == "string")
    return "string";
  return inner.canonicalName; // e.g. "bytes", "string"
}

/// Extract actor type name from a TypeExpr like ActorRef<MyActor>.
/// Returns the actor name or "" if not an actor reference.
inline std::string typeExprToTypeName(const ast::TypeExpr &te,
                                      ResolveTypeAliasExprFn resolveAlias = nullptr);

inline std::string typeExprToActorName(const ast::TypeExpr &te,
                                       ResolveTypeAliasExprFn resolveAlias = nullptr) {
  auto classified = classifyResolvedType(te, resolveAlias);
  if (!classified.isActorRef() || !classified.named || !classified.named->type_args ||
      classified.named->type_args->empty()) {
    return "";
  }
  return typeExprToTypeName((*classified.named->type_args)[0].value, resolveAlias);
}

inline bool typeExprIsReceiver(const ast::TypeExpr &te,
                               ResolveTypeAliasExprFn resolveAlias = nullptr) {
  return classifyResolvedType(te, resolveAlias).isReceiver();
}

inline bool isReceiverTypeName(llvm::StringRef name) {
  return canonicalResolvedTypeName(name) == "Receiver";
}

inline std::string typeExprToTypeName(const ast::TypeExpr &te,
                                      ResolveTypeAliasExprFn resolveAlias) {
  auto actor = typeExprToActorName(te, resolveAlias);
  if (!actor.empty())
    return actor;
  auto classified = classifyResolvedType(te, resolveAlias);
  if (classified.named)
    return classified.named->name;
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

/// Convert a type to the memref slot element type used for variable and
/// return-value storage. Only pointer-like Hew values need LLVM pointer slots;
/// other semantic types can stay in their native form.
inline mlir::Type toSlotStorageType(mlir::Type type) {
  return isPointerLikeType(type) ? toLLVMStorageType(type) : type;
}

/// Create an integer constant with a given type.
inline mlir::Value createIntConstant(mlir::OpBuilder &builder, mlir::Location loc, mlir::Type type,
                                     int64_t value) {
  return mlir::arith::ConstantIntOp::create(builder, loc, type, value);
}

/// Create a type-appropriate zero/default value (works for int, float, struct).
inline mlir::Value createDefaultValue(mlir::OpBuilder &builder, mlir::Location loc,
                                      mlir::Type type) {
  if (mlir::isa<mlir::IntegerType>(type))
    return mlir::arith::ConstantIntOp::create(builder, loc, type, 0);
  if (mlir::isa<mlir::FloatType>(type))
    return mlir::arith::ConstantOp::create(builder, loc, builder.getFloatAttr(type, 0.0));
  if (mlir::isa<mlir::LLVM::LLVMPointerType>(type))
    return mlir::LLVM::ZeroOp::create(builder, loc, type);
  // Hew pointer-like types: create a null pointer then cast to the Hew type
  if (isPointerLikeType(type) && !mlir::isa<mlir::LLVM::LLVMPointerType>(type)) {
    auto ptrType = mlir::LLVM::LLVMPointerType::get(type.getContext());
    auto zero = mlir::LLVM::ZeroOp::create(builder, loc, ptrType);
    return hew::BitcastOp::create(builder, loc, type, zero);
  }
  if (mlir::isa<mlir::LLVM::LLVMStructType>(type) || mlir::isa<mlir::LLVM::LLVMArrayType>(type))
    return mlir::LLVM::UndefOp::create(builder, loc, type);
  // Hew tuple/array types: create via dialect ops with default elements
  if (auto tupleType = mlir::dyn_cast<hew::HewTupleType>(type)) {
    llvm::SmallVector<mlir::Value, 4> elements;
    for (auto elemType : tupleType.getElementTypes())
      elements.push_back(createDefaultValue(builder, loc, elemType));
    return hew::TupleCreateOp::create(builder, loc, tupleType, elements);
  }
  if (auto arrayType = mlir::dyn_cast<hew::HewArrayType>(type)) {
    llvm::SmallVector<mlir::Value, 8> elements;
    for (int64_t i = 0; i < arrayType.getSize(); ++i)
      elements.push_back(createDefaultValue(builder, loc, arrayType.getElementType()));
    return hew::ArrayCreateOp::create(builder, loc, arrayType, elements);
  }
  if (mlir::isa<hew::HewTraitObjectType>(type)) {
    auto ptrType = mlir::LLVM::LLVMPointerType::get(type.getContext());
    auto nullPtr = mlir::LLVM::ZeroOp::create(builder, loc, ptrType);
    auto nullVtable = mlir::LLVM::ZeroOp::create(builder, loc, ptrType);
    return hew::TraitObjectCreateOp::create(builder, loc, type, nullPtr, nullVtable);
  }
  if (mlir::isa<hew::ClosureType>(type)) {
    auto ptrType = mlir::LLVM::LLVMPointerType::get(type.getContext());
    auto nullFn = mlir::LLVM::ZeroOp::create(builder, loc, ptrType);
    auto nullEnv = mlir::LLVM::ZeroOp::create(builder, loc, ptrType);
    return hew::ClosureCreateOp::create(builder, loc, type, nullFn, nullEnv);
  }
  // Hew dialect enum types: produce a default-valued enum via hew.enum_construct
  if (auto opt = mlir::dyn_cast<hew::OptionEnumType>(type)) {
    // Default Option: None (variant 0, no payloads). mlir::ArrayAttr{} is
    // intentionally null (absent) — verify() skips position checking when
    // payloads is empty.
    return hew::EnumConstructOp::create(builder, loc, type, static_cast<uint32_t>(0),
                                        llvm::StringRef("Option"), mlir::ValueRange{},
                                        /*payload_positions=*/mlir::ArrayAttr{});
  }
  if (auto res = mlir::dyn_cast<hew::ResultEnumType>(type)) {
    // Default Result: Ok(default_ok_value)
    auto okDefault = createDefaultValue(builder, loc, res.getOkType());
    return hew::EnumConstructOp::create(builder, loc, type, static_cast<uint32_t>(0),
                                        llvm::StringRef("__Result"), mlir::ValueRange{okDefault},
                                        /*payload_positions=*/builder.getI64ArrayAttr({1}));
  }
  // No valid default — this indicates a type we haven't handled.
  llvm::errs() << "MLIRGen: no default value for type: " << type << "\n";
  llvm::report_fatal_error("createDefaultValue: unhandled type");
}

/// Build an I64ArrayAttr for explicit enum payload positions, or nullptr
/// if positions are the default (1, 2, 3, ...).
inline mlir::ArrayAttr buildPayloadPositionsAttr(mlir::OpBuilder &builder,
                                                 llvm::ArrayRef<int64_t> positions,
                                                 size_t payloadCount) {
  if (positions.size() != payloadCount)
    return nullptr;
  for (size_t i = 0; i < positions.size(); ++i) {
    if (positions[i] != static_cast<int64_t>(i) + 1)
      return builder.getI64ArrayAttr(positions);
  }
  return nullptr;
}

/// Check if a statement has nested sub-statements (compound control flow).
inline bool stmtIsCompound(const ast::Stmt &s) {
  return std::holds_alternative<ast::StmtIf>(s.kind) ||
         std::holds_alternative<ast::StmtIfLet>(s.kind) ||
         std::holds_alternative<ast::StmtWhile>(s.kind) ||
         std::holds_alternative<ast::StmtFor>(s.kind) ||
         std::holds_alternative<ast::StmtLoop>(s.kind) ||
         std::holds_alternative<ast::StmtMatch>(s.kind);
}

/// Check if a statement might contain a return (recursively).
inline bool stmtMightContainReturn(const ast::Stmt &s) {
  return std::holds_alternative<ast::StmtReturn>(s.kind) || stmtIsCompound(s);
}

/// Check if a statement might contain break or continue.
inline bool stmtMightContainBreakOrContinue(const ast::Stmt &s) {
  return std::holds_alternative<ast::StmtBreak>(s.kind) ||
         std::holds_alternative<ast::StmtContinue>(s.kind) ||
         std::holds_alternative<ast::StmtReturn>(s.kind) || stmtIsCompound(s);
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

/// Extract the payload type at a given struct field index from any enum-like
/// type: LLVMStructType, OptionEnumType, or ResultEnumType.
inline mlir::Type getEnumFieldType(mlir::Type type, int64_t idx) {
  if (auto st = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(type))
    return st.getBody()[idx];
  if (auto ot = mlir::dyn_cast<hew::OptionEnumType>(type)) {
    if (idx == 1)
      return ot.getInnerType();
    return mlir::IntegerType::get(type.getContext(), 32);
  }
  if (auto rt = mlir::dyn_cast<hew::ResultEnumType>(type)) {
    if (idx == 1)
      return rt.getOkType();
    if (idx == 2)
      return rt.getErrType();
    return mlir::IntegerType::get(type.getContext(), 32);
  }
  return nullptr;
}

/// Check if a type is an enum-like type (has a tag + payload struct layout).
inline bool isEnumLikeType(mlir::Type type) {
  return mlir::isa<mlir::LLVM::LLVMStructType, hew::OptionEnumType, hew::ResultEnumType>(type);
}

} // namespace hew

#endif // HEW_MLIR_MLIRGEN_HELPERS_H
