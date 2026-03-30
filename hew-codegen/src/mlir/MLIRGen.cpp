//===- MLIRGen.cpp - AST-to-MLIR lowering for Hew -------------------------===//
//
// Implements the MLIRGen class that walks the Hew AST and emits MLIR
// operations using func, arith, scf, memref, and the custom Hew dialect.
//
// Phase 1 covers: functions, arithmetic, comparisons, let/var, if/else,
// while, for, loop, print/println, function calls, return.
//
//===----------------------------------------------------------------------===//

#include "hew/mlir/MLIRGen.h"
#include "hew/ast_helpers.h"
#include "hew/mlir/HewDialect.h"
#include "hew/mlir/HewOps.h"
#include "hew/mlir/HewTypes.h"

#include "mlir/Dialect/Arith/IR/Arith.h"
#include "mlir/Dialect/ControlFlow/IR/ControlFlowOps.h"
#include "mlir/Dialect/Func/IR/FuncOps.h"
#include "mlir/Dialect/LLVMIR/LLVMDialect.h"
#include "mlir/Dialect/MemRef/IR/MemRef.h"
#include "mlir/Dialect/SCF/IR/SCF.h"
#include "mlir/IR/Builders.h"
#include "mlir/IR/BuiltinAttributes.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/BuiltinTypes.h"
#include "mlir/IR/Diagnostics.h"
#include "mlir/IR/Location.h"
#include "mlir/IR/Value.h"
#include "mlir/IR/Verifier.h"

#include "llvm/ADT/ScopedHashTable.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

#include "MLIRGenHelpers.h"

#include <algorithm>
#include <cassert>
#include <functional>
#include <iostream>
#include <map>
#include <string>

using namespace hew;
using namespace mlir;

// ============================================================================
// Helpers
// ============================================================================

static ast::Spanned<ast::TypeExpr> cloneSpannedTypeExpr(const ast::Spanned<ast::TypeExpr> &type);

static std::unique_ptr<ast::Spanned<ast::TypeExpr>>
cloneTypeExprPtr(const std::unique_ptr<ast::Spanned<ast::TypeExpr>> &type) {
  if (!type)
    return nullptr;
  return std::make_unique<ast::Spanned<ast::TypeExpr>>(cloneSpannedTypeExpr(*type));
}

static std::vector<ast::Spanned<ast::TypeExpr>>
cloneTypeExprList(const std::vector<ast::Spanned<ast::TypeExpr>> &types) {
  std::vector<ast::Spanned<ast::TypeExpr>> cloned;
  cloned.reserve(types.size());
  for (const auto &type : types)
    cloned.push_back(cloneSpannedTypeExpr(type));
  return cloned;
}

static std::optional<std::vector<ast::Spanned<ast::TypeExpr>>>
cloneOptionalTypeExprList(const std::optional<std::vector<ast::Spanned<ast::TypeExpr>>> &types) {
  if (!types)
    return std::nullopt;
  return cloneTypeExprList(*types);
}

static ast::TraitBound cloneTraitBound(const ast::TraitBound &bound) {
  ast::TraitBound cloned;
  cloned.name = bound.name;
  cloned.type_args = cloneOptionalTypeExprList(bound.type_args);
  return cloned;
}

static ast::TypeExpr cloneTypeExpr(const ast::TypeExpr &type) {
  ast::TypeExpr cloned;

  if (auto *named = std::get_if<ast::TypeNamed>(&type.kind)) {
    ast::TypeNamed namedClone;
    namedClone.name = named->name;
    namedClone.type_args = cloneOptionalTypeExprList(named->type_args);
    cloned.kind = std::move(namedClone);
    return cloned;
  }

  if (auto *result = std::get_if<ast::TypeResult>(&type.kind)) {
    ast::TypeResult resultClone;
    resultClone.ok = cloneTypeExprPtr(result->ok);
    resultClone.err = cloneTypeExprPtr(result->err);
    cloned.kind = std::move(resultClone);
    return cloned;
  }

  if (auto *opt = std::get_if<ast::TypeOption>(&type.kind)) {
    ast::TypeOption optionClone;
    optionClone.inner = cloneTypeExprPtr(opt->inner);
    cloned.kind = std::move(optionClone);
    return cloned;
  }

  if (auto *tuple = std::get_if<ast::TypeTuple>(&type.kind)) {
    ast::TypeTuple tupleClone;
    tupleClone.elements = cloneTypeExprList(tuple->elements);
    cloned.kind = std::move(tupleClone);
    return cloned;
  }

  if (auto *array = std::get_if<ast::TypeArray>(&type.kind)) {
    ast::TypeArray arrayClone;
    arrayClone.element = cloneTypeExprPtr(array->element);
    arrayClone.size = array->size;
    cloned.kind = std::move(arrayClone);
    return cloned;
  }

  if (auto *slice = std::get_if<ast::TypeSlice>(&type.kind)) {
    ast::TypeSlice sliceClone;
    sliceClone.inner = cloneTypeExprPtr(slice->inner);
    cloned.kind = std::move(sliceClone);
    return cloned;
  }

  if (auto *fn = std::get_if<ast::TypeFunction>(&type.kind)) {
    ast::TypeFunction fnClone;
    fnClone.params = cloneTypeExprList(fn->params);
    fnClone.return_type = cloneTypeExprPtr(fn->return_type);
    cloned.kind = std::move(fnClone);
    return cloned;
  }

  if (auto *ptr = std::get_if<ast::TypePointer>(&type.kind)) {
    ast::TypePointer ptrClone;
    ptrClone.is_mutable = ptr->is_mutable;
    ptrClone.pointee = cloneTypeExprPtr(ptr->pointee);
    cloned.kind = std::move(ptrClone);
    return cloned;
  }

  if (auto *traitObj = std::get_if<ast::TypeTraitObject>(&type.kind)) {
    ast::TypeTraitObject traitClone;
    traitClone.bounds.reserve(traitObj->bounds.size());
    for (const auto &bound : traitObj->bounds)
      traitClone.bounds.push_back(cloneTraitBound(bound));
    cloned.kind = std::move(traitClone);
    return cloned;
  }

  if (std::holds_alternative<ast::TypeInfer>(type.kind)) {
    cloned.kind = ast::TypeInfer{};
    return cloned;
  }

  llvm_unreachable("unhandled TypeExpr alternative");
}

static ast::Spanned<ast::TypeExpr> cloneSpannedTypeExpr(const ast::Spanned<ast::TypeExpr> &type) {
  return {cloneTypeExpr(type.value), type.span};
}

static ast::VariantDecl cloneVariantDecl(const ast::VariantDecl &variant) {
  ast::VariantDecl cloned;
  cloned.name = variant.name;

  if (std::holds_alternative<ast::VariantDecl::VariantUnit>(variant.kind)) {
    cloned.kind = ast::VariantDecl::VariantUnit{};
    return cloned;
  }

  if (auto *tuple = std::get_if<ast::VariantDecl::VariantTuple>(&variant.kind)) {
    ast::VariantDecl::VariantTuple tupleClone;
    tupleClone.fields = cloneTypeExprList(tuple->fields);
    cloned.kind = std::move(tupleClone);
    return cloned;
  }

  if (auto *strct = std::get_if<ast::VariantDecl::VariantStruct>(&variant.kind)) {
    ast::VariantDecl::VariantStruct structClone;
    structClone.fields.reserve(strct->fields.size());
    for (const auto &field : strct->fields) {
      ast::VariantDecl::VariantStructField clonedField;
      clonedField.name = field.name;
      clonedField.ty = cloneSpannedTypeExpr(field.ty);
      structClone.fields.push_back(std::move(clonedField));
    }
    cloned.kind = std::move(structClone);
    return cloned;
  }

  llvm_unreachable("unhandled VariantDecl alternative");
}

/// Convert a TypeDecl with wire metadata into a WireDecl for the existing
/// codegen pipeline. This is temporary until the wire codegen is refactored
/// to work directly with TypeDecl.
static ast::WireDecl wireMetadataToWireDecl(const ast::TypeDecl &td) {
  ast::WireDecl wd;
  wd.visibility = td.visibility;
  wd.kind =
      (td.kind == ast::TypeDeclKind::Enum) ? ast::WireDeclKind::Enum : ast::WireDeclKind::Struct;
  wd.name = td.name;

  const auto &wm = *td.wire;

  // Build a map from field name → type string from body items
  std::unordered_map<std::string, std::string> fieldTypeMap;
  for (const auto &bodyItem : td.body) {
    if (auto *f = std::get_if<ast::TypeBodyItemField>(&bodyItem.kind)) {
      // For wire types, fields are simple Named types
      if (auto *named = std::get_if<ast::TypeNamed>(&f->ty.value.kind)) {
        fieldTypeMap[f->name] = named->name;
      }
    }
  }

  // Convert WireFieldMeta → WireFieldDecl
  for (const auto &fm : wm.field_meta) {
    ast::WireFieldDecl wf;
    wf.name = fm.field_name;
    wf.ty = fieldTypeMap.count(fm.field_name) ? fieldTypeMap[fm.field_name] : "";
    wf.field_number = fm.field_number;
    wf.is_optional = fm.is_optional;
    wf.is_repeated = fm.is_repeated;
    wf.is_reserved = false;
    wf.is_deprecated = fm.is_deprecated;
    wf.json_name = fm.json_name;
    wf.yaml_name = fm.yaml_name;
    wf.since = fm.since;
    wd.fields.push_back(std::move(wf));
  }

  for (const auto &bodyItem : td.body) {
    if (auto *variant = std::get_if<ast::TypeBodyVariant>(&bodyItem.kind))
      wd.variants.push_back(cloneVariantDecl(variant->variant));
  }

  wd.json_case = wm.json_case;
  wd.yaml_case = wm.yaml_case;
  wd.version = wm.version;
  wd.min_version = wm.min_version;
  return wd;
}

// ============================================================================
// Constructor
// ============================================================================

MLIRGen::MLIRGen(mlir::MLIRContext &context, const std::string &targetTriple,
                 const std::string &sourcePath, const std::vector<size_t> &lineMap)
    : lineMap_(lineMap), context(context), builder(&context), targetTriple(targetTriple),
      currentLoc(builder.getUnknownLoc()) {
  fileIdentifier = builder.getStringAttr(sourcePath.empty() ? "<unknown>" : sourcePath);
  isWasm32_ = targetTriple.find("wasm32") != std::string::npos;
  cachedSizeType_ = mlir::IntegerType::get(&context, isWasm32_ ? 32 : 64);
}

void MLIRGen::initReturnFlagAndSlot(mlir::ArrayRef<mlir::Type> resultTypes,
                                    mlir::Location location) {
  auto i1Type = builder.getI1Type();
  auto flagMemrefType = mlir::MemRefType::get({}, i1Type);
  returnFlag = mlir::memref::AllocaOp::create(builder, location, flagMemrefType);
  auto falseVal = createIntConstant(builder, location, i1Type, 0);
  mlir::memref::StoreOp::create(builder, location, falseVal, returnFlag);

  // earlyReturnFlag: set ONLY by generateReturnStmt, not by trailing
  // expressions.  Used to distinguish early return from normal flow
  // in path-specific drop logic.
  earlyReturnFlag = mlir::memref::AllocaOp::create(builder, location, flagMemrefType);
  mlir::memref::StoreOp::create(builder, location, falseVal, earlyReturnFlag);

  if (!resultTypes.empty() && !mlir::isa<mlir::LLVM::LLVMStructType>(resultTypes[0]) &&
      !mlir::isa<mlir::LLVM::LLVMArrayType>(resultTypes[0]) &&
      !mlir::isa<hew::HewTupleType>(resultTypes[0]) &&
      !mlir::isa<hew::HewArrayType>(resultTypes[0]) &&
      !mlir::isa<hew::HewTraitObjectType>(resultTypes[0])) {
    auto storageType = toSlotStorageType(resultTypes[0]);
    auto slotMemrefType = mlir::MemRefType::get({}, storageType);
    returnSlot = mlir::memref::AllocaOp::create(builder, location, slotMemrefType);
    if (storageType != resultTypes[0])
      slotSemanticTypes[returnSlot] = resultTypes[0];
  }
}

void MLIRGen::ensureReturnSlot(mlir::Location location) {
  if (returnSlot)
    return; // Already created (simple types create it eagerly).
  if (!currentFunction || currentFunction.getResultTypes().empty())
    return;

  auto resultType = currentFunction.getResultTypes()[0];

  // For aggregate types (structs, tuples, arrays, trait objects), create
  // the returnSlot lazily at the function entry block. This avoids enabling
  // return guards for functions that don't need them (which would change
  // how body values are produced), while still supporting early returns from
  // nested SCF regions.
  mlir::OpBuilder::InsertionGuard guard(builder);
  auto &entryBlock = currentFunction.getBody().front();
  builder.setInsertionPointToStart(&entryBlock);

  auto slotMemrefType = mlir::MemRefType::get({}, resultType);
  returnSlot = mlir::memref::AllocaOp::create(builder, location, slotMemrefType);
  returnSlotIsLazy = true;
}

// ============================================================================
// Name mangling
// ============================================================================

std::string MLIRGen::mangleName(const std::vector<std::string> &modulePath,
                                const std::string &typeName, const std::string &funcName) {
  if (funcName == "main")
    return "main";
  std::string mangled = "_H";
  for (const auto &seg : modulePath) {
    mangled += "M" + std::to_string(seg.size()) + seg;
  }
  if (!typeName.empty()) {
    mangled += "T" + std::to_string(typeName.size()) + typeName;
  }
  mangled += "F" + std::to_string(funcName.size()) + funcName;
  return mangled;
}

mlir::IntegerType MLIRGen::sizeType() const {
  return cachedSizeType_;
}

// ============================================================================
// Location helper
// ============================================================================

std::pair<unsigned, unsigned> MLIRGen::byteOffsetToLineCol(size_t offset) const {
  if (lineMap_.empty())
    return {0, 0};
  auto it = std::upper_bound(lineMap_.begin(), lineMap_.end(), offset);
  if (it == lineMap_.begin())
    return {1, static_cast<unsigned>(offset + 1)};
  --it;
  unsigned line = static_cast<unsigned>(std::distance(lineMap_.begin(), it)) + 1;
  unsigned col = static_cast<unsigned>(offset - *it) + 1;
  return {line, col};
}

mlir::Location MLIRGen::loc(const ast::Span &span) {
  auto [line, col] = byteOffsetToLineCol(span.start);
  if (line == 0) {
    // No line map — fall back to byte offset as before
    return mlir::FileLineColLoc::get(fileIdentifier, static_cast<unsigned>(span.start), 0);
  }
  return mlir::FileLineColLoc::get(fileIdentifier, line, col);
}

// ============================================================================
// Type conversion
// ============================================================================

mlir::Type MLIRGen::defaultIntType() {
  return builder.getI64Type();
}

mlir::Type MLIRGen::defaultFloatType() {
  return builder.getF64Type();
}

std::string MLIRGen::resolveTypeAlias(const std::string &name) const {
  auto it = typeAliases.find(name);
  if (it != typeAliases.end()) {
    if (auto *named = std::get_if<ast::TypeNamed>(&it->second->kind))
      return resolveTypeAlias(named->name); // recurse for chained aliases
  }
  return name;
}

mlir::Type MLIRGen::convertType(const ast::TypeExpr &type,
                               std::optional<mlir::Location> errorLoc) {
  auto diagLoc = errorLoc.value_or(currentLoc);
  if (auto *named = std::get_if<ast::TypeNamed>(&type.kind)) {
    // Resolve type parameter substitutions (generics monomorphization) by
    // replacing the name and falling through to the main resolution logic.
    // This ensures substituted names get the full resolution path (primitives,
    // Vec, HashMap, generic structs, etc.) instead of an incomplete copy.
    std::string name = named->name;
    bool fromSubstitution = false;
    auto subst = typeParamSubstitutions.find(name);
    if (subst != typeParamSubstitutions.end()) {
      fromSubstitution = true;
      name = subst->second;
    }
    // Check for type aliases
    auto alias = typeAliases.find(name);
    if (alias != typeAliases.end()) {
      return convertType(*alias->second, errorLoc);
    }
    if (name == "i8")
      return builder.getIntegerType(8);
    if (name == "i16")
      return builder.getIntegerType(16);
    if (name == "i32")
      return builder.getI32Type();
    if (name == "i64" || name == "int" || name == "Int")
      return builder.getI64Type();
    // Unsigned integers: use signless MLIR types (arith ops require signless)
    if (name == "u8" || name == "byte")
      return builder.getIntegerType(8);
    if (name == "u16")
      return builder.getIntegerType(16);
    if (name == "u32")
      return builder.getIntegerType(32);
    if (name == "u64" || name == "uint")
      return builder.getIntegerType(64);
    if (name == "f32")
      return builder.getF32Type();
    if (name == "f64" || name == "float")
      return builder.getF64Type();
    if (name == "bool")
      return builder.getI1Type();
    if (name == "Range") {
      if (!named->type_args || named->type_args->empty()) {
        emitError(diagLoc) << "Range type requires a type argument";
        return mlir::NoneType::get(&context);
      }
      auto elemType = convertTypeOrError(
          (*named->type_args)[0].value, "cannot resolve element type for Range", errorLoc);
      if (!elemType)
        return nullptr;
      return hew::HewTupleType::get(&context, {elemType, elemType});
    }
    if (name == "char")
      return builder.getI32Type();
    if (name == "duration")
      return builder.getI64Type();
    if (name == "String" || name == "string" || name == "str")
      return hew::StringRefType::get(&context);
    // bytes = mutable byte buffer; stored as Vec<i32> to reuse existing runtime
    if (name == "bytes")
      return hew::VecType::get(&context, builder.getI32Type());
    // Vec<T>: extract element type from generic args
    if (name == "Vec") {
      if (!(named->type_args && !named->type_args->empty())) {
        ++errorCount_;
        emitError(diagLoc)
            << "cannot determine element type for Vec; add explicit type annotation";
        return nullptr;
      }
      auto elemType = convertTypeOrError(
          (*named->type_args)[0].value, "cannot resolve element type for Vec", errorLoc);
      if (!elemType)
        return nullptr;
      return hew::VecType::get(&context, elemType);
    }
    // HashMap<K,V>: extract key/value types from generic args
    if (name == "HashMap") {
      if (!(named->type_args && named->type_args->size() >= 2)) {
        ++errorCount_;
        emitError(diagLoc)
            << "cannot determine key/value types for HashMap; add explicit type annotation";
        return nullptr;
      }
      auto keyType = convertTypeOrError(
          (*named->type_args)[0].value, "cannot resolve key type for HashMap", errorLoc);
      if (!keyType)
        return nullptr;
      auto valType = convertTypeOrError(
          (*named->type_args)[1].value, "cannot resolve value type for HashMap", errorLoc);
      if (!valType)
        return nullptr;
      return hew::HashMapType::get(&context, keyType, valType);
    }
    // HashSet<T>: opaque pointer (backed by HashMap<T, ()> in runtime)
    if (name == "HashSet") {
      if (!(named->type_args && !named->type_args->empty())) {
        ++errorCount_;
        emitError(diagLoc)
            << "cannot determine element type for HashSet; add explicit type annotation";
        return nullptr;
      }
      // HashSet is an opaque handle type
      return hew::HandleType::get(&context, builder.getStringAttr("HashSet"));
    }
    if (name == "ActorRef" || name == "Actor")
      return hew::ActorRefType::get(&context);
    if (name == "Task" || name == "scope.Task")
      return hew::HandleType::get(&context, builder.getStringAttr("Task"));
    // Stream<T> and Sink<T>: opaque heap pointers to HewStream / HewSink
    if (name == "Stream" || name == "Sink" || name == "stream.Stream" || name == "stream.Sink" ||
        name == "StreamPair" || name == "stream.StreamPair")
      return mlir::LLVM::LLVMPointerType::get(&context);
    // Sender<T> and Receiver<T>: opaque MPSC channel handles
    if (name == "Sender" || name == "Receiver" ||
        name == "channel.Sender" || name == "channel.Receiver" ||
        name == "ChannelPair" || name == "channel.ChannelPair")
      return mlir::LLVM::LLVMPointerType::get(&context);
    // Data-driven handle type recognition (replaces hardcoded list).
    // Handle type metadata flows from the Rust type checker via serialization.
    if (knownHandleTypes.count(name)) {
      auto reprIt = handleTypeRepr.find(name);
      if (reprIt != handleTypeRepr.end() && reprIt->second == "i32")
        return builder.getI32Type();
      return hew::HandleType::get(&context, builder.getStringAttr(name));
    }
    // Actor type names resolve to typed actor refs
    if (actorRegistry.count(name))
      return hew::TypedActorRefType::get(&context, builder.getStringAttr(name));
    // Check if it's a registered struct type
    auto it = structTypes.find(name);
    if (it != structTypes.end())
      return it->second.mlirType;
    // Generic struct specialization: handles both cases:
    // 1. Explicit type args: Pair<int> (type_args present in the AST)
    // 2. Type param substitutions: Pair<T> where T→int via typeParamSubstitutions
    auto genStructIt = genericStructs.find(name);
    if (genStructIt != genericStructs.end()) {
      const auto *genDecl = genStructIt->second;
      // Resolve concrete type arg names for mangling
      std::vector<std::string> typeArgNames;
      bool hasTypeArgs = false;
      if (named->type_args && !named->type_args->empty() && genDecl->type_params &&
          genDecl->type_params->size() == named->type_args->size()) {
        // Explicit type args: Pair<int>, Box<Pair<int>> → resolve each recursively
        hasTypeArgs = true;
        for (const auto &ta : *named->type_args)
          typeArgNames.push_back(resolveTypeArgMangledName(ta.value));
      } else if (!typeParamSubstitutions.empty() && genDecl->type_params) {
        // Implicit via substitutions: Pair<T> with T→int active
        hasTypeArgs = true;
        for (const auto &tp : *genDecl->type_params) {
          auto substIt = typeParamSubstitutions.find(tp.name);
          if (substIt != typeParamSubstitutions.end())
            typeArgNames.push_back(substIt->second);
          else
            typeArgNames.push_back(tp.name); // unresolved — shouldn't happen
        }
      }
      if (hasTypeArgs && !typeArgNames.empty()) {
        // Build mangled name (e.g., Pair_int, Pair_float)
        std::string mangledName = name;
        for (const auto &ta : typeArgNames)
          mangledName += "_" + ta;
        // Already specialized?
        auto alreadyIt = structTypes.find(mangledName);
        if (alreadyIt != structTypes.end())
          return alreadyIt->second.mlirType;
        // Set up type param substitutions for field type resolution
        auto prevSubstitutions = std::move(typeParamSubstitutions);
        typeParamSubstitutions.clear();
        for (size_t i = 0; i < genDecl->type_params->size(); ++i)
          typeParamSubstitutions[(*genDecl->type_params)[i].name] = typeArgNames[i];
        // Register under the base name, then move to mangled name
        registerTypeDecl(*genDecl);
        auto baseIt = structTypes.find(name);
        if (baseIt != structTypes.end()) {
          auto info = std::move(baseIt->second);
          structTypes.erase(baseIt);
          auto mangledStructType = mlir::LLVM::LLVMStructType::getIdentified(&context, mangledName);
          llvm::SmallVector<mlir::Type, 4> fieldTypes;
          for (const auto &f : info.fields)
            fieldTypes.push_back(f.type);
          if (!mangledStructType.isInitialized())
            (void)mangledStructType.setBody(fieldTypes, /*isPacked=*/false);
          info.name = mangledName;
          info.mlirType = mangledStructType;
          structTypes[mangledName] = std::move(info);
          // Record origin for generic impl method specialization.
          structTypeOrigin[mangledName] = {name, typeArgNames};
          typeParamSubstitutions = std::move(prevSubstitutions);
          return mangledStructType;
        }
        typeParamSubstitutions = std::move(prevSubstitutions);
      }
    }
    // Self-reference in indirect enum: return pointer (breaks infinite recursion)
    if (pendingIndirectEnums.count(name))
      return mlir::LLVM::LLVMPointerType::get(&context);
    // Check if it's a registered enum type
    auto enumIt = enumTypes.find(name);
    if (enumIt != enumTypes.end())
      return enumIt->second.mlirType;
    // Strip module prefix (e.g. "bench.Suite" → "Suite") for cross-module
    // struct/enum types defined in stdlib .hew files.
    auto dotPos = name.find('.');
    if (dotPos != std::string::npos) {
      auto unqualified = name.substr(dotPos + 1);
      auto sqIt = structTypes.find(unqualified);
      if (sqIt != structTypes.end())
        return sqIt->second.mlirType;
      auto eqIt = enumTypes.find(unqualified);
      if (eqIt != enumTypes.end())
        return eqIt->second.mlirType;
    }
    // Unresolved type: emit an error and force codegen failure.
    ++errorCount_;
    if (fromSubstitution) {
      emitError(diagLoc)
          << "unresolved type substitution '" << name << "' for type parameter '" << named->name
          << "' — no builtin, struct, enum, or actor with this name is defined";
    } else {
      emitError(diagLoc)
          << "unresolved type '" << name
          << "' — no struct, enum, or actor with this name is defined";
    }
    return mlir::NoneType::get(&context);
  }
  // Tuple types
  if (auto *tuple = std::get_if<ast::TypeTuple>(&type.kind)) {
    if (tuple->elements.empty())
      return mlir::NoneType::get(&context); // unit type
    llvm::SmallVector<mlir::Type, 4> elemTypes;
    for (const auto &elem : tuple->elements) {
      auto elemType =
          convertTypeOrError(elem.value, "cannot resolve element type in tuple", errorLoc);
      if (!elemType)
        return mlir::NoneType::get(&context);
      elemTypes.push_back(elemType);
    }
    return hew::HewTupleType::get(&context, elemTypes);
  }

  // Array types
  if (auto *array = std::get_if<ast::TypeArray>(&type.kind)) {
    auto elemType = convertTypeOrError(array->element->value,
                                       "cannot resolve element type in array", errorLoc);
    if (!elemType)
      return mlir::NoneType::get(&context);
    return hew::HewArrayType::get(&context, elemType, array->size);
  }

  // Option<T> → !hew.option<T>
  if (auto *option = std::get_if<ast::TypeOption>(&type.kind)) {
    auto innerType =
        convertTypeOrError(option->inner->value, "cannot resolve inner type for Option", errorLoc);
    if (!innerType)
      return mlir::NoneType::get(&context);
    return hew::OptionEnumType::get(&context, innerType);
  }

  // Result<T, E> → !hew.result<T, E>
  if (auto *result = std::get_if<ast::TypeResult>(&type.kind)) {
    auto okType =
        convertTypeOrError(result->ok->value, "cannot resolve ok type for Result", errorLoc);
    auto errType = okType
                       ? convertTypeOrError(result->err->value,
                                            "cannot resolve err type for Result", errorLoc)
                       : nullptr;
    if (!okType || !errType)
      return mlir::NoneType::get(&context);
    return hew::ResultEnumType::get(&context, okType, errType);
  }

  // Function types: fn(i32, i32) -> i32  →  !hew.closure<(i32, i32) -> i32>
  // All fn(T...) -> R type annotations produce a closure fat pointer so that
  // lambdas with captures can be passed as first-class values.
  if (auto *function = std::get_if<ast::TypeFunction>(&type.kind)) {
    llvm::SmallVector<mlir::Type, 4> paramTypes;
    for (const auto &pt : function->params) {
      auto paramType = convertTypeOrError(pt.value, "cannot resolve parameter type in function type",
                                          errorLoc);
      if (!paramType)
        return mlir::NoneType::get(&context);
      paramTypes.push_back(paramType);
    }
    // Use NoneType as sentinel for void return (no return type)
    mlir::Type retType = function->return_type ? convertType(function->return_type->value, errorLoc)
                                               : mlir::NoneType::get(&context);
    return hew::ClosureType::get(&context, paramTypes, retType);
  }

  // Trait object types: dyn Trait → fat pointer {data_ptr, type_tag}
  if (auto *to = std::get_if<ast::TypeTraitObject>(&type.kind)) {
    std::string traitName = !to->bounds.empty() ? to->bounds[0].name : "Unknown";
    return hew::HewTraitObjectType::get(&context, traitName);
  }

  ++errorCount_;
  emitError(diagLoc)
      << "unsupported type expression in MLIR codegen (kind index " << type.kind.index() << ")";
  return mlir::NoneType::get(&context);
}

// ============================================================================
// Type conversion with validation
// ============================================================================

mlir::Type MLIRGen::convertTypeOrError(const ast::TypeExpr &type, llvm::StringRef context,
                                       std::optional<mlir::Location> errorLoc) {
  auto diagLoc = errorLoc.value_or(currentLoc);
  auto result = convertType(type, errorLoc);
  if (!isValidType(result)) {
    ++errorCount_;
    emitError(diagLoc) << context;
    return nullptr;
  }
  return result;
}

// ============================================================================
// Type coercion
// ============================================================================

/// Recursively compare two LLVM struct types for layout compatibility.
/// Returns true if both have the same number of fields and each field pair
/// is either identical or (for nested structs) recursively compatible.
/// This handles generic type alias mangling (e.g. Pair_int vs Pair_i64
/// when both contain identical primitive fields).
static bool structsLayoutCompatible(mlir::LLVM::LLVMStructType a, mlir::LLVM::LLVMStructType b) {
  if (a == b)
    return true;
  auto bodyA = a.getBody();
  auto bodyB = b.getBody();
  if (bodyA.size() != bodyB.size())
    return false;
  for (size_t i = 0; i < bodyA.size(); ++i) {
    if (bodyA[i] == bodyB[i])
      continue;
    auto nestedA = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(bodyA[i]);
    auto nestedB = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(bodyB[i]);
    if (!nestedA || !nestedB || !structsLayoutCompatible(nestedA, nestedB))
      return false;
  }
  return true;
}

mlir::Value MLIRGen::coerceType(mlir::Value value, mlir::Type targetType, mlir::Location location,
                                bool isUnsigned) {
  if (!value || value.getType() == targetType)
    return value;

  // Tuple coercion: element-wise coercion
  auto srcTuple = mlir::dyn_cast<hew::HewTupleType>(value.getType());
  auto dstTuple = mlir::dyn_cast<hew::HewTupleType>(targetType);
  if (srcTuple && dstTuple &&
      srcTuple.getElementTypes().size() == dstTuple.getElementTypes().size()) {
    llvm::SmallVector<mlir::Value> elements;
    for (size_t i = 0; i < srcTuple.getElementTypes().size(); ++i) {
      auto elem = hew::TupleExtractOp::create(builder, location, srcTuple.getElementTypes()[i],
                                              value, builder.getI64IntegerAttr(i));
      auto coercedElem = coerceType(elem, dstTuple.getElementTypes()[i], location);
      if (!coercedElem)
        return nullptr;
      elements.push_back(coercedElem);
    }
    return hew::TupleCreateOp::create(builder, location, dstTuple, elements);
  }

  bool srcIsInt = llvm::isa<mlir::IntegerType>(value.getType());
  bool dstIsFloat = llvm::isa<mlir::FloatType>(targetType);
  bool srcIsFloat = llvm::isa<mlir::FloatType>(value.getType());
  bool dstIsInt = llvm::isa<mlir::IntegerType>(targetType);

  // Create a hew.cast and optionally tag it as unsigned for integer operands.
  auto emitCast = [&]() -> mlir::Value {
    auto castOp = hew::CastOp::create(builder, location, targetType, value);
    if (isUnsigned)
      castOp->setAttr("is_unsigned", builder.getBoolAttr(true));
    return castOp;
  };

  // Numeric conversions go through hew.cast so the dialect folder /
  // canonicalizer can optimise cast chains before lowering.
  if ((srcIsInt && dstIsFloat) || (srcIsFloat && dstIsInt))
    return emitCast();
  // int -> int width conversion (e.g. i64 literal to i32 field, or i32 to i64)
  if (srcIsInt && dstIsInt) {
    auto srcWidth = mlir::cast<mlir::IntegerType>(value.getType()).getWidth();
    auto dstWidth = mlir::cast<mlir::IntegerType>(targetType).getWidth();
    if (srcWidth != dstWidth)
      return emitCast();
  }
  // float -> float width conversion (f32 ↔ f64)
  if (srcIsFloat && dstIsFloat) {
    auto srcWidth = mlir::cast<mlir::FloatType>(value.getType()).getWidth();
    auto dstWidth = mlir::cast<mlir::FloatType>(targetType).getWidth();
    if (srcWidth != dstWidth)
      return emitCast();
  }
  // concrete struct → dyn Trait coercion
  if (auto traitObjType = mlir::dyn_cast<hew::HewTraitObjectType>(targetType)) {
    if (auto identStruct = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(value.getType())) {
      if (identStruct.isIdentified()) {
        std::string structName = identStruct.getName().str();
        std::string traitName = traitObjType.getTraitName().str();
        auto result = coerceToDynTrait(value, structName, traitName, location);
        if (result)
          return result;
      }
    }
  }
  // Hew pointer-like type → !llvm.ptr: bitcast for LLVM storage (e.g. actor
  // field assignment where the field slot is an opaque pointer).
  if (isPointerLikeType(value.getType()) && mlir::isa<mlir::LLVM::LLVMPointerType>(targetType)) {
    return hew::BitcastOp::create(builder, location, targetType, value);
  }
  // !llvm.ptr → Hew pointer-like type: bitcast on the return path.
  if (mlir::isa<mlir::LLVM::LLVMPointerType>(value.getType()) && isPointerLikeType(targetType)) {
    return hew::BitcastOp::create(builder, location, targetType, value);
  }

  // FunctionType → ClosureType: wrap a top-level function in a thunk closure.
  // The thunk ignores the env pointer and forwards to the real function.
  if (auto closureType = mlir::dyn_cast<hew::ClosureType>(targetType)) {
    auto srcFuncType = mlir::dyn_cast<mlir::FunctionType>(value.getType());
    if (srcFuncType) {
      auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);

      // Recover the function symbol name from the defining op.
      // This coercion path is only valid for named function references
      // (produced by func.constant); other values should not reach here.
      std::string funcName;
      if (auto constOp = value.getDefiningOp<mlir::func::ConstantOp>()) {
        funcName = constOp.getValue().str();
      }
      if (funcName.empty()) {
        // Not a named function reference — cannot generate a thunk.
        // Fall through to return the value unchanged.
        return value;
      }
      std::string thunkName = "__thunk_" + funcName;

      // Check thunk cache; generate only once per original function
      if (!closureThunkCache.count(funcName)) {
        // Build thunk signature: (ptr %env, user_params...) -> ret
        llvm::SmallVector<mlir::Type, 8> thunkParams;
        thunkParams.push_back(ptrType); // env (ignored)
        for (auto inTy : closureType.getInputTypes())
          thunkParams.push_back(inTy);

        auto retType = closureType.getResultType();
        bool hasReturn = retType && !mlir::isa<mlir::NoneType>(retType);
        auto thunkFuncType = hasReturn ? mlir::FunctionType::get(&context, thunkParams, {retType})
                                       : mlir::FunctionType::get(&context, thunkParams, {});

        auto savedIP = builder.saveInsertionPoint();
        builder.setInsertionPointToEnd(module.getBody());
        auto thunkOp = mlir::func::FuncOp::create(builder, location, thunkName, thunkFuncType);
        thunkOp.setVisibility(mlir::SymbolTable::Visibility::Private);

        auto &entry = *thunkOp.addEntryBlock();
        builder.setInsertionPointToStart(&entry);

        // Forward user args (skip env at index 0) to the real function
        llvm::SmallVector<mlir::Value, 8> forwardArgs;
        for (unsigned i = 1; i < entry.getNumArguments(); ++i)
          forwardArgs.push_back(entry.getArgument(i));

        auto realFunc = module.lookupSymbol<mlir::func::FuncOp>(funcName);
        if (!realFunc) {
          ++errorCount_;
          emitError(location) << "thunk target function '" << funcName
                              << "' not found in module";
          thunkOp.erase();
          builder.restoreInsertionPoint(savedIP);
          return value; // return original value unchanged
        }
        auto callOp = mlir::func::CallOp::create(builder, location, realFunc, forwardArgs);
        if (callOp.getNumResults() > 0)
          mlir::func::ReturnOp::create(builder, location, callOp.getResults());
        else
          mlir::func::ReturnOp::create(builder, location);

        builder.restoreInsertionPoint(savedIP);
        closureThunkCache[funcName] = thunkName;
      }

      auto &cachedThunkName = closureThunkCache[funcName];

      // Look up the thunk and create a closure with null env
      assert(module.lookupSymbol<mlir::func::FuncOp>(cachedThunkName) &&
             "thunk must exist after cache insert");
      auto fnPtrVal = hew::FuncPtrOp::create(builder, location, ptrType,
                                             mlir::SymbolRefAttr::get(&context, cachedThunkName));
      auto nullEnv = mlir::LLVM::ZeroOp::create(builder, location, ptrType);
      return hew::ClosureCreateOp::create(builder, location, closureType, fnPtrVal, nullEnv);
    }
  }

  // ClosureType → ClosureType coercion: return-type widening via wrapper thunk.
  // When a closure's inferred return type (e.g. i32 from String.len()) differs
  // from the expected return type (e.g. i64), generate a thin wrapper that
  // forwards the call and coerces the result.
  if (auto srcClosure = mlir::dyn_cast<hew::ClosureType>(value.getType())) {
    if (auto dstClosure = mlir::dyn_cast<hew::ClosureType>(targetType)) {
      if (srcClosure.getInputTypes() == dstClosure.getInputTypes() &&
          srcClosure.getResultType() != dstClosure.getResultType()) {
        auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
        auto srcRet = srcClosure.getResultType();
        auto dstRet = dstClosure.getResultType();

        std::string thunkName =
            "__closure_coerce_" + std::to_string(closureCoercionCounter++);

        // Wrapper env layout: { ptr fnPtr, ptr envPtr } — stores the
        // original closure's function pointer and environment.
        auto envStructType =
            mlir::LLVM::LLVMStructType::getLiteral(&context, {ptrType, ptrType});

        // Build thunk signature: (ptr %env, user_params...) -> dstRet
        // Use LLVM storage types for the thunk signature and the inner
        // indirect call so that func.call_indirect passes legality checks.
        auto llvmSrcRet = toLLVMStorageType(srcRet);
        auto llvmDstRet = toLLVMStorageType(dstRet);

        llvm::SmallVector<mlir::Type, 8> thunkParams;
        thunkParams.push_back(ptrType);
        for (auto inTy : dstClosure.getInputTypes())
          thunkParams.push_back(toLLVMStorageType(inTy));

        bool dstHasReturn = dstRet && !mlir::isa<mlir::NoneType>(dstRet);
        auto thunkFuncType =
            dstHasReturn ? mlir::FunctionType::get(&context, thunkParams, {llvmDstRet})
                         : mlir::FunctionType::get(&context, thunkParams, {});

        auto savedIP = builder.saveInsertionPoint();
        builder.setInsertionPointToEnd(module.getBody());
        auto thunkOp =
            mlir::func::FuncOp::create(builder, location, thunkName, thunkFuncType);
        thunkOp.setVisibility(mlir::SymbolTable::Visibility::Private);

        auto &entry = *thunkOp.addEntryBlock();
        builder.setInsertionPointToStart(&entry);

        auto envArg = entry.getArgument(0);

        // Extract the original closure's function pointer from the wrapper env.
        auto fnGep = mlir::LLVM::GEPOp::create(
            builder, location, ptrType, envStructType, envArg,
            llvm::ArrayRef<mlir::LLVM::GEPArg>{static_cast<int32_t>(0),
                                                static_cast<int32_t>(0)});
        auto origFnPtr =
            mlir::LLVM::LoadOp::create(builder, location, ptrType, fnGep);

        // Extract the original closure's environment pointer.
        auto envGep = mlir::LLVM::GEPOp::create(
            builder, location, ptrType, envStructType, envArg,
            llvm::ArrayRef<mlir::LLVM::GEPArg>{static_cast<int32_t>(0),
                                                static_cast<int32_t>(1)});
        auto origEnvPtr =
            mlir::LLVM::LoadOp::create(builder, location, ptrType, envGep);

        // Build the original closure's indirect-call signature.
        llvm::SmallVector<mlir::Type, 8> origCallParams;
        origCallParams.push_back(ptrType);
        for (auto inTy : srcClosure.getInputTypes())
          origCallParams.push_back(toLLVMStorageType(inTy));

        bool srcHasReturn = srcRet && !mlir::isa<mlir::NoneType>(srcRet);
        auto origCallFuncType =
            srcHasReturn
                ? mlir::FunctionType::get(&context, origCallParams, {llvmSrcRet})
                : mlir::FunctionType::get(&context, origCallParams, {});

        auto fnRef =
            hew::BitcastOp::create(builder, location, origCallFuncType, origFnPtr);

        llvm::SmallVector<mlir::Value, 8> callArgs;
        callArgs.push_back(origEnvPtr);
        for (unsigned i = 1; i < entry.getNumArguments(); ++i)
          callArgs.push_back(entry.getArgument(i));

        auto callOp =
            mlir::func::CallIndirectOp::create(builder, location, fnRef, callArgs);

        if (dstHasReturn && srcHasReturn) {
          auto result = callOp.getResult(0);
          auto coerced = coerceType(result, llvmDstRet, location);
          if (!coerced) {
            thunkOp.erase();
            builder.restoreInsertionPoint(savedIP);
            return nullptr;
          }
          mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{coerced});
        } else {
          mlir::func::ReturnOp::create(builder, location);
        }

        builder.restoreInsertionPoint(savedIP);

        // Populate the wrapper env with the original closure's pointers.
        auto srcFnPtr =
            hew::ClosureGetFnOp::create(builder, location, ptrType, value);
        auto srcEnvPtr =
            hew::ClosureGetEnvOp::create(builder, location, ptrType, value);

        // RC-clone the original env so it stays alive while the wrapper exists.
        hew::RcCloneOp::create(builder, location, ptrType, srcEnvPtr);

        auto envSize = hew::SizeOfOp::create(builder, location, sizeType(),
                                             mlir::TypeAttr::get(envStructType));
        auto nullData = mlir::LLVM::ZeroOp::create(builder, location, ptrType);
        auto nullDropFn = mlir::LLVM::ZeroOp::create(builder, location, ptrType);
        auto wrapperEnv = hew::RcNewOp::create(builder, location, ptrType,
                                               nullData, envSize, nullDropFn);

        auto fnStore = mlir::LLVM::GEPOp::create(
            builder, location, ptrType, envStructType, wrapperEnv,
            llvm::ArrayRef<mlir::LLVM::GEPArg>{static_cast<int32_t>(0),
                                                static_cast<int32_t>(0)});
        mlir::LLVM::StoreOp::create(builder, location, srcFnPtr, fnStore);

        auto envStore = mlir::LLVM::GEPOp::create(
            builder, location, ptrType, envStructType, wrapperEnv,
            llvm::ArrayRef<mlir::LLVM::GEPArg>{static_cast<int32_t>(0),
                                                static_cast<int32_t>(1)});
        mlir::LLVM::StoreOp::create(builder, location, srcEnvPtr, envStore);

        auto thunkFnPtr = hew::FuncPtrOp::create(
            builder, location, ptrType,
            mlir::SymbolRefAttr::get(&context, thunkName));
        return hew::ClosureCreateOp::create(builder, location, dstClosure,
                                            thunkFnPtr, wrapperEnv);
      }
    }
  }

  // [T; N] → Vec<T> coercion: create Vec, push each array element
  if (auto arrayType = mlir::dyn_cast<hew::HewArrayType>(value.getType())) {
    if (auto vecType = mlir::dyn_cast<hew::VecType>(targetType)) {
      auto elemType = vecType.getElementType();
      auto vec = hew::VecNewOp::create(builder, location, vecType).getResult();
      auto arraySize = arrayType.getSize();
      for (int64_t i = 0; i < arraySize; ++i) {
        auto elem = hew::ArrayExtractOp::create(builder, location, arrayType.getElementType(),
                                                value, builder.getI64IntegerAttr(i));
        auto coerced = coerceType(elem, elemType, location);
        if (!coerced)
          return nullptr;
        hew::VecPushOp::create(builder, location, vec, coerced);
      }
      return vec;
    }
  }

  // TypedActorRef<T> → ActorRef: typed actor refs are subtypes of untyped refs.
  // Both lower to !llvm.ptr at the LLVM level.
  if (mlir::isa<hew::TypedActorRefType>(value.getType()) &&
      mlir::isa<hew::ActorRefType>(targetType)) {
    return hew::BitcastOp::create(builder, location, targetType, value);
  }

  // Remote actor ref: i64 PID (from Node::lookup) used where an actor ref is
  // expected. Keep the value as i64 — ActorSendOp/AskOp lowering handles the
  // i64 target by routing through hew_actor_send_by_id.
  if (mlir::isa<mlir::IntegerType>(value.getType()) &&
      (mlir::isa<hew::TypedActorRefType>(targetType) || mlir::isa<hew::ActorRefType>(targetType))) {
    return value;
  }

  // Structurally identical LLVM structs with different mangled names (e.g.,
  // Wrapper_int vs Wrapper_i64 from generics with type aliases). Both have
  // the same field layout — return the value as-is. The source type name
  // propagates but is layout-compatible, so codegen produces correct LLVM IR.
  // Uses recursive comparison for nested generics (Box<Pair<int>> vs
  // Box<Pair<i64>>).
  if (auto srcStruct = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(value.getType())) {
    if (auto dstStruct = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(targetType)) {
      if (structsLayoutCompatible(srcStruct, dstStruct)) {
        return value;
      }
    }
  }

  // Fallthrough: no explicit coercion found. Fail closed so callers can stop
  // materialising downstream IR from a mismatched SSA value.
  ++errorCount_;
  emitError(location) << "coerceType: no known conversion from " << value.getType() << " to "
                      << targetType;
  return nullptr;
}

mlir::Value MLIRGen::coerceTypeForSink(mlir::Value value, mlir::Type targetType,
                                       mlir::Location location) {
  auto coerced = coerceType(value, targetType, location);
  if (coerced && coerced.getType() == targetType)
    return coerced;

  ++errorCount_;
  return createDefaultValue(builder, location, targetType);
}

// ============================================================================
// Symbol table operations
// ============================================================================

mlir::Value MLIRGen::createHoistedAlloca(mlir::Type storageType, mlir::Type semanticType) {
  auto memrefType = mlir::MemRefType::get({}, storageType);
  mlir::Value alloca;
  if (returnFlag && currentFunction) {
    auto savedIP = builder.saveInsertionPoint();
    auto &entryBlock = currentFunction.front();
    builder.setInsertionPointToStart(&entryBlock);
    alloca = mlir::memref::AllocaOp::create(builder, builder.getUnknownLoc(), memrefType);
    if (mlir::isa<mlir::LLVM::LLVMPointerType>(storageType) || isPointerLikeType(semanticType)) {
      auto zero = createDefaultValue(builder, builder.getUnknownLoc(), storageType);
      mlir::memref::StoreOp::create(builder, builder.getUnknownLoc(), zero, alloca);
    } else if (mlir::isa<mlir::LLVM::LLVMStructType>(storageType)) {
      // Struct types get zeroinitializer (all pointer fields null) so that
      // unconditional drops on zero-initialized allocas are safe.
      auto zero = mlir::LLVM::ZeroOp::create(builder, builder.getUnknownLoc(), storageType);
      mlir::memref::StoreOp::create(builder, builder.getUnknownLoc(), zero, alloca);
    }
    builder.restoreInsertionPoint(savedIP);
  } else {
    alloca = mlir::memref::AllocaOp::create(builder, builder.getUnknownLoc(), memrefType);
  }
  return alloca;
}

void MLIRGen::declareVariable(llvm::StringRef name, mlir::Value value) {
  // Intern the name so the StringRef stored in the ScopedHashTable
  // outlives any transient std::string (e.g. from ident_name()).
  name = intern(name.str());

  // When return guards are active, let-bindings may end up inside scf.if
  // guard regions. Raw SSA values defined in one guard region cannot be
  // referenced in a sibling guard region (dominance violation).  Promote
  // compatible types to memref (alloca+store) so that lookupVariable loads
  // them via memref::LoadOp.  The alloca is placed in the function entry
  // block so it dominates all uses.
  if (returnFlag && value && currentFunction) {
    auto semanticType = value.getType();
    auto storageType = toSlotStorageType(semanticType);
    bool canPromote = mlir::isa<mlir::IntegerType>(storageType) ||
                      mlir::isa<mlir::FloatType>(storageType) ||
                      mlir::isa<mlir::LLVM::LLVMPointerType>(storageType) ||
                      mlir::isa<mlir::IndexType>(storageType) || isPointerLikeType(semanticType);
    // Struct types only need alloca promotion when return guards are active
    // (returnSlotIsLazy), to handle let-bindings inside guarded scf.if regions.
    if (returnSlotIsLazy && mlir::isa<mlir::LLVM::LLVMStructType>(storageType))
      canPromote = true;
    if (canPromote) {
      auto alloca = createHoistedAlloca(storageType, semanticType);
      // Store the value at the current insertion point
      auto storedValue = coerceType(value, storageType, builder.getUnknownLoc());
      mlir::memref::StoreOp::create(builder, builder.getUnknownLoc(), storedValue, alloca);
      if (storageType != semanticType)
        slotSemanticTypes[alloca] = semanticType;
      mutableVars.insert(name, alloca);
      return;
    }
  }
  symbolTable.insert(name, value);
}

void MLIRGen::declareMutableVariable(llvm::StringRef name, mlir::Type type,
                                     mlir::Value initialValue) {
  name = intern(name.str());
  auto storageType = toSlotStorageType(type);
  auto alloca = createHoistedAlloca(storageType, type);
  if (initialValue) {
    auto storedValue = coerceType(initialValue, storageType, builder.getUnknownLoc());
    mlir::memref::StoreOp::create(builder, builder.getUnknownLoc(), storedValue, alloca);
  }
  if (storageType != type)
    slotSemanticTypes[alloca] = type;
  mutableVars.insert(name, alloca);
}

void MLIRGen::storeVariable(llvm::StringRef name, mlir::Value value) {
  auto it = getMutableVarSlot(name);
  if (!it) {
    ++errorCount_;
    emitError(builder.getUnknownLoc())
        << "cannot assign to undeclared mutable variable '" << name << "'";
    return;
  }
  // Heap-cell indirection: the memref holds a pointer to a heap cell.
  auto cellIt = heapCellValueTypes.find(it);
  if (cellIt != heapCellValueTypes.end()) {
    auto cellPtr = mlir::memref::LoadOp::create(builder, builder.getUnknownLoc(), it);
    mlir::LLVM::StoreOp::create(builder, builder.getUnknownLoc(), value, cellPtr);
    return;
  }
  auto storedValue = value;
  if (auto semIt = slotSemanticTypes.find(it); semIt != slotSemanticTypes.end()) {
    auto storageType = mlir::cast<mlir::MemRefType>(it.getType()).getElementType();
    storedValue = coerceType(value, storageType, builder.getUnknownLoc());
  }
  mlir::memref::StoreOp::create(builder, builder.getUnknownLoc(), storedValue, it);
}

mlir::Value MLIRGen::getMutableVarSlot(llvm::StringRef name) {
  auto slot = mutableVars.lookup(name);
  if (!slot)
    return nullptr;
  auto remap = heapCellRebindings.find(slot);
  if (remap != heapCellRebindings.end())
    return remap->second;
  return slot;
}

mlir::Value MLIRGen::lookupVariable(llvm::StringRef name) {
  // First check mutable variables (load from memref)
  auto mutVal = getMutableVarSlot(name);
  if (mutVal) {
    // Heap-cell indirection: the memref holds a pointer to a heap cell.
    auto cellIt = heapCellValueTypes.find(mutVal);
    if (cellIt != heapCellValueTypes.end()) {
      auto cellPtr = mlir::memref::LoadOp::create(builder, builder.getUnknownLoc(), mutVal);
      auto origType = cellIt->second;
      auto loadType = toLLVMStorageType(origType);
      mlir::Value loaded =
          mlir::LLVM::LoadOp::create(builder, builder.getUnknownLoc(), loadType, cellPtr);
      // Bitcast back to the original dialect type so callers see the
      // expected type (e.g. !hew.handle<...>).
      if (loadType != origType)
        loaded = hew::BitcastOp::create(builder, builder.getUnknownLoc(), origType, loaded);
      return loaded;
    }
    auto loaded =
        mlir::memref::LoadOp::create(builder, builder.getUnknownLoc(), mutVal).getResult();
    if (auto semIt = slotSemanticTypes.find(mutVal); semIt != slotSemanticTypes.end())
      return coerceType(loaded, semIt->second, builder.getUnknownLoc());
    return loaded;
  }
  // Then check immutable bindings
  auto val = symbolTable.lookup(name);
  if (val)
    return val;

  // Don't emit error here — caller may have fallback lookup
  return nullptr;
}

// ============================================================================
// Global string management
// ============================================================================

std::string MLIRGen::getOrCreateGlobalString(llvm::StringRef value) {
  std::string key = value.str();
  auto it = globalStrings.find(key);
  if (it != globalStrings.end())
    return it->second;

  std::string symName = "str" + std::to_string(globalStringCounter++);
  globalStrings[key] = symName;

  // Insert at module scope
  auto savedIP = builder.saveInsertionPoint();
  builder.setInsertionPointToStart(module.getBody());
  hew::GlobalStringOp::create(builder, builder.getUnknownLoc(), builder.getStringAttr(symName),
                              builder.getStringAttr(value));
  builder.restoreInsertionPoint(savedIP);
  return symName;
}

// ============================================================================
// Extern function declarations
// ============================================================================

mlir::func::FuncOp MLIRGen::getOrCreateExternFunc(llvm::StringRef name, mlir::FunctionType type) {
  if (auto func = module.lookupSymbol<mlir::func::FuncOp>(name))
    return func;
  auto savedIP = builder.saveInsertionPoint();
  builder.setInsertionPointToStart(module.getBody());
  auto funcOp = mlir::func::FuncOp::create(builder, builder.getUnknownLoc(), name, type);
  funcOp.setVisibility(mlir::SymbolTable::Visibility::Private);
  builder.restoreInsertionPoint(savedIP);
  return funcOp;
}

// ============================================================================
// Extern block generation
// ============================================================================

void MLIRGen::generateExternBlock(const ast::ExternBlock &block,
                                  std::optional<mlir::Location> fallbackLoc) {
  auto typeLoc = [&](const ast::Spanned<ast::TypeExpr> &type) {
    return type.span.end > type.span.start ? loc(type.span) : fallbackLoc.value_or(currentLoc);
  };

  for (const auto &fn : block.functions) {
    llvm::SmallVector<mlir::Type, 4> paramTypes;
    bool externOk = true;
    for (const auto &param : fn.params) {
      // Extern "C" functions always use LLVM-level types — convert any
      // Hew dialect types (handles, strings, vecs, …) to !llvm.ptr so
      // that the type conversion framework doesn't have to chase them.
      auto paramType = convertTypeOrError(param.ty.value,
                                          "cannot resolve parameter type in extern function",
                                          typeLoc(param.ty));
      if (!paramType) {
        externOk = false;
        break;
      }
      paramTypes.push_back(toLLVMStorageType(paramType));
    }
    if (!externOk)
      continue;

    mlir::Type resultType = nullptr;
    mlir::Type semanticResultType = nullptr;
    if (fn.return_type) {
      semanticResultType = convertTypeOrError(fn.return_type->value,
                                              "cannot resolve return type in extern function",
                                              typeLoc(*fn.return_type));
      if (!semanticResultType)
        continue;
      resultType = toLLVMStorageType(semanticResultType);
    }

    auto funcType = resultType ? mlir::FunctionType::get(&context, paramTypes, {resultType})
                               : mlir::FunctionType::get(&context, paramTypes, {});

    // If variadic, we need to use LLVM-level variadic support
    // For now, create a regular extern declaration
    getOrCreateExternFunc(fn.name, funcType);
    if (semanticResultType && mlir::isa<hew::VecType, hew::HashMapType, hew::StringRefType>(semanticResultType)) {
      externSemanticReturnTypes[fn.name] = semanticResultType;
    } else {
      externSemanticReturnTypes.erase(fn.name);
    }
  }
}

// ============================================================================
// Import generation (from extern_funcs)
// ============================================================================

void MLIRGen::generateImport(const ast::ImportDecl &decl) {
  // Import handling is done at the Rust frontend level; codegen is a no-op.
}

// ============================================================================
// Builtin function calls
// ============================================================================

mlir::Value MLIRGen::generateBuiltinCall(const std::string &name,
                                         const std::vector<ast::CallArg> &args,
                                         mlir::Location location) {
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);

  // println_str / print_str: takes a string (ptr), prints it
  if (name == "println_str" || name == "print_str") {
    if (args.empty()) {
      emitError(location) << name << " requires at least 1 argument";
      return nullptr;
    }
    auto val = generateExpression(ast::callArgExpr(args[0]).value);
    if (!val)
      return nullptr;
    bool newline = (name == "println_str");
    hew::PrintOp::create(builder, location, val, builder.getBoolAttr(newline));
    materializeTemporary(val, ast::callArgExpr(args[0]).value);
    return nullptr;
  }

  // println_int / print_int: takes an integer, prints it
  if (name == "println_int" || name == "print_int") {
    if (args.empty()) {
      emitError(location) << name << " requires at least 1 argument";
      return nullptr;
    }
    auto val = generateExpression(ast::callArgExpr(args[0]).value);
    if (!val)
      return nullptr;
    bool newline = (name == "println_int");
    auto printOp = hew::PrintOp::create(builder, location, val, builder.getBoolAttr(newline));
    if (auto *argType = resolvedTypeOf(ast::callArgExpr(args[0]).span))
      if (isUnsignedTypeExpr(*argType))
        printOp->setAttr("is_unsigned", builder.getBoolAttr(true));
    return nullptr;
  }

  // println_f64 / print_f64: takes a float, prints it
  if (name == "println_f64" || name == "print_f64") {
    if (args.empty()) {
      emitError(location) << name << " requires at least 1 argument";
      return nullptr;
    }
    auto val = generateExpression(ast::callArgExpr(args[0]).value);
    if (!val)
      return nullptr;
    bool newline = (name == "println_f64");
    hew::PrintOp::create(builder, location, val, builder.getBoolAttr(newline));
    return nullptr;
  }

  // println_bool / print_bool: takes a bool, prints it
  if (name == "println_bool" || name == "print_bool") {
    if (args.empty()) {
      emitError(location) << name << " requires at least 1 argument";
      return nullptr;
    }
    auto val = generateExpression(ast::callArgExpr(args[0]).value);
    if (!val)
      return nullptr;
    bool newline = (name == "println_bool");
    hew::PrintOp::create(builder, location, val, builder.getBoolAttr(newline));
    return nullptr;
  }

  // sqrt(x) -> f64
  if (name == "sqrt") {
    if (args.empty()) {
      emitError(location) << name << " requires at least 1 argument";
      return nullptr;
    }
    auto arg = generateExpression(ast::callArgExpr(args[0]).value);
    if (!arg)
      return nullptr;
    auto f64Type = builder.getF64Type();
    arg = coerceType(arg, f64Type, location);
    auto sqrtType = builder.getFunctionType({f64Type}, {f64Type});
    auto sqrtFunc = getOrCreateExternFunc("sqrt", sqrtType);
    return mlir::func::CallOp::create(builder, location, sqrtFunc, mlir::ValueRange{arg})
        .getResult(0);
  }

  // to_float(x) -> f64: convert any integer to f64
  if (name == "to_float") {
    if (args.empty()) {
      emitError(location) << name << " requires at least 1 argument";
      return nullptr;
    }
    auto arg = generateExpression(ast::callArgExpr(args[0]).value);
    if (!arg)
      return nullptr;
    return coerceType(arg, builder.getF64Type(), location);
  }

  // abs(x) -> i64
  if (name == "abs") {
    if (args.empty()) {
      emitError(location) << name << " requires at least 1 argument";
      return nullptr;
    }
    auto arg = generateExpression(ast::callArgExpr(args[0]).value);
    if (!arg)
      return nullptr;
    auto i64Type = builder.getI64Type();
    arg = coerceType(arg, i64Type, location);
    auto zero = mlir::arith::ConstantIntOp::create(builder, location, i64Type, 0);
    auto neg = mlir::arith::SubIOp::create(builder, location, zero, arg);
    auto cmp =
        mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::sgt, arg, zero);
    return mlir::arith::SelectOp::create(builder, location, cmp, arg, neg).getResult();
  }

  // min(a, b) -> i64
  if (name == "min") {
    if (args.size() < 2) {
      emitError(location) << name << " requires at least 2 arguments";
      return nullptr;
    }
    auto a = generateExpression(ast::callArgExpr(args[0]).value);
    auto b = generateExpression(ast::callArgExpr(args[1]).value);
    if (!a || !b)
      return nullptr;
    auto i64Type = builder.getI64Type();
    a = coerceType(a, i64Type, location);
    b = coerceType(b, i64Type, location);
    auto cmp =
        mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::slt, a, b);
    return mlir::arith::SelectOp::create(builder, location, cmp, a, b).getResult();
  }

  // max(a, b) -> i64
  if (name == "max") {
    if (args.size() < 2) {
      emitError(location) << name << " requires at least 2 arguments";
      return nullptr;
    }
    auto a = generateExpression(ast::callArgExpr(args[0]).value);
    auto b = generateExpression(ast::callArgExpr(args[1]).value);
    if (!a || !b)
      return nullptr;
    auto i64Type = builder.getI64Type();
    a = coerceType(a, i64Type, location);
    b = coerceType(b, i64Type, location);
    auto cmp =
        mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::sgt, a, b);
    return mlir::arith::SelectOp::create(builder, location, cmp, a, b).getResult();
  }

  // string_concat(a, b) -> string_ref
  if (name == "string_concat") {
    if (args.size() < 2) {
      emitError(location) << name << " requires at least 2 arguments";
      return nullptr;
    }
    auto a = generateExpression(ast::callArgExpr(args[0]).value);
    auto b = generateExpression(ast::callArgExpr(args[1]).value);
    if (!a || !b)
      return nullptr;
    materializeTemporary(a, ast::callArgExpr(args[0]).value);
    materializeTemporary(b, ast::callArgExpr(args[1]).value);
    return hew::StringConcatOp::create(builder, location, hew::StringRefType::get(&context), a, b)
        .getResult();
  }

  // string_length(s) -> i32
  if (name == "string_length") {
    if (args.empty()) {
      emitError(location) << name << " requires at least 1 argument";
      return nullptr;
    }
    auto s = generateExpression(ast::callArgExpr(args[0]).value);
    if (!s)
      return nullptr;
    materializeTemporary(s, ast::callArgExpr(args[0]).value);
    return hew::StringMethodOp::create(builder, location, builder.getI32Type(),
                                       builder.getStringAttr("length"), s, mlir::ValueRange{})
        .getResult();
  }

  // string_equals(a, b) -> i32
  if (name == "string_equals") {
    if (args.size() < 2) {
      emitError(location) << name << " requires at least 2 arguments";
      return nullptr;
    }
    auto a = generateExpression(ast::callArgExpr(args[0]).value);
    auto b = generateExpression(ast::callArgExpr(args[1]).value);
    if (!a || !b)
      return nullptr;
    materializeTemporary(a, ast::callArgExpr(args[0]).value);
    materializeTemporary(b, ast::callArgExpr(args[1]).value);
    return hew::StringMethodOp::create(builder, location, builder.getI32Type(),
                                       builder.getStringAttr("equals"), a, mlir::ValueRange{b})
        .getResult();
  }

  // sleep_ms(ms) -> void
  if (name == "sleep_ms") {
    if (args.empty()) {
      emitError(location) << name << " requires at least 1 argument";
      return nullptr;
    }
    auto ms = generateExpression(ast::callArgExpr(args[0]).value);
    if (!ms)
      return nullptr;
    hew::SleepOp::create(builder, location, ms);
    return nullptr;
  }

  // cooperate() -> void
  if (name == "cooperate") {
    hew::CooperateOp::create(builder, location);
    return nullptr;
  }

  // bytes::from([...]) -> !hew.vec<i32>  (create vec then push each element)
  if (name == "bytes::from") {
    if (args.empty()) {
      emitError(location) << "bytes::from requires an array argument";
      return nullptr;
    }
    // Create a new byte vec
    auto bytesType = hew::VecType::get(&context, builder.getI32Type());
    auto vec = hew::VecNewOp::create(builder, location, bytesType).getResult();
    // The argument should be an array literal — push each element
    if (auto *arr = std::get_if<ast::ExprArray>(&ast::callArgExpr(args[0]).value.kind)) {
      for (const auto &elem : arr->elements) {
        auto val = generateExpression(elem->value);
        if (!val)
          return nullptr;
        val = coerceType(val, builder.getI32Type(), location);
        hew::VecPushOp::create(builder, location, vec, val);
      }
    } else {
      emitError(location) << "bytes::from expects an array literal argument";
      return nullptr;
    }
    return vec;
  }

  // Vec::from([...]) -> !hew.vec<T>  (create vec then push each element)
  if (name == "Vec::from") {
    if (args.empty()) {
      emitError(location) << "Vec::from requires an array argument";
      return nullptr;
    }
    // The argument should be an array literal
    if (auto *arr = std::get_if<ast::ExprArray>(&ast::callArgExpr(args[0]).value.kind)) {
      if (arr->elements.empty()) {
        emitError(location) << "Vec::from requires a non-empty array";
        return nullptr;
      }
      // Generate all element values first to determine element type
      llvm::SmallVector<mlir::Value, 8> values;
      for (const auto &elem : arr->elements) {
        auto val = generateExpression(elem->value);
        if (!val)
          return nullptr;
        values.push_back(val);
      }
      auto elemType = values[0].getType();
      auto vecType = hew::VecType::get(&context, elemType);
      auto vec = hew::VecNewOp::create(builder, location, vecType).getResult();
      for (auto val : values) {
        val = coerceType(val, elemType, location);
        hew::VecPushOp::create(builder, location, vec, val);
      }
      return vec;
    }
    emitError(location) << "Vec::from expects an array literal argument";
    return nullptr;
  }

  // Vec::new() -> !hew.vec<T>  |  bytes::new() -> !hew.vec<i32>
  if (name == "Vec::new" || name == "bytes::new") {
    // bytes::new() always creates a byte buffer (stored as Vec<i32> to match runtime ABI)
    if (name == "bytes::new") {
      auto bytesType = hew::VecType::get(&context, builder.getI32Type());
      return hew::VecNewOp::create(builder, location, bytesType).getResult();
    }
    // Use the declared type from the enclosing let/var if available
    mlir::Type vecType;
    if (pendingDeclaredType && mlir::isa<hew::VecType>(*pendingDeclaredType)) {
      vecType = *pendingDeclaredType;
      pendingDeclaredType.reset();
    } else {
      emitError(location) << "cannot determine element type for Vec; add explicit type annotation";
      return nullptr;
    }
    return hew::VecNewOp::create(builder, location, vecType).getResult();
  }

  // HashMap::new() -> !hew.hashmap<K,V>
  if (name == "HashMap::new") {
    mlir::Type hmType;
    if (pendingDeclaredType && mlir::isa<hew::HashMapType>(*pendingDeclaredType)) {
      hmType = *pendingDeclaredType;
      pendingDeclaredType.reset();
    } else {
      emitError(location)
          << "cannot determine key/value types for HashMap; add explicit type annotation";
      return nullptr;
    }
    return hew::HashMapNewOp::create(builder, location, hmType).getResult();
  }

  // HashSet::new() -> !hew.handle<"HashSet">
  if (name == "HashSet::new") {
    mlir::Type setType;
    if (pendingDeclaredType && mlir::isa<hew::HandleType>(*pendingDeclaredType)) {
      setType = *pendingDeclaredType;
      pendingDeclaredType.reset();
    } else {
      emitError(location)
          << "cannot determine element type for HashSet; add explicit type annotation";
      return nullptr;
    }
    // Call the runtime function hew_hashset_new
    return hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{setType},
                                      mlir::SymbolRefAttr::get(&context, "hew_hashset_new"),
                                      mlir::ValueRange{})
        .getResult();
  }

  // duration::from_nanos(x) -> i64: identity passthrough (duration is i64 nanos)
  if (name == "duration::from_nanos") {
    if (args.empty()) {
      emitError(location) << name << " requires 1 argument";
      return nullptr;
    }
    auto val = generateExpression(ast::callArgExpr(args[0]).value);
    if (!val)
      return nullptr;
    auto i64Type = builder.getI64Type();
    if (val.getType() != i64Type)
      val = coerceType(val, i64Type, location);
    return val;
  }

  // stop(actor) -> void: stop an actor
  if (name == "stop") {
    if (args.empty()) {
      emitError(location) << name << " requires at least 1 argument";
      return nullptr;
    }
    auto actorVal = generateExpression(ast::callArgExpr(args[0]).value);
    if (!actorVal)
      return nullptr;
    hew::ActorStopOp::create(builder, location, actorVal);
    return nullptr;
  }

  // close(actor) -> actor: close an actor's mailbox and return the same ref
  if (name == "close") {
    if (args.empty()) {
      emitError(location) << name << " requires at least 1 argument";
      return nullptr;
    }
    auto actorVal = generateExpression(ast::callArgExpr(args[0]).value);
    if (!actorVal)
      return nullptr;
    hew::ActorCloseOp::create(builder, location, actorVal);
    return actorVal;
  }

  // link(actor_ref) — link current actor to target
  if (name == "link") {
    if (args.empty()) {
      emitError(location) << name << " requires at least 1 argument";
      return nullptr;
    }
    auto targetVal = generateExpression(ast::callArgExpr(args[0]).value);
    if (!targetVal)
      return nullptr;
    // Get current actor via hew.actor.self
    auto selfRef = hew::ActorSelfOp::create(builder, location, ptrType);
    hew::ActorLinkOp::create(builder, location, selfRef, targetVal);
    return nullptr;
  }

  // unlink(actor_ref) — unlink current actor from target
  if (name == "unlink") {
    if (args.empty()) {
      emitError(location) << name << " requires at least 1 argument";
      return nullptr;
    }
    auto targetVal = generateExpression(ast::callArgExpr(args[0]).value);
    if (!targetVal)
      return nullptr;
    auto selfRef = hew::ActorSelfOp::create(builder, location, ptrType);
    hew::ActorUnlinkOp::create(builder, location, selfRef, targetVal);
    return nullptr;
  }

  // monitor(actor_ref) -> i64 (ref_id)
  if (name == "monitor") {
    if (args.empty()) {
      emitError(location) << name << " requires at least 1 argument";
      return nullptr;
    }
    auto targetVal = generateExpression(ast::callArgExpr(args[0]).value);
    if (!targetVal)
      return nullptr;
    auto selfRef = hew::ActorSelfOp::create(builder, location, ptrType);
    return hew::ActorMonitorOp::create(builder, location, builder.getI64Type(), selfRef, targetVal)
        .getResult();
  }

  // demonitor(ref_id) — cancel a monitor by reference id
  if (name == "demonitor") {
    if (args.empty()) {
      emitError(location) << name << " requires at least 1 argument";
      return nullptr;
    }
    auto refIdVal = generateExpression(ast::callArgExpr(args[0]).value);
    if (!refIdVal)
      return nullptr;
    hew::ActorDemonitorOp::create(builder, location, refIdVal);
    return nullptr;
  }

  // supervisor_child(sup, index) -> actor_ptr or supervisor_ptr
  if (name == "supervisor_child") {
    if (args.size() < 2) {
      emitError(location) << name << " requires at least 2 arguments";
      return nullptr;
    }
    auto supVal = generateExpression(ast::callArgExpr(args[0]).value);
    auto idxVal = generateExpression(ast::callArgExpr(args[1]).value);
    if (!supVal || !idxVal)
      return nullptr;

    // Determine if the child at this index is a supervisor.
    bool childIsSupervisor = false;
    const auto &supArg = ast::callArgExpr(args[0]).value;
    if (auto *supIdent = std::get_if<ast::ExprIdentifier>(&supArg.kind)) {
      auto supIt = actorVarTypes.find(supIdent->name);
      if (supIt != actorVarTypes.end()) {
        auto childrenIt = supervisorChildren.find(supIt->second);
        if (childrenIt != supervisorChildren.end()) {
          const auto &idxArg = ast::callArgExpr(args[1]).value;
          if (auto *idxLit = std::get_if<ast::ExprLiteral>(&idxArg.kind)) {
            if (auto *intLit = std::get_if<ast::LitInteger>(&idxLit->lit)) {
              auto idx = intLit->value;
              if (idx >= 0 && static_cast<size_t>(idx) < childrenIt->second.size()) {
                auto childType = childrenIt->second[static_cast<size_t>(idx)];
                childIsSupervisor = supervisorChildren.count(childType) > 0;
              }
            }
          }
        }
      }
    }

    // Cast index to i32 for C ABI
    auto i32Type = builder.getI32Type();
    if (idxVal.getType() != i32Type) {
      idxVal = mlir::arith::TruncIOp::create(builder, location, i32Type, idxVal);
    }
    auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);

    if (childIsSupervisor) {
      return hew::RuntimeCallOp::create(
                 builder, location, mlir::TypeRange{ptrType},
                 mlir::SymbolRefAttr::get(&context, "hew_supervisor_get_child_supervisor"),
                 mlir::ValueRange{supVal, idxVal})
          .getResult();
    }
    // Wait up to 5 seconds for the child to be available during restarts.
    auto timeoutVal = mlir::arith::ConstantIntOp::create(
        builder, location, builder.getI32Type(), 5000);
    return hew::RuntimeCallOp::create(
               builder, location, mlir::TypeRange{ptrType},
               mlir::SymbolRefAttr::get(&context, "hew_supervisor_get_child_wait"),
               mlir::ValueRange{supVal, idxVal, timeoutVal})
        .getResult();
  }

  // supervisor_stop(sup) -> void
  if (name == "supervisor_stop") {
    if (args.empty()) {
      emitError(location) << name << " requires at least 1 argument";
      return nullptr;
    }
    auto supVal = generateExpression(ast::callArgExpr(args[0]).value);
    if (!supVal)
      return nullptr;
    hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                               mlir::SymbolRefAttr::get(&context, "hew_supervisor_stop"),
                               mlir::ValueRange{supVal});
    return nullptr;
  }

  // panic() — terminate the current actor (or process if called from main)
  if (name == "panic") {
    if (!args.empty()) {
      // panic("message") — print the message via hew_panic_msg, then panic
      auto msg = generateExpression(ast::callArgExpr(args[0]).value);
      if (msg) {
        auto panicMsgAttr = mlir::SymbolRefAttr::get(&context, "hew_panic_msg");
        hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{}, panicMsgAttr,
                                   mlir::ValueRange{msg});
        // hew_panic_msg calls hew_panic internally — it never returns.
        // Still emit PanicOp for MLIR's control flow analysis.
      }
    }
    hew::PanicOp::create(builder, location);
    return nullptr;
  }

  // string_char_at(s, idx) -> i32
  if (name == "string_char_at") {
    if (args.size() < 2) {
      emitError(location) << name << " requires at least 2 arguments";
      return nullptr;
    }
    auto s = generateExpression(ast::callArgExpr(args[0]).value);
    auto idx = generateExpression(ast::callArgExpr(args[1]).value);
    if (!s || !idx)
      return nullptr;
    materializeTemporary(s, ast::callArgExpr(args[0]).value);
    // Coerce idx to i32 (runtime function expects i32)
    idx = coerceType(idx, builder.getI32Type(), location);
    return hew::StringMethodOp::create(builder, location, builder.getI32Type(),
                                       builder.getStringAttr("char_at"), s, mlir::ValueRange{idx})
        .getResult();
  }

  // string_slice(s, start, end) -> string
  if (name == "string_slice" || name == "substring") {
    if (args.size() < 3) {
      emitError(location) << name << " requires at least 3 arguments";
      return nullptr;
    }
    auto s = generateExpression(ast::callArgExpr(args[0]).value);
    auto start = generateExpression(ast::callArgExpr(args[1]).value);
    auto end = generateExpression(ast::callArgExpr(args[2]).value);
    if (!s || !start || !end)
      return nullptr;
    materializeTemporary(s, ast::callArgExpr(args[0]).value);
    // Coerce start/end to i32 (runtime function expects i32)
    start = coerceType(start, builder.getI32Type(), location);
    end = coerceType(end, builder.getI32Type(), location);
    return hew::StringMethodOp::create(builder, location, hew::StringRefType::get(&context),
                                       builder.getStringAttr("slice"), s,
                                       mlir::ValueRange{start, end})
        .getResult();
  }

  // string_find(haystack, needle) -> i32
  if (name == "string_find") {
    if (args.size() < 2) {
      emitError(location) << name << " requires at least 2 arguments";
      return nullptr;
    }
    auto a = generateExpression(ast::callArgExpr(args[0]).value);
    auto b = generateExpression(ast::callArgExpr(args[1]).value);
    if (!a || !b)
      return nullptr;
    materializeTemporary(a, ast::callArgExpr(args[0]).value);
    materializeTemporary(b, ast::callArgExpr(args[1]).value);
    return hew::StringMethodOp::create(builder, location, builder.getI32Type(),
                                       builder.getStringAttr("find"), a, mlir::ValueRange{b})
        .getResult();
  }

  // string_contains(haystack, needle) -> bool
  if (name == "string_contains") {
    if (args.size() < 2) {
      emitError(location) << name << " requires at least 2 arguments";
      return nullptr;
    }
    auto a = generateExpression(ast::callArgExpr(args[0]).value);
    auto b = generateExpression(ast::callArgExpr(args[1]).value);
    if (!a || !b)
      return nullptr;
    materializeTemporary(a, ast::callArgExpr(args[0]).value);
    materializeTemporary(b, ast::callArgExpr(args[1]).value);
    return hew::StringMethodOp::create(builder, location, builder.getI1Type(),
                                       builder.getStringAttr("contains"), a, mlir::ValueRange{b})
        .getResult();
  }

  // string_starts_with(s, prefix) -> i32
  if (name == "string_starts_with") {
    if (args.size() < 2) {
      emitError(location) << name << " requires at least 2 arguments";
      return nullptr;
    }
    auto a = generateExpression(ast::callArgExpr(args[0]).value);
    auto b = generateExpression(ast::callArgExpr(args[1]).value);
    if (!a || !b)
      return nullptr;
    materializeTemporary(a, ast::callArgExpr(args[0]).value);
    materializeTemporary(b, ast::callArgExpr(args[1]).value);
    return hew::StringMethodOp::create(builder, location, builder.getI32Type(),
                                       builder.getStringAttr("starts_with"), a, mlir::ValueRange{b})
        .getResult();
  }

  // string_ends_with(s, suffix) -> i32
  if (name == "string_ends_with") {
    if (args.size() < 2) {
      emitError(location) << name << " requires at least 2 arguments";
      return nullptr;
    }
    auto a = generateExpression(ast::callArgExpr(args[0]).value);
    auto b = generateExpression(ast::callArgExpr(args[1]).value);
    if (!a || !b)
      return nullptr;
    materializeTemporary(a, ast::callArgExpr(args[0]).value);
    materializeTemporary(b, ast::callArgExpr(args[1]).value);
    return hew::StringMethodOp::create(builder, location, builder.getI32Type(),
                                       builder.getStringAttr("ends_with"), a, mlir::ValueRange{b})
        .getResult();
  }

  // string_trim(s) -> string
  if (name == "string_trim") {
    if (args.empty()) {
      emitError(location) << name << " requires at least 1 argument";
      return nullptr;
    }
    auto s = generateExpression(ast::callArgExpr(args[0]).value);
    if (!s)
      return nullptr;
    materializeTemporary(s, ast::callArgExpr(args[0]).value);
    return hew::StringMethodOp::create(builder, location, hew::StringRefType::get(&context),
                                       builder.getStringAttr("trim"), s, mlir::ValueRange{})
        .getResult();
  }

  // string_replace(s, from, to) -> string
  if (name == "string_replace") {
    if (args.size() < 3) {
      emitError(location) << name << " requires at least 3 arguments";
      return nullptr;
    }
    auto s = generateExpression(ast::callArgExpr(args[0]).value);
    auto from = generateExpression(ast::callArgExpr(args[1]).value);
    auto to = generateExpression(ast::callArgExpr(args[2]).value);
    if (!s || !from || !to)
      return nullptr;
    materializeTemporary(s, ast::callArgExpr(args[0]).value);
    materializeTemporary(from, ast::callArgExpr(args[1]).value);
    materializeTemporary(to, ast::callArgExpr(args[2]).value);
    return hew::StringMethodOp::create(builder, location, hew::StringRefType::get(&context),
                                       builder.getStringAttr("replace"), s,
                                       mlir::ValueRange{from, to})
        .getResult();
  }

  // string_to_int(s) -> i32
  if (name == "string_to_int") {
    if (args.empty()) {
      emitError(location) << name << " requires at least 1 argument";
      return nullptr;
    }
    auto s = generateExpression(ast::callArgExpr(args[0]).value);
    if (!s)
      return nullptr;
    materializeTemporary(s, ast::callArgExpr(args[0]).value);
    return hew::StringMethodOp::create(builder, location, builder.getI32Type(),
                                       builder.getStringAttr("to_int"), s, mlir::ValueRange{})
        .getResult();
  }

  // string_from_int(n) / int_to_string(n) -> string
  if (name == "string_from_int" || name == "int_to_string") {
    if (args.empty()) {
      emitError(location) << name << " requires at least 1 argument";
      return nullptr;
    }
    auto n = generateExpression(ast::callArgExpr(args[0]).value);
    if (!n)
      return nullptr;
    auto toStr = hew::ToStringOp::create(builder, location, hew::StringRefType::get(&context), n);
    if (auto *argType = resolvedTypeOf(ast::callArgExpr(args[0]).span))
      if (isUnsignedTypeExpr(*argType))
        toStr->setAttr("is_unsigned", builder.getBoolAttr(true));
    return toStr.getResult();
  }

  // char_to_string(c) -> string
  if (name == "char_to_string") {
    if (args.empty()) {
      emitError(location) << name << " requires at least 1 argument";
      return nullptr;
    }
    auto c = generateExpression(ast::callArgExpr(args[0]).value);
    if (!c)
      return nullptr;
    return hew::ToStringOp::create(builder, location, hew::StringRefType::get(&context), c)
        .getResult();
  }

  // ── Node API builtins ──────────────────────────────────────────────────────

  // Node::start(addr) -> void: create and start a node
  if (name == "Node::start") {
    if (args.empty()) {
      emitError(location) << name << " requires 1 argument (bind address)";
      return nullptr;
    }
    auto addr = generateExpression(ast::callArgExpr(args[0]).value);
    if (!addr)
      return nullptr;
    materializeTemporary(addr, ast::callArgExpr(args[0]).value);
    hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                               mlir::SymbolRefAttr::get(&context, "hew_node_api_start"),
                               mlir::ValueRange{addr});
    return nullptr;
  }

  // Node::shutdown() -> void: stop the current node
  if (name == "Node::shutdown") {
    hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                               mlir::SymbolRefAttr::get(&context, "hew_node_api_shutdown"),
                               mlir::ValueRange{});
    return nullptr;
  }

  // Node::connect(addr) -> void: connect to a peer node
  if (name == "Node::connect") {
    if (args.empty()) {
      emitError(location) << name << " requires 1 argument (peer address)";
      return nullptr;
    }
    auto addr = generateExpression(ast::callArgExpr(args[0]).value);
    if (!addr)
      return nullptr;
    materializeTemporary(addr, ast::callArgExpr(args[0]).value);
    hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                               mlir::SymbolRefAttr::get(&context, "hew_node_api_connect"),
                               mlir::ValueRange{addr});
    return nullptr;
  }

  // Node::set_transport(name) -> void: set transport before start
  if (name == "Node::set_transport") {
    if (args.empty()) {
      emitError(location) << name << " requires 1 argument (\"tcp\" or \"quic\")";
      return nullptr;
    }
    auto tname = generateExpression(ast::callArgExpr(args[0]).value);
    if (!tname)
      return nullptr;
    materializeTemporary(tname, ast::callArgExpr(args[0]).value);
    hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                               mlir::SymbolRefAttr::get(&context, "hew_node_api_set_transport"),
                               mlir::ValueRange{tname});
    return nullptr;
  }

  // Node::register(name, actor) -> void: register a named actor
  if (name == "Node::register") {
    if (args.size() < 2) {
      emitError(location) << name << " requires 2 arguments (name, actor)";
      return nullptr;
    }
    auto regName = generateExpression(ast::callArgExpr(args[0]).value);
    auto actorVal = generateExpression(ast::callArgExpr(args[1]).value);
    if (!regName || !actorVal)
      return nullptr;
    materializeTemporary(regName, ast::callArgExpr(args[0]).value);
    // Pass the actor ref pointer directly; the runtime extracts the PID.
    hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                               mlir::SymbolRefAttr::get(&context, "hew_node_api_register"),
                               mlir::ValueRange{regName, actorVal});
    return nullptr;
  }

  // Node::lookup(name) -> actor_id (u64)
  if (name == "Node::lookup") {
    if (args.empty()) {
      emitError(location) << name << " requires 1 argument (actor name)";
      return nullptr;
    }
    auto lookupName = generateExpression(ast::callArgExpr(args[0]).value);
    if (!lookupName)
      return nullptr;
    materializeTemporary(lookupName, ast::callArgExpr(args[0]).value);
    auto u64Ty = builder.getI64Type();
    return hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{u64Ty},
                                      mlir::SymbolRefAttr::get(&context, "hew_node_api_lookup"),
                                      mlir::ValueRange{lookupName})
        .getResult();
  }

  // read_file(path) -> string
  if (name == "read_file") {
    if (args.empty()) {
      emitError(location) << name << " requires at least 1 argument";
      return nullptr;
    }
    auto path = generateExpression(ast::callArgExpr(args[0]).value);
    if (!path)
      return nullptr;
    materializeTemporary(path, ast::callArgExpr(args[0]).value);
    return hew::RuntimeCallOp::create(
               builder, location, mlir::TypeRange{hew::StringRefType::get(&context)},
               mlir::SymbolRefAttr::get(&context, "hew_read_file"), mlir::ValueRange{path})
        .getResult();
  }

  // assert(cond) -> void: abort if cond is falsy
  if (name == "assert") {
    if (args.empty()) {
      emitError(location) << name << " requires at least 1 argument";
      return nullptr;
    }
    auto cond = generateExpression(ast::callArgExpr(args[0]).value);
    if (!cond)
      return nullptr;
    hew::AssertOp::create(builder, location, cond);
    return nullptr;
  }

  // assert_eq(left, right) -> void: abort if left != right
  if (name == "assert_eq") {
    if (args.size() < 2) {
      emitError(location) << name << " requires at least 2 arguments";
      return nullptr;
    }
    auto left = generateExpression(ast::callArgExpr(args[0]).value);
    auto right = generateExpression(ast::callArgExpr(args[1]).value);
    if (!left || !right)
      return nullptr;
    hew::AssertEqOp::create(builder, location, left, right);
    materializeTemporary(left, ast::callArgExpr(args[0]).value);
    materializeTemporary(right, ast::callArgExpr(args[1]).value);
    return nullptr;
  }

  // assert_ne(left, right) -> void: abort if left == right
  if (name == "assert_ne") {
    if (args.size() < 2) {
      emitError(location) << name << " requires at least 2 arguments";
      return nullptr;
    }
    auto left = generateExpression(ast::callArgExpr(args[0]).value);
    auto right = generateExpression(ast::callArgExpr(args[1]).value);
    if (!left || !right)
      return nullptr;
    hew::AssertNeOp::create(builder, location, left, right);
    materializeTemporary(left, ast::callArgExpr(args[0]).value);
    materializeTemporary(right, ast::callArgExpr(args[1]).value);
    return nullptr;
  }

  // Not a builtin
  return nullptr;
}

// ============================================================================
// Main entry point
// ============================================================================

mlir::ModuleOp MLIRGen::generate(const ast::Program &program) {
  module = mlir::ModuleOp::create(builder.getUnknownLoc());

  // Set pointer width attribute so lowering patterns use the correct size type.
  int ptrWidth = isWasm32_ ? 32 : 64;
  module->setAttr("hew.ptr_width",
                  mlir::IntegerAttr::get(mlir::IntegerType::get(&context, 32), ptrWidth));

  // Build the expression type lookup map from the serialized type checker data.
  for (const auto &entry : program.expr_types) {
    exprTypeMap[{entry.start, entry.end}] = &entry.ty.value;
  }

  // Populate handle type metadata from the Rust type checker.
  // This replaces hardcoded handle type lists in convertType().
  for (const auto &ht : program.handle_types) {
    knownHandleTypes.insert(ht);
  }
  handleTypeRepr = program.handle_type_repr;

  // Register built-in Option/Result variant names so that match patterns
  // (None, Some(x), Ok(x), Err(_)) resolve correctly via variantLookup.
  // The actual MLIR types are inferred from context; enumTypes entries are
  // needed for correct payload offset calculation in match destructuring.
  variantLookup["None"] = {"__Option", 0};
  variantLookup["Some"] = {"__Option", 1};
  variantLookup["Ok"] = {"__Result", 0};
  variantLookup["Err"] = {"__Result", 1};

  // Register built-in enum type info for payload offset calculation.
  // Option<T> layout: (tag, T) — None has no payload, Some has 1
  // Result<T, E> layout: (tag, T, E) — Ok has 1 payload, Err has 1 payload
  //
  // NOTE: The i32 placeholder types below are LOW-RISK registration defaults
  // used only for variant lookup table structure (payload count and positions).
  // They do NOT affect actual type inference — OptionEnumType/ResultEnumType
  // carry the real inner types, and match codegen resolves payload types from
  // the scrutinee's concrete type, not from these defaults.
  {
    EnumTypeInfo optInfo;
    optInfo.name = "__Option";
    optInfo.hasPayloads = true;
    EnumVariantInfo noneV;
    noneV.name = "None";
    noneV.index = 0;
    optInfo.variants.push_back(noneV);
    EnumVariantInfo someV;
    someV.name = "Some";
    someV.index = 1;
    someV.payloadTypes.push_back(builder.getI32Type()); // placeholder; not used for type inference
    optInfo.variants.push_back(someV);
    enumTypes["__Option"] = std::move(optInfo);

    EnumTypeInfo resInfo;
    resInfo.name = "__Result";
    resInfo.hasPayloads = true;
    EnumVariantInfo okV;
    okV.name = "Ok";
    okV.index = 0;
    okV.payloadTypes.push_back(builder.getI32Type()); // placeholder; not used for type inference
    resInfo.variants.push_back(okV);
    EnumVariantInfo errV;
    errV.name = "Err";
    errV.index = 1;
    errV.payloadTypes.push_back(builder.getI32Type()); // placeholder; not used for type inference
    resInfo.variants.push_back(errV);
    enumTypes["__Result"] = std::move(resInfo);
  }

  // Build ordered list of item groups with their module paths.
  // When module_graph is present, modules are processed in topological order
  // so that dependencies are available before dependents.
  struct ItemGroup {
    std::vector<std::string> modulePath;
    const std::vector<ast::Spanned<ast::Item>> *items;
  };
  std::vector<ItemGroup> itemGroups;

  if (program.module_graph) {
    for (const auto &modId : program.module_graph->topo_order) {
      auto it = program.module_graph->modules.find(modId);
      if (it != program.module_graph->modules.end()) {
        std::string dbgPath;
        for (const auto &s : modId.path)
          dbgPath += (dbgPath.empty() ? "" : "::") + s;
        itemGroups.push_back({modId.path, &it->second.items});
        // Register the last path segment as a module name for qualified calls.
        if (!modId.path.empty()) {
          moduleNameToPath[modId.path.back()] = modId.path;
        }
        // Track imported module paths for cross-module function resolution.
        std::string modKey;
        for (const auto &seg : modId.path)
          modKey += (modKey.empty() ? "" : "::") + seg;
        auto &impList = moduleImports[modKey];
        for (const auto &imp : it->second.imports) {
          impList.push_back(imp.target.path);
          // Register per-module import aliases.
          if (imp.spec) {
            if (auto *names = std::get_if<ast::ImportSpecNames>(&*imp.spec)) {
              for (const auto &name : names->names) {
                if (name.alias) {
                  aliasToFunction[modKey + "::" + *name.alias] = {imp.target.path, name.name};
                }
              }
            }
          }
        }
      }
    }
    // Compute transitive import closure: for each module, include all
    // imports of imported modules (handles diamond dependencies).
    // Use a set per module for O(1) duplicate checking.
    std::unordered_map<std::string, std::unordered_set<std::string>> importSets;
    auto joinPath = [](const std::vector<std::string> &path) {
      std::string key;
      for (const auto &seg : path)
        key += (key.empty() ? "" : "::") + seg;
      return key;
    };
    for (auto &[modKey, impList] : moduleImports) {
      auto &s = importSets[modKey];
      for (const auto &impPath : impList)
        s.insert(joinPath(impPath));
    }
    bool changed = true;
    while (changed) {
      changed = false;
      for (auto &[modKey, impList] : moduleImports) {
        auto &seen = importSets[modKey];
        std::vector<std::vector<std::string>> toAdd;
        for (const auto &impPath : impList) {
          auto transIt = moduleImports.find(joinPath(impPath));
          if (transIt != moduleImports.end()) {
            for (const auto &transPath : transIt->second) {
              auto transKey = joinPath(transPath);
              if (seen.insert(transKey).second)
                toAdd.push_back(transPath);
            }
          }
        }
        if (!toAdd.empty()) {
          for (auto &p : toAdd)
            impList.push_back(std::move(p));
          changed = true;
        }
      }
    }
  } else {
    itemGroups.push_back({{}, &program.items});
  }

  // Helper: iterate all items across module groups, setting currentModulePath.
  auto forEachItem = [&](auto &&fn) {
    for (const auto &group : itemGroups) {
      currentModulePath = group.modulePath;
      for (const auto &spannedItem : *group.items) {
        fn(spannedItem);
      }
    }
    currentModulePath.clear();
  };

  // Pass 0: Process imports early so that stdlib extern declarations are
  // available before any function bodies are generated.
  forEachItem([&](const auto &spannedItem) {
    const auto &item = spannedItem.value;
    if (auto *importDecl = std::get_if<ast::ImportDecl>(&item.kind))
      generateImport(*importDecl);
  });

  // Pass 1a: Register type aliases before type declarations so that
  // struct/enum fields can reference aliased types.
  forEachItem([&](const auto &spannedItem) {
    const auto &item = spannedItem.value;
    if (auto *alias = std::get_if<ast::TypeAliasDecl>(&item.kind)) {
      typeAliases[alias->name] = &alias->ty.value;
    }
  });

  // Pass 1b: Register all type declarations so that functions and actors
  // can reference struct/enum types. Wire structs (#[wire]) are skipped here
  // and handled by pass 1b2 below, since they need wireTypeToMLIR (not
  // convertType) for correct field types.
  forEachItem([&](const auto &spannedItem) {
    const auto &item = spannedItem.value;
    if (auto *td = std::get_if<ast::TypeDecl>(&item.kind)) {
      registerTypeDecl(*td);
    } else if (auto *md = std::get_if<ast::MachineDecl>(&item.kind)) {
      registerMachineDecl(*md);
    }
  });

  // Pass 1b2: Pre-register wire struct types with wire-aware field types.
  // This must happen before pass 1e (actor registration) so that actors with
  // wire-typed receive parameters can resolve the struct type. Uses
  // wireTypeToMLIR instead of convertType to produce correct wire field types.
  forEachItem([&](const auto &spannedItem) {
    const auto &item = spannedItem.value;
    if (auto *td = std::get_if<ast::TypeDecl>(&item.kind)) {
      if (td->wire.has_value()) {
        auto wd = wireMetadataToWireDecl(*td);
        preRegisterWireStructType(wd);
      }
    } else if (auto *wd = std::get_if<ast::WireDecl>(&item.kind)) {
      preRegisterWireStructType(*wd);
    }
  });

  // Pass 1c: Register trait declarations so that impl blocks can look up
  // default method bodies.
  forEachItem([&](const auto &spannedItem) {
    const auto &item = spannedItem.value;
    if (auto *td = std::get_if<ast::TraitDecl>(&item.kind)) {
      registerTraitDecl(*td);
    }
  });

  // Pass 1d: Pre-register all function signatures (without bodies) so that
  // functions and actor receive handlers can call any function regardless
  // of declaration order.
  forEachItem([&](const auto &spannedItem) {
    const auto &item = spannedItem.value;
    if (auto *fn = std::get_if<ast::FnDecl>(&item.kind)) {
      registerFunctionSignature(*fn, "", loc(spannedItem.span));
    } else if (auto *impl = std::get_if<ast::ImplDecl>(&item.kind)) {
      // Skip generic impl blocks — they are deferred for specialization.
      if (impl->type_params && !impl->type_params->empty())
        return;
      // Pre-register impl methods with mangled names
      std::string typeName;
      if (auto *named = std::get_if<ast::TypeNamed>(&impl->target_type.value.kind)) {
        typeName = named->name;
      }
      if (!typeName.empty()) {
        // Use the type's defining module for mangling (not the importing module).
        auto savedModPath = currentModulePath;
        if (typeDefModulePath.count(typeName)) {
          currentModulePath = typeDefModulePath[typeName];
        }

        // Set Self substitution for bare self parameter resolution
        typeParamSubstitutions["Self"] = typeName;

        for (const auto &method : impl->methods) {
          std::string mangledMethod = mangleName(currentModulePath, typeName, method.name);
          registerFunctionSignature(method, mangledMethod, loc(spannedItem.span));
        }

        // Pre-register default trait methods not overridden in this impl
        std::string traitName = impl->trait_bound ? impl->trait_bound->name : "";
        auto traitIt = traitRegistry.find(traitName);
        if (traitIt != traitRegistry.end()) {
          std::set<std::string> overridden;
          for (const auto &m : impl->methods) {
            overridden.insert(m.name);
          }
          for (const auto *tm : traitIt->second.methods) {
            if (tm->body && overridden.find(tm->name) == overridden.end()) {
              // Build a forward declaration for the default method
              std::string mangledName = mangleName(currentModulePath, typeName, tm->name);
              if (!module.lookupSymbol<mlir::func::FuncOp>(mangledName)) {
                auto typeLoc = [&](const ast::Spanned<ast::TypeExpr> &type) {
                  return type.span.end > type.span.start ? loc(type.span) : loc(spannedItem.span);
                };
                llvm::SmallVector<mlir::Type, 4> paramTypes;
                for (const auto &p : tm->params)
                  paramTypes.push_back(convertType(p.ty.value, typeLoc(p.ty)));
                llvm::SmallVector<mlir::Type, 1> resultTypes;
                if (tm->return_type) {
                  auto retTy = convertType(tm->return_type->value, typeLoc(*tm->return_type));
                  if (!llvm::isa<mlir::NoneType>(retTy))
                    resultTypes.push_back(retTy);
                }
                auto funcType = builder.getFunctionType(paramTypes, resultTypes);
                auto savedIP = builder.saveInsertionPoint();
                builder.setInsertionPointToEnd(module.getBody());
                mlir::func::FuncOp::create(builder, loc(spannedItem.span), mangledName, funcType);
                builder.restoreInsertionPoint(savedIP);
              }
            }
          }
        }

        typeParamSubstitutions.erase("Self");
        currentModulePath = savedModPath;
      }
    }
  });

  // Pass 1e: Register all actor declarations first (struct types, field tracking,
  // registry entries) so forward references between actors work.
  forEachItem([&](const auto &spannedItem) {
    const auto &item = spannedItem.value;
    if (auto *ad = std::get_if<ast::ActorDecl>(&item.kind)) {
      registerActorDecl(*ad, loc(spannedItem.span));
    }
  });

  // Pass 1e2: Pre-register supervisor declarations so that spawn expressions
  // in function bodies (Pass 1k) can recognise supervisor names. The full
  // supervisor init functions are generated later in Pass 2.
  forEachItem([&](const auto &spannedItem) {
    const auto &item = spannedItem.value;
    if (auto *sd = std::get_if<ast::SupervisorDecl>(&item.kind)) {
      std::vector<std::string> childTypes;
      std::vector<std::pair<std::string, std::string>> childNameTypes;
      for (const auto &child : sd->children) {
        childTypes.push_back(child.actor_type);
        childNameTypes.emplace_back(child.name, child.actor_type);
      }
      supervisorChildren[sd->name] = std::move(childTypes);
      supervisorChildNames[sd->name] = std::move(childNameTypes);
    }
  });

  // Pass 1f: Pre-register Drop impl mappings so that function bodies can
  // register droppable struct variables. ImplDecl bodies are generated
  // later in Pass 2, but we need the type→drop_func mapping NOW.
  forEachItem([&](const auto &spannedItem) {
    const auto &item = spannedItem.value;
    auto *impl = std::get_if<ast::ImplDecl>(&item.kind);
    if (!impl)
      return;
    if (impl->trait_bound && impl->trait_bound->name == "Drop") {
      if (auto *named = std::get_if<ast::TypeNamed>(&impl->target_type.value.kind)) {
        // Use the type's defining module for mangling (not the importing module).
        const auto &dropModPath = typeDefModulePath.count(named->name)
                                      ? typeDefModulePath[named->name]
                                      : currentModulePath;
        userDropFuncs[named->name] = mangleName(dropModPath, named->name, "drop");
      }
    }
  });

  // Pass 1g: Pre-register module-level constants so function bodies can
  // reference them. ConstDecl stores the AST expression for inline codegen.
  forEachItem([&](const auto &spannedItem) {
    const auto &item = spannedItem.value;
    if (auto *cd = std::get_if<ast::ConstDecl>(&item.kind)) {
      moduleConstants[cd->name] = &cd->value.value;
    }
  });

  // Pass 1h: Generate wire declarations (encode/decode/json/yaml functions)
  // so wire struct/enum types exist before extern signature conversion.
  forEachItem([&](const auto &spannedItem) {
    const auto &item = spannedItem.value;
    if (auto *wd = std::get_if<ast::WireDecl>(&item.kind)) {
      generateWireDecl(*wd);
    } else if (auto *td = std::get_if<ast::TypeDecl>(&item.kind)) {
      if (td->wire.has_value()) {
        auto wd = wireMetadataToWireDecl(*td);
        generateWireDecl(wd);
      }
    }
  });

  // Pass 1i: Generate extern block declarations so function bodies can call
  // extern functions. getOrCreateExternFunc guards against duplicates.
  forEachItem([&](const auto &spannedItem) {
    const auto &item = spannedItem.value;
    if (auto *eb = std::get_if<ast::ExternBlock>(&item.kind)) {
      generateExternBlock(*eb, loc(spannedItem.span));
    }
  });

  // Pass 1j: Pre-register trait dispatch tables from impl declarations so that
  // function bodies can use dyn trait dispatch. Generate vtable dispatch shim
  // functions. Method bodies are NOT generated here — that happens in Pass 2
  // via generateImplDecl.
  forEachItem([&](const auto &spannedItem) {
    const auto &item = spannedItem.value;
    auto *impl = std::get_if<ast::ImplDecl>(&item.kind);
    if (!impl)
      return;
    std::string typeName;
    if (auto *named = std::get_if<ast::TypeNamed>(&impl->target_type.value.kind)) {
      typeName = named->name;
    }
    if (typeName.empty())
      return;
    // Use the type's defining module path for trait impl registration.
    auto savedModPath2 = currentModulePath;
    if (typeDefModulePath.count(typeName)) {
      currentModulePath = typeDefModulePath[typeName];
    }
    std::string traitName = impl->trait_bound ? impl->trait_bound->name : "";
    auto traitIt = traitRegistry.find(traitName);
    if (traitIt != traitRegistry.end()) {
      std::vector<std::string> methodNames;
      for (const auto *tm : traitIt->second.methods)
        methodNames.push_back(tm->name);
      registerTraitImpl(typeName, traitName, methodNames);
      // Shim functions are generated in generateImplDecl after method bodies exist
    }
    currentModulePath = savedModPath2;
  });

  // Pass 1k0: Generate machine step() and state_name() functions.
  // Must run before function bodies so user code can call machine methods.
  forEachItem([&](const auto &spannedItem) {
    const auto &item = spannedItem.value;
    if (auto *md = std::get_if<ast::MachineDecl>(&item.kind)) {
      generateMachineDecl(*md);
    }
  });

  // Pass 1k1: Register generic impl blocks so that method specialization
  // is available when function bodies trigger method calls on monomorphized
  // generic structs (e.g. Box_int::unwrap from impl<T> Box<T>).
  forEachItem([&](const auto &spannedItem) {
    const auto &item = spannedItem.value;
    auto *impl = std::get_if<ast::ImplDecl>(&item.kind);
    if (!impl || !impl->type_params || impl->type_params->empty())
      return;
    std::string typeName;
    if (auto *named = std::get_if<ast::TypeNamed>(&impl->target_type.value.kind))
      typeName = named->name;
    if (typeName.empty())
      return;
    GenericImplInfo info;
    info.typeParams = &*impl->type_params;
    for (const auto &method : impl->methods)
      info.methods.push_back(&method);
    genericImplMethods[typeName] = std::move(info);
  });

  // Pass 1k: Generate regular function bodies before actor bodies so that
  // actor receive functions can call user-defined functions (including void
  // functions that were skipped by the forward-declaration pass 1d).
  forEachItem([&](const auto &spannedItem) {
    const auto &item = spannedItem.value;
    auto *fn = std::get_if<ast::FnDecl>(&item.kind);
    if (!fn)
      return;
    if (fn->type_params && !fn->type_params->empty()) {
      genericFunctions[fn->name] = fn;
    } else if (fn->is_generator) {
      generateGeneratorFunction(*fn);
    } else {
      generateFunction(*fn, "", loc(spannedItem.span));
    }
  });

  // Pass 1l: Generate actor bodies (receive fn implementations, dispatch functions).
  // Runs after ALL actors are registered so cross-actor method calls resolve.
  forEachItem([&](const auto &spannedItem) {
    const auto &item = spannedItem.value;
    if (auto *ad = std::get_if<ast::ActorDecl>(&item.kind)) {
      generateActorDecl(*ad);
    }
  });

  // Pass 2: Generate remaining items (supervisor decls, etc.)
  // Items already handled in earlier passes are skipped.
  forEachItem([&](const auto &spannedItem) {
    const auto &item = spannedItem.value;
    if (std::holds_alternative<ast::TypeDecl>(item.kind) ||
        std::holds_alternative<ast::ActorDecl>(item.kind) ||
        std::holds_alternative<ast::TypeAliasDecl>(item.kind) ||
        std::holds_alternative<ast::FnDecl>(item.kind) ||
        std::holds_alternative<ast::ConstDecl>(item.kind) ||
        std::holds_alternative<ast::ExternBlock>(item.kind) ||
        std::holds_alternative<ast::WireDecl>(item.kind) ||
        std::holds_alternative<ast::MachineDecl>(item.kind))
      return; // already handled in pass 1
    generateItem(item, loc(spannedItem.span));
  });

  // If actors were used, inject hew_sched_init/shutdown into main
  if (hasActors) {
    if (auto mainFunc = module.lookupSymbol<mlir::func::FuncOp>("main")) {
      auto &entryBlock = mainFunc.getBody().front();

      // Insert hew.sched.init at the very beginning of main
      auto savedIP = builder.saveInsertionPoint();
      builder.setInsertionPointToStart(&entryBlock);
      hew::SchedInitOp::create(builder, builder.getUnknownLoc());

      // Insert hew.sched.shutdown before each return in main
      for (auto &block : mainFunc.getBody()) {
        if (block.empty())
          continue;
        auto &lastOp = block.back();
        if (mlir::isa<mlir::func::ReturnOp>(lastOp)) {
          builder.setInsertionPoint(&lastOp);
          hew::SchedShutdownOp::create(builder, builder.getUnknownLoc());
        }
      }

      builder.restoreInsertionPoint(savedIP);
    }
  }

  // Verify the module
  if (mlir::failed(mlir::verify(module))) {
    // Dump module before returning error so we can diagnose
    module.dump();
    module.emitError("module verification failed");
    return nullptr;
  }

  // If any emitError calls were made during generation, fail even if the
  // module happens to verify (e.g. undeclared variables that produced
  // nullptr but didn't corrupt the IR).
  if (errorCount_ > 0) {
    std::cerr << "Error: " << errorCount_ << " error" << (errorCount_ > 1 ? "s" : "")
              << " during MLIR generation\n";
    return nullptr;
  }

  return module;
}

// ============================================================================
// Item generation
// ============================================================================

void MLIRGen::generateItem(const ast::Item &item, std::optional<mlir::Location> fallbackLoc) {
  if (auto *fn = std::get_if<ast::FnDecl>(&item.kind)) {
    if (fn->type_params && !fn->type_params->empty()) {
      // Generic function: store for later specialization, don't generate yet
      genericFunctions[fn->name] = fn;
    } else if (fn->is_generator) {
      generateGeneratorFunction(*fn);
    } else {
      generateFunction(*fn, "", fallbackLoc);
    }
  } else if (auto *cd = std::get_if<ast::ConstDecl>(&item.kind)) {
    // Store for inline generation when referenced
    moduleConstants[cd->name] = &cd->value.value;
  } else if (auto *td = std::get_if<ast::TypeDecl>(&item.kind)) {
    registerTypeDecl(*td);
  } else if (auto *id = std::get_if<ast::ImplDecl>(&item.kind)) {
    generateImplDecl(*id, fallbackLoc);
  } else if (auto *eb = std::get_if<ast::ExternBlock>(&item.kind)) {
    generateExternBlock(*eb);
  } else if (auto *ad = std::get_if<ast::ActorDecl>(&item.kind)) {
    generateActorDecl(*ad);
  } else if (auto *wd = std::get_if<ast::WireDecl>(&item.kind)) {
    generateWireDecl(*wd);
  } else if (std::holds_alternative<ast::ImportDecl>(item.kind)) {
    // Imports are processed in pass 0; nothing to do here.
  } else if (std::holds_alternative<ast::TraitDecl>(item.kind)) {
    // Trait declarations are registered in pass 1c; nothing to generate.
  } else if (auto *sd = std::get_if<ast::SupervisorDecl>(&item.kind)) {
    generateSupervisorDecl(*sd);
  } else if (std::holds_alternative<ast::TypeAliasDecl>(item.kind)) {
    // Handled in pre-registration pass
  } else if (std::holds_alternative<ast::MachineDecl>(item.kind)) {
    // Handled in pass 1b (registration) and pass 1m (code generation)
  }
}

// ============================================================================
// Function signature pre-registration (for order-independent resolution)
// ============================================================================

void MLIRGen::registerFunctionSignature(const ast::FnDecl &fn, const std::string &nameOverride,
                                        std::optional<mlir::Location> fallbackLoc) {
  // Skip generic functions — they're specialized on demand
  if (fn.type_params && !fn.type_params->empty())
    return;

  // Skip generator functions — they have a transformed return type
  // (pointer to coroutine state) that differs from the declared type
  if (fn.is_generator)
    return;

  // Functions without explicit return types are declared as returning void.
  // The actual return type is inferred from the body during generateFunction()
  // which overwrites this declaration. This ensures cross-module method calls
  // can resolve the symbol even before the body is generated.

  // When nameOverride is provided, the caller has already computed the
  // (possibly mangled) symbol name.  When empty, mangle the bare fn.name.
  std::string funcName =
      nameOverride.empty() ? mangleName(currentModulePath, "", fn.name) : nameOverride;

  // Skip if already registered (e.g. extern declaration)
  if (module.lookupSymbol<mlir::func::FuncOp>(funcName))
    return;

  // Build the function type from parameter and return type annotations
  auto typeLoc = [&](const ast::Spanned<ast::TypeExpr> &type) {
    return type.span.end > type.span.start ? loc(type.span) : fallbackLoc.value_or(currentLoc);
  };
  llvm::SmallVector<mlir::Type, 4> paramTypes;
  for (const auto &param : fn.params) {
    paramTypes.push_back(convertType(param.ty.value, typeLoc(param.ty)));
  }

  llvm::SmallVector<mlir::Type, 1> resultTypes;
  if (fn.return_type) {
    auto retTy = convertType(fn.return_type->value, typeLoc(*fn.return_type));
    if (!llvm::isa<mlir::NoneType>(retTy)) {
      resultTypes.push_back(retTy);
    }
  }

  auto funcType = builder.getFunctionType(paramTypes, resultTypes);
  auto location = fallbackLoc.value_or(builder.getUnknownLoc());

  // Create a declaration (no body) at module scope
  auto savedIP = builder.saveInsertionPoint();
  builder.setInsertionPointToEnd(module.getBody());
  mlir::func::FuncOp::create(builder, location, funcName, funcType);
  builder.restoreInsertionPoint(savedIP);
}

// ============================================================================
// Type declaration registration
// ============================================================================

void MLIRGen::registerTypeDecl(const ast::TypeDecl &decl) {

  const std::string &declName = decl.name;

  // Track which module defined this type, so impl methods can be mangled
  // with the defining module's path (not the importing module's path).
  if (typeDefModulePath.find(declName) == typeDefModulePath.end()) {
    typeDefModulePath[declName] = currentModulePath;
  }

  // Wire structs use wireTypeToMLIR (not convertType) for field types.
  // They are pre-registered by preRegisterWireStructType() in pass 1b2.
  if (decl.wire.has_value())
    return;

  // Generic struct/enum: store for lazy specialization, don't register yet.
  // (Field types like T can't be resolved without typeParamSubstitutions.)
  if (decl.type_params && !decl.type_params->empty() && typeParamSubstitutions.empty()) {
    genericStructs[declName] = &decl;
    return;
  }

  if (decl.kind == ast::TypeDeclKind::Enum) {
    // Register enum type
    EnumTypeInfo info;
    info.name = declName;
    info.isIndirect = decl.is_indirect;

    // For indirect enums, mark as pending so self-references in variant
    // field types resolve to pointer type instead of infinite recursion.
    if (decl.is_indirect)
      pendingIndirectEnums.insert(declName);

    unsigned idx = 0;
    for (const auto &bodyItem : decl.body) {
      if (auto *variantItem = std::get_if<ast::TypeBodyVariant>(&bodyItem.kind)) {
        EnumVariantInfo vi;
        vi.name = variantItem->variant.name;
        vi.index = idx++;
        if (auto *tuple = std::get_if<ast::VariantDecl::VariantTuple>(&variantItem->variant.kind)) {
          bool variantOk = true;
          for (const auto &ty : tuple->fields) {
            auto payloadType = convertTypeOrError(
                ty.value, "cannot resolve payload type for variant '" + vi.name + "'");
            if (!payloadType) {
              variantOk = false;
              break;
            }
            vi.payloadTypes.push_back(payloadType);
          }
          if (!variantOk)
            return;
        } else if (auto *strct =
                       std::get_if<ast::VariantDecl::VariantStruct>(&variantItem->variant.kind)) {
          bool variantOk = true;
          for (const auto &field : strct->fields) {
            auto payloadType = convertTypeOrError(
                field.ty.value, "cannot resolve payload type for variant '" + vi.name + "'");
            if (!payloadType) {
              variantOk = false;
              break;
            }
            vi.payloadTypes.push_back(payloadType);
            vi.fieldNames.push_back(field.name);
          }
          if (!variantOk)
            return;
        }
        info.variants.push_back(std::move(vi));
      }
    }

    if (decl.is_indirect)
      pendingIndirectEnums.erase(declName);

    // Register variant name → (enum, index) for quick lookup
    // Both unqualified ("Circle") and qualified ("Shape::Circle") forms.
    for (const auto &v : info.variants) {
      variantLookup[v.name] = {info.name, v.index};
      variantLookup[info.name + "::" + v.name] = {info.name, v.index};
    }

    // Determine if any variant has payloads
    bool anyPayload = false;
    size_t maxPayloadFields = 0;
    for (const auto &v : info.variants) {
      if (!v.payloadTypes.empty()) {
        anyPayload = true;
        maxPayloadFields = std::max(maxPayloadFields, v.payloadTypes.size());
      }
    }
    info.hasPayloads = anyPayload;

    if (!anyPayload) {
      // All-unit enum: represent as i32 tag
      info.mlirType = builder.getI32Type();
    } else {
      // Build an LLVM struct type: { i32 tag, payload_fields... }
      //
      // When all payload variants at each position have compatible types
      // (all scalars or all the same type), we use a union-style layout
      // where the widest type is selected per position.
      // When types at a position are incompatible (e.g. i32 vs pointer),
      // we fall back to per-variant fields so each variant gets its own
      // dedicated struct slot — avoiding type mismatches in the verifier.

      // Determine if union-style is safe for every position.
      bool canUnionize = true;
      for (size_t fieldIdx = 0; fieldIdx < maxPayloadFields && canUnionize; ++fieldIdx) {
        mlir::Type firstStorageTy = nullptr;
        for (const auto &v : info.variants) {
          if (fieldIdx >= v.payloadTypes.size())
            continue;
          auto storageTy = toLLVMStorageType(v.payloadTypes[fieldIdx]);
          if (!firstStorageTy) {
            firstStorageTy = storageTy;
          } else if (firstStorageTy != storageTy) {
            bool aScalar = mlir::isa<mlir::IntegerType, mlir::FloatType>(firstStorageTy);
            bool bScalar = mlir::isa<mlir::IntegerType, mlir::FloatType>(storageTy);
            if (!aScalar || !bScalar)
              canUnionize = false;
          }
        }
      }

      llvm::SmallVector<mlir::Type, 4> structFields;
      structFields.push_back(builder.getI32Type()); // tag

      if (canUnionize) {
        // Union-style: all variants share fields, use widest type per position
        for (size_t fieldIdx = 0; fieldIdx < maxPayloadFields; ++fieldIdx) {
          mlir::Type fieldType = nullptr;
          for (const auto &v : info.variants) {
            if (fieldIdx < v.payloadTypes.size()) {
              auto ty = v.payloadTypes[fieldIdx];
              if (!fieldType) {
                fieldType = ty;
              } else if (fieldType != ty) {
                // Mixed scalar types at same position: use the wider one
                unsigned existingBits = 0, newBits = 0;
                if (auto intTy = llvm::dyn_cast<mlir::IntegerType>(fieldType))
                  existingBits = intTy.getWidth();
                if (auto fltTy = llvm::dyn_cast<mlir::FloatType>(fieldType))
                  existingBits = fltTy.getWidth();
                if (auto intTy = llvm::dyn_cast<mlir::IntegerType>(ty))
                  newBits = intTy.getWidth();
                if (auto fltTy = llvm::dyn_cast<mlir::FloatType>(ty))
                  newBits = fltTy.getWidth();
                if (newBits > existingBits)
                  fieldType = ty;
              }
            }
          }
          if (fieldType)
            structFields.push_back(fieldType);
        }
      } else {
        // Per-variant fields: each variant gets its own struct slots.
        int64_t nextField = 1;
        for (auto &v : info.variants) {
          v.payloadPositions.clear();
          for (const auto &pt : v.payloadTypes) {
            v.payloadPositions.push_back(nextField++);
            structFields.push_back(pt);
          }
        }
      }

      auto innerStructType = mlir::LLVM::LLVMStructType::getLiteral(&context, structFields);

      if (decl.is_indirect) {
        // Indirect enum: value is a pointer to the heap-allocated struct
        info.innerStructType = innerStructType;
        info.mlirType = mlir::LLVM::LLVMPointerType::get(&context);
      } else {
        info.mlirType = innerStructType;
      }
    }

    enumTypes[declName] = std::move(info);

    // Generate drop function for indirect enums
    if (decl.is_indirect)
      generateIndirectEnumDropFunc(declName);

    return;
  }

  if (decl.kind != ast::TypeDeclKind::Struct)
    return;

  StructTypeInfo info;
  info.name = declName;

  llvm::SmallVector<mlir::Type, 4> fieldTypes;
  unsigned idx = 0;
  for (const auto &bodyItem : decl.body) {
    if (auto *fieldItem = std::get_if<ast::TypeBodyItemField>(&bodyItem.kind)) {
      StructFieldInfo field;
      field.name = fieldItem->name;
      field.semanticType = convertTypeOrError(
          fieldItem->ty.value, "cannot resolve type for field '" + fieldItem->name + "'");
      if (!field.semanticType)
        return;
      field.type = toLLVMStorageType(field.semanticType);
      field.index = idx++;
      // Preserve original type expression for collection dispatch
      // (e.g., Vec<BenchResult> → "Vec<BenchResult>" so struct field
      // accesses can trigger Vec method dispatch)
      field.typeExprStr = typeExprToCollectionString(
          fieldItem->ty.value, [this](const std::string &n) { return resolveTypeAlias(n); });
      fieldTypes.push_back(field.type);
      info.fields.push_back(std::move(field));
    }
  }

  // Create an identified LLVM struct type
  info.mlirType = mlir::LLVM::LLVMStructType::getIdentified(&context, declName);
  if (info.mlirType.isInitialized()) {
    // Already registered (e.g. forward reference resolved) — skip
  } else {
    (void)info.mlirType.setBody(fieldTypes, /*isPacked=*/false);
  }

  structTypes[declName] = std::move(info);

  // Generate serialization functions for struct types with all-encodable fields.
  // Wire types handle their own serialization; skip actors and generic templates.
  if (decl.kind == ast::TypeDeclKind::Struct && !decl.wire.has_value() &&
      !decl.type_params.has_value()) {
    // Check if all field types are serialization-compatible (primitive, string, bool, float)
    bool allEncodable = true;
    for (const auto &ft : fieldTypes) {
      if (ft.isInteger(1) || ft.isInteger(8) || ft.isInteger(16) || ft.isInteger(32) ||
          ft.isInteger(64) || ft.isF32() || ft.isF64() ||
          mlir::isa<mlir::LLVM::LLVMPointerType>(ft)) {
        continue; // primitive int, float, bool, or string (ptr)
      }
      allEncodable = false;
      break;
    }
    if (allEncodable && !fieldTypes.empty()) {
      generateStructEncodeWrappers(declName);
    }
  }
}

// ============================================================================
// Indirect enum drop function generation
// ============================================================================

void MLIRGen::generateIndirectEnumDropFunc(const std::string &enumName) {
  auto enumIt = enumTypes.find(enumName);
  if (enumIt == enumTypes.end())
    return;
  const auto &info = enumIt->second;
  if (!info.isIndirect || !info.innerStructType)
    return;

  auto location = builder.getUnknownLoc();
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto dropFuncName = "__hew_drop_" + enumName;

  // Declare malloc/free if needed
  auto freeFuncType = mlir::FunctionType::get(&context, {ptrType}, {});
  getOrCreateExternFunc("free", freeFuncType);

  // Create the drop function: void __hew_drop_EnumName(ptr)
  auto funcType = mlir::FunctionType::get(&context, {ptrType}, {});

  // Forward-declare the function first (it's recursive)
  auto funcOp = module.lookupSymbol<mlir::func::FuncOp>(dropFuncName);
  if (funcOp)
    return; // Already generated (e.g. from a previous generic specialization)

  auto savedIP = builder.saveInsertionPoint();
  builder.setInsertionPointToEnd(module.getBody());
  funcOp = mlir::func::FuncOp::create(builder, location, dropFuncName, funcType);
  funcOp.setPrivate();

  auto *entryBlock = funcOp.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto argPtr = entryBlock->getArgument(0);

  // if ptr == null: return
  auto nullPtr = mlir::LLVM::ZeroOp::create(builder, location, ptrType);
  auto isNull =
      mlir::LLVM::ICmpOp::create(builder, location, mlir::LLVM::ICmpPredicate::eq, argPtr, nullPtr);
  // Use a simple null check: if null, return early
  // We avoid scf.if here because it auto-generates yield terminators.
  // Instead, use LLVM-style conditional branching with func blocks.
  auto *nullBlock = funcOp.addBlock();
  auto *dropBlock = funcOp.addBlock();

  mlir::LLVM::CondBrOp::create(builder, location, isNull, nullBlock, dropBlock);

  // Null block: just return
  builder.setInsertionPointToStart(nullBlock);
  mlir::func::ReturnOp::create(builder, location);

  // Drop block: non-null, load struct and process
  builder.setInsertionPointToStart(dropBlock);

  // Load the struct from the pointer
  auto structVal = mlir::LLVM::LoadOp::create(builder, location, info.innerStructType, argPtr);

  // Extract tag
  auto tag =
      mlir::LLVM::ExtractValueOp::create(builder, location, structVal, llvm::ArrayRef<int64_t>{0});

  // For each variant with self-referential payload fields, recursively drop them
  for (const auto &variant : info.variants) {
    bool hasDroppablePayloads = false;
    for (const auto &payloadTy : variant.payloadTypes) {
      if (mlir::isa<mlir::LLVM::LLVMPointerType>(payloadTy)) {
        hasDroppablePayloads = true;
        break;
      }
    }
    if (!hasDroppablePayloads)
      continue;

    // Compare tag
    auto tagVal = mlir::arith::ConstantIntOp::create(builder, location, builder.getI32Type(),
                                                     static_cast<int64_t>(variant.index));
    auto tagMatch =
        mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::eq, tag, tagVal);
    auto variantIfOp = mlir::scf::IfOp::create(builder, location, mlir::TypeRange{}, tagMatch,
                                               /*withElseRegion=*/false);

    // Insert before the auto-generated yield in the then block
    builder.setInsertionPoint(variantIfOp.getThenRegion().front().getTerminator());

    // Drop each self-referential payload field
    for (size_t i = 0; i < variant.payloadTypes.size(); ++i) {
      if (!mlir::isa<mlir::LLVM::LLVMPointerType>(variant.payloadTypes[i]))
        continue;

      int64_t fieldIdx;
      if (i < variant.payloadPositions.size())
        fieldIdx = variant.payloadPositions[i];
      else
        fieldIdx = static_cast<int64_t>(i) + 1;

      auto fieldVal = mlir::LLVM::ExtractValueOp::create(builder, location, structVal,
                                                         llvm::ArrayRef<int64_t>{fieldIdx});

      // Recursively call this drop function on the child pointer
      mlir::func::CallOp::create(builder, location, funcOp, mlir::ValueRange{fieldVal});
    }

    builder.setInsertionPointAfter(variantIfOp);
  }

  // Free the pointer
  mlir::func::CallOp::create(builder, location, "free", mlir::TypeRange{},
                             mlir::ValueRange{argPtr});

  mlir::func::ReturnOp::create(builder, location);

  builder.restoreInsertionPoint(savedIP);

  // Register as a user drop function so variables of this type get auto-dropped
  userDropFuncs[enumName] = dropFuncName;
}

// ============================================================================
// Machine declaration registration
// ============================================================================

void MLIRGen::registerMachineDecl(const ast::MachineDecl &decl) {
  const auto &machineName = decl.name;

  // ── Register the machine type as an enum (states are variants) ──
  {
    EnumTypeInfo info;
    info.name = machineName;

    unsigned idx = 0;
    for (const auto &state : decl.states) {
      EnumVariantInfo vi;
      vi.name = state.name;
      vi.index = idx++;
      bool stateOk = true;
      for (const auto &[fieldName, fieldType] : state.fields) {
        auto payloadType = convertTypeOrError(
            fieldType.value, "cannot resolve type for state field '" + fieldName + "'");
        if (!payloadType) {
          stateOk = false;
          break;
        }
        vi.payloadTypes.push_back(payloadType);
        vi.fieldNames.push_back(fieldName);
      }
      if (!stateOk)
        return;
      info.variants.push_back(std::move(vi));
    }

    // Register variant name → (machine, index) for quick lookup
    // Register both unqualified (Off) and qualified (Light::Off) forms
    for (const auto &v : info.variants) {
      variantLookup[v.name] = {machineName, v.index};
      variantLookup[machineName + "::" + v.name] = {machineName, v.index};
    }

    // Determine if any variant has payloads
    bool anyPayload = false;
    for (const auto &v : info.variants) {
      if (!v.payloadTypes.empty()) {
        anyPayload = true;
        break;
      }
    }
    // Machines always use struct layout, so hasPayloads must be true
    // for EnumConstructOp to emit struct-based code (not bare i32).
    info.hasPayloads = true;

    // Machines always use an identified struct type (even unit-only) so that
    // method dispatch (step(), state_name()) can resolve the type name.
    {
      llvm::SmallVector<mlir::Type, 4> structFields;
      structFields.push_back(builder.getI32Type()); // tag

      if (anyPayload) {
        // Union overlay: find max fields across all variants
        unsigned maxFields = 0;
        for (const auto &v : info.variants)
          maxFields = std::max(maxFields, (unsigned)v.payloadTypes.size());

        // For each position, use the widest type across all variants
        for (unsigned pos = 0; pos < maxFields; pos++) {
          mlir::Type widest;
          unsigned widestBits = 0;
          for (const auto &v : info.variants) {
            if (pos < v.payloadTypes.size()) {
              auto ty = v.payloadTypes[pos];
              unsigned bits = 64; // default for pointers/other
              if (auto intTy = llvm::dyn_cast<mlir::IntegerType>(ty))
                bits = intTy.getWidth();
              else if (auto fltTy = llvm::dyn_cast<mlir::FloatType>(ty))
                bits = fltTy.getWidth();
              if (!widest || bits > widestBits) {
                widest = ty;
                widestBits = bits;
              }
            }
          }
          structFields.push_back(widest);
        }

        // All variants overlay starting from position 1
        for (auto &v : info.variants) {
          v.payloadPositions.clear();
          for (unsigned i = 0; i < v.payloadTypes.size(); i++)
            v.payloadPositions.push_back(1 + i);
        }
      }

      auto structType = mlir::LLVM::LLVMStructType::getIdentified(&context, machineName);
      if (!structType.isInitialized())
        (void)structType.setBody(structFields, /*isPacked=*/false);
      info.mlirType = structType;
    }

    enumTypes[machineName] = std::move(info);
  }

  // ── Register the event type as a separate enum ──
  {
    std::string eventTypeName = machineName + "Event";
    EnumTypeInfo info;
    info.name = eventTypeName;

    unsigned idx = 0;
    for (const auto &event : decl.events) {
      EnumVariantInfo vi;
      vi.name = event.name;
      vi.index = idx++;
      bool eventOk = true;
      for (const auto &[fieldName, fieldType] : event.fields) {
        auto payloadType = convertTypeOrError(
            fieldType.value, "cannot resolve type for event field '" + fieldName + "'");
        if (!payloadType) {
          eventOk = false;
          break;
        }
        vi.payloadTypes.push_back(payloadType);
        vi.fieldNames.push_back(fieldName);
      }
      if (!eventOk)
        return;
      info.variants.push_back(std::move(vi));
    }

    for (const auto &v : info.variants) {
      variantLookup[v.name] = {eventTypeName, v.index};
      variantLookup[eventTypeName + "::" + v.name] = {eventTypeName, v.index};
    }

    bool anyPayload = false;
    size_t maxPayloadFields = 0;
    for (const auto &v : info.variants) {
      if (!v.payloadTypes.empty()) {
        anyPayload = true;
        maxPayloadFields = std::max(maxPayloadFields, v.payloadTypes.size());
      }
    }
    info.hasPayloads = anyPayload;

    if (!anyPayload) {
      info.mlirType = builder.getI32Type();
    } else {
      llvm::SmallVector<mlir::Type, 4> structFields;
      structFields.push_back(builder.getI32Type()); // tag

      int64_t nextField = 1;
      for (auto &v : info.variants) {
        v.payloadPositions.clear();
        for (unsigned i = 0; i < v.payloadTypes.size(); i++)
          v.payloadPositions.push_back(nextField + i);
      }

      // Union overlay: for each position, use the widest type
      for (unsigned pos = 0; pos < maxPayloadFields; pos++) {
        mlir::Type widest;
        unsigned widestBits = 0;
        for (const auto &v : info.variants) {
          if (pos < v.payloadTypes.size()) {
            auto ty = v.payloadTypes[pos];
            unsigned bits = 64;
            if (auto intTy = llvm::dyn_cast<mlir::IntegerType>(ty))
              bits = intTy.getWidth();
            else if (auto fltTy = llvm::dyn_cast<mlir::FloatType>(ty))
              bits = fltTy.getWidth();
            if (!widest || bits > widestBits) {
              widest = ty;
              widestBits = bits;
            }
          }
        }
        structFields.push_back(widest);
      }

      info.mlirType = mlir::LLVM::LLVMStructType::getLiteral(&context, structFields);
    }

    enumTypes[eventTypeName] = std::move(info);
  }
}

// ============================================================================
// Machine declaration code generation (step + state_name functions)
// ============================================================================

void MLIRGen::generateMachineDecl(const ast::MachineDecl &decl) {
  const auto &machineName = decl.name;
  std::string eventTypeName = machineName + "Event";

  auto machineEnumIt = enumTypes.find(machineName);
  auto eventEnumIt = enumTypes.find(eventTypeName);
  if (machineEnumIt == enumTypes.end() || eventEnumIt == enumTypes.end())
    return;

  const auto &machineInfo = machineEnumIt->second;
  const auto &eventInfo = eventEnumIt->second;
  auto machineType = machineInfo.mlirType;
  auto eventType = eventInfo.mlirType;
  auto location = builder.getUnknownLoc();

  // ── Generate state_name() function ──
  {
    std::string funcName = mangleName(currentModulePath, machineName, "state_name");
    auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
    auto funcType = builder.getFunctionType({machineType}, {ptrType});

    auto savedIP = builder.saveInsertionPoint();
    builder.setInsertionPointToEnd(module.getBody());
    auto funcOp = mlir::func::FuncOp::create(builder, location, funcName, funcType);
    auto *entryBlock = funcOp.addEntryBlock();
    builder.setInsertionPointToStart(entryBlock);

    auto selfArg = entryBlock->getArgument(0);
    auto tag = hew::EnumExtractTagOp::create(builder, location, builder.getI32Type(), selfArg);

    // Build if-else chain: if tag==0 return "State0", elif tag==1 ...
    mlir::Value result = nullptr;
    for (int i = static_cast<int>(decl.states.size()) - 1; i >= 0; --i) {
      auto symName = getOrCreateGlobalString(decl.states[i].name);
      auto strVal = hew::ConstantOp::create(builder, location, hew::StringRefType::get(&context),
                                            builder.getStringAttr(symName));
      auto strPtr = hew::BitcastOp::create(builder, location, ptrType, strVal);

      if (result == nullptr) {
        // Last (default) case
        result = strPtr;
      } else {
        auto tagVal = createIntConstant(builder, location, builder.getI32Type(), i);
        auto cond = mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::eq,
                                                tag, tagVal);
        result = mlir::arith::SelectOp::create(builder, location, cond, strPtr, result);
      }
    }

    mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{result});
    builder.restoreInsertionPoint(savedIP);
  }

  // ── Generate step() function ──
  {
    std::string funcName = mangleName(currentModulePath, machineName, "step");
    auto funcType = builder.getFunctionType({machineType, eventType}, {machineType});

    auto savedIP = builder.saveInsertionPoint();
    builder.setInsertionPointToEnd(module.getBody());
    auto funcOp = mlir::func::FuncOp::create(builder, location, funcName, funcType);
    auto *entryBlock = funcOp.addEntryBlock();
    builder.setInsertionPointToStart(entryBlock);

    auto selfArg = entryBlock->getArgument(0);
    auto eventArg = entryBlock->getArgument(1);

    auto stateTag = hew::EnumExtractTagOp::create(builder, location, builder.getI32Type(), selfArg);
    auto eventTag =
        hew::EnumExtractTagOp::create(builder, location, builder.getI32Type(), eventArg);

    // Build a map from (source_state_idx, event_idx) → list of transition info.
    // Wildcard source "_" maps to all states not explicitly covered.
    // Multiple transitions for the same pair are allowed when guards are present.
    struct TransitionTarget {
      unsigned targetStateIdx;
      const ast::MachineTransition *transition = nullptr;
    };
    std::map<std::pair<unsigned, unsigned>, std::vector<TransitionTarget>> transitionMap;

    // Build event name → index map
    std::unordered_map<std::string, unsigned> eventNameToIdx;
    for (unsigned i = 0; i < decl.events.size(); ++i)
      eventNameToIdx[decl.events[i].name] = i;

    // Build state name → index map
    std::unordered_map<std::string, unsigned> stateNameToIdx;
    for (unsigned i = 0; i < decl.states.size(); ++i)
      stateNameToIdx[decl.states[i].name] = i;

    // Collect wildcard transitions (source == "_")
    std::unordered_map<unsigned, std::pair<unsigned, const ast::MachineTransition *>>
        wildcardEventToTarget;

    for (const auto &trans : decl.transitions) {
      auto eventIt = eventNameToIdx.find(trans.event_name);
      if (eventIt == eventNameToIdx.end())
        continue;
      unsigned eventIdx = eventIt->second;

      if (trans.source_state == "_") {
        // Wildcard: determine target
        if (trans.target_state == "_") {
          // self transition — target = source (handled per-state below)
          wildcardEventToTarget[eventIdx] = {UINT_MAX, &trans}; // sentinel: self
        } else {
          auto targetIt = stateNameToIdx.find(trans.target_state);
          if (targetIt != stateNameToIdx.end())
            wildcardEventToTarget[eventIdx] = {targetIt->second, &trans};
        }
      } else {
        auto sourceIt = stateNameToIdx.find(trans.source_state);
        if (sourceIt == stateNameToIdx.end())
          continue;
        unsigned sourceIdx = sourceIt->second;
        unsigned targetIdx = sourceIdx; // default: self
        if (trans.target_state != "_") {
          auto targetIt = stateNameToIdx.find(trans.target_state);
          if (targetIt != stateNameToIdx.end())
            targetIdx = targetIt->second;
        }
        transitionMap[{sourceIdx, eventIdx}].push_back({targetIdx, &trans});
      }
    }

    // Fill wildcard slots for states that don't have explicit transitions
    for (unsigned si = 0; si < decl.states.size(); ++si) {
      for (const auto &[eventIdx, wildcardInfo] : wildcardEventToTarget) {
        auto key = std::make_pair(si, eventIdx);
        if (transitionMap.find(key) == transitionMap.end()) {
          unsigned finalTarget = (wildcardInfo.first == UINT_MAX) ? si : wildcardInfo.first;
          transitionMap[key].push_back({finalTarget, wildcardInfo.second});
        }
      }
    }

    // Helper: check if body expression is just the identifier `self`
    auto isSelfBodyExpr = [](const ast::Expr &expr) -> bool {
      if (auto *ident = std::get_if<ast::ExprIdentifier>(&expr.kind))
        return ident->name == "state";
      return false;
    };

    // Build nested if-else chain over (stateTag, eventTag) pairs.
    // Result defaults to self (identity transition for unmatched pairs).
    mlir::Value result = selfArg;

    // Iterate in reverse so the first pair ends up as the outermost condition
    for (auto it = transitionMap.rbegin(); it != transitionMap.rend(); ++it) {
      unsigned sourceIdx = it->first.first;
      unsigned eventIdx = it->first.second;
      const auto &targets = it->second;

      // Build base condition: stateTag == sourceIdx && eventTag == eventIdx
      auto stateConst = createIntConstant(builder, location, builder.getI32Type(), sourceIdx);
      auto eventConst = createIntConstant(builder, location, builder.getI32Type(), eventIdx);
      auto stateCmp = mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::eq,
                                                  stateTag, stateConst);
      auto eventCmp = mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::eq,
                                                  eventTag, eventConst);
      auto baseCond = mlir::arith::AndIOp::create(builder, location, stateCmp, eventCmp);

      // Process transitions in reverse order so earlier ones take priority
      for (auto ti = targets.rbegin(); ti != targets.rend(); ++ti) {
        const auto *trans = ti->transition;
        auto cond = baseCond;

        mlir::Value targetVal;

        if (trans && isSelfBodyExpr(trans->body.value)) {
          // Self-transition: return selfArg unchanged (preserves all fields)
          targetVal = selfArg;
        } else if (trans) {
          // Evaluate the transition body expression (e.g. Count { n: self.n + 1 })
          SymbolTableScopeT bodyScope(symbolTable);
          declareVariable("state", selfArg);
          declareVariable("event", eventArg);
          currentMachineSourceVariant_ = decl.states[sourceIdx].name;
          currentMachineEventVariant_ = trans->event_name;
          currentMachineEventTypeName_ = machineName + "Event";

          // Evaluate guard if present
          if (trans->guard) {
            auto guardVal = generateExpression(trans->guard->value);
            if (guardVal)
              cond = mlir::arith::AndIOp::create(builder, location, cond, guardVal);
          }

          targetVal = generateExpression(trans->body.value);
          currentMachineSourceVariant_.clear();
          currentMachineEventVariant_.clear();
          currentMachineEventTypeName_.clear();
        } else {
          // Fallback: construct tag-only value
          targetVal = hew::EnumConstructOp::create(builder, location, machineType,
                                                   static_cast<uint32_t>(ti->targetStateIdx),
                                                   llvm::StringRef(machineName), mlir::ValueRange{},
                                                   /*payload_positions=*/mlir::ArrayAttr{});
        }

        if (targetVal)
          result = mlir::arith::SelectOp::create(builder, location, cond, targetVal, result);
      }
    }

    mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{result});
    builder.restoreInsertionPoint(savedIP);
  }
}

// ============================================================================
// Impl declaration generation
// ============================================================================

void MLIRGen::generateImplDecl(const ast::ImplDecl &decl,
                               std::optional<mlir::Location> fallbackLoc) {
  // Get the target type name
  std::string typeName;
  if (auto *named = std::get_if<ast::TypeNamed>(&decl.target_type.value.kind)) {
    typeName = named->name;
  }
  if (typeName.empty())
    return;

  // Generic impl: defer method generation until concrete instantiation.
  if (decl.type_params && !decl.type_params->empty()) {
    GenericImplInfo info;
    info.typeParams = &*decl.type_params;
    for (const auto &method : decl.methods)
      info.methods.push_back(&method);
    genericImplMethods[typeName] = std::move(info);
    return;
  }

  // Use the module where the type was DEFINED for mangling, not the
  // current (possibly importing) module. This prevents duplicate generation
  // when an importing module re-processes the impl from an imported module.
  // Temporarily swap currentModulePath so all internal calls (registerTraitImpl,
  // generateTraitImplShims, etc.) use the correct path.
  auto savedModulePath = currentModulePath;
  if (typeDefModulePath.count(typeName)) {
    currentModulePath = typeDefModulePath[typeName];
  }

  // Set Self substitution so bare self parameters resolve to the target type
  typeParamSubstitutions["Self"] = typeName;

  // Generate each method as a free function with mangled name
  std::set<std::string> overriddenMethods;
  for (const auto &method : decl.methods) {
    std::string mangledMethod = mangleName(currentModulePath, typeName, method.name);
    generateFunction(method, mangledMethod, fallbackLoc);
    overriddenMethods.insert(method.name);
  }

  // Generate default method bodies from the trait for methods not overridden
  std::string traitName = decl.trait_bound ? decl.trait_bound->name : "";
  auto traitIt = traitRegistry.find(traitName);
  if (traitIt != traitRegistry.end()) {
    for (const auto *tm : traitIt->second.methods) {
      if (tm->body && overriddenMethods.find(tm->name) == overriddenMethods.end()) {
        std::string mangledDefault = mangleName(currentModulePath, typeName, tm->name);
        generateTraitDefaultMethod(*tm, typeName, mangledDefault, fallbackLoc);
      }
    }
  }

  // Register this type for trait dispatch and generate shim functions
  if (traitIt != traitRegistry.end()) {
    std::vector<std::string> methodNames;
    for (const auto *tm : traitIt->second.methods) {
      methodNames.push_back(tm->name);
    }
    registerTraitImpl(typeName, traitName, methodNames);
    generateTraitImplShims(typeName, traitName);
  }

  if (traitName == "Drop") {
    userDropFuncs[typeName] = mangleName(currentModulePath, typeName, "drop");
  }

  typeParamSubstitutions.erase("Self");
  currentModulePath = savedModulePath;
}

// ============================================================================
// Trait declaration registration
// ============================================================================

void MLIRGen::registerTraitDecl(const ast::TraitDecl &decl) {
  TraitInfo info;
  // Inherit methods from super-traits (e.g. trait Pet: Animal inherits legs())
  if (decl.super_traits) {
    for (const auto &bound : *decl.super_traits) {
      auto superIt = traitRegistry.find(bound.name);
      if (superIt != traitRegistry.end()) {
        for (const auto *m : superIt->second.methods) {
          info.methodIndex[m->name] = info.methods.size();
          info.methods.push_back(m);
        }
      }
    }
  }
  for (const auto &item : decl.items) {
    if (auto *methodItem = std::get_if<ast::TraitItemMethod>(&item)) {
      info.methodIndex[methodItem->method.name] = info.methods.size();
      info.methods.push_back(&methodItem->method);
    }
  }
  traitRegistry[decl.name] = std::move(info);
}

// ============================================================================
// dyn Trait: register implementation for vtable-based dispatch
// ============================================================================

void MLIRGen::registerTraitImpl(const std::string &typeName, const std::string &traitName,
                                const std::vector<std::string> &methodNames) {
  auto &dispatchInfo = traitDispatchRegistry[traitName];
  // Check if already registered
  for (const auto &impl : dispatchInfo.impls) {
    if (impl.typeName == typeName)
      return;
  }
  dispatchInfo.methodNames = methodNames;
  TraitImplInfo implInfo;
  implInfo.typeName = typeName;
  implInfo.vtableName = "__vtable" + mangleName(currentModulePath, typeName, traitName);
  // Pre-compute shim function names (actual functions generated later)
  auto traitIt = traitRegistry.find(traitName);
  if (traitIt != traitRegistry.end()) {
    for (const auto *tm : traitIt->second.methods) {
      std::string implFuncName = mangleName(currentModulePath, typeName, tm->name);
      implInfo.shimFunctions.push_back("__dyn." + implFuncName);
    }
  }
  dispatchInfo.impls.push_back(implInfo);
}

// ============================================================================
// Trait object coercion: concrete value → dyn Trait fat pointer {data_ptr, vtable_ptr}
// ============================================================================

mlir::Value MLIRGen::coerceToDynTrait(mlir::Value concreteVal, const std::string &typeName,
                                      const std::string &traitName, mlir::Location location) {
  auto dispIt = traitDispatchRegistry.find(traitName);
  if (dispIt == traitDispatchRegistry.end()) {
    emitError(location) << "no dispatch info for trait '" << traitName << "'";
    return nullptr;
  }

  // Find the impl info for this concrete type
  const TraitImplInfo *implInfo = nullptr;
  for (const auto &impl : dispIt->second.impls) {
    if (impl.typeName == typeName) {
      implInfo = &impl;
      break;
    }
  }
  if (!implInfo) {
    emitError(location) << typeName << " does not implement " << traitName;
    return nullptr;
  }

  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto fatPtrType = hew::HewTraitObjectType::get(&context, traitName);

  // Allocate concrete value on the heap (uses arena when active during dispatch)
  auto concreteType = concreteVal.getType();
  auto sizeVal =
      hew::SizeOfOp::create(builder, location, sizeType(), mlir::TypeAttr::get(concreteType));

  auto dataPtr = hew::ArenaMallocOp::create(builder, location, ptrType, sizeVal);
  mlir::LLVM::StoreOp::create(builder, location, concreteVal, dataPtr);

  // Register data pointer for scope-exit cleanup so the heap copy is freed.
  // The fat pointer is a value type (stack struct); only the data pointer leaks.
  {
    std::string tmpName =
        std::string("\0__dyn_data_", 12) + std::to_string(tempMaterializationCounter++);
    declareVariable(tmpName, dataPtr);
    registerDroppable(tmpName, "free");
  }

  // Get vtable pointer via VtableRefOp
  llvm::SmallVector<mlir::Attribute> funcAttrs;
  for (const auto &shimName : implInfo->shimFunctions)
    funcAttrs.push_back(builder.getStringAttr(shimName));
  auto vtablePtr = hew::VtableRefOp::create(builder, location, ptrType,
                                            builder.getStringAttr(implInfo->vtableName),
                                            builder.getArrayAttr(funcAttrs));

  // Build fat pointer: { data_ptr, vtable_ptr }
  mlir::Value fatPtr =
      hew::TraitObjectCreateOp::create(builder, location, fatPtrType, dataPtr, vtablePtr);

  return fatPtr;
}

// ============================================================================
// Vtable dispatch shim generation
// ============================================================================

void MLIRGen::generateDynDispatchShim(const std::string &implFuncName) {
  std::string shimName = "__dyn." + implFuncName;

  // Check if shim already exists
  if (module.lookupSymbol<mlir::func::FuncOp>(shimName))
    return;

  // Look up the impl function to get its type
  auto implFunc = module.lookupSymbol<mlir::func::FuncOp>(implFuncName);
  if (!implFunc) {
    emitError(builder.getUnknownLoc()) << "vtable shim generation failed: function '"
                                       << implFuncName << "' not found for dyn dispatch shim";
    return;
  }

  auto implType = implFunc.getFunctionType();
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);

  // Build shim type: (ptr, extra_args...) -> result
  llvm::SmallVector<mlir::Type> shimArgTypes = {ptrType};
  for (unsigned i = 1; i < implType.getNumInputs(); ++i)
    shimArgTypes.push_back(implType.getInput(i));
  auto shimType = builder.getFunctionType(shimArgTypes, implType.getResults());

  // Create shim function at module level
  auto savedIP = builder.saveInsertionPoint();
  builder.setInsertionPointToEnd(module.getBody());
  auto loc = builder.getUnknownLoc();
  auto shimFunc = mlir::func::FuncOp::create(builder, loc, shimName, shimType);
  auto *entryBlock = shimFunc.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  // Load concrete value from data pointer — use LLVM storage type since
  // this is an LLVM::LoadOp which requires LLVM-legal result types.
  auto origSelfType = implType.getInput(0);
  auto selfType = toLLVMStorageType(origSelfType);
  mlir::Value selfVal =
      mlir::LLVM::LoadOp::create(builder, loc, selfType, entryBlock->getArgument(0));

  // If the impl expects a dialect type (e.g. !hew.handle), bitcast so
  // the func.call types match the impl signature before lowering.
  if (selfType != origSelfType)
    selfVal = hew::BitcastOp::create(builder, loc, origSelfType, selfVal);

  // Build call args: loaded self + forwarded extra args
  llvm::SmallVector<mlir::Value> callArgs = {selfVal};
  for (unsigned i = 1; i < shimArgTypes.size(); ++i)
    callArgs.push_back(entryBlock->getArgument(i));

  // Call impl function
  auto call =
      mlir::func::CallOp::create(builder, loc, implFuncName, implType.getResults(), callArgs);

  // Return
  if (implType.getNumResults() > 0)
    mlir::func::ReturnOp::create(builder, loc, call.getResults());
  else
    mlir::func::ReturnOp::create(builder, loc);

  builder.restoreInsertionPoint(savedIP);
}

void MLIRGen::generateTraitImplShims(const std::string &typeName, const std::string &traitName) {
  auto traitIt = traitRegistry.find(traitName);
  if (traitIt == traitRegistry.end())
    return;

  // Generate shim function bodies for each trait method
  for (const auto *tm : traitIt->second.methods) {
    std::string implFuncName = mangleName(currentModulePath, typeName, tm->name);
    generateDynDispatchShim(implFuncName);
  }
}

// ============================================================================
// Trait default method generation
// ============================================================================

void MLIRGen::generateTraitDefaultMethod(const ast::TraitMethod &method,
                                         const std::string &targetTypeName,
                                         const std::string &mangledName,
                                         std::optional<mlir::Location> fallbackLoc) {
  if (!method.body)
    return;
  SymbolTableScopeT varScope(symbolTable);
  MutableTableScopeT mutScope(mutableVars);

  auto typeLoc = [&](const ast::Spanned<ast::TypeExpr> &type) {
    return type.span.end > type.span.start ? loc(type.span) : fallbackLoc.value_or(currentLoc);
  };
  llvm::SmallVector<mlir::Type, 4> paramTypes;
  for (const auto &param : method.params) {
    paramTypes.push_back(convertType(param.ty.value, typeLoc(param.ty)));
  }

  llvm::SmallVector<mlir::Type, 1> resultTypes;
  if (method.return_type) {
    auto retTy = convertType(method.return_type->value, typeLoc(*method.return_type));
    if (!llvm::isa<mlir::NoneType>(retTy))
      resultTypes.push_back(retTy);
  }

  auto funcType = builder.getFunctionType(paramTypes, resultTypes);
  auto location = fallbackLoc.value_or(currentLoc);

  // Erase forward declaration if one was created in pass 1d
  if (auto existing = module.lookupSymbol<mlir::func::FuncOp>(mangledName)) {
    if (existing.isDeclaration())
      existing.erase();
  }

  auto savedIP = builder.saveInsertionPoint();
  builder.setInsertionPointToEnd(module.getBody());
  auto funcOp = mlir::func::FuncOp::create(builder, location, mangledName, funcType);

  auto *entryBlock = funcOp.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto prevFunction = currentFunction;
  currentFunction = funcOp;
  auto prevReturnFlag = returnFlag;
  auto prevReturnSlot = returnSlot;
  auto prevReturnSlotIsLazy = returnSlotIsLazy;
  auto prevEarlyReturnFlag = earlyReturnFlag;
  auto prevChannelIntOutValidAlloca = channelIntOutValidAlloca;
  auto prevFuncLevelDropExcludeVars = std::move(funcLevelDropExcludeVars);
  auto prevFuncLevelDropExcludeValues = std::move(funcLevelDropExcludeValues);
  auto prevFuncLevelDropExcludeResolvedNames = std::move(funcLevelDropExcludeResolvedNames);
  auto prevFuncLevelEarlyReturnExcludeValues = std::move(funcLevelEarlyReturnExcludeValues);
  auto prevFuncLevelEarlyReturnExcludeResolvedNames =
      std::move(funcLevelEarlyReturnExcludeResolvedNames);
  auto prevFuncLevelDropScopeBase = funcLevelDropScopeBase;
  auto prevPendingParamDrops = std::move(pendingFunctionParamDrops);
  auto prevFnDefers = std::move(currentFnDefers);
  returnFlag = nullptr;
  returnSlot = nullptr;
  returnSlotIsLazy = false;
  earlyReturnFlag = nullptr;
  channelIntOutValidAlloca = nullptr;
  funcLevelDropExcludeVars.clear();
  funcLevelDropExcludeValues.clear();
  funcLevelDropExcludeResolvedNames.clear();
  funcLevelEarlyReturnExcludeValues.clear();
  funcLevelEarlyReturnExcludeResolvedNames.clear();
  funcLevelDropScopeBase = dropScopes.size();
  currentFnDefers.clear();

  uint32_t paramIdx = 0;
  for (const auto &param : method.params) {
    declareVariable(param.name, entryBlock->getArgument(paramIdx));
    ++paramIdx;
  }

  // Early return support — always create returnFlag so that `return` inside
  // nested SCF regions (match arms, if branches) stores to the flag instead
  // of emitting an illegal func.return inside an scf.if.
  initReturnFlagAndSlot(resultTypes, location);

  // NOTE: param drops are not yet registered here.  Adding them requires
  // null-after-move tracking to avoid double-frees when a param is consumed
  // by match destructuring, callee move, or return.  See RAII Phase 1 plan.

  mlir::Value bodyValue = generateBlock(*method.body);

  auto *currentBlock = builder.getInsertionBlock();
  if (currentBlock &&
      (currentBlock->empty() || !currentBlock->back().hasTrait<mlir::OpTrait::IsTerminator>())) {
    if (returnFlag && returnSlot && !resultTypes.empty()) {
      auto flagVal =
          mlir::memref::LoadOp::create(builder, location, returnFlag, mlir::ValueRange{});
      auto selectOp = mlir::scf::IfOp::create(builder, location, resultTypes[0], flagVal,
                                              /*withElseRegion=*/true);

      builder.setInsertionPointToStart(&selectOp.getThenRegion().front());
      auto slotVal = mlir::memref::LoadOp::create(builder, location, returnSlot, mlir::ValueRange{})
                         .getResult();
      slotVal = coerceTypeForSink(slotVal, resultTypes[0], location);
      mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{slotVal});

      builder.setInsertionPointToStart(&selectOp.getElseRegion().front());
      mlir::Value normalValue = bodyValue;
      if (!normalValue)
        normalValue = createDefaultValue(builder, location, resultTypes[0]);
      normalValue = coerceTypeForSink(normalValue, resultTypes[0], location);
      mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{normalValue});

      builder.setInsertionPointAfter(selectOp);
      mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{selectOp.getResult(0)});
    } else if (bodyValue && !resultTypes.empty()) {
      bodyValue = coerceTypeForSink(bodyValue, resultTypes[0], location);
      mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{bodyValue});
    } else {
      mlir::func::ReturnOp::create(builder, location);
    }
  }

  returnFlag = prevReturnFlag;
  returnSlot = prevReturnSlot;
  returnSlotIsLazy = prevReturnSlotIsLazy;
  earlyReturnFlag = prevEarlyReturnFlag;
  channelIntOutValidAlloca = prevChannelIntOutValidAlloca;
  funcLevelDropExcludeVars = std::move(prevFuncLevelDropExcludeVars);
  funcLevelDropExcludeValues = std::move(prevFuncLevelDropExcludeValues);
  funcLevelDropExcludeResolvedNames = std::move(prevFuncLevelDropExcludeResolvedNames);
  funcLevelEarlyReturnExcludeValues = std::move(prevFuncLevelEarlyReturnExcludeValues);
  funcLevelEarlyReturnExcludeResolvedNames =
      std::move(prevFuncLevelEarlyReturnExcludeResolvedNames);
  funcLevelDropScopeBase = prevFuncLevelDropScopeBase;
  pendingFunctionParamDrops = std::move(prevPendingParamDrops);
  currentFnDefers = std::move(prevFnDefers);
  currentFunction = prevFunction;
  builder.restoreInsertionPoint(savedIP);
}

// ============================================================================
// Function generation
// ============================================================================

mlir::func::FuncOp MLIRGen::generateFunction(const ast::FnDecl &fn,
                                             const std::string &nameOverride,
                                             std::optional<mlir::Location> fallbackLoc) {
  // Create symbol table scopes for this function
  SymbolTableScopeT varScope(symbolTable);
  MutableTableScopeT mutScope(mutableVars);

  // Determine parameter types
  auto typeLoc = [&](const ast::Spanned<ast::TypeExpr> &type) {
    return type.span.end > type.span.start ? loc(type.span) : fallbackLoc.value_or(currentLoc);
  };
  llvm::SmallVector<mlir::Type, 4> paramTypes;
  for (const auto &param : fn.params) {
    paramTypes.push_back(convertType(param.ty.value, typeLoc(param.ty)));
  }

  // Determine return type
  llvm::SmallVector<mlir::Type, 1> resultTypes;
  if (fn.return_type) {
    auto retTy = convertType(fn.return_type->value, typeLoc(*fn.return_type));
    // Don't add NoneType to results (unit return = no results)
    if (!llvm::isa<mlir::NoneType>(retTy)) {
      resultTypes.push_back(retTy);
    }
  }

  auto location = fallbackLoc.value_or(currentLoc);

  // Create the function at module scope.
  // When nameOverride is provided, the caller has already mangled the name.
  std::string funcName =
      nameOverride.empty() ? mangleName(currentModulePath, "", fn.name) : nameOverride;

  // fn main() without an explicit return type implicitly returns i32 exit
  // code 0.  This keeps the POSIX ABI contract (int main) without forcing
  // every program to write a trailing "0".  We defer adding i32 to
  // resultTypes until after body generation so the body is generated in
  // void context (no returnSlot/returnFlag, trailing expressions treated
  // as statements).
  bool isImplicitMainReturn = funcName == "main" && resultTypes.empty();

  auto funcType = builder.getFunctionType(paramTypes, resultTypes);

  // If a forward declaration exists (from pass 1.5), erase it — we'll
  // replace it with the full definition that includes a body.
  // If a full definition already exists (e.g. imported module's impl was
  // already generated), skip — don't create a duplicate.
  if (auto existing = module.lookupSymbol<mlir::func::FuncOp>(funcName)) {
    if (existing.isDeclaration()) {
      existing.erase();
    } else {
      // Already generated (e.g. from the defining module) — skip.
      return existing;
    }
  }

  auto savedIP = builder.saveInsertionPoint();
  builder.setInsertionPointToEnd(module.getBody());
  auto funcOp = mlir::func::FuncOp::create(builder, location, funcName, funcType);

  // Pub-aware linkage: main is always public, pub functions are public,
  // everything else is private.
  if (funcName == "main") {
    // main is always public — no visibility change needed (default is public)
  } else if (ast::is_pub(fn.visibility)) {
    funcOp.setVisibility(mlir::SymbolTable::Visibility::Public);
  } else {
    funcOp.setVisibility(mlir::SymbolTable::Visibility::Private);
  }

  // Create entry block with parameter arguments
  auto *entryBlock = funcOp.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  // Save and reset function-scoped state before parameter registration,
  // so nested generateFunction calls (e.g., generics specialization) don't
  // contaminate the outer function's allocas.
  auto prevFunction = currentFunction;
  currentFunction = funcOp;

  auto prevReturnFlag = returnFlag;
  auto prevReturnSlot = returnSlot;
  auto prevReturnSlotIsLazy = returnSlotIsLazy;
  auto prevEarlyReturnFlag = earlyReturnFlag;
  auto prevChannelIntOutValidAlloca = channelIntOutValidAlloca;
  auto prevFuncLevelDropExcludeVars = std::move(funcLevelDropExcludeVars);
  auto prevFuncLevelDropExcludeValues = std::move(funcLevelDropExcludeValues);
  auto prevFuncLevelDropExcludeResolvedNames = std::move(funcLevelDropExcludeResolvedNames);
  auto prevFuncLevelEarlyReturnExcludeValues = std::move(funcLevelEarlyReturnExcludeValues);
  auto prevFuncLevelEarlyReturnExcludeResolvedNames =
      std::move(funcLevelEarlyReturnExcludeResolvedNames);
  auto prevFuncLevelDropScopeBase = funcLevelDropScopeBase;
  auto prevPendingParamDrops = std::move(pendingFunctionParamDrops);
  auto prevFnDefers = std::move(currentFnDefers);
  returnFlag = nullptr;
  returnSlot = nullptr;
  returnSlotIsLazy = false;
  earlyReturnFlag = nullptr;
  channelIntOutValidAlloca = nullptr;
  funcLevelDropExcludeVars.clear();
  funcLevelDropExcludeValues.clear();
  funcLevelDropExcludeResolvedNames.clear();
  funcLevelEarlyReturnExcludeValues.clear();
  funcLevelEarlyReturnExcludeResolvedNames.clear();
  currentFnDefers.clear();
  uint32_t paramIdx = 0;
  for (const auto &param : fn.params) {
    const auto &paramName = param.name;
    declareVariable(paramName, entryBlock->getArgument(paramIdx));

    // Track collection/handle/actor parameter types from type annotation
    const auto &paramTy = param.ty.value;
    {
      auto handleStr = typeExprToHandleString(paramTy);
      if (!handleStr.empty())
        handleVarTypes[paramName] = handleStr;
      auto actorName = typeExprToActorName(paramTy);
      if (!actorName.empty() && actorRegistry.count(actorName))
        actorVarTypes[paramName] = actorName;
    }

    // Track dyn Trait parameter types for dynamic dispatch
    if (auto *traitObj = std::get_if<ast::TypeTraitObject>(&paramTy.kind)) {
      if (!traitObj->bounds.empty()) {
        dynTraitVarTypes[paramName] = traitObj->bounds[0].name;
      }
    }
    ++paramIdx;
  }

  // Early return support: always create returnFlag so that `return` inside
  // nested SCF regions (match arms, if branches) stores to the flag instead
  // of emitting an illegal func.return inside an scf.if.
  initReturnFlagAndSlot(resultTypes, location);

  // Determine trailing expression variable names BEFORE generating the body.
  // For struct init expressions, collect all field variable references to
  // prevent double-free when the struct takes ownership of the values.
  funcLevelDropExcludeVars.clear();
  funcLevelDropScopeBase = dropScopes.size();
  // Recursively collect identifiers from the return position of an
  // expression (including if/match/block branches) so their owning
  // variables are excluded from the function-level drop scope.
  // Each candidate stores (name, depth) until lowering resolves it to a
  // stable binding identity. The temporary depth tag lets us distinguish a
  // returned outer binding from a shadowed inner binding with the same name.
  // These three helpers are mutually recursive (expr ↔ block ↔ stmtIf).
  using ExcludeSet = std::set<std::pair<std::string, size_t>>;
  std::function<void(const ast::Expr &, ExcludeSet &, size_t)> collectExcludeVars;
  std::function<void(const ast::Block &, ExcludeSet &, size_t, bool)> collectExcludeVarsFromBlock;
  std::function<void(const ast::StmtIf &, ExcludeSet &, size_t, bool)> collectExcludeVarsFromStmtIf;
  collectExcludeVarsFromStmtIf = [&collectExcludeVarsFromBlock, &collectExcludeVarsFromStmtIf](
      const ast::StmtIf &ifStmt, ExcludeSet &out, size_t depth, bool producesValue) {
    // StmtIf branches go through generateBlock which pushes a scope → depth+1
    collectExcludeVarsFromBlock(ifStmt.then_block, out, depth + 1, producesValue);
    if (ifStmt.else_block) {
      if (ifStmt.else_block->block)
        collectExcludeVarsFromBlock(*ifStmt.else_block->block, out, depth + 1, producesValue);
      if (ifStmt.else_block->if_stmt) {
        const auto &nested = ifStmt.else_block->if_stmt->value;
        // else-if doesn't add a scope — stay at same depth
        if (auto *nestedIf = std::get_if<ast::StmtIf>(&nested.kind))
          collectExcludeVarsFromStmtIf(*nestedIf, out, depth, producesValue);
      }
    }
  };
  collectExcludeVarsFromBlock = [&collectExcludeVars, &collectExcludeVarsFromStmtIf,
                                 &collectExcludeVarsFromBlock](
      const ast::Block &blk, ExcludeSet &out, size_t depth, bool producesValue) {
    if (blk.trailing_expr) {
      collectExcludeVars(blk.trailing_expr->value, out, depth);
    } else if (producesValue && !blk.stmts.empty()) {
      // When the block is expected to produce a value (non-void function
      // body, expression-position block), the last statement's expression
      // IS the implicit return value.  Exclude its variables from drops.
      // When the block does NOT produce a value (void/unit functions),
      // the last statement's result is discarded — do NOT exclude.
      const auto &last = blk.stmts.back()->value;
      if (auto *exprStmt = std::get_if<ast::StmtExpression>(&last.kind)) {
        collectExcludeVars(exprStmt->expr.value, out, depth);
      } else if (auto *ifStmt = std::get_if<ast::StmtIf>(&last.kind)) {
        collectExcludeVarsFromStmtIf(*ifStmt, out, depth, true);
      } else if (auto *matchStmt = std::get_if<ast::StmtMatch>(&last.kind)) {
        for (const auto &arm : matchStmt->arms) {
          if (arm.body)
            collectExcludeVars(arm.body->value, out, depth);
        }
      }
    }
    // Scan ALL statements for return expressions and let/var bindings
    // that transfer ownership. Return statements can appear anywhere in
    // the block (not just at the end), and each one's referenced variables
    // must be excluded from function-level drops. We recurse into nested
    // statement forms (if/for/while/match) to catch returns in inner scopes.
    for (const auto &stmt : blk.stmts) {
      if (auto *retStmt = std::get_if<ast::StmtReturn>(&stmt->value.kind)) {
        if (retStmt->value)
          collectExcludeVars(retStmt->value->value, out, depth);
        continue;
      }
      // Recurse into nested control flow to find return statements.
      if (auto *ifStmt = std::get_if<ast::StmtIf>(&stmt->value.kind)) {
        collectExcludeVarsFromBlock(ifStmt->then_block, out, depth + 1, producesValue);
        if (ifStmt->else_block) {
          if (ifStmt->else_block->block)
            collectExcludeVarsFromBlock(*ifStmt->else_block->block, out, depth + 1, producesValue);
          if (ifStmt->else_block->if_stmt) {
            const auto &nested = ifStmt->else_block->if_stmt->value;
            if (auto *nestedIf = std::get_if<ast::StmtIf>(&nested.kind))
              collectExcludeVarsFromStmtIf(*nestedIf, out, depth, producesValue);
          }
        }
        continue;
      }
      if (auto *forStmt = std::get_if<ast::StmtFor>(&stmt->value.kind)) {
        collectExcludeVarsFromBlock(forStmt->body, out, depth + 1, producesValue);
        continue;
      }
      if (auto *whileStmt = std::get_if<ast::StmtWhile>(&stmt->value.kind)) {
        collectExcludeVarsFromBlock(whileStmt->body, out, depth + 1, producesValue);
        continue;
      }
      if (auto *matchStmt = std::get_if<ast::StmtMatch>(&stmt->value.kind)) {
        for (const auto &arm : matchStmt->arms) {
          if (arm.body)
            collectExcludeVars(arm.body->value, out, depth);
        }
        continue;
      }
      // Scan let/var bindings whose RHS is a match/if/block expression.
      const ast::Expr *rhs = nullptr;
      if (auto *letStmt = std::get_if<ast::StmtLet>(&stmt->value.kind)) {
        if (letStmt->value)
          rhs = &letStmt->value->value;
      } else if (auto *varStmt = std::get_if<ast::StmtVar>(&stmt->value.kind)) {
        if (varStmt->value)
          rhs = &varStmt->value->value;
      }
      if (!rhs)
        continue;
      // Only trace into match/if/block expressions whose arm results
      // transfer ownership into the let binding.
      if (std::get_if<ast::ExprMatch>(& rhs->kind) ||
          std::get_if<ast::ExprIf>(& rhs->kind) ||
          std::get_if<ast::ExprIfLet>(& rhs->kind) ||
          std::get_if<ast::ExprBlock>(& rhs->kind)) {
        collectExcludeVars(*rhs, out, depth);
      }
    }
  };
  collectExcludeVars = [&collectExcludeVars, &collectExcludeVarsFromBlock](
      const ast::Expr &expr, ExcludeSet &out, size_t depth) {
    if (auto *identExpr = std::get_if<ast::ExprIdentifier>(&expr.kind)) {
      out.insert({identExpr->name, depth});
    } else if (auto *si = std::get_if<ast::ExprStructInit>(&expr.kind)) {
      for (const auto &[fieldName, fieldVal] : si->fields) {
        if (auto *id = std::get_if<ast::ExprIdentifier>(&fieldVal->value.kind))
          out.insert({id->name, depth});
      }
    } else if (auto *ifE = std::get_if<ast::ExprIf>(&expr.kind)) {
      // ExprIf doesn't push scopes — branches stay at same depth
      if (ifE->then_block)
        collectExcludeVars(ifE->then_block->value, out, depth);
      if (ifE->else_block && *ifE->else_block)
        collectExcludeVars((*ifE->else_block)->value, out, depth);
    } else if (auto *ifLet = std::get_if<ast::ExprIfLet>(&expr.kind)) {
      // ExprIfLet bodies are blocks → generateBlock pushes scope
      collectExcludeVarsFromBlock(ifLet->body, out, depth + 1, true);
      if (ifLet->else_body)
        collectExcludeVarsFromBlock(*ifLet->else_body, out, depth + 1, true);
    } else if (auto *matchE = std::get_if<ast::ExprMatch>(&expr.kind)) {
      // ExprMatch arms don't push scopes — stay at same depth
      for (const auto &arm : matchE->arms) {
        if (arm.body)
          collectExcludeVars(arm.body->value, out, depth);
      }
    } else if (auto *blockE = std::get_if<ast::ExprBlock>(&expr.kind)) {
      // ExprBlock → generateBlock pushes scope
      collectExcludeVarsFromBlock(blockE->block, out, depth + 1, true);
    } else if (auto *tupleE = std::get_if<ast::ExprTuple>(&expr.kind)) {
      for (const auto &elem : tupleE->elements)
        collectExcludeVars(elem->value, out, depth);
    } else if (auto *unsafeE = std::get_if<ast::ExprUnsafe>(&expr.kind)) {
      // ExprUnsafe wraps a Block — descend like ExprBlock
      collectExcludeVarsFromBlock(unsafeE->block, out, depth + 1, true);
    } else if (auto *callE = std::get_if<ast::ExprCall>(&expr.kind)) {
      // Only descend into enum variant constructors (Ok, Some, Err, etc.)
      // where argument ownership transfers to the return value.  Regular
      // function calls borrow arguments — the return value is independent.
      //
      // Enum variants are simple uppercase identifiers without :: path
      // separators.  Qualified paths like Vec::new, Node::lookup, and
      // generated names like Metric_from_yaml are NOT constructors.
      bool isVariantCtor = false;
      if (auto *id = std::get_if<ast::ExprIdentifier>(&callE->function->value.kind)) {
        const auto &name = id->name;
        if (!name.empty() && name.find("::") == std::string::npos &&
            name.find('_') == std::string::npos &&
            std::isupper(static_cast<unsigned char>(name[0])))
          isVariantCtor = true;
      }
      if (isVariantCtor) {
        for (const auto &arg : callE->args)
          collectExcludeVars(ast::callArgExpr(arg).value, out, depth);
      }
    }
  };
  // Determine whether the function body produces a value (non-void return).
  // When producesValue is false, the last statement's result is discarded,
  // so variables in it should NOT be excluded from drops.
  bool fnProducesValue = false;
  if (fn.return_type) {
    // Check if return type is unit/void — if not, the body produces a value.
    if (auto *named = std::get_if<ast::TypeNamed>(&fn.return_type->value.kind)) {
      fnProducesValue = (named->name != "()" && named->name != "unit" &&
                         named->name != "void" && named->name != "Never");
    } else {
      fnProducesValue = true; // Tuple, generic, etc. — non-void
    }
  }
  collectExcludeVarsFromBlock(fn.body, funcLevelDropExcludeVars, 0, fnProducesValue);

  // Build a flat (depth-independent) set of variable names appearing in the
  // return expression.  Used by RAII close exclusion where the depth-based
  // set doesn't work (enricher-wrapped unsafe blocks shift depths).
  funcLevelReturnVarNames.clear();
  for (const auto &[name, depth] : funcLevelDropExcludeVars)
    funcLevelReturnVarNames.insert(name);
  resolveFunctionDropExclusionCandidates();

  // Build a separate set of vars referenced ONLY by explicit return
  // statements. Used for path-specific drops when returnSlotIsLazy:
  // the early-return path excludes these vars, the normal path does not.
  funcLevelEarlyReturnVarNames.clear();
  bool hasNestedReturn = false;
  std::function<void(const ast::Block &)> scanReturns;
  std::function<void(const ast::StmtIf &)> scanReturnsIf;
  scanReturnsIf = [&scanReturns, &scanReturnsIf](const ast::StmtIf &ifStmt) {
    scanReturns(ifStmt.then_block);
    if (ifStmt.else_block) {
      if (ifStmt.else_block->block)
        scanReturns(*ifStmt.else_block->block);
      if (ifStmt.else_block->if_stmt) {
        auto &nested = ifStmt.else_block->if_stmt->value;
        if (auto *nestedIf = std::get_if<ast::StmtIf>(&nested.kind))
          scanReturnsIf(*nestedIf);
      }
    }
  };
  scanReturns = [&scanReturns, &scanReturnsIf, &collectExcludeVars,
                 &hasNestedReturn, this](const ast::Block &blk) {
    for (const auto &stmt : blk.stmts) {
      if (auto *retStmt = std::get_if<ast::StmtReturn>(&stmt->value.kind)) {
        hasNestedReturn = true;
        if (retStmt->value) {
          ExcludeSet tmp;
          collectExcludeVars(retStmt->value->value, tmp, 0);
          for (const auto &[name, d] : tmp)
            funcLevelEarlyReturnVarNames.insert(name);
        }
      } else if (auto *ifStmt = std::get_if<ast::StmtIf>(&stmt->value.kind)) {
        scanReturnsIf(*ifStmt);
      } else if (auto *forStmt = std::get_if<ast::StmtFor>(&stmt->value.kind)) {
        scanReturns(forStmt->body);
      } else if (auto *whileStmt = std::get_if<ast::StmtWhile>(&stmt->value.kind)) {
        scanReturns(whileStmt->body);
      }
    }
  };
  scanReturns(fn.body);

  // NOTE: param drops are not yet registered here.  Adding them requires
  // null-after-move tracking to avoid double-frees when a param is consumed
  // by match destructuring, callee move, or return.  See RAII Phase 1 plan.

  // If the pre-scan found return statements in nested scopes (if/for/while)
  // and the function returns an aggregate type that didn't get an eager
  // returnSlot, create the slot now so useReturnGuards activates in
  // generateBlock. Without this, statements after the early return would
  // execute unconditionally (creating and leaking temporaries).
  if (hasNestedReturn && returnFlag && !returnSlot) {
    ensureReturnSlot(location);
  }

  // Generate the function body
  mlir::Value bodyValue = generateBlock(fn.body);
  funcLevelDropExcludeVars.clear();
  funcLevelReturnVarNames.clear();
  funcLevelEarlyReturnVarNames.clear();

  // Infer return type from body if not explicitly annotated (skip for
  // implicit main — we handle its return separately below).
  if (!isImplicitMainReturn && resultTypes.empty() && bodyValue && bodyValue.getType() &&
      !llvm::isa<mlir::NoneType>(bodyValue.getType())) {
    resultTypes.push_back(bodyValue.getType());
    auto newFuncType = builder.getFunctionType(paramTypes, resultTypes);
    funcOp.setFunctionType(newFuncType);
  }

  // Handle return: check whether early return was taken or use body value
  auto *currentBlock = builder.getInsertionBlock();
  if (currentBlock &&
      (currentBlock->empty() || !currentBlock->back().hasTrait<mlir::OpTrait::IsTerminator>())) {
    if (returnFlag && returnSlot && !resultTypes.empty()) {
      // Select between returnSlot (early return) and bodyValue (normal flow).
      // Drops were already emitted path-specifically in popDropScope.
      auto flagVal =
          mlir::memref::LoadOp::create(builder, location, returnFlag, mlir::ValueRange{});

      auto selectOp = mlir::scf::IfOp::create(builder, location, resultTypes[0], flagVal,
                                              /*withElseRegion=*/true);

      // Then (early return was taken): yield slot value
      builder.setInsertionPointToStart(&selectOp.getThenRegion().front());
      auto slotVal = mlir::memref::LoadOp::create(builder, location, returnSlot, mlir::ValueRange{})
                         .getResult();
      slotVal = coerceTypeForSink(slotVal, resultTypes[0], location);
      mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{slotVal});

      // Else (normal flow): yield body value
      builder.setInsertionPointToStart(&selectOp.getElseRegion().front());
      mlir::Value normalValue = bodyValue;
      if (!normalValue) {
        normalValue = createDefaultValue(builder, location, resultTypes[0]);
      }
      normalValue = coerceTypeForSink(normalValue, resultTypes[0], location);
      mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{normalValue});

      builder.setInsertionPointAfter(selectOp);
      mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{selectOp.getResult(0)});
    } else if (bodyValue && !resultTypes.empty()) {
      // Emit drops before return, excluding the exact bindings whose
      // ownership was transferred into the function result.
      if (!funcLevelDropExcludeValues.empty())
        emitDropsExcept(funcLevelDropExcludeValues);
      else
        emitAllDrops();
      auto coercedBody = coerceTypeForSink(bodyValue, resultTypes[0], location);
      mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{coercedBody});
    } else {
      emitAllDrops();
      if (isImplicitMainReturn) {
        // Patch the function type to return i32 and emit return 0
        resultTypes.push_back(builder.getI32Type());
        funcOp.setFunctionType(builder.getFunctionType(paramTypes, resultTypes));
        auto zero = createIntConstant(builder, location, builder.getI32Type(), 0);
        mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{zero});
      } else {
        mlir::func::ReturnOp::create(builder, location);
      }
    }
  }

  // Restore previous function state
  returnFlag = prevReturnFlag;
  returnSlot = prevReturnSlot;
  returnSlotIsLazy = prevReturnSlotIsLazy;
  earlyReturnFlag = prevEarlyReturnFlag;
  channelIntOutValidAlloca = prevChannelIntOutValidAlloca;
  funcLevelDropExcludeVars = std::move(prevFuncLevelDropExcludeVars);
  funcLevelDropExcludeValues = std::move(prevFuncLevelDropExcludeValues);
  funcLevelDropExcludeResolvedNames = std::move(prevFuncLevelDropExcludeResolvedNames);
  funcLevelEarlyReturnExcludeValues = std::move(prevFuncLevelEarlyReturnExcludeValues);
  funcLevelEarlyReturnExcludeResolvedNames =
      std::move(prevFuncLevelEarlyReturnExcludeResolvedNames);
  funcLevelDropScopeBase = prevFuncLevelDropScopeBase;
  pendingFunctionParamDrops = std::move(prevPendingParamDrops);
  currentFnDefers = std::move(prevFnDefers);
  currentFunction = prevFunction;
  builder.restoreInsertionPoint(savedIP);
  return funcOp;
}

// ============================================================================
// Generator function codegen (LLVM coroutine-based)
// ============================================================================
//
// Generates a coroutine body function `fnName__coro` that uses natural control
// flow (loops, conditionals, etc.) and suspend markers at yield points.  The
// init, __next, and __done functions are generated as stubs at the MLIR level
// and then rewritten at the LLVM IR level to use LLVM coroutine intrinsics.
//
// This approach preserves loop variables across yields because LLVM's
// CoroSplit pass automatically handles frame spilling of all live values.
// ============================================================================

void MLIRGen::generateGeneratorFunction(const ast::FnDecl &fn) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i64Type = builder.getI64Type();

  // Determine the yield value type from the return type annotation
  mlir::Type yieldType = i64Type; // default to i64 (Hew int)
  if (fn.return_type) {
    auto retTy = convertType(fn.return_type->value);
    if (!llvm::isa<mlir::NoneType>(retTy))
      yieldType = retTy;
  }

  const std::string &fnName = fn.name;

  // Register as generator function and record its yield type
  generatorFunctions.insert(fnName);
  generatorYieldTypes[fnName] = yieldType;

  // Collect function parameter types
  llvm::SmallVector<mlir::Type, 4> fnParamTypes;
  for (const auto &param : fn.params) {
    fnParamTypes.push_back(convertType(param.ty.value));
  }

  // ── Generate coroutine body: <name>__coro(params..., !llvm.ptr promise) -> void
  // The promise pointer points to a single yieldType-sized slot where yield
  // values are stored.  The function body is generated naturally (with loops,
  // conditionals, etc.) and yield expressions store to the promise and call
  // the suspend marker.
  {
    auto savedIP = builder.saveInsertionPoint();
    builder.setInsertionPointToEnd(module.getBody());

    std::string coroName = fnName + "__coro";

    // Parameters: original fn params + promise ptr
    llvm::SmallVector<mlir::Type, 4> coroParamTypes(fnParamTypes);
    coroParamTypes.push_back(ptrType); // promise ptr (last param)

    auto coroFuncType = builder.getFunctionType(coroParamTypes, {});
    auto coroFunc = mlir::func::FuncOp::create(builder, location, coroName, coroFuncType);
    coroFunc.setVisibility(mlir::SymbolTable::Visibility::Private);
    auto *entryBlock = coroFunc.addEntryBlock();
    builder.setInsertionPointToStart(entryBlock);

    // Set up function context
    SymbolTableScopeT varScope(symbolTable);
    MutableTableScopeT mutScope(mutableVars);
    auto prevFunction = currentFunction;
    currentFunction = coroFunc;

    auto prevReturnFlag = returnFlag;
    auto prevReturnSlot = returnSlot;
    auto prevReturnSlotIsLazy = returnSlotIsLazy;
    auto prevEarlyReturnFlag = earlyReturnFlag;
    auto prevChannelIntOutValidAlloca = channelIntOutValidAlloca;
    auto prevFuncLevelDropExcludeVars = std::move(funcLevelDropExcludeVars);
    auto prevFuncLevelDropExcludeValues = std::move(funcLevelDropExcludeValues);
    auto prevFuncLevelDropExcludeResolvedNames = std::move(funcLevelDropExcludeResolvedNames);
    auto prevFuncLevelEarlyReturnExcludeValues = std::move(funcLevelEarlyReturnExcludeValues);
    auto prevFuncLevelEarlyReturnExcludeResolvedNames =
        std::move(funcLevelEarlyReturnExcludeResolvedNames);
    auto prevFuncLevelDropScopeBase = funcLevelDropScopeBase;
    auto prevPendingParamDrops = std::move(pendingFunctionParamDrops);
    auto prevFnDefers = std::move(currentFnDefers);
    returnFlag = nullptr;
    returnSlot = nullptr;
    returnSlotIsLazy = false;
    earlyReturnFlag = nullptr;
    channelIntOutValidAlloca = nullptr;
    funcLevelDropExcludeVars.clear();
    funcLevelDropExcludeValues.clear();
    funcLevelDropExcludeResolvedNames.clear();
    funcLevelEarlyReturnExcludeValues.clear();
    funcLevelEarlyReturnExcludeResolvedNames.clear();
    funcLevelDropScopeBase = dropScopes.size();
    currentFnDefers.clear();

    // Bind function parameters
    uint32_t paramIdx = 0;
    for (const auto &param : fn.params) {
      declareVariable(param.name, entryBlock->getArgument(paramIdx));
      ++paramIdx;
    }

    // The last parameter is the promise pointer
    auto prevCoroPromisePtr = currentCoroPromisePtr;
    currentCoroPromisePtr = entryBlock->getArgument(paramIdx);

    // Declare the suspend marker function if needed
    // __hew_coro_suspend(ptr promise) -> void
    auto suspendMarker = module.lookupSymbol<mlir::func::FuncOp>("__hew_coro_suspend");
    if (!suspendMarker) {
      auto savedIP2 = builder.saveInsertionPoint();
      builder.setInsertionPointToStart(module.getBody());
      auto suspendType = builder.getFunctionType({ptrType}, {});
      suspendMarker =
          mlir::func::FuncOp::create(builder, location, "__hew_coro_suspend", suspendType);
      suspendMarker.setPrivate();
      builder.restoreInsertionPoint(savedIP2);
    }

    // NOTE: param drops are not yet registered here.  Adding them requires
    // null-after-move tracking to avoid double-frees when a param is consumed
    // by match destructuring, callee move, or return.  See RAII Phase 1 plan.

    // Generate the function body naturally — loops, conditionals all work
    generateBlock(fn.body);

    // Ensure terminator
    auto *currentBlock = builder.getInsertionBlock();
    if (currentBlock &&
        (currentBlock->empty() || !currentBlock->back().hasTrait<mlir::OpTrait::IsTerminator>())) {
      mlir::func::ReturnOp::create(builder, location);
    }

    // Restore state
    currentCoroPromisePtr = prevCoroPromisePtr;
    returnFlag = prevReturnFlag;
    returnSlot = prevReturnSlot;
    returnSlotIsLazy = prevReturnSlotIsLazy;
    earlyReturnFlag = prevEarlyReturnFlag;
    channelIntOutValidAlloca = prevChannelIntOutValidAlloca;
    funcLevelDropExcludeVars = std::move(prevFuncLevelDropExcludeVars);
    funcLevelDropExcludeValues = std::move(prevFuncLevelDropExcludeValues);
    funcLevelDropExcludeResolvedNames = std::move(prevFuncLevelDropExcludeResolvedNames);
    funcLevelEarlyReturnExcludeValues = std::move(prevFuncLevelEarlyReturnExcludeValues);
    funcLevelEarlyReturnExcludeResolvedNames =
        std::move(prevFuncLevelEarlyReturnExcludeResolvedNames);
    funcLevelDropScopeBase = prevFuncLevelDropScopeBase;
    pendingFunctionParamDrops = std::move(prevPendingParamDrops);
    currentFnDefers = std::move(prevFnDefers);
    currentFunction = prevFunction;
    builder.restoreInsertionPoint(savedIP);
  }

  // ── Generate init function: <name>(params...) -> !llvm.ptr ─────────
  // This is a stub that will be replaced at LLVM IR level with the
  // coroutine ramp function.  For now, it calls malloc and the coro body.
  {
    auto savedIP = builder.saveInsertionPoint();
    builder.setInsertionPointToEnd(module.getBody());

    auto initFuncType = builder.getFunctionType(fnParamTypes, {ptrType});

    // Erase any forward declaration
    if (auto existing = module.lookupSymbol<mlir::func::FuncOp>(fnName)) {
      if (existing.isDeclaration())
        existing.erase();
    }

    auto initFunc = mlir::func::FuncOp::create(builder, location, fnName, initFuncType);
    auto *entryBlock = initFunc.addEntryBlock();
    builder.setInsertionPointToStart(entryBlock);

    // The init function is a placeholder — at LLVM IR level it becomes the
    // coroutine ramp.  For now we just return a null pointer; the LLVM IR
    // transformation will replace this entire function body.
    auto nullPtr = mlir::LLVM::ZeroOp::create(builder, location, ptrType);
    mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{nullPtr});

    builder.restoreInsertionPoint(savedIP);
  }

  // ── Generate next function: <name>__next(!llvm.ptr) -> <yieldType> ─
  // Stub — replaced at LLVM IR level with coro.resume + promise read.
  {
    auto savedIP = builder.saveInsertionPoint();
    builder.setInsertionPointToEnd(module.getBody());

    std::string nextName = fnName + "__next";
    auto nextFuncType = builder.getFunctionType({ptrType}, {yieldType});
    auto nextFunc = mlir::func::FuncOp::create(builder, location, nextName, nextFuncType);
    auto *entryBlock = nextFunc.addEntryBlock();
    builder.setInsertionPointToStart(entryBlock);

    // Stub return — will be replaced at LLVM IR level
    mlir::Value defaultVal;
    if (llvm::isa<mlir::IntegerType>(yieldType)) {
      defaultVal = mlir::arith::ConstantIntOp::create(builder, location, yieldType, 0);
    } else if (llvm::isa<mlir::FloatType>(yieldType)) {
      defaultVal =
          mlir::arith::ConstantOp::create(builder, location, builder.getFloatAttr(yieldType, 0.0));
    } else {
      defaultVal = mlir::LLVM::ZeroOp::create(builder, location, yieldType);
    }
    mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{defaultVal});

    builder.restoreInsertionPoint(savedIP);
  }

  // ── Generate done function: <name>__done(!llvm.ptr) -> i1 ──────────
  // Stub — replaced at LLVM IR level with coro.done.
  {
    auto savedIP = builder.saveInsertionPoint();
    builder.setInsertionPointToEnd(module.getBody());

    std::string doneName = fnName + "__done";
    auto i1Type = builder.getI1Type();
    auto doneFuncType = builder.getFunctionType({ptrType}, {i1Type});
    auto doneFunc = mlir::func::FuncOp::create(builder, location, doneName, doneFuncType);
    auto *entryBlock = doneFunc.addEntryBlock();
    builder.setInsertionPointToStart(entryBlock);

    // Stub return false — will be replaced at LLVM IR level
    auto falseVal = mlir::arith::ConstantIntOp::create(builder, location, i1Type, 0);
    mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{falseVal});

    builder.restoreInsertionPoint(savedIP);
  }
}

// ============================================================================
// Generics monomorphization
// ============================================================================

std::string MLIRGen::mangleGenericName(const std::string &baseName,
                                       const std::vector<std::string> &typeArgs) {
  std::string mangled = baseName + "$";
  for (size_t i = 0; i < typeArgs.size(); ++i) {
    if (i > 0)
      mangled += "_";
    mangled += typeArgs[i];
  }
  return mangled;
}

mlir::func::FuncOp MLIRGen::specializeGenericFunction(const std::string &baseName,
                                                      const std::vector<std::string> &typeArgs) {
  auto mangled = mangleGenericName(baseName, typeArgs);

  // Already specialized?
  if (specializedFunctions.count(mangled))
    return module.lookupSymbol<mlir::func::FuncOp>(mangled);

  auto it = genericFunctions.find(baseName);
  if (it == genericFunctions.end()) {
    emitError(builder.getUnknownLoc()) << "unknown generic function '" << baseName << "'";
    return nullptr;
  }

  const ast::FnDecl *fn = it->second;
  if (!fn->type_params || fn->type_params->empty()) {
    emitError(builder.getUnknownLoc())
        << "generic function '" << baseName << "' has no type params";
    return nullptr;
  }
  const auto &params = *fn->type_params;

  if (typeArgs.size() != params.size()) {
    emitError(builder.getUnknownLoc())
        << "generic function '" << baseName << "' expects " << params.size()
        << " type arguments, got " << typeArgs.size();
    return nullptr;
  }

  // Set up type parameter substitutions
  auto prevSubstitutions = std::move(typeParamSubstitutions);
  typeParamSubstitutions.clear();
  for (size_t i = 0; i < params.size(); ++i)
    typeParamSubstitutions[params[i].name] = typeArgs[i];

  // Generate the specialized function with the mangled name
  auto funcOp = generateFunction(*fn, mangled);

  // Restore previous substitutions
  typeParamSubstitutions = std::move(prevSubstitutions);
  specializedFunctions.insert(mangled);
  return funcOp;
}

mlir::func::FuncOp MLIRGen::specializeGenericImplMethod(
    const std::string &baseTypeName,
    const std::vector<std::string> &typeArgs,
    const std::string &methodName) {
  auto implIt = genericImplMethods.find(baseTypeName);
  if (implIt == genericImplMethods.end())
    return nullptr;

  const auto &implInfo = implIt->second;

  // Build the monomorphized type name (e.g. "Box_int")
  std::string mangledTypeName = baseTypeName;
  for (const auto &ta : typeArgs)
    mangledTypeName += "_" + ta;

  std::string mangledMethod = mangleName(currentModulePath, mangledTypeName, methodName);

  // Already specialized?
  if (specializedFunctions.count(mangledMethod))
    return module.lookupSymbol<mlir::func::FuncOp>(mangledMethod);

  // Find the method in the generic impl
  const ast::FnDecl *fn = nullptr;
  for (const auto *m : implInfo.methods) {
    if (m->name == methodName) {
      fn = m;
      break;
    }
  }
  if (!fn) {
    emitError(builder.getUnknownLoc())
        << "no method '" << methodName << "' in generic impl for '" << baseTypeName << "'";
    return nullptr;
  }

  if (typeArgs.size() != implInfo.typeParams->size()) {
    emitError(builder.getUnknownLoc())
        << "generic impl for '" << baseTypeName << "' expects "
        << implInfo.typeParams->size() << " type arguments, got " << typeArgs.size();
    return nullptr;
  }

  // Set up type parameter substitutions (T→int, U→string, …)
  auto prevSubstitutions = std::move(typeParamSubstitutions);
  typeParamSubstitutions.clear();
  for (size_t i = 0; i < implInfo.typeParams->size(); ++i)
    typeParamSubstitutions[(*implInfo.typeParams)[i].name] = typeArgs[i];
  // Also map Self → monomorphized type name
  typeParamSubstitutions["Self"] = mangledTypeName;

  auto funcOp = generateFunction(*fn, mangledMethod);

  typeParamSubstitutions = std::move(prevSubstitutions);
  specializedFunctions.insert(mangledMethod);
  return funcOp;
}

std::string MLIRGen::resolveTypeArgMangledName(const ast::TypeExpr &type) {
  if (auto *named = std::get_if<ast::TypeNamed>(&type.kind)) {
    // Resolve through active substitutions first (e.g., T → int)
    std::string baseName = named->name;
    auto substIt = typeParamSubstitutions.find(baseName);
    if (substIt != typeParamSubstitutions.end())
      baseName = substIt->second;

    // If the type has nested type args (e.g., Pair<int> or Vec<T>),
    // recursively resolve each and build a compound mangled name.
    if (named->type_args && !named->type_args->empty()) {
      std::string mangled = baseName;
      for (const auto &ta : *named->type_args)
        mangled += "_" + resolveTypeArgMangledName(ta.value);
      // Ensure the nested generic struct is actually specialized by
      // calling convertType, which triggers monomorphization.
      (void)convertType(type);
      return mangled;
    }
    return baseName;
  }
  return "unknown";
}

// ── Drop tracking (RAII) ─────────────────────────────────────────

void MLIRGen::pushDropScope() {
  dropScopes.emplace_back();
}

mlir::Value MLIRGen::resolveCurrentBindingIdentity(llvm::StringRef name) {
  if (auto slot = getMutableVarSlot(name))
    return slot;
  return symbolTable.lookup(name);
}

void MLIRGen::maybeRecordFunctionDropExclusion(const std::string &varName,
                                               mlir::Value bindingIdentity) {
  if (!bindingIdentity || dropScopes.size() <= funcLevelDropScopeBase)
    return;
  size_t relDepth = dropScopes.size() - 1 - funcLevelDropScopeBase;
  if (funcLevelDropExcludeVars.count({varName, relDepth})) {
    funcLevelDropExcludeValues.insert(bindingIdentity);
    funcLevelDropExcludeResolvedNames.insert(varName);
  }
}

void MLIRGen::resolveFunctionDropExclusionCandidates() {
  if (dropScopes.size() <= funcLevelDropScopeBase)
    return;
  for (size_t i = funcLevelDropScopeBase; i < dropScopes.size(); ++i) {
    size_t relDepth = i - funcLevelDropScopeBase;
    for (const auto &entry : dropScopes[i]) {
      if (!entry.bindingIdentity)
        continue;
      if (funcLevelDropExcludeVars.count({entry.varName, relDepth})) {
        funcLevelDropExcludeValues.insert(entry.bindingIdentity);
        funcLevelDropExcludeResolvedNames.insert(entry.varName);
      }
    }
  }
}

bool MLIRGen::isFunctionDropExcluded(const DropEntry &entry,
                                     const DropValueSet &excludeValues) const {
  return entry.bindingIdentity && excludeValues.count(entry.bindingIdentity) != 0;
}

void MLIRGen::collectVisibleBindingIdentities(const ast::Expr &expr, DropValueSet &out,
                                              std::set<std::string> *resolvedNames) {
  if (auto *id = std::get_if<ast::ExprIdentifier>(&expr.kind)) {
    if (auto bindingIdentity = resolveCurrentBindingIdentity(id->name)) {
      out.insert(bindingIdentity);
      if (resolvedNames)
        resolvedNames->insert(id->name);
    }
    return;
  }
  if (auto *si = std::get_if<ast::ExprStructInit>(&expr.kind)) {
    for (const auto &[fieldName, fieldVal] : si->fields) {
      if (auto *id = std::get_if<ast::ExprIdentifier>(&fieldVal->value.kind)) {
        if (auto bindingIdentity = resolveCurrentBindingIdentity(id->name)) {
          out.insert(bindingIdentity);
          if (resolvedNames)
            resolvedNames->insert(id->name);
        }
      }
    }
    return;
  }
  if (auto *te = std::get_if<ast::ExprTuple>(&expr.kind)) {
    for (const auto &elem : te->elements) {
      if (auto *id = std::get_if<ast::ExprIdentifier>(&elem->value.kind)) {
        if (auto bindingIdentity = resolveCurrentBindingIdentity(id->name)) {
          out.insert(bindingIdentity);
          if (resolvedNames)
            resolvedNames->insert(id->name);
        }
      }
    }
  }
}

void MLIRGen::emitDropsWithExclusion(const std::vector<DropEntry> &scope, size_t relDepth) {
  for (auto it = scope.rbegin(); it != scope.rend(); ++it) {
    if (isFunctionDropExcluded(*it, funcLevelDropExcludeValues))
      continue;
    if (!funcLevelDropExcludeResolvedNames.count(it->varName)) {
      if (it->closeAlloca) {
        if (funcLevelReturnVarNames.count(it->varName))
          continue;
      } else if (funcLevelDropExcludeVars.count({it->varName, relDepth})) {
        continue;
      }
    }
    emitDropEntry(*it);
  }
}

void MLIRGen::popDropScope() {
  if (dropScopes.empty())
    return;
  if (builder.getInsertionBlock()) {
    auto *block = builder.getInsertionBlock();
    auto *parentOp = block->getParentOp();
    bool isFuncLevel = mlir::isa<mlir::func::FuncOp>(parentOp);

    // Scope depth relative to the current function body. Legacy name-based
    // fallback still uses this for entries that predate stable identity
    // capture; resolved function-level exclusions key off bindingIdentity.
    size_t relDepth = (dropScopes.size() > funcLevelDropScopeBase)
                          ? dropScopes.size() - 1 - funcLevelDropScopeBase
                          : 0;

    if (isFuncLevel) {
      // Function-level scope: emit drops here while the symbol table is
      // still alive (the DropScopeGuard destructor runs before the
      // SymbolTableScope destructor in generateBlock).
      //
      // When returnSlot was lazily created (aggregate return types with
      // nested returns), emit PATH-SPECIFIC drops using an scf.if on
      // earlyReturnFlag. The early-return path excludes return-expression
      // vars; the normal-flow path excludes trailing-expression vars.
      if (returnSlotIsLazy && !hasRealTerminator(block)) {
        emitDeferredCalls();
        auto &scope = dropScopes.back();
        auto loc = builder.getUnknownLoc();
        auto flagVal =
            mlir::memref::LoadOp::create(builder, loc, earlyReturnFlag, mlir::ValueRange{});
        auto guard = mlir::scf::IfOp::create(builder, loc, mlir::TypeRange{}, flagVal,
                                             /*withElseRegion=*/true);

        // Then (early return): drop everything except return-expr vars.
        builder.setInsertionPointToStart(&guard.getThenRegion().front());
        for (auto it = scope.rbegin(); it != scope.rend(); ++it) {
          if (isFunctionDropExcluded(*it, funcLevelEarlyReturnExcludeValues))
            continue;
          if (!funcLevelEarlyReturnExcludeResolvedNames.count(it->varName) &&
              funcLevelEarlyReturnVarNames.count(it->varName))
            continue;
          emitDropEntry(*it);
        }

        // Else (normal flow): drop everything except trailing-expr vars.
        builder.setInsertionPointToStart(&guard.getElseRegion().front());
        for (auto it = scope.rbegin(); it != scope.rend(); ++it) {
          if (isFunctionDropExcluded(*it, funcLevelDropExcludeValues))
            continue;
          if (!funcLevelDropExcludeResolvedNames.count(it->varName)) {
            if (it->closeAlloca) {
              if (funcLevelReturnVarNames.count(it->varName))
                continue;
            } else if (funcLevelDropExcludeVars.count({it->varName, relDepth})) {
              continue;
            }
          }
          emitDropEntry(*it);
        }

        builder.setInsertionPointAfter(guard);
        dropScopes.pop_back();
        return;
      }
      //
      // Drops are emitted UNCONDITIONALLY (no !returnFlag guard).  Early
      // returns inside SCF regions only store to returnSlot / set returnFlag
      // — they do NOT emit their own drops.  The trailing-expression path
      // also sets returnFlag.  In both cases the function-level drops here
      // are the sole point of resource cleanup.  funcLevelDropExcludeVars
      // already excludes the variable whose value is being returned, so
      // there is no double-free risk.
      //
      // Top-level early returns (directly in FuncOp) call emitAllDrops()
      // and emit func.return, producing a terminator — hasRealTerminator()
      // will be true and this block is skipped entirely.
      if (!hasRealTerminator(block)) {
        emitDeferredCalls();
        emitDropsWithExclusion(dropScopes.back(), relDepth);
      }
      dropScopes.pop_back();
      return;
    }

    if (!hasRealTerminator(block)) {
      // Inner scope: emit drops, guarded by !returnFlag when applicable.
      // If the block ends with an auto-inserted scf.yield (no operands),
      // insert drops before it so they execute before the yield.
      if (!block->empty()) {
        if (auto yieldOp = mlir::dyn_cast<mlir::scf::YieldOp>(block->back())) {
          if (yieldOp.getNumOperands() == 0)
            builder.setInsertionPoint(yieldOp);
        }
      }

      if (returnFlag) {
        auto loc = builder.getUnknownLoc();
        auto flagVal = mlir::memref::LoadOp::create(builder, loc, returnFlag, mlir::ValueRange{});
        auto trueConst = createIntConstant(builder, loc, builder.getI1Type(), 1);
        auto notReturned = mlir::arith::XOrIOp::create(builder, loc, flagVal, trueConst);
        auto guard = mlir::scf::IfOp::create(builder, loc, mlir::TypeRange{}, notReturned,
                                             /*withElseRegion=*/false);
        builder.setInsertionPointToStart(&guard.getThenRegion().front());
        emitDropsWithExclusion(dropScopes.back(), relDepth);
        // scf.if auto-adds yield; set insertion after guard
        builder.setInsertionPointAfter(guard);
      } else {
        emitDropsWithExclusion(dropScopes.back(), relDepth);
      }
    }
  }
  dropScopes.pop_back();
}

void MLIRGen::registerDroppable(const std::string &varName, const std::string &dropFunc,
                                bool isUserDrop) {
  if (!dropScopes.empty()) {
    DropEntry entry;
    entry.varName = varName;
    entry.dropFuncName = dropFunc;
    entry.isUserDrop = isUserDrop;
    // Capture the promoted alloca (if any) so the drop works even after
    // the declaring scope's MutableTableScopeT has been popped (e.g. when
    // a let-binding or materialized temp is inside a match arm whose
    // symbol table scope ends before function-level drops fire).
    entry.promotedSlot = getMutableVarSlot(varName);
    entry.bindingIdentity =
        entry.promotedSlot ? entry.promotedSlot : resolveCurrentBindingIdentity(varName);
    maybeRecordFunctionDropExclusion(varName, entry.bindingIdentity);
    dropScopes.back().push_back(std::move(entry));
  }
}

void MLIRGen::unregisterDroppable(const std::string &varName) {
  for (auto &scope : dropScopes) {
    scope.erase(std::remove_if(scope.begin(), scope.end(),
                               [&](const DropEntry &e) { return e.varName == varName; }),
                scope.end());
  }
}

std::string MLIRGen::dropFuncForType(const ast::TypeExpr &ty) const {
  // NOTE: This operates on AST type annotations only.  Closure types are
  // detected by runtime type (hew::ClosureType) in MLIRGenStmt.cpp, not by
  // annotation, so they cannot be handled here.  When activating param drops,
  // closure params need separate handling via mlir::isa<hew::ClosureType>.
  auto *named = std::get_if<ast::TypeNamed>(&ty.kind);
  if (!named)
    return "";
  auto typeName = resolveTypeAlias(named->name);
  if (typeName == "Vec" || typeName == "bytes")
    return "hew_vec_free";
  if (typeName == "HashMap")
    return "hew_hashmap_free_impl";
  if (typeName == "HashSet")
    return "hew_hashset_free";
  if (typeName == "String" || typeName == "string" || typeName == "str")
    return "hew_string_drop";
  auto dropIt = userDropFuncs.find(typeName);
  if (dropIt != userDropFuncs.end())
    return dropIt->second;
  return "";
}

std::string MLIRGen::dropFuncForMLIRType(mlir::Type type) const {
  if (mlir::isa<hew::StringRefType>(type))
    return "hew_string_drop";
  if (mlir::isa<hew::VecType>(type))
    return "hew_vec_free";
  if (mlir::isa<hew::HashMapType>(type))
    return "hew_hashmap_free_impl";
  if (mlir::isa<hew::ClosureType>(type))
    return "hew_rc_drop";
  if (auto structTy = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(type)) {
    if (structTy.isIdentified()) {
      // HashSet uses identified struct "HewHashSet"
      if (structTy.getName() == "HewHashSet")
        return "hew_hashset_free";
      auto it = userDropFuncs.find(structTy.getName().str());
      if (it != userDropFuncs.end())
        return it->second;
      // Structs without user Drop but with owned fields (String, Vec, etc.)
      // get a sentinel drop that emitDropEntry handles by freeing fields.
      if (structHasOwnedFields(structTy.getName().str()))
        return "__auto_field_drop";
    }
  }
  return "";
}

bool MLIRGen::structHasOwnedFields(const std::string &name) const {
  auto it = structTypes.find(name);
  if (it == structTypes.end())
    return false;
  for (const auto &field : it->second.fields) {
    // Recursive check: use a simplified test — only check Hew-level heap types,
    // not nested structs (those are handled by recursive field drops).
    if (mlir::isa<hew::StringRefType>(field.semanticType) ||
        mlir::isa<hew::VecType>(field.semanticType) ||
        mlir::isa<hew::HashMapType>(field.semanticType) ||
        mlir::isa<hew::ClosureType>(field.semanticType))
      return true;
    if (auto fst = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(field.semanticType)) {
      if (fst.isIdentified()) {
        if (fst.getName() == "HewHashSet")
          return true;
        if (userDropFuncs.count(fst.getName().str()))
          return true;
        if (structHasOwnedFields(fst.getName().str()))
          return true;
      }
    }
  }
  return false;
}

void MLIRGen::emitDropEntry(const DropEntry &entry) {
  // Stream/Sink RAII: load from alloca, null-check, call close, null out.
  // This prevents double-free if .close() was called explicitly (which
  // nulls the same alloca).
  if (entry.closeAlloca) {
    auto loc = builder.getUnknownLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
    auto ptr = mlir::memref::LoadOp::create(builder, loc, entry.closeAlloca, mlir::ValueRange{});
    auto nullPtr = mlir::LLVM::ZeroOp::create(builder, loc, ptrType);
    auto i1Type = builder.getI1Type();
    auto isNotNull = mlir::LLVM::ICmpOp::create(builder, loc, i1Type,
                                                  mlir::LLVM::ICmpPredicate::ne, ptr, nullPtr);
    auto guard = mlir::scf::IfOp::create(builder, loc, mlir::TypeRange{}, isNotNull,
                                         /*withElseRegion=*/false);
    builder.setInsertionPointToStart(&guard.getThenRegion().front());
    hew::DropOp::create(builder, loc, ptr, entry.dropFuncName, false);
    mlir::memref::StoreOp::create(builder, loc, nullPtr, entry.closeAlloca, mlir::ValueRange{});
    builder.setInsertionPointAfter(guard);
    return;
  }
  // When a promoted alloca was captured at registration time, load from it
  // directly.  This is necessary because the symbol table scope that held
  // the variable entry may have been popped (e.g. match arm scope exits
  // before function-level drops fire).
  mlir::Value val;
  auto loc = builder.getUnknownLoc();
  if (entry.promotedSlot) {
    val = mlir::memref::LoadOp::create(builder, loc, entry.promotedSlot).getResult();
    if (auto semIt = slotSemanticTypes.find(entry.promotedSlot); semIt != slotSemanticTypes.end())
      val = coerceType(val, semIt->second, loc);
  } else {
    val = lookupVariable(entry.varName);
  }
  if (!val)
    return;

  // Auto-field-drop: struct has no user Drop but has owned fields (String,
  // Vec, etc.).  Just drop the fields directly — no function call needed.
  if (entry.dropFuncName == "__auto_field_drop") {
    emitFieldDropsForUserStruct(val, loc);
    return;
  }

  auto ptrType = mlir::LLVM::LLVMPointerType::get(builder.getContext());
  mlir::Value dropVal = val;
  if (mlir::isa<hew::ClosureType>(val.getType()))
    dropVal = hew::ClosureGetEnvOp::create(builder, loc, ptrType, val);
  if (!mlir::isa<mlir::LLVM::LLVMPointerType>(dropVal.getType()) && !entry.isUserDrop)
    dropVal = hew::BitcastOp::create(builder, loc, ptrType, dropVal);

  // Null-guard: skip the drop if the value is a null pointer.  This handles
  // zero-initialized allocas (early return before the let-binding) and will
  // support null-after-move once ownership transfer tracking is added.
  if (mlir::isa<mlir::LLVM::LLVMPointerType>(dropVal.getType())) {
    auto nullPtr = mlir::LLVM::ZeroOp::create(builder, loc, ptrType);
    auto isNotNull = mlir::LLVM::ICmpOp::create(
        builder, loc, builder.getI1Type(), mlir::LLVM::ICmpPredicate::ne, dropVal, nullPtr);
    auto guard = mlir::scf::IfOp::create(builder, loc, mlir::TypeRange{}, isNotNull,
                                         /*withElseRegion=*/false);
    builder.setInsertionPointToStart(&guard.getThenRegion().front());
    hew::DropOp::create(builder, loc, dropVal, entry.dropFuncName, entry.isUserDrop);
    // Null out the promoted slot after dropping to prevent double-free
    // when the drop scope fires again (e.g. while loop body re-entry
    // where the alloca still holds the previous iteration's freed pointer).
    if (entry.promotedSlot) {
      auto zero = mlir::LLVM::ZeroOp::create(builder, loc, ptrType);
      mlir::memref::StoreOp::create(builder, loc, zero, entry.promotedSlot);
    }
    builder.setInsertionPointAfter(guard);
    return;
  }
  // Null-guard for user-defined drops on struct types: extract the first
  // pointer field and skip the drop if it's null.  For scalar-only structs
  // (no pointer fields), compare the entire struct with zeroinitializer.
  // This handles zero-initialized struct allocas (variable never assigned
  // because the code path was skipped by return guards).
  if (entry.isUserDrop) {
    if (auto structTy = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(dropVal.getType())) {
      auto bodyTypes = structTy.getBody();
      bool guardEmitted = false;
      // First try: find a pointer field to null-check.
      for (unsigned i = 0; i < bodyTypes.size(); ++i) {
        if (mlir::isa<mlir::LLVM::LLVMPointerType>(bodyTypes[i])) {
          auto fieldVal = mlir::LLVM::ExtractValueOp::create(builder, loc, bodyTypes[i], dropVal, i);
          auto nullPtr = mlir::LLVM::ZeroOp::create(builder, loc, ptrType);
          auto isNotNull = mlir::LLVM::ICmpOp::create(
              builder, loc, builder.getI1Type(), mlir::LLVM::ICmpPredicate::ne, fieldVal, nullPtr);
          auto guard = mlir::scf::IfOp::create(builder, loc, mlir::TypeRange{}, isNotNull,
                                               /*withElseRegion=*/false);
          builder.setInsertionPointToStart(&guard.getThenRegion().front());
          hew::DropOp::create(builder, loc, dropVal, entry.dropFuncName, entry.isUserDrop);
          builder.setInsertionPointAfter(guard);
          guardEmitted = true;
          break;
        }
      }
      if (guardEmitted)
        return;
    }
  }
   hew::DropOp::create(builder, loc, dropVal, entry.dropFuncName, entry.isUserDrop);
  if (entry.isUserDrop)
    emitFieldDropsForUserStruct(val, loc);
}

void MLIRGen::emitFieldDropsForUserStruct(mlir::Value structVal, mlir::Location loc) {
  auto structTy = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(structVal.getType());
  if (!structTy || !structTy.isIdentified())
    return;
  auto it = structTypes.find(structTy.getName().str());
  if (it == structTypes.end())
    return;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  for (const auto &field : it->second.fields) {
    auto drop = dropFuncForMLIRType(field.semanticType);
    if (drop.empty())
      continue;
    auto fieldVal = hew::FieldGetOp::create(builder, loc, field.type, structVal,
                                            builder.getStringAttr(field.name),
                                            builder.getI64IntegerAttr(field.index));
    // __auto_field_drop sentinel: recurse into nested struct fields
    // instead of emitting a DropOp for the non-existent function.
    if (drop == "__auto_field_drop") {
      emitFieldDropsForUserStruct(fieldVal, loc);
      continue;
    }
    // Check if this field's drop is itself a user-defined Drop (nested struct).
    bool fieldIsUserDrop = false;
    if (auto fst = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(field.semanticType))
      if (fst.isIdentified())
        fieldIsUserDrop = userDropFuncs.count(fst.getName().str()) > 0;
    mlir::Value dropVal = fieldVal;
    if (!fieldIsUserDrop && !mlir::isa<mlir::LLVM::LLVMPointerType>(dropVal.getType()))
      dropVal = hew::BitcastOp::create(builder, loc, ptrType, dropVal);
    hew::DropOp::create(builder, loc, dropVal, drop, fieldIsUserDrop);
    // Recursively drop owned fields of nested user-drop structs.
    if (fieldIsUserDrop)
      emitFieldDropsForUserStruct(fieldVal, loc);
  }
}

void MLIRGen::nullOutRaiiAlloca(const std::string &varName) {
  for (auto scopeIt = dropScopes.rbegin(); scopeIt != dropScopes.rend(); ++scopeIt) {
    for (auto &entry : *scopeIt) {
      if (entry.varName == varName && entry.closeAlloca) {
        auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
        auto nullPtr = mlir::LLVM::ZeroOp::create(builder, builder.getUnknownLoc(), ptrType);
        mlir::memref::StoreOp::create(builder, builder.getUnknownLoc(), nullPtr, entry.closeAlloca,
                                      mlir::ValueRange{});
        return;
      }
    }
  }
}

void MLIRGen::emitDropForVariable(const std::string &varName) {
  for (auto &scope : dropScopes) {
    for (auto &entry : scope) {
      if (entry.varName == varName) {
        emitDropEntry(entry);
        return;
      }
    }
  }
}

void MLIRGen::emitDropsForScope(const std::vector<DropEntry> &scope) {
  for (auto it = scope.rbegin(); it != scope.rend(); ++it)
    emitDropEntry(*it);
}

void MLIRGen::emitDropsForCurrentScope() {
  if (dropScopes.empty())
    return;
  auto &scope = dropScopes.back();
  if (scope.empty())
    return;
  // Only emit drops when directly inside a FuncOp
  auto *parentOp = builder.getInsertionBlock()->getParentOp();
  if (!mlir::isa<mlir::func::FuncOp>(parentOp))
    return;
  for (auto it = scope.rbegin(); it != scope.rend(); ++it)
    emitDropEntry(*it);
}

void MLIRGen::emitAllDrops() {
  auto *parentOp = builder.getInsertionBlock()->getParentOp();
  if (!mlir::isa<mlir::func::FuncOp>(parentOp))
    return;
  // Only iterate drop scopes belonging to the current function (from
  // funcLevelDropScopeBase onwards).  Without this limit, nested function
  // generation (e.g. generic specialization) would emit drops for the
  // caller's variables, referencing allocas from a different FuncOp.
  for (size_t i = dropScopes.size(); i > funcLevelDropScopeBase; --i)
    for (auto it = dropScopes[i - 1].rbegin(); it != dropScopes[i - 1].rend(); ++it)
      emitDropEntry(*it);
}

void MLIRGen::emitStringDrop(mlir::Value v) {
  if (!v || !builder.getInsertionBlock())
    return;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(builder.getContext());
  mlir::Value dropVal = v;
  if (!mlir::isa<mlir::LLVM::LLVMPointerType>(v.getType()))
    dropVal = hew::BitcastOp::create(builder, builder.getUnknownLoc(), ptrType, v);
  auto funcType = mlir::FunctionType::get(builder.getContext(), {ptrType}, {});
  getOrCreateExternFunc("hew_string_drop", funcType);
  mlir::func::CallOp::create(builder, builder.getUnknownLoc(), "hew_string_drop", mlir::TypeRange{},
                             dropVal);
}

bool MLIRGen::isTemporaryString(mlir::Value v) {
  if (!v)
    return false;
  // Must be a string type
  if (!mlir::isa<hew::StringRefType>(v.getType()) &&
      !mlir::isa<mlir::LLVM::LLVMPointerType>(v.getType()))
    return false;
  // Constants (global string refs) are NOT temporaries
  if (v.getDefiningOp<hew::ConstantOp>())
    return false;
  // Variable loads are NOT temporaries — they have their own drop scope
  if (v.getDefiningOp<mlir::memref::LoadOp>())
    return false;
  // Block arguments are NOT temporaries
  if (mlir::isa<mlir::BlockArgument>(v))
    return false;
  // Vec/HashMap .get() now returns strdup'd owned copies — treat as temporary
  // Everything else is a temporary: StringConcatOp, ToStringOp, RuntimeCallOp, etc.
  return true;
}

MLIRGen::DropInfo MLIRGen::inferDropFuncForTemporary(mlir::Value val,
                                                     const ast::Expr &astExpr) const {
  if (!val)
    return {};

  // Identifiers are already variable-bound — not temporaries.
  if (std::holds_alternative<ast::ExprIdentifier>(astExpr.kind))
    return {};

  // Non-string literals (int, float, bool, char) don't heap-allocate.
  // String literals DO heap-allocate (strdup from cstr) and need drops.
  if (auto *lit = std::get_if<ast::ExprLiteral>(&astExpr.kind)) {
    if (!std::holds_alternative<ast::LitString>(lit->lit))
      return {};
  }

  // Index access: Vec<int>[i] borrows, but Vec<String>[i] returns a strdup'd
  // copy via hew_vec_get_str.  Treat String-typed index results as owned temps.
  if (std::holds_alternative<ast::ExprIndex>(astExpr.kind)) {
    if (!mlir::isa<hew::StringRefType>(val.getType()))
      return {};
  }

  // Field accesses borrow from the receiver — not owned temporaries.
  if (std::holds_alternative<ast::ExprFieldAccess>(astExpr.kind))
    return {};

  // Value types never need drops.
  auto valType = val.getType();
  if (mlir::isa<mlir::IntegerType>(valType) || mlir::isa<mlir::FloatType>(valType) ||
      mlir::isa<mlir::IndexType>(valType))
    return {};

  // Block arguments and variable loads are NOT temporaries.
  if (mlir::isa<mlir::BlockArgument>(val))
    return {};
  if (val.getDefiningOp<mlir::memref::LoadOp>())
    return {};
  // Non-string global constants are value types — not temporaries.
  // String constants ARE heap-allocated (strdup in lowering) and need drops.
  if (val.getDefiningOp<hew::ConstantOp>()) {
    if (!mlir::isa<hew::StringRefType>(val.getType()))
      return {};
  }

  // Closures are handled by existing RC drop mechanism (emitted post-call
  // for inline lambdas, and by let-stmt registration for bound closures).
  if (mlir::isa<hew::ClosureType>(valType))
    return {};

  // LLVM literal struct types (dyn Trait fat pointers, anonymous tuples) are
  // value types at the LLVM level.  Identified (user-defined) structs fall
  // through to the resolvedTypeOf check which detects Drop implementations.
  if (auto structTy = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(valType))
    if (!structTy.isIdentified())
      return {};

  // Try the resolved AST type first — most reliable source of drop information.
  if (auto *resolvedType = resolvedTypeOf(astExpr.span)) {
    auto dropFunc = dropFuncForType(*resolvedType);
    if (!dropFunc.empty()) {
      // Check whether this is a user-defined Drop implementation.
      bool isUser = false;
      if (auto *named = std::get_if<ast::TypeNamed>(&resolvedType->kind)) {
        auto typeName = resolveTypeAlias(named->name);
        isUser = userDropFuncs.count(typeName) > 0;
      }
      return {dropFunc, isUser};
    }
  }

  // For StringRefType without a resolved type, use the isTemporaryString
  // heuristic (already filters out constants, loads, and block args above).
  if (mlir::isa<hew::StringRefType>(valType))
    return {"hew_string_drop", false};

  // Check defining op for known constructors (fallback when resolved type
  // is unavailable, e.g. from runtime calls with no AST annotation).
  if (auto *defOp = val.getDefiningOp()) {
    if (mlir::isa<hew::VecNewOp>(defOp))
      return {"hew_vec_free", false};
    if (mlir::isa<hew::HashMapNewOp>(defOp))
      return {"hew_hashmap_free_impl", false};
  }

  return {};
}

bool MLIRGen::materializeTemporary(mlir::Value val, const ast::Expr &astExpr) {
  auto info = inferDropFuncForTemporary(val, astExpr);
  if (info.dropFunc.empty())
    return false;

  // Prefix with \x00 to prevent collision with user identifiers (the lexer
  // rejects null bytes, so no user binding can shadow this name).
  std::string tmpRawName = std::string("\0__tmp_", 7) + std::to_string(tempMaterializationCounter++);
  auto tmpName = intern(tmpRawName);

  // Always create a hoisted alloca at function entry.  This ensures:
  // 1. The alloca dominates function-level drops (which fire after inner
  //    scopes like match arms have been popped).
  // 2. Zero-initialization means the null-guard in emitDropEntry safely
  //    skips the drop when the creating scope was never entered.
  // 3. registerDroppable captures the alloca in DropEntry::promotedSlot,
  //    making drops independent of symbol table lifetime.
  auto semanticType = val.getType();
  auto storageType = toSlotStorageType(semanticType);
  auto memrefType = mlir::MemRefType::get({}, storageType);

  auto savedIP = builder.saveInsertionPoint();
  auto &entryBlock = currentFunction.front();
  builder.setInsertionPointToStart(&entryBlock);
  auto alloca = mlir::memref::AllocaOp::create(builder, builder.getUnknownLoc(), memrefType);
  if (mlir::isa<mlir::LLVM::LLVMPointerType>(storageType) || isPointerLikeType(semanticType)) {
    auto zero = createDefaultValue(builder, builder.getUnknownLoc(), storageType);
    mlir::memref::StoreOp::create(builder, builder.getUnknownLoc(), zero, alloca);
  }
  builder.restoreInsertionPoint(savedIP);

  // In loops, the alloca may already hold a value from a previous
  // iteration.  Drop it before storing the new value to prevent leaks
  // from overwritten temporaries.
  if (mlir::isa<mlir::LLVM::LLVMPointerType>(storageType) || isPointerLikeType(semanticType)) {
    auto loc = builder.getUnknownLoc();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
    auto oldVal = mlir::memref::LoadOp::create(builder, loc, alloca, mlir::ValueRange{}).getResult();
    if (storageType != ptrType && isPointerLikeType(semanticType))
      oldVal = hew::BitcastOp::create(builder, loc, ptrType, oldVal);
    auto nullPtr = mlir::LLVM::ZeroOp::create(builder, loc, ptrType);
    auto isNotNull = mlir::LLVM::ICmpOp::create(
        builder, loc, builder.getI1Type(), mlir::LLVM::ICmpPredicate::ne, oldVal, nullPtr);
    auto guard = mlir::scf::IfOp::create(builder, loc, mlir::TypeRange{}, isNotNull,
                                         /*withElseRegion=*/false);
    builder.setInsertionPointToStart(&guard.getThenRegion().front());
    hew::DropOp::create(builder, loc, oldVal, info.dropFunc, false);
    builder.setInsertionPointAfter(guard);
  }

  auto stored = coerceType(val, storageType, builder.getUnknownLoc());
  mlir::memref::StoreOp::create(builder, builder.getUnknownLoc(), stored, alloca);
  if (storageType != semanticType)
    slotSemanticTypes[alloca] = semanticType;
  mutableVars.insert(tmpName, alloca);

  registerDroppable(tmpName.str(), info.dropFunc, info.isUserDrop);

  // When inside a nested scope (e.g. while-loop body), the scope-exit
  // drop may be skipped by an early return (returnFlag guards it).
  // Register a DUPLICATE drop at function level as a safety net: the
  // scope-exit drop nulls the alloca after freeing, so the function-level
  // drop is a no-op in the normal case and only fires when scope-exit
  // was skipped.
  if (dropScopes.size() > funcLevelDropScopeBase + 1) {
    DropEntry funcEntry;
    funcEntry.varName = tmpName.str();
    funcEntry.dropFuncName = info.dropFunc;
    funcEntry.isUserDrop = info.isUserDrop;
    funcEntry.promotedSlot = alloca;
    funcEntry.bindingIdentity = alloca;
    dropScopes[funcLevelDropScopeBase].push_back(std::move(funcEntry));
  }

  return true;
}

void MLIRGen::emitDropsExcept(const std::string &excludeVar) {
  std::set<std::string> excludeSet;
  excludeSet.insert(excludeVar);
  emitDropsExcept(excludeSet);
}

void MLIRGen::emitDropsExcept(const DropValueSet &excludeValues) {
  auto *parentOp = builder.getInsertionBlock()->getParentOp();
  if (!mlir::isa<mlir::func::FuncOp>(parentOp))
    return;
  for (size_t i = dropScopes.size(); i > funcLevelDropScopeBase; --i)
    for (auto it = dropScopes[i - 1].rbegin(); it != dropScopes[i - 1].rend(); ++it)
      if (!isFunctionDropExcluded(*it, excludeValues))
        emitDropEntry(*it);
}

void MLIRGen::emitDropsExcept(const std::set<std::string> &excludeVars) {
  auto *parentOp = builder.getInsertionBlock()->getParentOp();
  if (!mlir::isa<mlir::func::FuncOp>(parentOp))
    return;
  DropValueSet excludeValues;
  std::set<std::string> unresolvedNames;
  for (const auto &name : excludeVars) {
    if (auto bindingIdentity = resolveCurrentBindingIdentity(name))
      excludeValues.insert(bindingIdentity);
    else
      unresolvedNames.insert(name);
  }
  // Only iterate scopes from funcLevelDropScopeBase onwards to avoid
  // cross-function drops in nested function generation.
  for (size_t i = dropScopes.size(); i > funcLevelDropScopeBase; --i)
    for (auto it = dropScopes[i - 1].rbegin(); it != dropScopes[i - 1].rend(); ++it)
      if (!isFunctionDropExcluded(*it, excludeValues) &&
          (!unresolvedNames.count(it->varName) || it->bindingIdentity))
        emitDropEntry(*it);
}

// ── Defer execution ──────────────────────────────────────────

void MLIRGen::emitDeferredCalls() {
  // Move defers into a local before iterating. This prevents infinite
  // recursion when a deferred expression is a block: generateBlock →
  // DropScopeGuard dtor → popDropScope → emitDeferredCalls. With the
  // vector cleared first, the re-entrant call finds nothing to emit.
  auto defers = std::move(currentFnDefers);
  currentFnDefers.clear();
  // Execute in LIFO order (last registered first).
  for (auto it = defers.rbegin(); it != defers.rend(); ++it) {
    generateExpression(*it->expr);
  }
}
