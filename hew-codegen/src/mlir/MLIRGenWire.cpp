//===- MLIRGenWire.cpp - Wire type codegen for Hew MLIRGen ----------------===//
//
// Generates helper functions for wire declarations.
// Each `wire struct Foo { ... }` produces:
//   - Foo_encode(fields...) -> !llvm.ptr  (returns heap-owned wire buffer pointer)
//   - Foo_decode(!llvm.ptr, i64) -> struct  (decodes from buffer)
//   - Foo_to_json(fields...) -> !llvm.ptr  (returns malloc'd JSON string)
//   - Foo_from_json(!llvm.ptr) -> struct   (parses JSON string)
//   - Foo_to_yaml(fields...) -> !llvm.ptr  (returns malloc'd YAML string)
//   - Foo_from_yaml(!llvm.ptr) -> struct   (parses YAML string)
// Each unit-only `wire enum Foo { ... }` produces:
//   - Foo_to_json(enum) -> !llvm.ptr
//   - Foo_from_json(!llvm.ptr) -> enum
//   - Foo_to_yaml(enum) -> !llvm.ptr
//   - Foo_from_yaml(!llvm.ptr) -> enum
//
// For non-wire structs with all-primitive fields (see generateStructEncodeWrappers):
//   - Foo_to_json(struct) -> !llvm.ptr
//   - Foo_from_json(!llvm.ptr) -> !hew.result<struct, !hew.string_ref>
//   - Foo_to_yaml(struct) -> !llvm.ptr
//   - Foo_from_yaml(!llvm.ptr) -> !hew.result<struct, !hew.string_ref>
//   - Foo_to_toml(struct) -> !llvm.ptr
//   - Foo_from_toml(!llvm.ptr) -> !hew.result<struct, !hew.string_ref>
// The non-wire from_* functions return Ok(struct) on success or Err(message)
// when hew_{format}_parse returns null (invalid input document).
//
//===----------------------------------------------------------------------===//

#include "hew/mlir/HewOps.h"
#include "hew/mlir/MLIRGen.h"
#include "MLIRGenHelpers.h"

#include "mlir/Dialect/Arith/IR/Arith.h"
#include "mlir/Dialect/Func/IR/FuncOps.h"
#include "mlir/Dialect/LLVMIR/LLVMDialect.h"
#include "mlir/Dialect/SCF/IR/SCF.h"
#include "mlir/IR/Builders.h"
#include "mlir/IR/BuiltinTypes.h"

#include <cctype>
#include <functional>
#include <unordered_map>

namespace hew {

// ============================================================================
// WireTypeInfo — single source of truth for primitive wire type attributes
// ============================================================================
//
// Each entry captures the four boolean predicates that were previously
// duplicated across needsZigzag / isUnsignedWireType / isVarintType /
// isWirePrimitiveType.  All entries in this table are primitives by definition.
//
// Key invariants encoded here:
//   • "int" and "Int" are BOTH listed as zigzag-signed (both map to i64).
//   • "duration" is varint but neither zigzag nor unsigned (nanosecond i64,
//     always non-negative in practice — zigzag would be wrong).
//   • "bytes" is distinct from "String"/"string"/"str" at the wire level even
//     though they share JSON behaviour; the dispatch helpers (wireKindOf,
//     encodeFunc) still handle bytes/string separately for wire encoding.
//
// To add a new type alias: add ONE row here.  No other predicate needs changing.

struct WireTypeInfo {
  bool zigzag;    ///< signed integer needing zigzag encoding
  bool isUnsigned; ///< unsigned integer (zero-extend to i64 for JSON output)
  bool isVarint;  ///< varint wire encoding (not fixed-width, not length-delimited)
};

/// Lookup table for all known primitive wire type names.
/// Non-primitive (user-defined struct) names are NOT present; callers that
/// need to distinguish primitives from structs can check for nullptr return.
static const std::unordered_map<std::string, WireTypeInfo> &wireTypeInfoTable() {
  // clang-format off
  static const std::unordered_map<std::string, WireTypeInfo> kTable = {
    // name         zigzag  unsigned  varint
    { "bool",     { false,  false,    true  } },
    { "i8",       { true,   false,    true  } },
    { "u8",       { false,  true,     true  } },
    { "i16",      { true,   false,    true  } },
    { "u16",      { false,  true,     true  } },
    { "i32",      { true,   false,    true  } },
    { "u32",      { false,  true,     true  } },
    { "i64",      { true,   false,    true  } },
    { "u64",      { false,  true,     true  } },
    { "f32",      { false,  false,    false } },
    { "f64",      { false,  false,    false } },
    { "String",   { false,  false,    false } },
    { "string",   { false,  false,    false } },
    { "str",      { false,  false,    false } },
    { "bytes",    { false,  false,    false } },
    // Hew type aliases — must mirror the semantics of the canonical types above.
    { "int",      { true,   false,    true  } }, // alias for i64 (signed)
    { "Int",      { true,   false,    true  } }, // alias for i64 (capital form)
    { "uint",     { false,  true,     true  } }, // alias for u64
    { "float",    { false,  false,    false } }, // alias for f64
    { "byte",     { false,  true,     true  } }, // alias for u8
    { "char",     { false,  true,     true  } }, // unicode codepoint, always >= 0
    { "duration", { false,  false,    true  } }, // nanosecond i64 varint, no zigzag
    { "usize",    { false,  true,     true  } }, // alias for u64
    { "isize",    { true,   false,    true  } }, // alias for i64
  };
  // clang-format on
  return kTable;
}

/// Returns a pointer to the WireTypeInfo for a known primitive type name, or
/// nullptr if the name is not a primitive (i.e. it is a user-defined struct).
static const WireTypeInfo *lookupWireType(const std::string &ty) {
  const auto &table = wireTypeInfoTable();
  auto it = table.find(ty);
  return it != table.end() ? &it->second : nullptr;
}

// ============================================================================
// Predicate helpers — now driven from wireTypeInfoTable
// ============================================================================

/// Check if a wire type name is a primitive scalar wire type.
/// Type names that are NOT primitives are treated as nested wire struct
/// references and require a struct-type lookup via structTypes.
static bool isWirePrimitiveType(const std::string &ty) {
  return lookupWireType(ty) != nullptr;
}

/// Check if a wire type is a signed integer needing zigzag encoding.
/// Covers i8/i16/i32/i64 and their wire-level aliases: int/Int map to i64 in
/// the type checker; isize is treated as i64 by the codegen (wireTypeToMLIR).
static bool needsZigzag(const std::string &ty) {
  const WireTypeInfo *info = lookupWireType(ty);
  return info && info->zigzag;
}

/// Check if a wire type is an unsigned integer (zero-extend when widening to
/// i64 for JSON integer output).
/// byte → u8, char → unicode codepoint (always non-negative), uint/usize → u64.
static bool isUnsignedWireType(const std::string &ty) {
  const WireTypeInfo *info = lookupWireType(ty);
  return info && info->isUnsigned;
}

/// Check if a wire type uses varint encoding (as opposed to fixed-width or
/// length-delimited).  Covers all integer primitives and their wire aliases.
/// duration is encoded as a nanosecond i64 varint (always non-negative in
/// practice, so no zigzag).
static bool isVarintType(const std::string &ty) {
  const WireTypeInfo *info = lookupWireType(ty);
  return info && info->isVarint;
}

// ============================================================================
// Dispatch helpers — keep explicit fallthrough/default for safety
// ============================================================================

/// Classifies a wire type for JSON/YAML serialization.
/// Resolves the semantic kind from the type name string.
enum class WireJsonKind { Bool, Float32, Float64, String, Bytes, Integer };

static WireJsonKind jsonKindOf(const std::string &ty) {
  if (ty == "bool")
    return WireJsonKind::Bool;
  if (ty == "f32")
    return WireJsonKind::Float32;
  if (ty == "f64" || ty == "float")
    return WireJsonKind::Float64;
  if (ty == "String" || ty == "string" || ty == "str")
    return WireJsonKind::String;
  if (ty == "bytes")
    return WireJsonKind::Bytes;
  return WireJsonKind::Integer;
}

/// Map a wire type name to the MLIR type used for the field value.
/// Includes Hew type aliases (int, uint, float, etc.) for correctness.
static mlir::Type wireTypeToMLIR(mlir::OpBuilder &builder, const std::string &ty) {
  auto ptrType = mlir::LLVM::LLVMPointerType::get(builder.getContext());
  if (ty == "i64" || ty == "u64" || ty == "int" || ty == "Int" || ty == "uint" ||
      ty == "duration" || ty == "usize" || ty == "isize")
    return builder.getI64Type();
  if (ty == "f32")
    return builder.getF32Type();
  if (ty == "f64" || ty == "float")
    return builder.getF64Type();
  if (ty == "String" || ty == "bytes" || ty == "string" || ty == "str")
    return ptrType;
  return builder.getI32Type(); // i32, u32, i16, u16, i8, u8, bool, byte, char
}

enum class WireKind { Bool, Float32, Float64, String, Bytes, Integer };

static WireKind wireKindOf(const std::string &ty) {
  if (ty == "bool")
    return WireKind::Bool;
  if (ty == "f32")
    return WireKind::Float32;
  if (ty == "f64" || ty == "float")
    return WireKind::Float64;
  if (ty == "String" || ty == "string" || ty == "str")
    return WireKind::String;
  if (ty == "bytes")
    return WireKind::Bytes;
  return WireKind::Integer;
}

/// Return the runtime encode function name for a wire field type.
static std::string encodeFunc(const std::string &ty) {
  if (ty == "f32")
    return "hew_wire_encode_field_fixed32";
  if (ty == "f64" || ty == "float")
    return "hew_wire_encode_field_fixed64";
  if (ty == "String" || ty == "string" || ty == "str")
    return "hew_wire_encode_field_string";
  if (ty == "bytes")
    return "hew_wire_encode_field_bytes";
  return "hew_wire_encode_field_varint";
}

struct WireSerialIntegerBounds {
  int64_t min;
  int64_t max;
};

/// Return the valid decode range for bounded integer wire types that are
/// represented as i32 in the lowered struct storage.
static std::optional<WireSerialIntegerBounds> wireFromSerialIntegerBounds(
    const std::string &ty) {
  if (ty == "i8")
    return WireSerialIntegerBounds{-128, 127};
  if (ty == "u8" || ty == "byte")
    return WireSerialIntegerBounds{0, 255};
  if (ty == "i16")
    return WireSerialIntegerBounds{-32768, 32767};
  if (ty == "u16")
    return WireSerialIntegerBounds{0, 65535};
  if (ty == "i32")
    return WireSerialIntegerBounds{-2147483648LL, 2147483647LL};
  if (ty == "u32")
    return WireSerialIntegerBounds{0, 4294967295LL};
  if (ty == "char")
    return WireSerialIntegerBounds{0, 1114111};
  return std::nullopt;
}

static std::string wireFromSerialIntegerDecodeErrorMessage(
    llvm::StringRef format, llvm::StringRef fieldName, llvm::StringRef ty,
    const WireSerialIntegerBounds &bounds) {
  return "wire " + format.str() + " decode error for field '" + fieldName.str() +
         "': expected " + ty.str() + " in range [" + std::to_string(bounds.min) + ", " +
         std::to_string(bounds.max) + "]";
}


/// Resolve the MLIR type for a wire field.  For non-primitive type names
/// (user-defined wire struct references), returns a (possibly forward-declared)
/// LLVM identified struct type so that reverse-order declarations work: MLIR
/// identified structs are mutable handles whose body is filled in later by
/// preRegisterWireStructType.
static mlir::Type resolveWireFieldType(mlir::OpBuilder &builder, const std::string &ty,
                                       const std::unordered_map<std::string, StructTypeInfo> &st) {
  if (!isWirePrimitiveType(ty)) {
    auto it = st.find(ty);
    if (it != st.end())
      return it->second.mlirType;
    // Not yet registered (reverse-order decl): return forward-declared identified struct.
    return mlir::LLVM::LLVMStructType::getIdentified(builder.getContext(), ty);
  }
  return wireTypeToMLIR(builder, ty);
}

// ============================================================================
// JSON / YAML helpers
// ============================================================================

/// Apply a naming convention to a field name string.
static std::string applyNamingCase(const std::string &name, ast::NamingCase nc) {
  switch (nc) {
  case ast::NamingCase::CamelCase: {
    std::string result;
    bool capitalize = false;
    for (char c : name) {
      if (c == '_') {
        capitalize = true;
        continue;
      }
      result += capitalize ? (char)std::toupper((unsigned char)c) : c;
      capitalize = false;
    }
    if (!result.empty())
      result[0] = (char)std::tolower((unsigned char)result[0]);
    return result;
  }
  case ast::NamingCase::PascalCase: {
    std::string result;
    bool capitalize = true;
    for (char c : name) {
      if (c == '_') {
        capitalize = true;
        continue;
      }
      result += capitalize ? (char)std::toupper((unsigned char)c) : c;
      capitalize = false;
    }
    return result;
  }
  case ast::NamingCase::SnakeCase: {
    std::string result = name;
    for (auto &c : result)
      c = (char)std::tolower((unsigned char)c);
    return result;
  }
  case ast::NamingCase::ScreamingSnake: {
    std::string result = name;
    for (auto &c : result)
      c = (char)std::toupper((unsigned char)c);
    return result;
  }
  case ast::NamingCase::KebabCase: {
    std::string result = name;
    for (auto &c : result) {
      if (c == '_')
        c = '-';
      else
        c = (char)std::tolower((unsigned char)c);
    }
    return result;
  }
  }
  return name;
}

/// Get the serialized key name for a wire field, honouring per-field overrides
/// and falling back to the struct-level naming convention.
static std::string wireSerialFieldName(const ast::WireFieldDecl &field,
                                       const std::optional<std::string> &overrideName,
                                       const std::optional<ast::NamingCase> &defCase) {
  if (overrideName.has_value())
    return *overrideName;
  if (defCase.has_value())
    return applyNamingCase(field.name, *defCase);
  return field.name;
}

static std::string wireEnumVariantName(llvm::StringRef name,
                                       const std::optional<ast::NamingCase> &namingCase) {
  if (namingCase.has_value())
    return applyNamingCase(name.str(), *namingCase);
  return name.str();
}

static std::string wireEnumQuotedScalar(llvm::StringRef value) {
  return "\"" + value.str() + "\"";
}

static std::string wireEnumToSerialErrorMessage(llvm::StringRef format, llvm::StringRef enumName) {
  return "wire " + format.str() + " encode error for enum '" + enumName.str() +
         "': invalid unit variant tag";
}

static std::string wireEnumFromSerialErrorMessage(llvm::StringRef format,
                                                  llvm::StringRef enumName) {
  return "wire " + format.str() + " decode error for enum '" + enumName.str() +
         "': expected a known unit variant string";
}

/// Load a global string as an !llvm.ptr suitable for C ABI calls.
/// Returns an !llvm.ptr pointing to the NUL-terminated string data.
mlir::Value MLIRGen::wireStringPtr(mlir::Location location, llvm::StringRef value) {
  auto sym = getOrCreateGlobalString(value);
  auto strRefType = hew::StringRefType::get(&context);
  auto strRef = hew::ConstantOp::create(builder, location, strRefType, builder.getStringAttr(sym))
                    .getResult();
  return hew::BitcastOp::create(builder, location, mlir::LLVM::LLVMPointerType::get(&context),
                                strRef)
      .getResult();
}

// ============================================================================
// Wire struct/enum declaration
// ============================================================================

void MLIRGen::preRegisterWireStructType(const ast::WireDecl &decl) {
  if (decl.kind != ast::WireDeclKind::Struct)
    return;

  const auto &declName = decl.name;
  if (structTypes.find(declName) != structTypes.end())
    return; // already registered

  StructTypeInfo info;
  info.name = declName;
  llvm::SmallVector<mlir::Type, 8> fieldTypes;
  unsigned fieldIdx = 0;
  for (const auto &field : decl.fields) {
    mlir::Type mlirTy;
    if (allWireStructNames_.count(field.ty)) {
      // Nested wire struct reference: use getIdentified so forward references
      // work — the body will be set when that struct is processed.
      mlirTy = mlir::LLVM::LLVMStructType::getIdentified(&context, field.ty);
    } else if (!isWirePrimitiveType(field.ty)) {
      // Unknown type: not a wire struct and not a known primitive — fail closed.
      ++errorCount_;
      emitError(builder.getUnknownLoc())
          << "wire struct '" << declName << "': field '" << field.name
          << "' has unsupported type '" << field.ty << "'";
      return;
    } else {
      mlirTy = wireTypeToMLIR(builder, field.ty);
    }
    fieldTypes.push_back(mlirTy);
    // Preserve Hew-level semantic type for owned-field detection.
    // wireTypeToMLIR maps String/bytes to !llvm.ptr, losing type info.
    auto semanticTy = (field.ty == "String" || field.ty == "string" || field.ty == "str")
                          ? mlir::Type(hew::StringRefType::get(&context))
                          : mlirTy;
    info.fields.push_back({field.name, mlirTy, semanticTy, fieldIdx, field.ty});
    ++fieldIdx;
  }
  info.mlirType = mlir::LLVM::LLVMStructType::getIdentified(&context, declName);
  if (!info.mlirType.isInitialized())
    (void)info.mlirType.setBody(fieldTypes, /*isPacked=*/false);
  structTypes[declName] = info;
}

/// Emit forward declarations (no body) for all six helper functions of a wire
/// struct so that encode/decode/JSON/YAML of outer structs can reference helpers
/// for inner structs regardless of declaration order.
void MLIRGen::predeclareWireHelpers(const ast::WireDecl &decl) {
  if (decl.kind != ast::WireDeclKind::Struct)
    return;

  const auto &name = decl.name;

  // Fail closed: skip pre-declaration for structs with unsupported field types.
  // preRegisterWireStructType already emitted the diagnostic; skip quietly here
  // to avoid generating invalid opaque-struct function signatures.
  for (const auto &field : decl.fields) {
    if (!isWirePrimitiveType(field.ty) && !allWireStructNames_.count(field.ty))
      return;
  }
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i64Type = builder.getI64Type();

  // The struct type may not have its body set yet if this runs before
  // preRegisterWireStructType for this decl; getIdentified gives a stable
  // forward-declared handle that resolves once setBody is called later.
  auto structMlirTy = mlir::LLVM::LLVMStructType::getIdentified(&context, name);

  // Collect field types for the encode/to_json/to_yaml signatures.
  llvm::SmallVector<mlir::Type, 8> fieldTypes;
  for (const auto &field : decl.fields) {
    fieldTypes.push_back(resolveWireFieldType(builder, field.ty, structTypes));
  }

  auto predeclare = [&](llvm::StringRef fnName, mlir::FunctionType fnType) {
    if (!module.lookupSymbol<mlir::func::FuncOp>(fnName)) {
      auto savedIP = builder.saveInsertionPoint();
      builder.setInsertionPointToEnd(module.getBody());
      auto fn = mlir::func::FuncOp::create(builder, builder.getUnknownLoc(), fnName, fnType);
      fn.setPrivate();
      builder.restoreInsertionPoint(savedIP);
    }
  };

  predeclare(name + "_encode",
             mlir::FunctionType::get(&context, fieldTypes, {ptrType}));
  predeclare(name + "_decode",
             mlir::FunctionType::get(&context, {ptrType, i64Type}, {structMlirTy}));
  predeclare(name + "_to_json",
             mlir::FunctionType::get(&context, fieldTypes, {ptrType}));
  predeclare(name + "_from_json",
             mlir::FunctionType::get(&context, {ptrType}, {structMlirTy}));
  predeclare(name + "_to_yaml",
             mlir::FunctionType::get(&context, fieldTypes, {ptrType}));
  predeclare(name + "_from_yaml",
             mlir::FunctionType::get(&context, {ptrType}, {structMlirTy}));
}

void MLIRGen::generateWireDecl(const ast::WireDecl &decl) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();
  auto nativeSizeType = sizeType();

  if (decl.kind == ast::WireDeclKind::Enum) {
    // ── Register wire enum as a regular enum type ─────────────────
    // Wire enums are represented as i32 tags with optional payloads
    EnumTypeInfo info;
    info.name = decl.name;

    bool hasPayloads = false;
    unsigned varIdx = 0;
    for (const auto &variant : decl.variants) {
      EnumVariantInfo vi;
      vi.name = variant.name;
      vi.index = varIdx++;

      // Convert variant payload types to MLIR types
      if (auto *tuple = std::get_if<ast::VariantDecl::VariantTuple>(&variant.kind)) {
        for (const auto &ty : tuple->fields) {
          vi.payloadTypes.push_back(convertType(ty.value));
        }
      } else if (auto *strct = std::get_if<ast::VariantDecl::VariantStruct>(&variant.kind)) {
        for (const auto &field : strct->fields) {
          vi.payloadTypes.push_back(convertType(field.ty.value));
          vi.fieldNames.push_back(field.name);
        }
      }
      if (!vi.payloadTypes.empty()) {
        hasPayloads = true;
      }

      info.variants.push_back(std::move(vi));
    }

    // For wire enums with payloads, create a tagged-union-compatible struct.
    // - Single payload shape: {tag, shared_payload_fields...}
    // - Mixed payload shapes: {tag, per_variant_payload_fields...}
    if (hasPayloads) {
      bool singlePayloadShape = true;
      bool sawPayloadVariant = false;
      llvm::SmallVector<mlir::Type, 8> sharedPayloadTypes;
      for (const auto &variant : info.variants) {
        if (variant.payloadTypes.empty())
          continue;
        if (!sawPayloadVariant) {
          sharedPayloadTypes.assign(variant.payloadTypes.begin(), variant.payloadTypes.end());
          sawPayloadVariant = true;
          continue;
        }
        if (variant.payloadTypes.size() != sharedPayloadTypes.size()) {
          singlePayloadShape = false;
          break;
        }
        for (size_t i = 0; i < variant.payloadTypes.size(); ++i) {
          if (variant.payloadTypes[i] != sharedPayloadTypes[i]) {
            singlePayloadShape = false;
            break;
          }
        }
        if (!singlePayloadShape)
          break;
      }

      llvm::SmallVector<mlir::Type, 8> structTypesVec{i32Type};
      if (singlePayloadShape && sawPayloadVariant) {
        structTypesVec.append(sharedPayloadTypes.begin(), sharedPayloadTypes.end());
        for (auto &variant : info.variants) {
          variant.payloadPositions.clear();
          for (size_t i = 0; i < variant.payloadTypes.size(); ++i)
            variant.payloadPositions.push_back(static_cast<int64_t>(i) + 1);
        }
      } else {
        int64_t nextField = 1;
        for (auto &variant : info.variants) {
          variant.payloadPositions.clear();
          for (const auto &payloadType : variant.payloadTypes) {
            variant.payloadPositions.push_back(nextField++);
            structTypesVec.push_back(payloadType);
          }
        }
      }

      info.mlirType = mlir::LLVM::LLVMStructType::getLiteral(&context, structTypesVec);
      info.hasPayloads = true;
    } else {
      // All-unit wire enum: just i32 tags
      info.mlirType = i32Type;
      info.hasPayloads = false;
    }

    // Register variant names for lookup (needs owning copy for the map value)
    std::string enumName = decl.name;
    for (const auto &variant : info.variants) {
      variantLookup[variant.name] = {enumName, variant.index};
      variantLookup[enumName + "::" + variant.name] = {enumName, variant.index};
    }
    enumTypes[enumName] = std::move(info);

    // JSON/YAML enum helpers are currently specified for unit-only wire enums.
    // Payload-bearing enum serial helpers remain a future extension.
    if (!enumTypes[enumName].hasPayloads) {
      generateWireToSerial(decl, "json", decl.json_case,
                           [](const ast::WireFieldDecl &) -> const std::optional<std::string> & {
                             static const std::optional<std::string> none;
                             return none;
                           });
      generateWireFromSerial(decl, "json", decl.json_case,
                             [](const ast::WireFieldDecl &) -> const std::optional<std::string> & {
                               static const std::optional<std::string> none;
                               return none;
                             });
      generateWireToSerial(decl, "yaml", decl.yaml_case,
                           [](const ast::WireFieldDecl &) -> const std::optional<std::string> & {
                             static const std::optional<std::string> none;
                             return none;
                           });
      generateWireFromSerial(decl, "yaml", decl.yaml_case,
                             [](const ast::WireFieldDecl &) -> const std::optional<std::string> & {
                               static const std::optional<std::string> none;
                               return none;
                             });
      generateWireMethodWrappers(decl);
    }

    return;
  }

  if (decl.kind != ast::WireDeclKind::Struct)
    return; // Unknown wire decl kind

  // ── Register the wire struct as a regular struct type ─────────────
  // This allows the rest of the compiler to work with the struct.
  // Skip if already registered by preRegisterWireStructType (pass 1b2).
  const auto &declName = decl.name;

  // Fail closed: reject any field whose type is neither a known wire primitive
  // nor a registered wire struct name before generating any function bodies.
  // preRegisterWireStructType already emits this error and skips registration;
  // this guard catches the case where that path was not taken (e.g. TypeDecl
  // wire metadata with a different registration flow).
  for (const auto &field : decl.fields) {
    if (!isWirePrimitiveType(field.ty) && !allWireStructNames_.count(field.ty)) {
      ++errorCount_;
      emitError(location) << "wire struct '" << declName << "': field '" << field.name
                          << "' has unsupported type '" << field.ty << "'";
      return;
    }
  }

  llvm::SmallVector<mlir::Type, 8> fieldTypes;
  for (const auto &field : decl.fields) {
    fieldTypes.push_back(resolveWireFieldType(builder, field.ty, structTypes));
  }
  if (structTypes.find(declName) == structTypes.end()) {
    StructTypeInfo info;
    info.name = declName;
    unsigned fieldIdx = 0;
    for (const auto &field : decl.fields) {
      auto mlirTy = fieldTypes[fieldIdx];
      info.fields.push_back({field.name, mlirTy, mlirTy, fieldIdx, ""});
      ++fieldIdx;
    }
    info.mlirType = mlir::LLVM::LLVMStructType::getIdentified(&context, declName);
    if (!info.mlirType.isInitialized())
      (void)info.mlirType.setBody(fieldTypes, /*isPacked=*/false);
    structTypes[declName] = info;
  }

  // Track this struct as wire-typed (for actor codegen to detect wire messages)
  wireStructNames[declName] = {mangleName(currentModulePath, declName, "encode"),
                               mangleName(currentModulePath, declName, "decode")};

  // ── Generate Foo_encode function ─────────────────────────────────
  // Signature: Foo_encode(field1, field2, ...) -> !llvm.ptr
  // Returns pointer to heap-owned hew_wire_buf (caller reads .data/.len and
  // releases with hew_wire_buf_destroy).
  {
    // Reuse fieldTypes computed during struct registration
    auto encodeFnType = mlir::FunctionType::get(&context, fieldTypes, {ptrType});
    std::string encodeName = declName + "_encode";

    auto savedIP = builder.saveInsertionPoint();
    builder.setInsertionPointToEnd(module.getBody());
    if (auto existing = module.lookupSymbol<mlir::func::FuncOp>(encodeName))
      existing.erase();
    auto encodeFn = mlir::func::FuncOp::create(builder, location, encodeName, encodeFnType);
    auto *entryBlock = encodeFn.addEntryBlock();
    builder.setInsertionPointToStart(entryBlock);

    // Allocate a heap-owned wire buffer via runtime helper.
    auto bufPtr = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                             mlir::SymbolRefAttr::get(&context, "hew_wire_buf_new"),
                                             mlir::ValueRange{})
                      .getResult();

    // If schema has a version, encode it as field 0 (reserved for version tag)
    if (decl.version.has_value()) {
      auto tagZero = createIntConstant(builder, location, i32Type, 0);
      auto versionVal = createIntConstant(builder, location, i64Type, *decl.version);
      hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{i32Type},
                                 mlir::SymbolRefAttr::get(&context, "hew_wire_encode_field_varint"),
                                 mlir::ValueRange{bufPtr, tagZero, versionVal});
    }

    // Encode each field
    unsigned encIdx = 0;
    for (const auto &field : decl.fields) {
      mlir::Value fieldVal = entryBlock->getArgument(encIdx);
      auto tagVal = createIntConstant(builder, location, i32Type, field.field_number);
      std::string funcName = encodeFunc(field.ty);

      auto wkind = wireKindOf(field.ty);
      if (wkind == WireKind::Bytes) {
        auto bytesBuf =
            hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                       mlir::SymbolRefAttr::get(&context, "hew_wire_bytes_to_buf"),
                                       mlir::ValueRange{fieldVal})
                .getResult();
        auto bytesData =
            hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                       mlir::SymbolRefAttr::get(&context, "hew_wire_buf_data"),
                                       mlir::ValueRange{bytesBuf})
                .getResult();
        auto bytesLen =
            hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{nativeSizeType},
                                       mlir::SymbolRefAttr::get(&context, "hew_wire_buf_len"),
                                       mlir::ValueRange{bytesBuf})
                .getResult();
        hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{i32Type},
                                   mlir::SymbolRefAttr::get(&context, funcName),
                                   mlir::ValueRange{bufPtr, tagVal, bytesData, bytesLen});
        hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                   mlir::SymbolRefAttr::get(&context, "hew_wire_buf_destroy"),
                                   mlir::ValueRange{bytesBuf});
      } else if (wkind == WireKind::String) {
        // hew_wire_encode_field_string(buf, tag, str_ptr)
        hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{i32Type},
                                   mlir::SymbolRefAttr::get(&context, funcName),
                                   mlir::ValueRange{bufPtr, tagVal, fieldVal});
      } else if (wkind == WireKind::Float32) {
        // hew_wire_encode_field_fixed32(buf, tag, bitcast_to_i32)
        auto asI32 = mlir::arith::BitcastOp::create(builder, location, i32Type, fieldVal);
        hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{i32Type},
                                   mlir::SymbolRefAttr::get(&context, funcName),
                                   mlir::ValueRange{bufPtr, tagVal, asI32});
      } else if (wkind == WireKind::Float64) {
        // hew_wire_encode_field_fixed64(buf, tag, bitcast_to_i64)
        auto asI64 = mlir::arith::BitcastOp::create(builder, location, i64Type, fieldVal);
        hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{i32Type},
                                   mlir::SymbolRefAttr::get(&context, funcName),
                                   mlir::ValueRange{bufPtr, tagVal, asI64});
      } else if (isVarintType(field.ty)) {
        // For varint types, extend to i64 for the runtime call
        mlir::Value valI64 = fieldVal;
        if (needsZigzag(field.ty)) {
          // Sign-extend to i64 first, then zigzag encode
          if (fieldVal.getType() == i32Type)
            valI64 = mlir::arith::ExtSIOp::create(builder, location, i64Type, fieldVal);
          valI64 = hew::RuntimeCallOp::create(
                       builder, location, mlir::TypeRange{i64Type},
                       mlir::SymbolRefAttr::get(&context, "hew_wire_zigzag_encode"),
                       mlir::ValueRange{valI64})
                       .getResult();
        } else {
          // Zero-extend unsigned to i64
          if (fieldVal.getType() == i32Type)
            valI64 = mlir::arith::ExtUIOp::create(builder, location, i64Type, fieldVal);
        }
        hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{i32Type},
                                   mlir::SymbolRefAttr::get(&context, funcName),
                                   mlir::ValueRange{bufPtr, tagVal, valI64});
      } else if (!isWirePrimitiveType(field.ty) && structTypes.count(field.ty)) {
        // Nested wire struct: call T_encode(sub_fields...) → inner buf, then
        // embed the resulting bytes as a length-prefixed bytes field.
        const auto &innerInfo = structTypes.at(field.ty);
        llvm::SmallVector<mlir::Value, 8> innerArgs;
        for (unsigned j = 0; j < innerInfo.fields.size(); ++j)
          innerArgs.push_back(
              mlir::LLVM::ExtractValueOp::create(builder, location, fieldVal, j));
        auto innerEncFn =
            module.lookupSymbol<mlir::func::FuncOp>(field.ty + "_encode");
        auto innerBuf =
            mlir::func::CallOp::create(builder, location, innerEncFn, innerArgs).getResult(0);
        auto innerData =
            hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                       mlir::SymbolRefAttr::get(&context, "hew_wire_buf_data"),
                                       mlir::ValueRange{innerBuf})
                .getResult();
        auto innerLen =
            hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{nativeSizeType},
                                       mlir::SymbolRefAttr::get(&context, "hew_wire_buf_len"),
                                       mlir::ValueRange{innerBuf})
                .getResult();
        hew::RuntimeCallOp::create(
            builder, location, mlir::TypeRange{i32Type},
            mlir::SymbolRefAttr::get(&context, "hew_wire_encode_field_bytes"),
            mlir::ValueRange{bufPtr, tagVal, innerData, innerLen});
        hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                   mlir::SymbolRefAttr::get(&context, "hew_wire_buf_destroy"),
                                   mlir::ValueRange{innerBuf});
      } else {
        ++errorCount_;
        emitError(location) << "wire struct '" << declName << "': field '" << field.name
                            << "' has unsupported type '" << field.ty << "' for binary encoding";
        return;
      }
      ++encIdx;
    }

    // Return the buffer pointer
    mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{bufPtr});
    builder.restoreInsertionPoint(savedIP);
  }

  // ── Generate Foo_decode function (TLV-based) ─────────────────────
  // Signature: Foo_decode(!llvm.ptr, i64) -> struct_type
  // Takes a buffer pointer and size, returns a struct with decoded fields.
  // Uses TLV dispatch loop for forward compatibility — unknown fields are
  // silently skipped, fields can appear in any order.
  {
    auto structType = structTypes.at(declName).mlirType;
    auto decodeFnType = mlir::FunctionType::get(&context, {ptrType, i64Type}, {structType});
    std::string decodeName = declName + "_decode";

    auto savedIP = builder.saveInsertionPoint();
    builder.setInsertionPointToEnd(module.getBody());
    if (auto existing = module.lookupSymbol<mlir::func::FuncOp>(decodeName))
      existing.erase();
    auto decodeFn = mlir::func::FuncOp::create(builder, location, decodeName, decodeFnType);
    auto *entryBlock = decodeFn.addEntryBlock();
    builder.setInsertionPointToStart(entryBlock);

    auto dataPtr = entryBlock->getArgument(0);
    auto dataSize = entryBlock->getArgument(1);

    // Allocate a hew_wire_buf on the stack (32 bytes: { ptr, i64, i64, i64 })
    auto bufPtr = mlir::LLVM::AllocaOp::create(builder, location, ptrType, builder.getI8Type(),
                                               createIntConstant(builder, location, i64Type, 32));

    // Initialize buffer for reading from existing data
    hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                               mlir::SymbolRefAttr::get(&context, "hew_wire_buf_init_read"),
                               mlir::ValueRange{bufPtr, dataPtr, dataSize});

    // Allocate per-field storage initialized with defaults.
    // We use individual allocas so the loop body can store decoded values.
    auto one = createIntConstant(builder, location, i64Type, 1);
    llvm::SmallVector<mlir::Value, 8> fieldSlots;
    for (unsigned i = 0; i < decl.fields.size(); ++i) {
      auto fty = fieldTypes[i];
      auto slot = mlir::LLVM::AllocaOp::create(builder, location, ptrType, fty, one);
      // Initialize with default: 0 for integers/floats, null for pointers
      mlir::Value defaultVal;
      if (fty == ptrType)
        defaultVal = mlir::LLVM::ZeroOp::create(builder, location, ptrType);
      else if (fty == builder.getF32Type())
        defaultVal =
            mlir::arith::ConstantOp::create(builder, location, builder.getF32FloatAttr(0.0f));
      else if (fty == builder.getF64Type())
        defaultVal =
            mlir::arith::ConstantOp::create(builder, location, builder.getF64FloatAttr(0.0));
      else if (mlir::isa<mlir::LLVM::LLVMStructType>(fty))
        // Nested wire struct: use an all-zeros struct as the default value.
        defaultVal = mlir::LLVM::ZeroOp::create(builder, location, fty);
      else
        defaultVal = createIntConstant(builder, location, fty, 0);
      mlir::LLVM::StoreOp::create(builder, location, defaultVal, slot);
      fieldSlots.push_back(slot);
    }

    // Scratch slots for decode out-params
    auto scratchI64 = mlir::LLVM::AllocaOp::create(builder, location, ptrType, i64Type, one);
    auto scratchI32 = mlir::LLVM::AllocaOp::create(builder, location, ptrType, i32Type, one);
    auto scratchPtr = mlir::LLVM::AllocaOp::create(builder, location, ptrType, ptrType, one);
    auto scratchLen =
        mlir::LLVM::AllocaOp::create(builder, location, ptrType, nativeSizeType, one);
    auto scratchFieldNum = mlir::LLVM::AllocaOp::create(builder, location, ptrType, i32Type, one);
    auto scratchWireType = mlir::LLVM::AllocaOp::create(builder, location, ptrType, i32Type, one);

    // Flag to signal a decode error (set in "after" block, checked in "before" block).
    // If hew_wire_decode_tag fails the buffer doesn't advance, so without this
    // flag the loop would spin forever on truncated input.
    auto decodeError = mlir::LLVM::AllocaOp::create(builder, location, ptrType, i32Type, one);
    mlir::LLVM::StoreOp::create(builder, location, createIntConstant(builder, location, i32Type, 0),
                                decodeError);

    // TLV dispatch loop: while(buf has remaining data && no decode error)
    auto whileOp =
        mlir::scf::WhileOp::create(builder, location, mlir::TypeRange{}, mlir::ValueRange{});

    // "before" block: check if buffer has remaining data and no prior error
    auto *beforeBlock = builder.createBlock(&whileOp.getBefore());
    builder.setInsertionPointToStart(beforeBlock);
    auto hasData =
        hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{i32Type},
                                   mlir::SymbolRefAttr::get(&context, "hew_wire_buf_has_remaining"),
                                   mlir::ValueRange{bufPtr})
            .getResult();
    auto hasDataBool =
        mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::ne, hasData,
                                    createIntConstant(builder, location, i32Type, 0));
    auto errVal = mlir::LLVM::LoadOp::create(builder, location, i32Type, decodeError);
    auto noError =
        mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::eq, errVal,
                                    createIntConstant(builder, location, i32Type, 0));
    auto cond = mlir::arith::AndIOp::create(builder, location, hasDataBool, noError);
    mlir::scf::ConditionOp::create(builder, location, cond, mlir::ValueRange{});

    // "after" block: decode tag, dispatch by field number
    auto *afterBlock = builder.createBlock(&whileOp.getAfter());
    builder.setInsertionPointToStart(afterBlock);

    // Decode the tag: hew_wire_decode_tag(buf, &field_num, &wire_type)
    // Returns 0 on success, non-zero on error (e.g. truncated buffer).
    auto tagResult =
        hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{i32Type},
                                   mlir::SymbolRefAttr::get(&context, "hew_wire_decode_tag"),
                                   mlir::ValueRange{bufPtr, scratchFieldNum, scratchWireType})
            .getResult();
    // On error, set the decodeError flag so the loop exits on next "before" check.
    auto tagFailed =
        mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::ne, tagResult,
                                    createIntConstant(builder, location, i32Type, 0));
    auto tagFailedIf = mlir::scf::IfOp::create(builder, location, tagFailed, /*hasElse=*/false);
    builder.setInsertionPointToStart(&tagFailedIf.getThenRegion().front());
    mlir::LLVM::StoreOp::create(builder, location, createIntConstant(builder, location, i32Type, 1),
                                decodeError);
    builder.setInsertionPointAfter(tagFailedIf);

    // Only dispatch if tag decode succeeded (tagResult == 0).
    // When it fails, decodeError is set and the loop exits on next "before" check.
    auto tagOk =
        mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::eq, tagResult,
                                    createIntConstant(builder, location, i32Type, 0));
    auto tagOkIf = mlir::scf::IfOp::create(builder, location, tagOk, /*hasElse=*/false);
    builder.setInsertionPointToStart(&tagOkIf.getThenRegion().front());

    auto fieldNum = mlir::LLVM::LoadOp::create(builder, location, i32Type, scratchFieldNum);
    auto wireType = mlir::LLVM::LoadOp::create(builder, location, i32Type, scratchWireType);

    // Track whether any field-number match fired, to skip unknown fields.
    auto matchedAny = mlir::LLVM::AllocaOp::create(builder, location, ptrType, i32Type, one);
    mlir::LLVM::StoreOp::create(builder, location, createIntConstant(builder, location, i32Type, 0),
                                matchedAny);

    // Dispatch by field number using chained if-else.
    // Each known field number decodes the value and stores it.
    // Unknown fields are skipped via hew_wire_skip_field.
    for (unsigned i = 0; i < decl.fields.size(); ++i) {
      const auto &field = decl.fields[i];
      auto fty = fieldTypes[i];
      auto fieldNumConst = createIntConstant(builder, location, i32Type, field.field_number);
      auto isMatch = mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::eq,
                                                 fieldNum, fieldNumConst);

      auto ifOp = mlir::scf::IfOp::create(builder, location, isMatch, /*hasElse=*/false);
      builder.setInsertionPointToStart(&ifOp.getThenRegion().front());

      // Decode the field value based on type
      mlir::Value decoded;
      auto wkind = wireKindOf(field.ty);
      if (wkind == WireKind::Float32) {
        hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{i32Type},
                                   mlir::SymbolRefAttr::get(&context, "hew_wire_decode_fixed32"),
                                   mlir::ValueRange{bufPtr, scratchI32});
        auto rawI32 = mlir::LLVM::LoadOp::create(builder, location, i32Type, scratchI32);
        decoded = mlir::arith::BitcastOp::create(builder, location, builder.getF32Type(), rawI32);
      } else if (wkind == WireKind::Float64) {
        hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{i32Type},
                                   mlir::SymbolRefAttr::get(&context, "hew_wire_decode_fixed64"),
                                   mlir::ValueRange{bufPtr, scratchI64});
        auto rawI64 = mlir::LLVM::LoadOp::create(builder, location, i64Type, scratchI64);
        decoded = mlir::arith::BitcastOp::create(builder, location, builder.getF64Type(), rawI64);
      } else if (wkind == WireKind::Bytes) {
        auto decodeBytesResult =
            hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{i32Type},
                                       mlir::SymbolRefAttr::get(&context, "hew_wire_decode_bytes"),
                                       mlir::ValueRange{bufPtr, scratchPtr, scratchLen})
                .getResult();
        auto bytesOk =
            mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::eq,
                                        decodeBytesResult,
                                        createIntConstant(builder, location, i32Type, 0));
        auto bytesIf = mlir::scf::IfOp::create(builder, location, ptrType, bytesOk,
                                               /*withElseRegion=*/true);

        builder.setInsertionPointToStart(&bytesIf.getThenRegion().front());
        auto rawPtr = mlir::LLVM::LoadOp::create(builder, location, ptrType, scratchPtr);
        auto rawLen = mlir::LLVM::LoadOp::create(builder, location, nativeSizeType, scratchLen);
        auto vecFromRawType = builder.getFunctionType({ptrType, nativeSizeType}, {ptrType});
        getOrCreateExternFunc("hew_vec_from_raw_bytes", vecFromRawType);
        auto bytesVec = mlir::func::CallOp::create(builder, location, "hew_vec_from_raw_bytes",
                                                   mlir::TypeRange{ptrType},
                                                   mlir::ValueRange{rawPtr, rawLen})
                            .getResult(0);
        mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{bytesVec});

        builder.setInsertionPointToStart(&bytesIf.getElseRegion().front());
        mlir::LLVM::StoreOp::create(builder, location,
                                    createIntConstant(builder, location, i32Type, 1), decodeError);
        auto nullPtr = mlir::LLVM::ZeroOp::create(builder, location, ptrType);
        mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{nullPtr});

        builder.setInsertionPointAfter(bytesIf);
        decoded = bytesIf.getResult(0);
      } else if (wkind == WireKind::String) {
        // Decode as null-terminated C string (copies data + appends '\0')
        auto decodedStr =
            hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                       mlir::SymbolRefAttr::get(&context, "hew_wire_decode_string"),
                                       mlir::ValueRange{bufPtr})
                .getResult();
        auto nullPtr = mlir::LLVM::ZeroOp::create(builder, location, ptrType);
        auto stringOk = mlir::LLVM::ICmpOp::create(builder, location,
                                                   mlir::LLVM::ICmpPredicate::ne, decodedStr,
                                                   nullPtr);
        auto stringIf = mlir::scf::IfOp::create(builder, location, ptrType, stringOk,
                                                /*withElseRegion=*/true);

        builder.setInsertionPointToStart(&stringIf.getThenRegion().front());
        mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{decodedStr});

        builder.setInsertionPointToStart(&stringIf.getElseRegion().front());
        mlir::LLVM::StoreOp::create(builder, location,
                                    createIntConstant(builder, location, i32Type, 1), decodeError);
        mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{nullPtr});

        builder.setInsertionPointAfter(stringIf);
        decoded = stringIf.getResult(0);
      } else if (!isWirePrimitiveType(field.ty) && structTypes.count(field.ty)) {
        // Nested wire struct: decode the bytes payload and call T_decode(data_ptr, len).
        // T_decode creates its own internal stack buffer from the raw pointer.
        // Mirror the WireKind::Bytes error-handling pattern: only load scratch
        // values on success, set decodeError and yield a zero struct on failure.
        auto decodeBytesResult =
            hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{i32Type},
                                       mlir::SymbolRefAttr::get(&context, "hew_wire_decode_bytes"),
                                       mlir::ValueRange{bufPtr, scratchPtr, scratchLen})
                .getResult();
        auto nestedBytesOk =
            mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::eq,
                                        decodeBytesResult,
                                        createIntConstant(builder, location, i32Type, 0));
        auto nestedIf =
            mlir::scf::IfOp::create(builder, location, fty, nestedBytesOk, /*withElseRegion=*/true);

        builder.setInsertionPointToStart(&nestedIf.getThenRegion().front());
        auto innerDataPtr = mlir::LLVM::LoadOp::create(builder, location, ptrType, scratchPtr);
        auto innerLen =
            mlir::LLVM::LoadOp::create(builder, location, nativeSizeType, scratchLen);
        auto innerDecFn = module.lookupSymbol<mlir::func::FuncOp>(field.ty + "_decode");
        auto innerStruct =
            mlir::func::CallOp::create(builder, location, innerDecFn,
                                       mlir::ValueRange{innerDataPtr, innerLen})
                .getResult(0);
        mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{innerStruct});

        builder.setInsertionPointToStart(&nestedIf.getElseRegion().front());
        mlir::LLVM::StoreOp::create(builder, location,
                                    createIntConstant(builder, location, i32Type, 1), decodeError);
        auto zeroStruct = mlir::LLVM::ZeroOp::create(builder, location, fty);
        mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{zeroStruct});

        builder.setInsertionPointAfter(nestedIf);
        decoded = nestedIf.getResult(0);
      } else if (!isWirePrimitiveType(field.ty)) {
        // Unknown type: not a wire struct and not a known primitive — fail closed.
        ++errorCount_;
        emitError(location) << "wire struct '" << declName << "': field '" << field.name
                            << "' has unsupported type '" << field.ty << "' for binary decoding";
        return;
      } else {
        hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{i32Type},
                                   mlir::SymbolRefAttr::get(&context, "hew_wire_decode_varint"),
                                   mlir::ValueRange{bufPtr, scratchI64});
        auto rawI64 = mlir::LLVM::LoadOp::create(builder, location, i64Type, scratchI64);
        mlir::Value val = rawI64;
        if (needsZigzag(field.ty)) {
          val = hew::RuntimeCallOp::create(
                    builder, location, mlir::TypeRange{i64Type},
                    mlir::SymbolRefAttr::get(&context, "hew_wire_zigzag_decode"),
                    mlir::ValueRange{rawI64})
                    .getResult();
        }
        if (auto bounds = wireFromSerialIntegerBounds(field.ty)) {
          auto minVal = createIntConstant(builder, location, i64Type, bounds->min);
          auto maxVal = createIntConstant(builder, location, i64Type, bounds->max);
          auto belowMin =
              mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::slt,
                                          val, minVal);
          auto aboveMax =
              mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::sgt,
                                          val, maxVal);
          auto outOfRange = mlir::arith::OrIOp::create(builder, location, belowMin, aboveMax);
          auto outOfRangeIf =
              mlir::scf::IfOp::create(builder, location, outOfRange, /*hasElse=*/false);
          builder.setInsertionPointToStart(&outOfRangeIf.getThenRegion().front());
          auto msgPtr =
              wireStringPtr(location,
                            wireFromSerialIntegerDecodeErrorMessage("binary", field.name, field.ty,
                                                                    *bounds));
          hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                     mlir::SymbolRefAttr::get(&context, "hew_panic_msg"),
                                     mlir::ValueRange{msgPtr});
          builder.setInsertionPointAfter(outOfRangeIf);
        }
        decoded = (fty == i32Type)
                      ? mlir::arith::TruncIOp::create(builder, location, i32Type, val).getResult()
                      : val;
      }
      mlir::LLVM::StoreOp::create(builder, location, decoded, fieldSlots[i]);
      mlir::LLVM::StoreOp::create(builder, location,
                                  createIntConstant(builder, location, i32Type, 1), matchedAny);

      // Move insertion point after this if-op for the next field check
      builder.setInsertionPointAfter(ifOp);
    }

    // If no field-number matched, skip the unknown field.
    {
      auto matched = mlir::LLVM::LoadOp::create(builder, location, i32Type, matchedAny);
      auto noneMatched =
          mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::eq, matched,
                                      createIntConstant(builder, location, i32Type, 0));
      auto skipIf = mlir::scf::IfOp::create(builder, location, noneMatched, /*hasElse=*/false);
      builder.setInsertionPointToStart(&skipIf.getThenRegion().front());
      hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{i32Type},
                                 mlir::SymbolRefAttr::get(&context, "hew_wire_skip_field"),
                                 mlir::ValueRange{bufPtr, wireType});
      builder.setInsertionPointAfter(skipIf);
    }

    // End of tagOk guard
    builder.setInsertionPointAfter(tagOkIf);

    mlir::scf::YieldOp::create(builder, location);

    // After the loop: load all field values and build the result struct
    builder.setInsertionPointAfter(whileOp);
    mlir::Value result = mlir::LLVM::UndefOp::create(builder, location, structType);
    for (unsigned i = 0; i < decl.fields.size(); ++i) {
      auto val = mlir::LLVM::LoadOp::create(builder, location, fieldTypes[i], fieldSlots[i]);
      result = mlir::LLVM::InsertValueOp::create(builder, location, result, val, i);
    }

    mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{result});
    builder.restoreInsertionPoint(savedIP);
  }

  // ── Generate JSON/YAML conversion functions ───────────────────────
  generateWireToSerial(decl, "json", decl.json_case,
                       [](const ast::WireFieldDecl &f) -> const std::optional<std::string> & {
                         return f.json_name;
                       });
  generateWireFromSerial(decl, "json", decl.json_case,
                         [](const ast::WireFieldDecl &f) -> const std::optional<std::string> & {
                           return f.json_name;
                         });
  generateWireToSerial(decl, "yaml", decl.yaml_case,
                       [](const ast::WireFieldDecl &f) -> const std::optional<std::string> & {
                         return f.yaml_name;
                       });
  generateWireFromSerial(decl, "yaml", decl.yaml_case,
                         [](const ast::WireFieldDecl &f) -> const std::optional<std::string> & {
                           return f.yaml_name;
                         });

  // ── Generate mangled method wrappers for method-style dispatch ───
  generateWireMethodWrappers(decl);
}

// ============================================================================
// Unified Foo_to_{json,yaml} generation
// ============================================================================

void MLIRGen::generateWireToSerial(
    const ast::WireDecl &decl, llvm::StringRef format,
    const std::optional<ast::NamingCase> &namingCase,
    llvm::function_ref<const std::optional<std::string> &(const ast::WireFieldDecl &)>
        fieldOverride) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();
  auto f64Type = builder.getF64Type();

  const auto &declName = decl.name;

  if (decl.kind == ast::WireDeclKind::Enum) {
    auto enumIt = enumTypes.find(declName);
    if (enumIt == enumTypes.end() || enumIt->second.hasPayloads)
      return;

    auto enumType = enumIt->second.mlirType;
    auto savedIP = builder.saveInsertionPoint();
    builder.setInsertionPointToEnd(module.getBody());
    std::string fnName = declName + "_to_" + format.str();
    if (auto existing = module.lookupSymbol<mlir::func::FuncOp>(fnName))
      existing.erase();
    auto fn = mlir::func::FuncOp::create(builder, location, fnName,
                                         mlir::FunctionType::get(&context, {enumType}, {ptrType}));
    auto *entry = fn.addEntryBlock();
    builder.setInsertionPointToStart(entry);

    std::string rtParse = "hew_" + format.str() + "_parse";
    std::string rtStringify = "hew_" + format.str() + "_stringify";
    std::string rtFree = "hew_" + format.str() + "_free";

    auto emitVariantString = [&](llvm::StringRef variantName) -> mlir::Value {
      auto quotedPtr = wireStringPtr(location, wireEnumQuotedScalar(variantName));
      auto parsed = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                               mlir::SymbolRefAttr::get(&context, rtParse),
                                               mlir::ValueRange{quotedPtr})
                        .getResult();
      auto result = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                               mlir::SymbolRefAttr::get(&context, rtStringify),
                                               mlir::ValueRange{parsed})
                        .getResult();
      hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                 mlir::SymbolRefAttr::get(&context, rtFree),
                                 mlir::ValueRange{parsed});
      return result;
    };

    std::function<mlir::Value(size_t)> emitDispatch = [&](size_t index) -> mlir::Value {
      if (index >= decl.variants.size()) {
        auto msgPtr = wireStringPtr(location, wireEnumToSerialErrorMessage(format, declName));
        hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                   mlir::SymbolRefAttr::get(&context, "hew_panic_msg"),
                                   mlir::ValueRange{msgPtr});
        return mlir::LLVM::ZeroOp::create(builder, location, ptrType);
      }

      auto isMatch = mlir::arith::CmpIOp::create(
          builder, location, mlir::arith::CmpIPredicate::eq, entry->getArgument(0),
          createIntConstant(builder, location, enumType, static_cast<int64_t>(index)));
      auto ifOp = mlir::scf::IfOp::create(builder, location, ptrType, isMatch, /*hasElse=*/true);

      builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
      auto thenValue =
          emitVariantString(wireEnumVariantName(decl.variants[index].name, namingCase));
      mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{thenValue});

      builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
      auto elseValue = emitDispatch(index + 1);
      mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{elseValue});

      builder.setInsertionPointAfter(ifOp);
      return ifOp.getResult(0);
    };

    auto result = emitDispatch(0);
    mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{result});
    builder.restoreInsertionPoint(savedIP);
    return;
  }

  if (decl.kind != ast::WireDeclKind::Struct)
    return;

  llvm::SmallVector<mlir::Type, 8> paramTypes;
  for (const auto &field : decl.fields)
    paramTypes.push_back(resolveWireFieldType(builder, field.ty, structTypes));

  auto savedIP = builder.saveInsertionPoint();
  builder.setInsertionPointToEnd(module.getBody());
  std::string fnName = declName + "_to_" + format.str();
  if (auto existing = module.lookupSymbol<mlir::func::FuncOp>(fnName))
    existing.erase();
  auto fn = mlir::func::FuncOp::create(builder, location, fnName,
                                       mlir::FunctionType::get(&context, paramTypes, {ptrType}));
  auto *entry = fn.addEntryBlock();
  builder.setInsertionPointToStart(entry);

  // Runtime function names: hew_{format}_object_new, hew_{format}_object_set_*, etc.
  std::string rtNew = "hew_" + format.str() + "_object_new";
  std::string rtSetBool = "hew_" + format.str() + "_object_set_bool";
  std::string rtSetFloat = "hew_" + format.str() + "_object_set_float";
  std::string rtSetString = "hew_" + format.str() + "_object_set_string";
  std::string rtSetBytes = "hew_" + format.str() + "_object_set_bytes";
  std::string rtSetInt = "hew_" + format.str() + "_object_set_int";
  std::string rtStringify = "hew_" + format.str() + "_stringify";
  std::string rtFree = "hew_" + format.str() + "_free";

  // obj = hew_{format}_object_new()
  auto objPtr =
      hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                 mlir::SymbolRefAttr::get(&context, rtNew), mlir::ValueRange{})
          .getResult();

  unsigned idx = 0;
  for (const auto &field : decl.fields) {
    auto keyPtr =
        wireStringPtr(location, wireSerialFieldName(field, fieldOverride(field), namingCase));
    mlir::Value fv = entry->getArgument(idx++);

    auto jkind = jsonKindOf(field.ty);
    if (!isWirePrimitiveType(field.ty) && structTypes.count(field.ty)) {
      // Nested wire struct: serialize via T_to_{format}(sub_fields...) → string,
      // parse that string back to a node, then embed it as a child object.
      const auto &innerInfo = structTypes.at(field.ty);
      llvm::SmallVector<mlir::Value, 8> innerArgs;
      for (unsigned j = 0; j < innerInfo.fields.size(); ++j)
        innerArgs.push_back(mlir::LLVM::ExtractValueOp::create(builder, location, fv, j));
      auto innerSerFn =
          module.lookupSymbol<mlir::func::FuncOp>(field.ty + "_to_" + format.str());
      auto innerStr =
          mlir::func::CallOp::create(builder, location, innerSerFn, innerArgs).getResult(0);
      std::string rtParseFn = "hew_" + format.str() + "_parse";
      std::string rtStrFreeFn = "hew_" + format.str() + "_string_free";
      std::string rtSetObjFn = "hew_" + format.str() + "_object_set";
      auto innerVal =
          hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                     mlir::SymbolRefAttr::get(&context, rtParseFn),
                                     mlir::ValueRange{innerStr})
              .getResult();
      // object_set takes ownership of innerVal; do not free separately.
      hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                 mlir::SymbolRefAttr::get(&context, rtSetObjFn),
                                 mlir::ValueRange{objPtr, keyPtr, innerVal});
      hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                 mlir::SymbolRefAttr::get(&context, rtStrFreeFn),
                                 mlir::ValueRange{innerStr});
    } else if (jkind == WireJsonKind::Bool) {
      hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                 mlir::SymbolRefAttr::get(&context, rtSetBool),
                                 mlir::ValueRange{objPtr, keyPtr, fv});
    } else if (jkind == WireJsonKind::Float32) {
      auto f64v = mlir::arith::ExtFOp::create(builder, location, f64Type, fv);
      hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                 mlir::SymbolRefAttr::get(&context, rtSetFloat),
                                 mlir::ValueRange{objPtr, keyPtr, f64v});
    } else if (jkind == WireJsonKind::Float64) {
      hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                 mlir::SymbolRefAttr::get(&context, rtSetFloat),
                                 mlir::ValueRange{objPtr, keyPtr, fv});
    } else if (jkind == WireJsonKind::String) {
      hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                 mlir::SymbolRefAttr::get(&context, rtSetString),
                                 mlir::ValueRange{objPtr, keyPtr, fv});
    } else if (jkind == WireJsonKind::Bytes) {
      hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                 mlir::SymbolRefAttr::get(&context, rtSetBytes),
                                 mlir::ValueRange{objPtr, keyPtr, fv});
    } else if (!isWirePrimitiveType(field.ty)) {
      // Unknown type: not a wire struct and not a known primitive — fail closed.
      ++errorCount_;
      emitError(location) << "wire struct '" << decl.name << "': field '" << field.name
                          << "' has unsupported type '" << field.ty
                          << "' for " << format.str() << " serialization";
      return;
    } else {
      // Integer types: extend to i64 (zero-extend unsigned, sign-extend signed)
      mlir::Value v64 = fv;
      if (fv.getType() == i32Type) {
        if (isUnsignedWireType(field.ty))
          v64 = mlir::arith::ExtUIOp::create(builder, location, i64Type, fv);
        else
          v64 = mlir::arith::ExtSIOp::create(builder, location, i64Type, fv);
      }
      hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                 mlir::SymbolRefAttr::get(&context, rtSetInt),
                                 mlir::ValueRange{objPtr, keyPtr, v64});
    }
  }

  // result = hew_{format}_stringify(obj); hew_{format}_free(obj)
  auto resultPtr = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                              mlir::SymbolRefAttr::get(&context, rtStringify),
                                              mlir::ValueRange{objPtr})
                       .getResult();
  hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                             mlir::SymbolRefAttr::get(&context, rtFree), mlir::ValueRange{objPtr});

  mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{resultPtr});
  builder.restoreInsertionPoint(savedIP);
}

// ============================================================================
// Unified Foo_from_{json,yaml} generation
// ============================================================================

void MLIRGen::generateWireFromSerial(
    const ast::WireDecl &decl, llvm::StringRef format,
    const std::optional<ast::NamingCase> &namingCase,
    llvm::function_ref<const std::optional<std::string> &(const ast::WireFieldDecl &)>
        fieldOverride) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();
  auto f64Type = builder.getF64Type();

  const auto &declName = decl.name;

  if (decl.kind == ast::WireDeclKind::Enum) {
    auto enumIt = enumTypes.find(declName);
    if (enumIt == enumTypes.end() || enumIt->second.hasPayloads)
      return;

    auto enumType = enumIt->second.mlirType;
    auto savedIP = builder.saveInsertionPoint();
    builder.setInsertionPointToEnd(module.getBody());
    std::string fnName = declName + "_from_" + format.str();
    if (auto existing = module.lookupSymbol<mlir::func::FuncOp>(fnName))
      existing.erase();
    auto fn = mlir::func::FuncOp::create(builder, location, fnName,
                                         mlir::FunctionType::get(&context, {ptrType}, {enumType}));
    auto *entry = fn.addEntryBlock();
    builder.setInsertionPointToStart(entry);

    std::string rtParse = "hew_" + format.str() + "_parse";
    std::string rtGetString = "hew_" + format.str() + "_get_string";
    std::string rtStringFree = "hew_" + format.str() + "_string_free";
    std::string rtFree = "hew_" + format.str() + "_free";

    auto strcmpType = mlir::FunctionType::get(&context, {ptrType, ptrType}, {i32Type});
    getOrCreateExternFunc("strcmp", strcmpType);

    auto parsed = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                             mlir::SymbolRefAttr::get(&context, rtParse),
                                             mlir::ValueRange{entry->getArgument(0)})
                      .getResult();
    auto variantName = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                                  mlir::SymbolRefAttr::get(&context, rtGetString),
                                                  mlir::ValueRange{parsed})
                           .getResult();

    auto emitDecodePanic = [&](bool freeVariantString) {
      if (freeVariantString) {
        hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                   mlir::SymbolRefAttr::get(&context, rtStringFree),
                                   mlir::ValueRange{variantName});
      }
      hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                 mlir::SymbolRefAttr::get(&context, rtFree),
                                 mlir::ValueRange{parsed});
      auto msgPtr = wireStringPtr(location, wireEnumFromSerialErrorMessage(format, declName));
      hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                 mlir::SymbolRefAttr::get(&context, "hew_panic_msg"),
                                 mlir::ValueRange{msgPtr});
    };

    std::function<mlir::Value(size_t)> emitDispatch = [&](size_t index) -> mlir::Value {
      if (index >= decl.variants.size()) {
        emitDecodePanic(/*freeVariantString=*/true);
        return createIntConstant(builder, location, enumType, 0);
      }

      auto expectedPtr =
          wireStringPtr(location, wireEnumVariantName(decl.variants[index].name, namingCase));
      auto cmp = mlir::func::CallOp::create(builder, location, "strcmp", mlir::TypeRange{i32Type},
                                            mlir::ValueRange{variantName, expectedPtr})
                     .getResult(0);
      auto isMatch = mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::eq,
                                                 cmp, createIntConstant(builder, location, i32Type, 0));
      auto ifOp =
          mlir::scf::IfOp::create(builder, location, enumType, isMatch, /*hasElse=*/true);

      builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
      auto thenValue = createIntConstant(builder, location, enumType, static_cast<int64_t>(index));
      mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{thenValue});

      builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
      auto elseValue = emitDispatch(index + 1);
      mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{elseValue});

      builder.setInsertionPointAfter(ifOp);
      return ifOp.getResult(0);
    };

    auto nullPtr = mlir::LLVM::ZeroOp::create(builder, location, ptrType);
    auto hasString = mlir::LLVM::ICmpOp::create(builder, location, mlir::LLVM::ICmpPredicate::ne,
                                                variantName, nullPtr);
    auto decodeIf =
        mlir::scf::IfOp::create(builder, location, enumType, hasString, /*hasElse=*/true);

    builder.setInsertionPointToStart(&decodeIf.getThenRegion().front());
    auto decoded = emitDispatch(0);
    mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{decoded});

    builder.setInsertionPointToStart(&decodeIf.getElseRegion().front());
    emitDecodePanic(/*freeVariantString=*/false);
    mlir::scf::YieldOp::create(builder, location,
                               mlir::ValueRange{createIntConstant(builder, location, enumType, 0)});

    builder.setInsertionPointAfter(decodeIf);
    hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                               mlir::SymbolRefAttr::get(&context, rtStringFree),
                               mlir::ValueRange{variantName});
    hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                               mlir::SymbolRefAttr::get(&context, rtFree),
                               mlir::ValueRange{parsed});
    mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{decodeIf.getResult(0)});
    builder.restoreInsertionPoint(savedIP);
    return;
  }

  if (decl.kind != ast::WireDeclKind::Struct)
    return;

  auto structType = structTypes.at(declName).mlirType;

  auto savedIP = builder.saveInsertionPoint();
  builder.setInsertionPointToEnd(module.getBody());
  std::string fnName = declName + "_from_" + format.str();
  if (auto existing = module.lookupSymbol<mlir::func::FuncOp>(fnName))
    existing.erase();
  auto fn = mlir::func::FuncOp::create(builder, location, fnName,
                                       mlir::FunctionType::get(&context, {ptrType}, {structType}));
  auto *entry = fn.addEntryBlock();
  builder.setInsertionPointToStart(entry);

  // Runtime function names
  std::string rtParse = "hew_" + format.str() + "_parse";
  std::string rtGetField = "hew_" + format.str() + "_get_field";
  std::string rtGetBool = "hew_" + format.str() + "_get_bool";
  std::string rtGetFloat = "hew_" + format.str() + "_get_float";
  std::string rtGetString = "hew_" + format.str() + "_get_string";
  std::string rtGetBytes = "hew_" + format.str() + "_get_bytes";
  std::string rtGetInt = "hew_" + format.str() + "_get_int";
  std::string rtFree = "hew_" + format.str() + "_free";

  // obj = hew_{format}_parse(str)
  auto objPtr = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                           mlir::SymbolRefAttr::get(&context, rtParse),
                                           mlir::ValueRange{entry->getArgument(0)})
                    .getResult();

  mlir::Value result = mlir::LLVM::UndefOp::create(builder, location, structType);

  unsigned idx = 0;
  for (const auto &field : decl.fields) {
    auto fieldName = wireSerialFieldName(field, fieldOverride(field), namingCase);
    auto keyPtr = wireStringPtr(location, fieldName);
    auto fieldJval = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                                mlir::SymbolRefAttr::get(&context, rtGetField),
                                                mlir::ValueRange{objPtr, keyPtr})
                         .getResult();

    mlir::Value decoded;
    auto jkind = jsonKindOf(field.ty);
    if (!isWirePrimitiveType(field.ty) && structTypes.count(field.ty)) {
      // Nested wire struct: stringify the child JSON node, then call T_from_{format}.
      std::string rtStringifyFn = "hew_" + format.str() + "_stringify";
      std::string rtStrFreeFn = "hew_" + format.str() + "_string_free";
      auto innerStr =
          hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                     mlir::SymbolRefAttr::get(&context, rtStringifyFn),
                                     mlir::ValueRange{fieldJval})
              .getResult();
      auto innerDecFn =
          module.lookupSymbol<mlir::func::FuncOp>(field.ty + "_from_" + format.str());
      decoded =
          mlir::func::CallOp::create(builder, location, innerDecFn, mlir::ValueRange{innerStr})
              .getResult(0);
      hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                 mlir::SymbolRefAttr::get(&context, rtStrFreeFn),
                                 mlir::ValueRange{innerStr});
    } else if (jkind == WireJsonKind::Bool) {
      decoded = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{i32Type},
                                           mlir::SymbolRefAttr::get(&context, rtGetBool),
                                           mlir::ValueRange{fieldJval})
                    .getResult();
    } else if (jkind == WireJsonKind::Float32) {
      auto f64v = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{f64Type},
                                             mlir::SymbolRefAttr::get(&context, rtGetFloat),
                                             mlir::ValueRange{fieldJval})
                      .getResult();
      decoded = mlir::arith::TruncFOp::create(builder, location, builder.getF32Type(), f64v);
    } else if (jkind == WireJsonKind::Float64) {
      decoded = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{f64Type},
                                           mlir::SymbolRefAttr::get(&context, rtGetFloat),
                                           mlir::ValueRange{fieldJval})
                    .getResult();
    } else if (jkind == WireJsonKind::String) {
      decoded = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                           mlir::SymbolRefAttr::get(&context, rtGetString),
                                           mlir::ValueRange{fieldJval})
                    .getResult();
    } else if (jkind == WireJsonKind::Bytes) {
      decoded = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                           mlir::SymbolRefAttr::get(&context, rtGetBytes),
                                           mlir::ValueRange{fieldJval})
                    .getResult();
    } else if (!isWirePrimitiveType(field.ty)) {
      // Unknown type: not a wire struct and not a known primitive — fail closed.
      ++errorCount_;
      emitError(location) << "wire struct '" << decl.name << "': field '" << field.name
                          << "' has unsupported type '" << field.ty
                          << "' for " << format.str() << " deserialization";
      return;
    } else {
      auto rawI64 = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{i64Type},
                                               mlir::SymbolRefAttr::get(&context, rtGetInt),
                                               mlir::ValueRange{fieldJval})
                       .getResult();
      if (auto bounds = wireFromSerialIntegerBounds(field.ty)) {
        auto minVal = createIntConstant(builder, location, i64Type, bounds->min);
        auto maxVal = createIntConstant(builder, location, i64Type, bounds->max);
        auto belowMin =
            mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::slt,
                                        rawI64, minVal);
        auto aboveMax =
            mlir::arith::CmpIOp::create(builder, location, mlir::arith::CmpIPredicate::sgt,
                                        rawI64, maxVal);
        auto outOfRange = mlir::arith::OrIOp::create(builder, location, belowMin, aboveMax);
        auto outOfRangeIf =
            mlir::scf::IfOp::create(builder, location, outOfRange, /*hasElse=*/false);
        builder.setInsertionPointToStart(&outOfRangeIf.getThenRegion().front());
        auto msgPtr = wireStringPtr(
            location, wireFromSerialIntegerDecodeErrorMessage(format, fieldName, field.ty, *bounds));
        hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                   mlir::SymbolRefAttr::get(&context, "hew_panic_msg"),
                                   mlir::ValueRange{msgPtr});
        builder.setInsertionPointAfter(outOfRangeIf);
      }
      auto fieldType = wireTypeToMLIR(builder, field.ty);
      decoded = (fieldType == i32Type)
                    ? mlir::arith::TruncIOp::create(builder, location, i32Type, rawI64).getResult()
                    : rawI64;
    }

    hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                               mlir::SymbolRefAttr::get(&context, rtFree),
                               mlir::ValueRange{fieldJval});

    result = mlir::LLVM::InsertValueOp::create(builder, location, result, decoded, idx++);
  }

  hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                             mlir::SymbolRefAttr::get(&context, rtFree), mlir::ValueRange{objPtr});

  mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{result});
  builder.restoreInsertionPoint(savedIP);
}

// ============================================================================
// Generate mangled method wrappers for wire types
// ============================================================================
//
// The old-style wire codegen produces functions like:
//   Point_to_json(x: i32, y: i32) -> ptr    (individual field args)
//   Point_from_json(ptr) -> struct           (returns struct)
//   Point_encode(x: i32, y: i32) -> ptr
//   Point_decode(ptr, i64) -> struct
//
// The struct method dispatch expects mangled names like:
//   _HT5PointF7to_json(struct) -> ptr        (takes struct as arg)
//   _HT5PointF9from_json(ptr) -> struct
//
// This function generates thin wrapper functions with mangled names that
// bridge between the two conventions:
//   Wire structs:
//     Instance methods (encode, to_json, to_yaml): extract fields, delegate
//     Static methods (decode, from_json, from_yaml): pass-through delegate
//   Unit wire enums:
//     Instance methods (to_json, to_yaml): pass-through delegate
//     Static methods (from_json, from_yaml): pass-through delegate
//
void MLIRGen::generateWireMethodWrappers(const ast::WireDecl &decl) {
  if (decl.kind != ast::WireDeclKind::Struct && decl.kind != ast::WireDeclKind::Enum)
    return;

  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  const auto &declName = decl.name;

  auto generatePassThroughInstanceWrapper = [&](llvm::StringRef methodName,
                                                const std::string &delegateName, mlir::Type selfType,
                                                mlir::Type resultType) {
    std::string mangledName = mangleName(currentModulePath, declName, methodName.str());

    auto savedIP = builder.saveInsertionPoint();
    builder.setInsertionPointToEnd(module.getBody());
    if (auto existing = module.lookupSymbol<mlir::func::FuncOp>(mangledName))
      existing.erase();

    auto wrapperType = mlir::FunctionType::get(&context, {selfType}, {resultType});
    auto wrapperFn = mlir::func::FuncOp::create(builder, location, mangledName, wrapperType);
    auto *entry = wrapperFn.addEntryBlock();
    builder.setInsertionPointToStart(entry);

    auto callee = module.lookupSymbol<mlir::func::FuncOp>(delegateName);
    auto callOp =
        mlir::func::CallOp::create(builder, location, callee, mlir::ValueRange{entry->getArgument(0)});
    mlir::func::ReturnOp::create(builder, location, callOp.getResults());
    builder.restoreInsertionPoint(savedIP);
  };

  auto generateStaticWrapper = [&](llvm::StringRef methodName, const std::string &delegateName,
                                   llvm::ArrayRef<mlir::Type> argTypes, mlir::Type resultType) {
    std::string mangledName = mangleName(currentModulePath, declName, methodName.str());

    auto savedIP = builder.saveInsertionPoint();
    builder.setInsertionPointToEnd(module.getBody());
    if (auto existing = module.lookupSymbol<mlir::func::FuncOp>(mangledName))
      existing.erase();

    auto wrapperType = mlir::FunctionType::get(&context, argTypes, {resultType});
    auto wrapperFn = mlir::func::FuncOp::create(builder, location, mangledName, wrapperType);
    auto *entry = wrapperFn.addEntryBlock();
    builder.setInsertionPointToStart(entry);

    llvm::SmallVector<mlir::Value, 4> args;
    for (unsigned i = 0; i < entry->getNumArguments(); ++i)
      args.push_back(entry->getArgument(i));

    auto callee = module.lookupSymbol<mlir::func::FuncOp>(delegateName);
    auto callOp = mlir::func::CallOp::create(builder, location, callee, args);
    mlir::func::ReturnOp::create(builder, location, callOp.getResults());
    builder.restoreInsertionPoint(savedIP);
  };

  if (decl.kind == ast::WireDeclKind::Enum) {
    auto enumIt = enumTypes.find(declName);
    if (enumIt == enumTypes.end() || enumIt->second.hasPayloads)
      return;
    auto enumType = enumIt->second.mlirType;
    generatePassThroughInstanceWrapper("to_json", declName + "_to_json", enumType, ptrType);
    generatePassThroughInstanceWrapper("to_yaml", declName + "_to_yaml", enumType, ptrType);
    generateStaticWrapper("from_json", declName + "_from_json", {ptrType}, enumType);
    generateStaticWrapper("from_yaml", declName + "_from_yaml", {ptrType}, enumType);
    return;
  }

  auto i64Type = builder.getI64Type();
  auto structType = structTypes.at(declName).mlirType;

  // Collect field types for extraction
  llvm::SmallVector<mlir::Type, 8> fieldTypes;
  for (const auto &field : decl.fields)
    fieldTypes.push_back(resolveWireFieldType(builder, field.ty, structTypes));

  // Helper: generate an instance method wrapper that takes a struct,
  // extracts fields, and calls the old-style per-field function.
  auto generateInstanceWrapper = [&](llvm::StringRef methodName, const std::string &delegateName,
                                     mlir::Type resultType) {
    std::string mangledName = mangleName(currentModulePath, declName, methodName.str());

    auto savedIP = builder.saveInsertionPoint();
    builder.setInsertionPointToEnd(module.getBody());
    if (auto existing = module.lookupSymbol<mlir::func::FuncOp>(mangledName))
      existing.erase();

    auto wrapperType = mlir::FunctionType::get(&context, {structType}, {resultType});
    auto wrapperFn = mlir::func::FuncOp::create(builder, location, mangledName, wrapperType);
    auto *entry = wrapperFn.addEntryBlock();
    builder.setInsertionPointToStart(entry);

    // Extract each field from the struct argument
    mlir::Value selfStruct = entry->getArgument(0);
    llvm::SmallVector<mlir::Value, 8> fieldArgs;
    for (unsigned i = 0; i < fieldTypes.size(); ++i)
      fieldArgs.push_back(mlir::LLVM::ExtractValueOp::create(builder, location, selfStruct, i));

    // Call the old-style function
    auto callee = module.lookupSymbol<mlir::func::FuncOp>(delegateName);
    auto callOp = mlir::func::CallOp::create(builder, location, callee, fieldArgs);
    mlir::func::ReturnOp::create(builder, location, callOp.getResults());
    builder.restoreInsertionPoint(savedIP);
  };

  // ── encode wrapper: struct -> bytes (HewVec*) ──────────────────────
  // Extracts fields, calls Foo_encode → HewWireBuf*, then converts to bytes.
  {
    std::string mangledName = mangleName(currentModulePath, declName, "encode");
    auto savedIP = builder.saveInsertionPoint();
    builder.setInsertionPointToEnd(module.getBody());
    if (auto existing = module.lookupSymbol<mlir::func::FuncOp>(mangledName))
      existing.erase();

    auto wrapperType = mlir::FunctionType::get(&context, {structType}, {ptrType});
    auto wrapperFn = mlir::func::FuncOp::create(builder, location, mangledName, wrapperType);
    auto *entry = wrapperFn.addEntryBlock();
    builder.setInsertionPointToStart(entry);

    // Extract fields and call Foo_encode → HewWireBuf*
    mlir::Value selfStruct = entry->getArgument(0);
    llvm::SmallVector<mlir::Value, 8> fieldArgs;
    for (unsigned i = 0; i < fieldTypes.size(); ++i)
      fieldArgs.push_back(mlir::LLVM::ExtractValueOp::create(builder, location, selfStruct, i));
    auto callee = module.lookupSymbol<mlir::func::FuncOp>(declName + "_encode");
    auto wireBuf = mlir::func::CallOp::create(builder, location, callee, fieldArgs).getResult(0);

    // Convert HewWireBuf* → bytes (HewVec*)
    auto bytesVec =
        hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                   mlir::SymbolRefAttr::get(&context, "hew_wire_buf_to_bytes"),
                                   mlir::ValueRange{wireBuf})
            .getResult();

    mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{bytesVec});
    builder.restoreInsertionPoint(savedIP);
  }

  // ── decode wrapper: bytes (HewVec*) -> struct ─────────────────────
  // Converts bytes to HewWireBuf*, extracts data/len, calls Foo_decode,
  // destroys the temp buf.
  {
    std::string mangledName = mangleName(currentModulePath, declName, "decode");
    auto savedIP = builder.saveInsertionPoint();
    builder.setInsertionPointToEnd(module.getBody());
    if (auto existing = module.lookupSymbol<mlir::func::FuncOp>(mangledName))
      existing.erase();

    auto wrapperType = mlir::FunctionType::get(&context, {ptrType}, {structType});
    auto wrapperFn = mlir::func::FuncOp::create(builder, location, mangledName, wrapperType);
    auto *entry = wrapperFn.addEntryBlock();
    builder.setInsertionPointToStart(entry);

    mlir::Value bytesVec = entry->getArgument(0);

    // Convert bytes → HewWireBuf*
    auto wireBuf =
        hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                   mlir::SymbolRefAttr::get(&context, "hew_wire_bytes_to_buf"),
                                   mlir::ValueRange{bytesVec})
            .getResult();

    // Extract data pointer and length from the wire buf
    auto dataPtr =
        hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                   mlir::SymbolRefAttr::get(&context, "hew_wire_buf_data"),
                                   mlir::ValueRange{wireBuf})
            .getResult();
    auto bufLen = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{i64Type},
                                             mlir::SymbolRefAttr::get(&context, "hew_wire_buf_len"),
                                             mlir::ValueRange{wireBuf})
                      .getResult();

    // Call Foo_decode(ptr, len) → struct
    auto decodeCallee = module.lookupSymbol<mlir::func::FuncOp>(declName + "_decode");
    auto decoded = mlir::func::CallOp::create(builder, location, decodeCallee,
                                              mlir::ValueRange{dataPtr, bufLen})
                       .getResult(0);

    // Free the temporary wire buffer. String fields are copied by
    // hew_wire_decode_string, and bytes fields are copied by
    // hew_vec_from_raw_bytes, so no use-after-free risk.
    hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                               mlir::SymbolRefAttr::get(&context, "hew_wire_buf_destroy"),
                               mlir::ValueRange{wireBuf});

    mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{decoded});
    builder.restoreInsertionPoint(savedIP);
  }

  // Instance method wrappers: struct -> result (to_json, to_yaml unchanged)
  generateInstanceWrapper("to_json", declName + "_to_json", ptrType);
  generateInstanceWrapper("to_yaml", declName + "_to_yaml", ptrType);

  // Static method wrappers: args -> struct (from_json, from_yaml unchanged)
  generateStaticWrapper("from_json", declName + "_from_json", {ptrType}, structType);
  generateStaticWrapper("from_yaml", declName + "_from_yaml", {ptrType}, structType);
}

// ============================================================================
// Struct-type serialization (auto-derived for Encode types)
// ============================================================================
//
// For regular struct types (not wire types), the type checker identifies which
// structs have all-encodable fields and registers to_json/from_json/etc methods.
// These functions generate the MLIR to implement those methods.

/// Classify an MLIR storage type for JSON serialization dispatch.
static WireJsonKind jsonKindOfMLIR(mlir::Type t) {
  if (t.isInteger(1))
    return WireJsonKind::Bool;
  if (t.isF32())
    return WireJsonKind::Float32;
  if (t.isF64())
    return WireJsonKind::Float64;
  if (mlir::isa<mlir::LLVM::LLVMPointerType>(t))
    return WireJsonKind::String; // strings are ptr at the LLVM level
  return WireJsonKind::Integer;  // i8, i16, i32, i64
}

void MLIRGen::generateStructToSerial(const std::string &typeName, llvm::StringRef format) {
  auto structIt = structTypes.find(typeName);
  if (structIt == structTypes.end())
    return;
  const auto &info = structIt->second;

  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();
  auto f64Type = builder.getF64Type();

  // Function: (struct) -> !llvm.ptr (string pointer)
  std::string mangledName = mangleName(currentModulePath, typeName, "to_" + format.str());

  auto savedIP = builder.saveInsertionPoint();
  builder.setInsertionPointToEnd(module.getBody());
  if (auto existing = module.lookupSymbol<mlir::func::FuncOp>(mangledName))
    existing.erase();

  auto fnType = mlir::FunctionType::get(&context, {info.mlirType}, {ptrType});
  auto fn = mlir::func::FuncOp::create(builder, location, mangledName, fnType);
  auto *entry = fn.addEntryBlock();
  builder.setInsertionPointToStart(entry);

  // Runtime function names — TOML uses "table" instead of "object"
  std::string objWord = (format == "toml") ? "table" : "object";
  std::string rtNew = "hew_" + format.str() + "_" + objWord + "_new";
  std::string rtSetBool = "hew_" + format.str() + "_" + objWord + "_set_bool";
  std::string rtSetFloat = "hew_" + format.str() + "_" + objWord + "_set_float";
  std::string rtSetString = "hew_" + format.str() + "_" + objWord + "_set_string";
  std::string rtSetInt = "hew_" + format.str() + "_" + objWord + "_set_int";
  std::string rtStringify = "hew_" + format.str() + "_stringify";
  std::string rtFree = "hew_" + format.str() + "_free";

  // obj = hew_{format}_{object|table}_new()
  auto objPtr =
      hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                 mlir::SymbolRefAttr::get(&context, rtNew), mlir::ValueRange{})
          .getResult();

  mlir::Value selfStruct = entry->getArgument(0);

  for (const auto &field : info.fields) {
    auto keyPtr = wireStringPtr(location, field.name);
    mlir::Value fv = mlir::LLVM::ExtractValueOp::create(builder, location, selfStruct, field.index);

    auto jkind = jsonKindOfMLIR(field.type);
    if (jkind == WireJsonKind::Bool) {
      // Bool is i1 in Hew, needs extending to i32 for the FFI
      mlir::Value i32v = mlir::arith::ExtUIOp::create(builder, location, i32Type, fv);
      hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                 mlir::SymbolRefAttr::get(&context, rtSetBool),
                                 mlir::ValueRange{objPtr, keyPtr, i32v});
    } else if (jkind == WireJsonKind::Float32) {
      mlir::Value f64v = mlir::arith::ExtFOp::create(builder, location, f64Type, fv);
      hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                 mlir::SymbolRefAttr::get(&context, rtSetFloat),
                                 mlir::ValueRange{objPtr, keyPtr, f64v});
    } else if (jkind == WireJsonKind::Float64) {
      hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                 mlir::SymbolRefAttr::get(&context, rtSetFloat),
                                 mlir::ValueRange{objPtr, keyPtr, fv});
    } else if (jkind == WireJsonKind::String) {
      hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                 mlir::SymbolRefAttr::get(&context, rtSetString),
                                 mlir::ValueRange{objPtr, keyPtr, fv});
    } else {
      // Integer types: sign-extend to i64 if needed
      mlir::Value v64 = fv;
      if (fv.getType() != i64Type)
        v64 = mlir::arith::ExtSIOp::create(builder, location, i64Type, fv);
      hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                 mlir::SymbolRefAttr::get(&context, rtSetInt),
                                 mlir::ValueRange{objPtr, keyPtr, v64});
    }
  }

  // result = hew_{format}_stringify(obj); hew_{format}_free(obj)
  auto resultPtr = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                              mlir::SymbolRefAttr::get(&context, rtStringify),
                                              mlir::ValueRange{objPtr})
                       .getResult();
  hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                             mlir::SymbolRefAttr::get(&context, rtFree), mlir::ValueRange{objPtr});

  mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{resultPtr});
  builder.restoreInsertionPoint(savedIP);
}

void MLIRGen::generateStructFromSerial(const std::string &typeName, llvm::StringRef format) {
  auto structIt = structTypes.find(typeName);
  if (structIt == structTypes.end())
    return;
  const auto &info = structIt->second;

  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();
  auto f64Type = builder.getF64Type();
  auto strRefType = hew::StringRefType::get(&context);

  // Function: (!llvm.ptr) -> !hew.result<struct, !hew.string_ref>
  // Returns Ok(struct) on success, Err(message) if the input is not valid.
  auto resultEnumType = hew::ResultEnumType::get(&context, info.mlirType, strRefType);
  std::string mangledName = mangleName(currentModulePath, typeName, "from_" + format.str());

  auto savedIP = builder.saveInsertionPoint();
  builder.setInsertionPointToEnd(module.getBody());
  if (auto existing = module.lookupSymbol<mlir::func::FuncOp>(mangledName))
    existing.erase();

  auto fnType = mlir::FunctionType::get(&context, {ptrType}, {resultEnumType});
  auto fn = mlir::func::FuncOp::create(builder, location, mangledName, fnType);
  auto *entry = fn.addEntryBlock();
  builder.setInsertionPointToStart(entry);

  // Runtime function names
  std::string rtParse = "hew_" + format.str() + "_parse";
  std::string rtGetField = "hew_" + format.str() + "_get_field";
  std::string rtGetBool = "hew_" + format.str() + "_get_bool";
  std::string rtGetFloat = "hew_" + format.str() + "_get_float";
  std::string rtGetString = "hew_" + format.str() + "_get_string";
  std::string rtGetInt = "hew_" + format.str() + "_get_int";
  std::string rtFree = "hew_" + format.str() + "_free";

  // obj = hew_{format}_parse(str) — returns null on invalid input
  auto objPtr = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                           mlir::SymbolRefAttr::get(&context, rtParse),
                                           mlir::ValueRange{entry->getArgument(0)})
                    .getResult();

  // Branch on null: null → Err("failed to parse ..."), non-null → Ok(struct)
  auto nullPtr = mlir::LLVM::ZeroOp::create(builder, location, ptrType);
  auto isNull = mlir::LLVM::ICmpOp::create(builder, location, mlir::LLVM::ICmpPredicate::eq,
                                           objPtr, nullPtr);
  auto ifOp = mlir::scf::IfOp::create(builder, location, resultEnumType, isNull,
                                       /*withElseRegion=*/true);

  // Then branch (null / parse failed): return Err("failed to parse {format}: invalid input")
  builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
  {
    auto errSym = getOrCreateGlobalString("failed to parse " + format.str() + ": invalid input");
    auto errStrRef =
        hew::ConstantOp::create(builder, location, strRefType, builder.getStringAttr(errSym))
            .getResult();
    auto errResult = hew::EnumConstructOp::create(
        builder, location, resultEnumType, /*variantIndex=*/1u,
        llvm::StringRef("__Result"), mlir::ValueRange{errStrRef},
        /*payload_positions=*/builder.getI64ArrayAttr({2}));
    mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{errResult});
  }

  // Else branch (non-null / parse succeeded): extract fields, return Ok(struct).
  //
  // Uses a recursive chain of scf::IfOp nodes (one per field) that each yield
  // the final resultEnumType.  Avoids mutable flag storage: if a field's
  // get_field call returns null (missing key / wrong container kind) that
  // branch immediately frees the document and returns Err; the else branch
  // decodes the scalar, frees the field node, and delegates to the next field.
  // Once all fields succeed, the innermost branch builds Ok(struct).
  //
  // The pattern mirrors the wire enum serial dispatch at the top of this file.
  builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
  {
    auto emitFieldErr = [&](llvm::StringRef fieldName) -> mlir::Value {
      // Free the document, then return Err("missing field: <name>").
      hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                 mlir::SymbolRefAttr::get(&context, rtFree),
                                 mlir::ValueRange{objPtr});
      auto errSym = getOrCreateGlobalString("failed to parse " + format.str() +
                                            ": missing field: " + fieldName.str());
      auto errStrRef =
          hew::ConstantOp::create(builder, location, strRefType, builder.getStringAttr(errSym))
              .getResult();
      return hew::EnumConstructOp::create(builder, location, resultEnumType, /*variantIndex=*/1u,
                                          llvm::StringRef("__Result"),
                                          mlir::ValueRange{errStrRef},
                                          builder.getI64ArrayAttr({2}));
    };

    // Recursive lambda: processes fields[fieldIdx..] given a partial struct
    // value; returns a resultEnumType SSA value (Ok or Err).
    std::function<mlir::Value(size_t, mlir::Value)> processFields =
        [&](size_t fieldIdx, mlir::Value currentStruct) -> mlir::Value {
      if (fieldIdx >= info.fields.size()) {
        // All fields decoded successfully.
        hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                   mlir::SymbolRefAttr::get(&context, rtFree),
                                   mlir::ValueRange{objPtr});
        return hew::EnumConstructOp::create(builder, location, resultEnumType,
                                            /*variantIndex=*/0u, llvm::StringRef("__Result"),
                                            mlir::ValueRange{currentStruct},
                                            builder.getI64ArrayAttr({1}));
      }

      const auto &field = info.fields[fieldIdx];
      auto keyPtr = wireStringPtr(location, field.name);
      auto fieldJval =
          hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                     mlir::SymbolRefAttr::get(&context, rtGetField),
                                     mlir::ValueRange{objPtr, keyPtr})
              .getResult();

      // If get_field returned null the key was missing or the document is not
      // an object — treat as a structural error.
      auto isFieldNull =
          mlir::LLVM::ICmpOp::create(builder, location, mlir::LLVM::ICmpPredicate::eq, fieldJval,
                                     nullPtr);
      auto fieldIfOp = mlir::scf::IfOp::create(builder, location, resultEnumType, isFieldNull,
                                                /*withElseRegion=*/true);

      // Then: field missing — free doc and return Err.
      builder.setInsertionPointToStart(&fieldIfOp.getThenRegion().front());
      mlir::scf::YieldOp::create(builder, location,
                                 mlir::ValueRange{emitFieldErr(field.name)});

      // Else: field present — decode scalar, free field node, recurse.
      //
      // Fail-closed coverage:
      //   - Missing field (get_field returned null): handled above via emitFieldErr.
      //   - String field with wrong type (get_string returns null): handled below
      //     via an explicit null check on the returned pointer.
      //   - Non-string scalar field with wrong type: get_int/get_bool/get_float
      //     return 0/false/0.0 on a null or wrong-typed node.  These cases
      //     silently coerce; catching them would require per-format type-code
      //     checks and is deferred as a separate lane.
      builder.setInsertionPointToStart(&fieldIfOp.getElseRegion().front());
      auto jkind = jsonKindOfMLIR(field.type);

      if (jkind == WireJsonKind::String) {
        // String case is handled entirely here: the getter returns null when the
        // field is present but is not a string — we must not put a null pointer
        // into the struct or we risk a crash downstream.  Both branches (wrong
        // type and correct type) manage free/insert/recurse themselves so we
        // can return early without hitting the common scalar path below.
        auto strPtr = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{ptrType},
                                                 mlir::SymbolRefAttr::get(&context, rtGetString),
                                                 mlir::ValueRange{fieldJval})
                          .getResult();
        auto strIsNull =
            mlir::LLVM::ICmpOp::create(builder, location, mlir::LLVM::ICmpPredicate::eq, strPtr,
                                       nullPtr);
        auto strTypeIfOp = mlir::scf::IfOp::create(builder, location, resultEnumType, strIsNull,
                                                    /*withElseRegion=*/true);

        // Then: wrong type — free field node, free document, return Err.
        builder.setInsertionPointToStart(&strTypeIfOp.getThenRegion().front());
        hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                   mlir::SymbolRefAttr::get(&context, rtFree),
                                   mlir::ValueRange{fieldJval});
        hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                   mlir::SymbolRefAttr::get(&context, rtFree),
                                   mlir::ValueRange{objPtr});
        {
          auto errSym = getOrCreateGlobalString(
              "failed to parse " + format.str() + ": field '" + field.name + "' is not a string");
          auto errStrRef =
              hew::ConstantOp::create(builder, location, strRefType,
                                      builder.getStringAttr(errSym))
                  .getResult();
          auto errVal = hew::EnumConstructOp::create(
              builder, location, resultEnumType, /*variantIndex=*/1u,
              llvm::StringRef("__Result"), mlir::ValueRange{errStrRef},
              builder.getI64ArrayAttr({2}));
          mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{errVal});
        }

        // Else: correct type — free field node, insert, recurse.
        builder.setInsertionPointToStart(&strTypeIfOp.getElseRegion().front());
        hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                   mlir::SymbolRefAttr::get(&context, rtFree),
                                   mlir::ValueRange{fieldJval});
        {
          auto nextStruct = mlir::LLVM::InsertValueOp::create(builder, location, currentStruct,
                                                              strPtr, field.index);
          auto innerResult = processFields(fieldIdx + 1, nextStruct);
          mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{innerResult});
        }

        builder.setInsertionPointAfter(strTypeIfOp);
        mlir::scf::YieldOp::create(builder, location,
                                   mlir::ValueRange{strTypeIfOp.getResult(0)});
        builder.setInsertionPointAfter(fieldIfOp);
        return fieldIfOp.getResult(0);
      }

      // Non-string scalars: decode via getter (returns 0/false/0.0 on null or
      // wrong-typed input — the coercion gap noted above).
      mlir::Value decoded;
      if (jkind == WireJsonKind::Bool) {
        auto raw = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{i32Type},
                                              mlir::SymbolRefAttr::get(&context, rtGetBool),
                                              mlir::ValueRange{fieldJval})
                       .getResult();
        decoded = mlir::arith::TruncIOp::create(builder, location, builder.getI1Type(), raw);
      } else if (jkind == WireJsonKind::Float32) {
        auto f64v = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{f64Type},
                                               mlir::SymbolRefAttr::get(&context, rtGetFloat),
                                               mlir::ValueRange{fieldJval})
                        .getResult();
        decoded = mlir::arith::TruncFOp::create(builder, location, builder.getF32Type(), f64v);
      } else if (jkind == WireJsonKind::Float64) {
        decoded = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{f64Type},
                                             mlir::SymbolRefAttr::get(&context, rtGetFloat),
                                             mlir::ValueRange{fieldJval})
                      .getResult();
      } else {
        auto rawI64 = hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{i64Type},
                                                 mlir::SymbolRefAttr::get(&context, rtGetInt),
                                                 mlir::ValueRange{fieldJval})
                          .getResult();
        if (field.type != i64Type)
          decoded =
              mlir::arith::TruncIOp::create(builder, location, field.type, rawI64).getResult();
        else
          decoded = rawI64;
      }
      hew::RuntimeCallOp::create(builder, location, mlir::TypeRange{},
                                 mlir::SymbolRefAttr::get(&context, rtFree),
                                 mlir::ValueRange{fieldJval});

      auto nextStruct = mlir::LLVM::InsertValueOp::create(builder, location, currentStruct,
                                                          decoded, field.index);
      auto innerResult = processFields(fieldIdx + 1, nextStruct);
      mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{innerResult});

      builder.setInsertionPointAfter(fieldIfOp);
      return fieldIfOp.getResult(0);
    };

    mlir::Value initialStruct = mlir::LLVM::UndefOp::create(builder, location, info.mlirType);
    mlir::Value finalResult = processFields(0, initialStruct);
    mlir::scf::YieldOp::create(builder, location, mlir::ValueRange{finalResult});
  }

  builder.setInsertionPointAfter(ifOp);
  mlir::func::ReturnOp::create(builder, location, mlir::ValueRange{ifOp.getResult(0)});
  builder.restoreInsertionPoint(savedIP);
}

void MLIRGen::generateStructEncodeWrappers(const std::string &typeName) {
  // Generate to_json/from_json, to_yaml/from_yaml, to_toml/from_toml
  for (const auto &format : {"json", "yaml", "toml"}) {
    generateStructToSerial(typeName, format);
    generateStructFromSerial(typeName, format);
  }
}

} // namespace hew
