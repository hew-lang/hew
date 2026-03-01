//===- MLIRGenWire.cpp - Wire type codegen for Hew MLIRGen ----------------===//
//
// Generates encode/decode functions for wire struct declarations.
// Each `wire struct Foo { ... }` produces:
//   - Foo_encode(fields...) -> !llvm.ptr  (returns heap-owned wire buffer pointer)
//   - Foo_decode(!llvm.ptr, i64) -> struct  (decodes from buffer)
//   - Foo_to_json(fields...) -> !llvm.ptr  (returns malloc'd JSON string)
//   - Foo_from_json(!llvm.ptr) -> struct   (parses JSON string)
//   - Foo_to_yaml(fields...) -> !llvm.ptr  (returns malloc'd YAML string)
//   - Foo_from_yaml(!llvm.ptr) -> struct   (parses YAML string)
//
//===----------------------------------------------------------------------===//

#include "hew/mlir/HewOps.h"
#include "hew/mlir/MLIRGen.h"
#include "MLIRGenHelpers.h"

#include "mlir/Dialect/Arith/IR/Arith.h"
#include "mlir/Dialect/Func/IR/FuncOps.h"
#include "mlir/Dialect/LLVMIR/LLVMDialect.h"
#include "mlir/IR/Builders.h"
#include "mlir/IR/BuiltinTypes.h"

#include <cctype>

namespace hew {

/// Classifies a wire type for JSON/YAML serialization and encode/decode dispatch.
/// Resolves the semantic kind from the type name string.
enum class WireJsonKind { Bool, Float32, Float64, String, Integer };

static WireJsonKind jsonKindOf(const std::string &ty) {
  if (ty == "bool")
    return WireJsonKind::Bool;
  if (ty == "f32")
    return WireJsonKind::Float32;
  if (ty == "f64")
    return WireJsonKind::Float64;
  if (ty == "String" || ty == "bytes")
    return WireJsonKind::String;
  return WireJsonKind::Integer;
}

/// Map a wire type name to the MLIR type used for the field value.
static mlir::Type wireTypeToMLIR(mlir::OpBuilder &builder, const std::string &ty) {
  auto ptrType = mlir::LLVM::LLVMPointerType::get(builder.getContext());
  if (ty == "i64" || ty == "u64")
    return builder.getI64Type();
  if (ty == "f32")
    return builder.getF32Type();
  if (ty == "f64")
    return builder.getF64Type();
  if (ty == "String" || ty == "bytes")
    return ptrType;
  return builder.getI32Type(); // i32, u32, i16, u16, i8, u8, bool
}

/// Return the runtime encode function name for a wire field type.
static std::string encodeFunc(const std::string &ty) {
  if (ty == "f32")
    return "hew_wire_encode_field_fixed32";
  if (ty == "f64")
    return "hew_wire_encode_field_fixed64";
  if (ty == "String")
    return "hew_wire_encode_field_string";
  if (ty == "bytes")
    return "hew_wire_encode_field_bytes";
  return "hew_wire_encode_field_varint";
}

/// Check if a wire type is a signed integer needing zigzag encoding.
static bool needsZigzag(const std::string &ty) {
  return ty == "i8" || ty == "i16" || ty == "i32" || ty == "i64";
}

static bool isUnsignedWireType(const std::string &ty) {
  return ty == "u8" || ty == "u16" || ty == "u32" || ty == "u64";
}

/// Check if a wire type uses a varint encoding.
static bool isVarintType(const std::string &ty) {
  return ty == "bool" || ty == "u8" || ty == "u16" || ty == "u32" || ty == "u64" || ty == "i8" ||
         ty == "i16" || ty == "i32" || ty == "i64";
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

/// Load a global string as an !llvm.ptr suitable for C ABI calls.
/// Returns an !llvm.ptr pointing to the NUL-terminated string data.
mlir::Value MLIRGen::wireStringPtr(mlir::Location location, llvm::StringRef value) {
  auto sym = getOrCreateGlobalString(value);
  auto strRefType = hew::StringRefType::get(&context);
  auto strRef =
      builder.create<hew::ConstantOp>(location, strRefType, builder.getStringAttr(sym)).getResult();
  return builder
      .create<hew::BitcastOp>(location, mlir::LLVM::LLVMPointerType::get(&context), strRef)
      .getResult();
}

// ============================================================================
// Wire struct/enum declaration
// ============================================================================

void MLIRGen::generateWireDecl(const ast::WireDecl &decl) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();

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
    }
    enumTypes[enumName] = std::move(info);

    return;
  }

  if (decl.kind != ast::WireDeclKind::Struct)
    return; // Unknown wire decl kind

  // ── Register the wire struct as a regular struct type ─────────────
  // This allows the rest of the compiler to work with the struct.
  const auto &declName = decl.name;
  StructTypeInfo info;
  info.name = declName;
  llvm::SmallVector<mlir::Type, 8> fieldTypes;
  unsigned fieldIdx = 0;
  for (const auto &field : decl.fields) {
    auto mlirTy = wireTypeToMLIR(builder, field.ty);
    info.fields.push_back({field.name, mlirTy, mlirTy, fieldIdx, ""});
    fieldTypes.push_back(mlirTy);
    ++fieldIdx;
  }
  info.mlirType = mlir::LLVM::LLVMStructType::getIdentified(&context, declName);
  if (!info.mlirType.isInitialized())
    (void)info.mlirType.setBody(fieldTypes, /*isPacked=*/false);
  structTypes[declName] = info;

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
    auto encodeFn = builder.create<mlir::func::FuncOp>(location, encodeName, encodeFnType);
    auto *entryBlock = encodeFn.addEntryBlock();
    builder.setInsertionPointToStart(entryBlock);

    // Allocate a heap-owned wire buffer via runtime helper.
    auto bufPtr =
        builder
            .create<hew::RuntimeCallOp>(location, mlir::TypeRange{ptrType},
                                        mlir::SymbolRefAttr::get(&context, "hew_wire_buf_new"),
                                        mlir::ValueRange{})
            .getResult();

    // Encode each field
    unsigned encIdx = 0;
    for (const auto &field : decl.fields) {
      mlir::Value fieldVal = entryBlock->getArgument(encIdx);
      auto tagVal = createIntConstant(builder, location, i32Type, field.field_number);
      std::string funcName = encodeFunc(field.ty);

      auto jkind = jsonKindOf(field.ty);
      if (jkind == WireJsonKind::String) {
        // hew_wire_encode_field_string(buf, tag, str_ptr)
        builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{i32Type},
                                           mlir::SymbolRefAttr::get(&context, funcName),
                                           mlir::ValueRange{bufPtr, tagVal, fieldVal});
      } else if (jkind == WireJsonKind::Float32) {
        // hew_wire_encode_field_fixed32(buf, tag, bitcast_to_i32)
        auto asI32 = builder.create<mlir::arith::BitcastOp>(location, i32Type, fieldVal);
        builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{i32Type},
                                           mlir::SymbolRefAttr::get(&context, funcName),
                                           mlir::ValueRange{bufPtr, tagVal, asI32});
      } else if (jkind == WireJsonKind::Float64) {
        // hew_wire_encode_field_fixed64(buf, tag, bitcast_to_i64)
        auto asI64 = builder.create<mlir::arith::BitcastOp>(location, i64Type, fieldVal);
        builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{i32Type},
                                           mlir::SymbolRefAttr::get(&context, funcName),
                                           mlir::ValueRange{bufPtr, tagVal, asI64});
      } else if (isVarintType(field.ty)) {
        // For varint types, extend to i64 for the runtime call
        mlir::Value valI64 = fieldVal;
        if (needsZigzag(field.ty)) {
          // Sign-extend to i64 first, then zigzag encode
          if (fieldVal.getType() == i32Type)
            valI64 = builder.create<mlir::arith::ExtSIOp>(location, i64Type, fieldVal);
          valI64 = builder
                       .create<hew::RuntimeCallOp>(
                           location, mlir::TypeRange{i64Type},
                           mlir::SymbolRefAttr::get(&context, "hew_wire_zigzag_encode"),
                           mlir::ValueRange{valI64})
                       .getResult();
        } else {
          // Zero-extend unsigned to i64
          if (fieldVal.getType() == i32Type)
            valI64 = builder.create<mlir::arith::ExtUIOp>(location, i64Type, fieldVal);
        }
        builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{i32Type},
                                           mlir::SymbolRefAttr::get(&context, funcName),
                                           mlir::ValueRange{bufPtr, tagVal, valI64});
      }
      ++encIdx;
    }

    // Return the buffer pointer
    builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{bufPtr});
    builder.restoreInsertionPoint(savedIP);
  }

  // ── Generate Foo_decode function ─────────────────────────────────
  // Signature: Foo_decode(!llvm.ptr, i64) -> struct_type
  // Takes a buffer pointer and size, returns a struct with decoded fields.
  {
    auto structType = info.mlirType;
    auto decodeFnType = mlir::FunctionType::get(&context, {ptrType, i64Type}, {structType});
    std::string decodeName = declName + "_decode";

    auto savedIP = builder.saveInsertionPoint();
    builder.setInsertionPointToEnd(module.getBody());
    if (auto existing = module.lookupSymbol<mlir::func::FuncOp>(decodeName))
      existing.erase();
    auto decodeFn = builder.create<mlir::func::FuncOp>(location, decodeName, decodeFnType);
    auto *entryBlock = decodeFn.addEntryBlock();
    builder.setInsertionPointToStart(entryBlock);

    auto dataPtr = entryBlock->getArgument(0);
    auto dataSize = entryBlock->getArgument(1);

    // Allocate a hew_wire_buf on the stack (32 bytes: { ptr, i64, i64, i64 })
    auto bufPtr = builder.create<mlir::LLVM::AllocaOp>(
        location, ptrType, builder.getI8Type(), createIntConstant(builder, location, i64Type, 32));

    // Initialize buffer for reading from existing data
    builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{},
                                       mlir::SymbolRefAttr::get(&context, "hew_wire_buf_init_read"),
                                       mlir::ValueRange{bufPtr, dataPtr, dataSize});

    // Create the result struct initialized with defaults
    mlir::Value result = builder.create<mlir::LLVM::UndefOp>(location, structType);

    // Allocate scratch slots for decode out-params (hoisted before loop).
    auto one = createIntConstant(builder, location, i64Type, 1);
    auto scratchI64 = builder.create<mlir::LLVM::AllocaOp>(location, ptrType, i64Type, one);
    auto scratchI32 = builder.create<mlir::LLVM::AllocaOp>(location, ptrType, i32Type, one);
    auto scratchPtr = builder.create<mlir::LLVM::AllocaOp>(location, ptrType, ptrType, one);
    auto scratchLen = builder.create<mlir::LLVM::AllocaOp>(location, ptrType, i64Type, one);

    unsigned decIdx = 0;
    for (const auto &field : decl.fields) {
      auto fieldType = fieldTypes[decIdx];

      // Skip the tag varint
      builder.create<hew::RuntimeCallOp>(
          location, mlir::TypeRange{i32Type},
          mlir::SymbolRefAttr::get(&context, "hew_wire_decode_varint"),
          mlir::ValueRange{bufPtr, scratchI64});

      mlir::Value decoded;
      auto jkind = jsonKindOf(field.ty);
      if (jkind == WireJsonKind::Float32) {
        // Decode fixed32 → bitcast to f32
        builder.create<hew::RuntimeCallOp>(
            location, mlir::TypeRange{i32Type},
            mlir::SymbolRefAttr::get(&context, "hew_wire_decode_fixed32"),
            mlir::ValueRange{bufPtr, scratchI32});
        auto rawI32 = builder.create<mlir::LLVM::LoadOp>(location, i32Type, scratchI32);
        decoded = builder.create<mlir::arith::BitcastOp>(location, builder.getF32Type(), rawI32);
      } else if (jkind == WireJsonKind::Float64) {
        // Decode fixed64 → bitcast to f64
        builder.create<hew::RuntimeCallOp>(
            location, mlir::TypeRange{i32Type},
            mlir::SymbolRefAttr::get(&context, "hew_wire_decode_fixed64"),
            mlir::ValueRange{bufPtr, scratchI64});
        auto rawI64 = builder.create<mlir::LLVM::LoadOp>(location, i64Type, scratchI64);
        decoded = builder.create<mlir::arith::BitcastOp>(location, builder.getF64Type(), rawI64);
      } else if (jkind == WireJsonKind::String) {
        // Decode length-delimited bytes via hew_wire_decode_bytes(buf, &out_ptr, &out_len)
        builder.create<hew::RuntimeCallOp>(
            location, mlir::TypeRange{i32Type},
            mlir::SymbolRefAttr::get(&context, "hew_wire_decode_bytes"),
            mlir::ValueRange{bufPtr, scratchPtr, scratchLen});
        // The returned pointer points into the wire buffer data
        decoded = builder.create<mlir::LLVM::LoadOp>(location, ptrType, scratchPtr);
      } else {
        // Varint types (bool, u8-u64, i8-i64)
        builder.create<hew::RuntimeCallOp>(
            location, mlir::TypeRange{i32Type},
            mlir::SymbolRefAttr::get(&context, "hew_wire_decode_varint"),
            mlir::ValueRange{bufPtr, scratchI64});
        auto rawI64 = builder.create<mlir::LLVM::LoadOp>(location, i64Type, scratchI64);
        mlir::Value val = rawI64;

        if (needsZigzag(field.ty)) {
          val = builder
                    .create<hew::RuntimeCallOp>(
                        location, mlir::TypeRange{i64Type},
                        mlir::SymbolRefAttr::get(&context, "hew_wire_zigzag_decode"),
                        mlir::ValueRange{rawI64})
                    .getResult();
        }

        // Truncate to field type if needed
        if (fieldType == i32Type) {
          decoded = builder.create<mlir::arith::TruncIOp>(location, i32Type, val);
        } else {
          decoded = val;
        }
      }

      result = builder.create<mlir::LLVM::InsertValueOp>(location, result, decoded, decIdx);
      ++decIdx;
    }

    builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{result});
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
}

// ============================================================================
// Unified Foo_to_{json,yaml} generation
// ============================================================================

void MLIRGen::generateWireToSerial(
    const ast::WireDecl &decl, llvm::StringRef format,
    const std::optional<ast::NamingCase> &namingCase,
    llvm::function_ref<const std::optional<std::string> &(const ast::WireFieldDecl &)>
        fieldOverride) {
  if (decl.kind != ast::WireDeclKind::Struct)
    return;
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();
  auto f64Type = builder.getF64Type();

  const auto &declName = decl.name;

  llvm::SmallVector<mlir::Type, 8> paramTypes;
  for (const auto &field : decl.fields)
    paramTypes.push_back(wireTypeToMLIR(builder, field.ty));

  auto savedIP = builder.saveInsertionPoint();
  builder.setInsertionPointToEnd(module.getBody());
  std::string fnName = declName + "_to_" + format.str();
  if (auto existing = module.lookupSymbol<mlir::func::FuncOp>(fnName))
    existing.erase();
  auto fn = builder.create<mlir::func::FuncOp>(
      location, fnName, mlir::FunctionType::get(&context, paramTypes, {ptrType}));
  auto *entry = fn.addEntryBlock();
  builder.setInsertionPointToStart(entry);

  // Runtime function names: hew_{format}_object_new, hew_{format}_object_set_*, etc.
  std::string rtNew = "hew_" + format.str() + "_object_new";
  std::string rtSetBool = "hew_" + format.str() + "_object_set_bool";
  std::string rtSetFloat = "hew_" + format.str() + "_object_set_float";
  std::string rtSetString = "hew_" + format.str() + "_object_set_string";
  std::string rtSetInt = "hew_" + format.str() + "_object_set_int";
  std::string rtStringify = "hew_" + format.str() + "_stringify";
  std::string rtFree = "hew_" + format.str() + "_free";

  // obj = hew_{format}_object_new()
  auto objPtr = builder
                    .create<hew::RuntimeCallOp>(location, mlir::TypeRange{ptrType},
                                                mlir::SymbolRefAttr::get(&context, rtNew),
                                                mlir::ValueRange{})
                    .getResult();

  unsigned idx = 0;
  for (const auto &field : decl.fields) {
    auto keyPtr = wireStringPtr(location, wireSerialFieldName(field, fieldOverride(field), namingCase));
    mlir::Value fv = entry->getArgument(idx++);

    auto jkind = jsonKindOf(field.ty);
    if (jkind == WireJsonKind::Bool) {
      builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{},
                                         mlir::SymbolRefAttr::get(&context, rtSetBool),
                                         mlir::ValueRange{objPtr, keyPtr, fv});
    } else if (jkind == WireJsonKind::Float32) {
      auto f64v = builder.create<mlir::arith::ExtFOp>(location, f64Type, fv);
      builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{},
                                         mlir::SymbolRefAttr::get(&context, rtSetFloat),
                                         mlir::ValueRange{objPtr, keyPtr, f64v});
    } else if (jkind == WireJsonKind::Float64) {
      builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{},
                                         mlir::SymbolRefAttr::get(&context, rtSetFloat),
                                         mlir::ValueRange{objPtr, keyPtr, fv});
    } else if (jkind == WireJsonKind::String) {
      builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{},
                                         mlir::SymbolRefAttr::get(&context, rtSetString),
                                         mlir::ValueRange{objPtr, keyPtr, fv});
    } else {
      // Integer types: extend to i64 (zero-extend unsigned, sign-extend signed)
      mlir::Value v64 = fv;
      if (fv.getType() == i32Type) {
        if (isUnsignedWireType(field.ty))
          v64 = builder.create<mlir::arith::ExtUIOp>(location, i64Type, fv);
        else
          v64 = builder.create<mlir::arith::ExtSIOp>(location, i64Type, fv);
      }
      builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{},
                                         mlir::SymbolRefAttr::get(&context, rtSetInt),
                                         mlir::ValueRange{objPtr, keyPtr, v64});
    }
  }

  // result = hew_{format}_stringify(obj); hew_{format}_free(obj)
  auto resultPtr = builder
                       .create<hew::RuntimeCallOp>(location, mlir::TypeRange{ptrType},
                                                   mlir::SymbolRefAttr::get(&context, rtStringify),
                                                   mlir::ValueRange{objPtr})
                       .getResult();
  builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{},
                                     mlir::SymbolRefAttr::get(&context, rtFree),
                                     mlir::ValueRange{objPtr});

  builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{resultPtr});
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
  if (decl.kind != ast::WireDeclKind::Struct)
    return;
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();
  auto f64Type = builder.getF64Type();

  const auto &declName = decl.name;
  auto structType = structTypes.at(declName).mlirType;

  auto savedIP = builder.saveInsertionPoint();
  builder.setInsertionPointToEnd(module.getBody());
  std::string fnName = declName + "_from_" + format.str();
  if (auto existing = module.lookupSymbol<mlir::func::FuncOp>(fnName))
    existing.erase();
  auto fn = builder.create<mlir::func::FuncOp>(
      location, fnName, mlir::FunctionType::get(&context, {ptrType}, {structType}));
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

  // obj = hew_{format}_parse(str)
  auto objPtr = builder
                    .create<hew::RuntimeCallOp>(location, mlir::TypeRange{ptrType},
                                                mlir::SymbolRefAttr::get(&context, rtParse),
                                                mlir::ValueRange{entry->getArgument(0)})
                    .getResult();

  mlir::Value result = builder.create<mlir::LLVM::UndefOp>(location, structType);

  unsigned idx = 0;
  for (const auto &field : decl.fields) {
    auto keyPtr =
        wireStringPtr(location, wireSerialFieldName(field, fieldOverride(field), namingCase));
    auto fieldJval = builder
                         .create<hew::RuntimeCallOp>(location, mlir::TypeRange{ptrType},
                                                     mlir::SymbolRefAttr::get(&context, rtGetField),
                                                     mlir::ValueRange{objPtr, keyPtr})
                         .getResult();

    mlir::Value decoded;
    auto jkind = jsonKindOf(field.ty);
    if (jkind == WireJsonKind::Bool) {
      decoded = builder
                    .create<hew::RuntimeCallOp>(location, mlir::TypeRange{i32Type},
                                                mlir::SymbolRefAttr::get(&context, rtGetBool),
                                                mlir::ValueRange{fieldJval})
                    .getResult();
    } else if (jkind == WireJsonKind::Float32) {
      auto f64v = builder
                      .create<hew::RuntimeCallOp>(location, mlir::TypeRange{f64Type},
                                                  mlir::SymbolRefAttr::get(&context, rtGetFloat),
                                                  mlir::ValueRange{fieldJval})
                      .getResult();
      decoded = builder.create<mlir::arith::TruncFOp>(location, builder.getF32Type(), f64v);
    } else if (jkind == WireJsonKind::Float64) {
      decoded = builder
                    .create<hew::RuntimeCallOp>(location, mlir::TypeRange{f64Type},
                                                mlir::SymbolRefAttr::get(&context, rtGetFloat),
                                                mlir::ValueRange{fieldJval})
                    .getResult();
    } else if (jkind == WireJsonKind::String) {
      decoded = builder
                    .create<hew::RuntimeCallOp>(location, mlir::TypeRange{ptrType},
                                                mlir::SymbolRefAttr::get(&context, rtGetString),
                                                mlir::ValueRange{fieldJval})
                    .getResult();
    } else {
      auto rawI64 = builder
                        .create<hew::RuntimeCallOp>(location, mlir::TypeRange{i64Type},
                                                    mlir::SymbolRefAttr::get(&context, rtGetInt),
                                                    mlir::ValueRange{fieldJval})
                        .getResult();
      auto fieldType = wireTypeToMLIR(builder, field.ty);
      decoded = (fieldType == i32Type)
                    ? builder.create<mlir::arith::TruncIOp>(location, i32Type, rawI64).getResult()
                    : rawI64;
    }

    builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{},
                                       mlir::SymbolRefAttr::get(&context, rtFree),
                                       mlir::ValueRange{fieldJval});

    result = builder.create<mlir::LLVM::InsertValueOp>(location, result, decoded, idx++);
  }

  builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{},
                                     mlir::SymbolRefAttr::get(&context, rtFree),
                                     mlir::ValueRange{objPtr});

  builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{result});
  builder.restoreInsertionPoint(savedIP);
}

} // namespace hew
