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
#include "mlir/Dialect/MemRef/IR/MemRef.h"
#include "mlir/Dialect/SCF/IR/SCF.h"
#include "mlir/IR/Builders.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/BuiltinTypes.h"

#include "llvm/Support/raw_ostream.h"

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

    std::string declName = decl.name;
    // Register variant names for lookup
    for (const auto &variant : info.variants) {
      variantLookup[variant.name] = {declName, variant.index};
    }
    enumTypes[declName] = std::move(info);

    return;
  }

  if (decl.kind != ast::WireDeclKind::Struct)
    return; // Unknown wire decl kind

  // ── Register the wire struct as a regular struct type ─────────────
  // This allows the rest of the compiler to work with the struct.
  std::string declName = decl.name;
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
    llvm::SmallVector<mlir::Type, 8> paramTypes;
    for (const auto &field : decl.fields)
      paramTypes.push_back(wireTypeToMLIR(builder, field.ty));

    auto encodeFnType = mlir::FunctionType::get(&context, paramTypes, {ptrType});
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

    // Decode each field sequentially (assumes fields encoded in tag order).
    // For each field: skip the TLV tag varint, then decode the value.
    // Allocate a scratch slot for varint/fixed out-params.
    auto scratchI64 = builder.create<mlir::LLVM::AllocaOp>(
        location, ptrType, i64Type, createIntConstant(builder, location, i64Type, 1));
    auto scratchI32 = builder.create<mlir::LLVM::AllocaOp>(
        location, ptrType, i32Type, createIntConstant(builder, location, i64Type, 1));

    unsigned decIdx = 0;
    for (const auto &field : decl.fields) {
      auto fieldType = wireTypeToMLIR(builder, field.ty);

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
        // Decode length-delimited bytes. We use scratch slots for out ptr and len.
        // scratchI64 holds the pointer-out, scratchI32 (recast) holds the len-out.
        // Actually, hew_wire_decode_bytes(buf, &out_ptr, &out_len) where both
        // out_ptr is void** and out_len is size_t*. Use two i64 scratch slots.
        auto scratchPtr = builder.create<mlir::LLVM::AllocaOp>(
            location, ptrType, ptrType, createIntConstant(builder, location, i64Type, 1));
        auto scratchLen = builder.create<mlir::LLVM::AllocaOp>(
            location, ptrType, i64Type, createIntConstant(builder, location, i64Type, 1));
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
  generateWireToJson(decl);
  generateWireFromJson(decl);
  generateWireToYaml(decl);
  generateWireFromYaml(decl);
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

/// Get the JSON key name for a wire field, honouring per-field json_name
/// overrides and falling back to the struct-level json_case convention.
static std::string jsonFieldName(const ast::WireFieldDecl &field,
                                 const std::optional<ast::NamingCase> &defCase) {
  if (field.json_name.has_value())
    return *field.json_name;
  if (defCase.has_value())
    return applyNamingCase(field.name, *defCase);
  return field.name;
}

/// Get the YAML key name for a wire field.
static std::string yamlFieldName(const ast::WireFieldDecl &field,
                                 const std::optional<ast::NamingCase> &defCase) {
  if (field.yaml_name.has_value())
    return *field.yaml_name;
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
// Foo_to_json / Foo_from_json
// ============================================================================

void MLIRGen::generateWireToJson(const ast::WireDecl &decl) {
  if (decl.kind != ast::WireDeclKind::Struct)
    return;
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();
  auto f64Type = builder.getF64Type();

  std::string declName = decl.name;
  const auto &jsonCase = decl.json_case;

  llvm::SmallVector<mlir::Type, 8> paramTypes;
  for (const auto &field : decl.fields)
    paramTypes.push_back(wireTypeToMLIR(builder, field.ty));

  auto savedIP = builder.saveInsertionPoint();
  builder.setInsertionPointToEnd(module.getBody());
  std::string toJsonName = declName + "_to_json";
  if (auto existing = module.lookupSymbol<mlir::func::FuncOp>(toJsonName))
    existing.erase();
  auto fn = builder.create<mlir::func::FuncOp>(
      location, toJsonName, mlir::FunctionType::get(&context, paramTypes, {ptrType}));
  auto *entry = fn.addEntryBlock();
  builder.setInsertionPointToStart(entry);

  // obj = hew_json_object_new()
  auto objPtr =
      builder
          .create<hew::RuntimeCallOp>(location, mlir::TypeRange{ptrType},
                                      mlir::SymbolRefAttr::get(&context, "hew_json_object_new"),
                                      mlir::ValueRange{})
          .getResult();

  unsigned idx = 0;
  for (const auto &field : decl.fields) {
    auto keyPtr = wireStringPtr(location, jsonFieldName(field, jsonCase));
    mlir::Value fv = entry->getArgument(idx++);

    if (jsonKindOf(field.ty) == WireJsonKind::Bool) {
      builder.create<hew::RuntimeCallOp>(
          location, mlir::TypeRange{},
          mlir::SymbolRefAttr::get(&context, "hew_json_object_set_bool"),
          mlir::ValueRange{objPtr, keyPtr, fv});
    } else if (jsonKindOf(field.ty) == WireJsonKind::Float32) {
      auto f64v = builder.create<mlir::arith::ExtFOp>(location, f64Type, fv);
      builder.create<hew::RuntimeCallOp>(
          location, mlir::TypeRange{},
          mlir::SymbolRefAttr::get(&context, "hew_json_object_set_float"),
          mlir::ValueRange{objPtr, keyPtr, f64v});
    } else if (jsonKindOf(field.ty) == WireJsonKind::Float64) {
      builder.create<hew::RuntimeCallOp>(
          location, mlir::TypeRange{},
          mlir::SymbolRefAttr::get(&context, "hew_json_object_set_float"),
          mlir::ValueRange{objPtr, keyPtr, fv});
    } else if (jsonKindOf(field.ty) == WireJsonKind::String) {
      builder.create<hew::RuntimeCallOp>(
          location, mlir::TypeRange{},
          mlir::SymbolRefAttr::get(&context, "hew_json_object_set_string"),
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
      builder.create<hew::RuntimeCallOp>(
          location, mlir::TypeRange{},
          mlir::SymbolRefAttr::get(&context, "hew_json_object_set_int"),
          mlir::ValueRange{objPtr, keyPtr, v64});
    }
  }

  // result = hew_json_stringify(obj); hew_json_free(obj)
  auto resultPtr =
      builder
          .create<hew::RuntimeCallOp>(location, mlir::TypeRange{ptrType},
                                      mlir::SymbolRefAttr::get(&context, "hew_json_stringify"),
                                      mlir::ValueRange{objPtr})
          .getResult();
  builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{},
                                     mlir::SymbolRefAttr::get(&context, "hew_json_free"),
                                     mlir::ValueRange{objPtr});

  builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{resultPtr});
  builder.restoreInsertionPoint(savedIP);
}

void MLIRGen::generateWireFromJson(const ast::WireDecl &decl) {
  if (decl.kind != ast::WireDeclKind::Struct)
    return;
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();
  auto f64Type = builder.getF64Type();

  std::string declName = decl.name;
  const auto &jsonCase = decl.json_case;
  auto structType = structTypes.at(declName).mlirType;

  auto savedIP = builder.saveInsertionPoint();
  builder.setInsertionPointToEnd(module.getBody());
  std::string fromJsonName = declName + "_from_json";
  if (auto existing = module.lookupSymbol<mlir::func::FuncOp>(fromJsonName))
    existing.erase();
  auto fn = builder.create<mlir::func::FuncOp>(
      location, fromJsonName, mlir::FunctionType::get(&context, {ptrType}, {structType}));
  auto *entry = fn.addEntryBlock();
  builder.setInsertionPointToStart(entry);

  // obj = hew_json_parse(json_str)
  auto objPtr =
      builder
          .create<hew::RuntimeCallOp>(location, mlir::TypeRange{ptrType},
                                      mlir::SymbolRefAttr::get(&context, "hew_json_parse"),
                                      mlir::ValueRange{entry->getArgument(0)})
          .getResult();

  mlir::Value result = builder.create<mlir::LLVM::UndefOp>(location, structType);

  unsigned idx = 0;
  for (const auto &field : decl.fields) {
    auto keyPtr = wireStringPtr(location, jsonFieldName(field, jsonCase));
    auto fieldJval =
        builder
            .create<hew::RuntimeCallOp>(location, mlir::TypeRange{ptrType},
                                        mlir::SymbolRefAttr::get(&context, "hew_json_get_field"),
                                        mlir::ValueRange{objPtr, keyPtr})
            .getResult();

    mlir::Value decoded;
    if (jsonKindOf(field.ty) == WireJsonKind::Bool) {
      decoded =
          builder
              .create<hew::RuntimeCallOp>(location, mlir::TypeRange{i32Type},
                                          mlir::SymbolRefAttr::get(&context, "hew_json_get_bool"),
                                          mlir::ValueRange{fieldJval})
              .getResult();
    } else if (jsonKindOf(field.ty) == WireJsonKind::Float32) {
      auto f64v =
          builder
              .create<hew::RuntimeCallOp>(location, mlir::TypeRange{f64Type},
                                          mlir::SymbolRefAttr::get(&context, "hew_json_get_float"),
                                          mlir::ValueRange{fieldJval})
              .getResult();
      decoded = builder.create<mlir::arith::TruncFOp>(location, builder.getF32Type(), f64v);
    } else if (jsonKindOf(field.ty) == WireJsonKind::Float64) {
      decoded =
          builder
              .create<hew::RuntimeCallOp>(location, mlir::TypeRange{f64Type},
                                          mlir::SymbolRefAttr::get(&context, "hew_json_get_float"),
                                          mlir::ValueRange{fieldJval})
              .getResult();
    } else if (jsonKindOf(field.ty) == WireJsonKind::String) {
      decoded =
          builder
              .create<hew::RuntimeCallOp>(location, mlir::TypeRange{ptrType},
                                          mlir::SymbolRefAttr::get(&context, "hew_json_get_string"),
                                          mlir::ValueRange{fieldJval})
              .getResult();
    } else {
      auto rawI64 =
          builder
              .create<hew::RuntimeCallOp>(location, mlir::TypeRange{i64Type},
                                          mlir::SymbolRefAttr::get(&context, "hew_json_get_int"),
                                          mlir::ValueRange{fieldJval})
              .getResult();
      auto fieldType = wireTypeToMLIR(builder, field.ty);
      decoded = (fieldType == i32Type)
                    ? builder.create<mlir::arith::TruncIOp>(location, i32Type, rawI64).getResult()
                    : rawI64;
    }

    builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{},
                                       mlir::SymbolRefAttr::get(&context, "hew_json_free"),
                                       mlir::ValueRange{fieldJval});

    result = builder.create<mlir::LLVM::InsertValueOp>(location, result, decoded, idx++);
  }

  builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{},
                                     mlir::SymbolRefAttr::get(&context, "hew_json_free"),
                                     mlir::ValueRange{objPtr});

  builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{result});
  builder.restoreInsertionPoint(savedIP);
}

// ============================================================================
// Foo_to_yaml / Foo_from_yaml
// ============================================================================

void MLIRGen::generateWireToYaml(const ast::WireDecl &decl) {
  if (decl.kind != ast::WireDeclKind::Struct)
    return;
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();
  auto f64Type = builder.getF64Type();

  std::string declName = decl.name;
  const auto &yamlCase = decl.yaml_case;

  llvm::SmallVector<mlir::Type, 8> paramTypes;
  for (const auto &field : decl.fields)
    paramTypes.push_back(wireTypeToMLIR(builder, field.ty));

  auto savedIP = builder.saveInsertionPoint();
  builder.setInsertionPointToEnd(module.getBody());
  std::string toYamlName = declName + "_to_yaml";
  if (auto existing = module.lookupSymbol<mlir::func::FuncOp>(toYamlName))
    existing.erase();
  auto fn = builder.create<mlir::func::FuncOp>(
      location, toYamlName, mlir::FunctionType::get(&context, paramTypes, {ptrType}));
  auto *entry = fn.addEntryBlock();
  builder.setInsertionPointToStart(entry);

  auto objPtr =
      builder
          .create<hew::RuntimeCallOp>(location, mlir::TypeRange{ptrType},
                                      mlir::SymbolRefAttr::get(&context, "hew_yaml_object_new"),
                                      mlir::ValueRange{})
          .getResult();

  unsigned idx = 0;
  for (const auto &field : decl.fields) {
    auto keyPtr = wireStringPtr(location, yamlFieldName(field, yamlCase));
    mlir::Value fv = entry->getArgument(idx++);

    if (jsonKindOf(field.ty) == WireJsonKind::Bool) {
      builder.create<hew::RuntimeCallOp>(
          location, mlir::TypeRange{},
          mlir::SymbolRefAttr::get(&context, "hew_yaml_object_set_bool"),
          mlir::ValueRange{objPtr, keyPtr, fv});
    } else if (jsonKindOf(field.ty) == WireJsonKind::Float32) {
      auto f64v = builder.create<mlir::arith::ExtFOp>(location, f64Type, fv);
      builder.create<hew::RuntimeCallOp>(
          location, mlir::TypeRange{},
          mlir::SymbolRefAttr::get(&context, "hew_yaml_object_set_float"),
          mlir::ValueRange{objPtr, keyPtr, f64v});
    } else if (jsonKindOf(field.ty) == WireJsonKind::Float64) {
      builder.create<hew::RuntimeCallOp>(
          location, mlir::TypeRange{},
          mlir::SymbolRefAttr::get(&context, "hew_yaml_object_set_float"),
          mlir::ValueRange{objPtr, keyPtr, fv});
    } else if (jsonKindOf(field.ty) == WireJsonKind::String) {
      builder.create<hew::RuntimeCallOp>(
          location, mlir::TypeRange{},
          mlir::SymbolRefAttr::get(&context, "hew_yaml_object_set_string"),
          mlir::ValueRange{objPtr, keyPtr, fv});
    } else {
      mlir::Value v64 = fv;
      if (fv.getType() == i32Type) {
        if (isUnsignedWireType(field.ty))
          v64 = builder.create<mlir::arith::ExtUIOp>(location, i64Type, fv);
        else
          v64 = builder.create<mlir::arith::ExtSIOp>(location, i64Type, fv);
      }
      builder.create<hew::RuntimeCallOp>(
          location, mlir::TypeRange{},
          mlir::SymbolRefAttr::get(&context, "hew_yaml_object_set_int"),
          mlir::ValueRange{objPtr, keyPtr, v64});
    }
  }

  auto resultPtr =
      builder
          .create<hew::RuntimeCallOp>(location, mlir::TypeRange{ptrType},
                                      mlir::SymbolRefAttr::get(&context, "hew_yaml_stringify"),
                                      mlir::ValueRange{objPtr})
          .getResult();
  builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{},
                                     mlir::SymbolRefAttr::get(&context, "hew_yaml_free"),
                                     mlir::ValueRange{objPtr});

  builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{resultPtr});
  builder.restoreInsertionPoint(savedIP);
}

void MLIRGen::generateWireFromYaml(const ast::WireDecl &decl) {
  if (decl.kind != ast::WireDeclKind::Struct)
    return;
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i32Type = builder.getI32Type();
  auto i64Type = builder.getI64Type();
  auto f64Type = builder.getF64Type();

  std::string declName = decl.name;
  const auto &yamlCase = decl.yaml_case;
  auto structType = structTypes.at(declName).mlirType;

  auto savedIP = builder.saveInsertionPoint();
  builder.setInsertionPointToEnd(module.getBody());
  std::string fromYamlName = declName + "_from_yaml";
  if (auto existing = module.lookupSymbol<mlir::func::FuncOp>(fromYamlName))
    existing.erase();
  auto fn = builder.create<mlir::func::FuncOp>(
      location, fromYamlName, mlir::FunctionType::get(&context, {ptrType}, {structType}));
  auto *entry = fn.addEntryBlock();
  builder.setInsertionPointToStart(entry);

  auto objPtr =
      builder
          .create<hew::RuntimeCallOp>(location, mlir::TypeRange{ptrType},
                                      mlir::SymbolRefAttr::get(&context, "hew_yaml_parse"),
                                      mlir::ValueRange{entry->getArgument(0)})
          .getResult();

  mlir::Value result = builder.create<mlir::LLVM::UndefOp>(location, structType);

  unsigned idx = 0;
  for (const auto &field : decl.fields) {
    auto keyPtr = wireStringPtr(location, yamlFieldName(field, yamlCase));
    auto fieldJval =
        builder
            .create<hew::RuntimeCallOp>(location, mlir::TypeRange{ptrType},
                                        mlir::SymbolRefAttr::get(&context, "hew_yaml_get_field"),
                                        mlir::ValueRange{objPtr, keyPtr})
            .getResult();

    mlir::Value decoded;
    if (jsonKindOf(field.ty) == WireJsonKind::Bool) {
      decoded =
          builder
              .create<hew::RuntimeCallOp>(location, mlir::TypeRange{i32Type},
                                          mlir::SymbolRefAttr::get(&context, "hew_yaml_get_bool"),
                                          mlir::ValueRange{fieldJval})
              .getResult();
    } else if (jsonKindOf(field.ty) == WireJsonKind::Float32) {
      auto f64v =
          builder
              .create<hew::RuntimeCallOp>(location, mlir::TypeRange{f64Type},
                                          mlir::SymbolRefAttr::get(&context, "hew_yaml_get_float"),
                                          mlir::ValueRange{fieldJval})
              .getResult();
      decoded = builder.create<mlir::arith::TruncFOp>(location, builder.getF32Type(), f64v);
    } else if (jsonKindOf(field.ty) == WireJsonKind::Float64) {
      decoded =
          builder
              .create<hew::RuntimeCallOp>(location, mlir::TypeRange{f64Type},
                                          mlir::SymbolRefAttr::get(&context, "hew_yaml_get_float"),
                                          mlir::ValueRange{fieldJval})
              .getResult();
    } else if (jsonKindOf(field.ty) == WireJsonKind::String) {
      decoded =
          builder
              .create<hew::RuntimeCallOp>(location, mlir::TypeRange{ptrType},
                                          mlir::SymbolRefAttr::get(&context, "hew_yaml_get_string"),
                                          mlir::ValueRange{fieldJval})
              .getResult();
    } else {
      auto rawI64 =
          builder
              .create<hew::RuntimeCallOp>(location, mlir::TypeRange{i64Type},
                                          mlir::SymbolRefAttr::get(&context, "hew_yaml_get_int"),
                                          mlir::ValueRange{fieldJval})
              .getResult();
      auto fieldType = wireTypeToMLIR(builder, field.ty);
      decoded = (fieldType == i32Type)
                    ? builder.create<mlir::arith::TruncIOp>(location, i32Type, rawI64).getResult()
                    : rawI64;
    }

    builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{},
                                       mlir::SymbolRefAttr::get(&context, "hew_yaml_free"),
                                       mlir::ValueRange{fieldJval});

    result = builder.create<mlir::LLVM::InsertValueOp>(location, result, decoded, idx++);
  }

  builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{},
                                     mlir::SymbolRefAttr::get(&context, "hew_yaml_free"),
                                     mlir::ValueRange{objPtr});

  builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{result});
  builder.restoreInsertionPoint(savedIP);
}

} // namespace hew
