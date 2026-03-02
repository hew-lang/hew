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
#include "mlir/Dialect/SCF/IR/SCF.h"
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

  // ── Generate Foo_decode function (TLV-based) ─────────────────────
  // Signature: Foo_decode(!llvm.ptr, i64) -> struct_type
  // Takes a buffer pointer and size, returns a struct with decoded fields.
  // Uses TLV dispatch loop for forward compatibility — unknown fields are
  // silently skipped, fields can appear in any order.
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

    // Allocate per-field storage initialized with defaults.
    // We use individual allocas so the loop body can store decoded values.
    auto one = createIntConstant(builder, location, i64Type, 1);
    llvm::SmallVector<mlir::Value, 8> fieldSlots;
    for (unsigned i = 0; i < decl.fields.size(); ++i) {
      auto fty = fieldTypes[i];
      auto slot = builder.create<mlir::LLVM::AllocaOp>(location, ptrType, fty, one);
      // Initialize with default: 0 for integers/floats, null for pointers
      mlir::Value defaultVal;
      if (fty == ptrType)
        defaultVal = builder.create<mlir::LLVM::ZeroOp>(location, ptrType);
      else if (fty == builder.getF32Type())
        defaultVal = builder.create<mlir::arith::ConstantOp>(
            location, builder.getF32FloatAttr(0.0f));
      else if (fty == builder.getF64Type())
        defaultVal = builder.create<mlir::arith::ConstantOp>(
            location, builder.getF64FloatAttr(0.0));
      else
        defaultVal = createIntConstant(builder, location, fty, 0);
      builder.create<mlir::LLVM::StoreOp>(location, defaultVal, slot);
      fieldSlots.push_back(slot);
    }

    // Scratch slots for decode out-params
    auto scratchI64 = builder.create<mlir::LLVM::AllocaOp>(location, ptrType, i64Type, one);
    auto scratchI32 = builder.create<mlir::LLVM::AllocaOp>(location, ptrType, i32Type, one);
    auto scratchPtr = builder.create<mlir::LLVM::AllocaOp>(location, ptrType, ptrType, one);
    auto scratchLen = builder.create<mlir::LLVM::AllocaOp>(location, ptrType, i64Type, one);
    auto scratchFieldNum = builder.create<mlir::LLVM::AllocaOp>(location, ptrType, i32Type, one);
    auto scratchWireType = builder.create<mlir::LLVM::AllocaOp>(location, ptrType, i32Type, one);

    // TLV dispatch loop: while(buf has remaining data)
    auto whileOp =
        builder.create<mlir::scf::WhileOp>(location, mlir::TypeRange{}, mlir::ValueRange{});

    // "before" block: check if buffer has remaining data
    auto *beforeBlock = builder.createBlock(&whileOp.getBefore());
    builder.setInsertionPointToStart(beforeBlock);
    auto hasData = builder
                       .create<hew::RuntimeCallOp>(
                           location, mlir::TypeRange{i32Type},
                           mlir::SymbolRefAttr::get(&context, "hew_wire_buf_has_remaining"),
                           mlir::ValueRange{bufPtr})
                       .getResult();
    auto cond = builder.create<mlir::arith::CmpIOp>(
        location, mlir::arith::CmpIPredicate::ne, hasData,
        createIntConstant(builder, location, i32Type, 0));
    builder.create<mlir::scf::ConditionOp>(location, cond, mlir::ValueRange{});

    // "after" block: decode tag, dispatch by field number
    auto *afterBlock = builder.createBlock(&whileOp.getAfter());
    builder.setInsertionPointToStart(afterBlock);

    // Decode the tag: hew_wire_decode_tag(buf, &field_num, &wire_type)
    builder.create<hew::RuntimeCallOp>(
        location, mlir::TypeRange{i32Type},
        mlir::SymbolRefAttr::get(&context, "hew_wire_decode_tag"),
        mlir::ValueRange{bufPtr, scratchFieldNum, scratchWireType});
    auto fieldNum = builder.create<mlir::LLVM::LoadOp>(location, i32Type, scratchFieldNum);
    auto wireType = builder.create<mlir::LLVM::LoadOp>(location, i32Type, scratchWireType);

    // Dispatch by field number using chained if-else.
    // Each known field number decodes the value and stores it.
    // Unknown fields are skipped via hew_wire_skip_field.
    for (unsigned i = 0; i < decl.fields.size(); ++i) {
      const auto &field = decl.fields[i];
      auto fty = fieldTypes[i];
      auto fieldNumConst = createIntConstant(builder, location, i32Type, field.field_number);
      auto isMatch = builder.create<mlir::arith::CmpIOp>(
          location, mlir::arith::CmpIPredicate::eq, fieldNum, fieldNumConst);

      auto ifOp = builder.create<mlir::scf::IfOp>(location, isMatch, /*hasElse=*/false);
      builder.setInsertionPointToStart(&ifOp.getThenRegion().front());

      // Decode the field value based on type
      mlir::Value decoded;
      auto jkind = jsonKindOf(field.ty);
      if (jkind == WireJsonKind::Float32) {
        builder.create<hew::RuntimeCallOp>(
            location, mlir::TypeRange{i32Type},
            mlir::SymbolRefAttr::get(&context, "hew_wire_decode_fixed32"),
            mlir::ValueRange{bufPtr, scratchI32});
        auto rawI32 = builder.create<mlir::LLVM::LoadOp>(location, i32Type, scratchI32);
        decoded = builder.create<mlir::arith::BitcastOp>(location, builder.getF32Type(), rawI32);
      } else if (jkind == WireJsonKind::Float64) {
        builder.create<hew::RuntimeCallOp>(
            location, mlir::TypeRange{i32Type},
            mlir::SymbolRefAttr::get(&context, "hew_wire_decode_fixed64"),
            mlir::ValueRange{bufPtr, scratchI64});
        auto rawI64 = builder.create<mlir::LLVM::LoadOp>(location, i64Type, scratchI64);
        decoded = builder.create<mlir::arith::BitcastOp>(location, builder.getF64Type(), rawI64);
      } else if (jkind == WireJsonKind::String) {
        builder.create<hew::RuntimeCallOp>(
            location, mlir::TypeRange{i32Type},
            mlir::SymbolRefAttr::get(&context, "hew_wire_decode_bytes"),
            mlir::ValueRange{bufPtr, scratchPtr, scratchLen});
        decoded = builder.create<mlir::LLVM::LoadOp>(location, ptrType, scratchPtr);
      } else {
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
        decoded = (fty == i32Type)
                      ? builder.create<mlir::arith::TruncIOp>(location, i32Type, val).getResult()
                      : val;
      }
      builder.create<mlir::LLVM::StoreOp>(location, decoded, fieldSlots[i]);

      // Move insertion point after this if-op for the next field check
      builder.setInsertionPointAfter(ifOp);
    }

    // After all known field checks: if none matched, skip the unknown field
    // We detect "none matched" by checking that fieldNum didn't match any known number.
    // Build a compound OR of all match conditions by checking if we're still here
    // after all the if-ops (if any matched, we decoded + stored; the skip is harmless
    // since hew_wire_skip_field only advances read_pos for the remaining payload).
    // Simpler approach: always call skip for unknown fields. We track whether we
    // decoded by using a flag.
    //
    // Actually the simplest correct approach: build a single "matched" flag.
    // If none of the if-ops fired, we need to skip. We use alloca + store pattern.
    {
      // Check all field numbers again and skip if none matched.
      // This is O(N) comparisons but N is small for wire structs.
      mlir::Value anyMatch = createIntConstant(builder, location, i32Type, 0);
      for (unsigned i = 0; i < decl.fields.size(); ++i) {
        auto fieldNumConst = createIntConstant(builder, location, i32Type, decl.fields[i].field_number);
        auto isMatch = builder.create<mlir::arith::CmpIOp>(
            location, mlir::arith::CmpIPredicate::eq, fieldNum, fieldNumConst);
        auto isMatchI32 = builder.create<mlir::arith::ExtUIOp>(location, i32Type, isMatch);
        anyMatch = builder.create<mlir::arith::OrIOp>(location, anyMatch, isMatchI32);
      }
      auto noneMatched = builder.create<mlir::arith::CmpIOp>(
          location, mlir::arith::CmpIPredicate::eq, anyMatch,
          createIntConstant(builder, location, i32Type, 0));
      auto skipIf = builder.create<mlir::scf::IfOp>(location, noneMatched, /*hasElse=*/false);
      builder.setInsertionPointToStart(&skipIf.getThenRegion().front());
      builder.create<hew::RuntimeCallOp>(
          location, mlir::TypeRange{i32Type},
          mlir::SymbolRefAttr::get(&context, "hew_wire_skip_field"),
          mlir::ValueRange{bufPtr, wireType});
      builder.setInsertionPointAfter(skipIf);
    }

    builder.create<mlir::scf::YieldOp>(location);

    // After the loop: load all field values and build the result struct
    builder.setInsertionPointAfter(whileOp);
    mlir::Value result = builder.create<mlir::LLVM::UndefOp>(location, structType);
    for (unsigned i = 0; i < decl.fields.size(); ++i) {
      auto val = builder.create<mlir::LLVM::LoadOp>(location, fieldTypes[i], fieldSlots[i]);
      result = builder.create<mlir::LLVM::InsertValueOp>(location, result, val, i);
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
//   Instance methods (encode, to_json, to_yaml): extract fields, delegate
//   Static methods (decode, from_json, from_yaml): pass-through delegate
//
void MLIRGen::generateWireMethodWrappers(const ast::WireDecl &decl) {
  if (decl.kind != ast::WireDeclKind::Struct)
    return;

  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i64Type = builder.getI64Type();
  const auto &declName = decl.name;
  auto structType = structTypes.at(declName).mlirType;

  // Collect field types for extraction
  llvm::SmallVector<mlir::Type, 8> fieldTypes;
  for (const auto &field : decl.fields)
    fieldTypes.push_back(wireTypeToMLIR(builder, field.ty));

  // Helper: generate an instance method wrapper that takes a struct,
  // extracts fields, and calls the old-style per-field function.
  auto generateInstanceWrapper = [&](llvm::StringRef methodName,
                                     const std::string &delegateName,
                                     mlir::Type resultType) {
    std::string mangledName = mangleName(currentModulePath, declName, methodName.str());

    auto savedIP = builder.saveInsertionPoint();
    builder.setInsertionPointToEnd(module.getBody());
    if (auto existing = module.lookupSymbol<mlir::func::FuncOp>(mangledName))
      existing.erase();

    auto wrapperType = mlir::FunctionType::get(&context, {structType}, {resultType});
    auto wrapperFn = builder.create<mlir::func::FuncOp>(location, mangledName, wrapperType);
    auto *entry = wrapperFn.addEntryBlock();
    builder.setInsertionPointToStart(entry);

    // Extract each field from the struct argument
    mlir::Value selfStruct = entry->getArgument(0);
    llvm::SmallVector<mlir::Value, 8> fieldArgs;
    for (unsigned i = 0; i < fieldTypes.size(); ++i)
      fieldArgs.push_back(
          builder.create<mlir::LLVM::ExtractValueOp>(location, selfStruct, i));

    // Call the old-style function
    auto callee = module.lookupSymbol<mlir::func::FuncOp>(delegateName);
    auto callOp = builder.create<mlir::func::CallOp>(location, callee, fieldArgs);
    builder.create<mlir::func::ReturnOp>(location, callOp.getResults());
    builder.restoreInsertionPoint(savedIP);
  };

  // Helper: generate a static method wrapper that just delegates.
  auto generateStaticWrapper = [&](llvm::StringRef methodName,
                                   const std::string &delegateName,
                                   llvm::ArrayRef<mlir::Type> argTypes,
                                   mlir::Type resultType) {
    std::string mangledName = mangleName(currentModulePath, declName, methodName.str());

    auto savedIP = builder.saveInsertionPoint();
    builder.setInsertionPointToEnd(module.getBody());
    if (auto existing = module.lookupSymbol<mlir::func::FuncOp>(mangledName))
      existing.erase();

    auto wrapperType = mlir::FunctionType::get(&context, argTypes, {resultType});
    auto wrapperFn = builder.create<mlir::func::FuncOp>(location, mangledName, wrapperType);
    auto *entry = wrapperFn.addEntryBlock();
    builder.setInsertionPointToStart(entry);

    // Forward all arguments to the delegate function
    llvm::SmallVector<mlir::Value, 4> args;
    for (unsigned i = 0; i < entry->getNumArguments(); ++i)
      args.push_back(entry->getArgument(i));

    auto callee = module.lookupSymbol<mlir::func::FuncOp>(delegateName);
    auto callOp = builder.create<mlir::func::CallOp>(location, callee, args);
    builder.create<mlir::func::ReturnOp>(location, callOp.getResults());
    builder.restoreInsertionPoint(savedIP);
  };

  // Instance method wrappers: struct -> result
  generateInstanceWrapper("encode", declName + "_encode", ptrType);
  generateInstanceWrapper("to_json", declName + "_to_json", ptrType);
  generateInstanceWrapper("to_yaml", declName + "_to_yaml", ptrType);

  // Static method wrappers: args -> struct
  generateStaticWrapper("decode", declName + "_decode", {ptrType, i64Type}, structType);
  generateStaticWrapper("from_json", declName + "_from_json", {ptrType}, structType);
  generateStaticWrapper("from_yaml", declName + "_from_yaml", {ptrType}, structType);
}

} // namespace hew
