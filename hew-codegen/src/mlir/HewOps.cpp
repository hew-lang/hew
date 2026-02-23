//===- HewOps.cpp - Hew MLIR dialect operation implementations ------------===//
//
// Implements verifiers and custom methods for Hew dialect operations.
//
//===----------------------------------------------------------------------===//

#include "hew/mlir/HewOps.h"
#include "hew/mlir/HewTypes.h"

#include "mlir/Dialect/LLVMIR/LLVMTypes.h"
#include "mlir/IR/Builders.h"
#include "mlir/IR/OpImplementation.h"

using namespace mlir;

//===----------------------------------------------------------------------===//
// ConstantOp — custom assembly format
//===----------------------------------------------------------------------===//
//
// Format:  hew.constant <value> : <type>
// e.g.:    %0 = hew.constant 42 : i64
//          %1 = hew.constant 3.14 : f64
//

/// Parse: hew.constant <value> : <type>
/// The value is printed without its type annotation; the `: <type>` suffix
/// provides the result (and thus the attribute) type.
mlir::ParseResult hew::ConstantOp::parse(OpAsmParser &parser, OperationState &result) {
  Attribute value;
  Type type;

  if (parser.parseAttribute(value) || parser.parseColon() || parser.parseType(type))
    return failure();

  result.addAttribute("value", value);
  result.addTypes(type);
  return success();
}

/// Print: hew.constant <value> : <type>
void hew::ConstantOp::print(OpAsmPrinter &printer) {
  printer << " ";
  printer.printAttributeWithoutType(getValue());
  printer << " : " << getType();
}

//===----------------------------------------------------------------------===//
// ConstantOp — verifier
//===----------------------------------------------------------------------===//

mlir::LogicalResult hew::ConstantOp::verify() {
  auto valueAttr = getValue();
  auto resultType = getResult().getType();

  // Integer attribute must match integer result type (including width)
  if (auto intAttr = llvm::dyn_cast<IntegerAttr>(valueAttr)) {
    if (!llvm::isa<IntegerType>(resultType)) {
      return emitOpError("integer constant must have integer result type, got ") << resultType;
    }
    if (intAttr.getType() != resultType) {
      return emitOpError("integer attribute type ")
             << intAttr.getType() << " does not match result type " << resultType;
    }
    return success();
  }

  // Float attribute must match float result type (including width)
  if (auto floatAttr = llvm::dyn_cast<FloatAttr>(valueAttr)) {
    if (!llvm::isa<FloatType>(resultType)) {
      return emitOpError("float constant must have float result type, got ") << resultType;
    }
    if (floatAttr.getType() != resultType) {
      return emitOpError("float attribute type ")
             << floatAttr.getType() << " does not match result type " << resultType;
    }
    return success();
  }

  // String attribute is allowed with any result type (for string_ref lowering)
  if (llvm::isa<StringAttr>(valueAttr)) {
    return success();
  }

  // For other attribute types, accept without further checking (extensible)
  return success();
}

//===----------------------------------------------------------------------===//
// ActorSpawnOp — verifier
//===----------------------------------------------------------------------===//

mlir::LogicalResult hew::ActorSpawnOp::verify() {
  auto stateType = getStateType();

  // state_type must be an LLVM struct type
  auto structType = llvm::dyn_cast<mlir::LLVM::LLVMStructType>(stateType);
  if (!structType) {
    return emitOpError("state_type must be an LLVM struct type, got ") << stateType;
  }

  // init_args count must match state struct field count
  auto numFields = structType.getBody().size();
  auto numArgs = getInitArgs().size();
  if (numArgs != numFields) {
    return emitOpError("init_args count (")
           << numArgs << ") does not match state_type field count (" << numFields << ")";
  }

  return success();
}

//===----------------------------------------------------------------------===//
// ActorSendOp — verifier
//===----------------------------------------------------------------------===//

mlir::LogicalResult hew::ActorSendOp::verify() {
  if (getMsgType() < 0) {
    return emitOpError("msg_type must be non-negative, got ") << getMsgType();
  }
  return success();
}

//===----------------------------------------------------------------------===//
// ActorAskOp — verifier
//===----------------------------------------------------------------------===//

mlir::LogicalResult hew::ActorAskOp::verify() {
  if (getMsgType() < 0) {
    return emitOpError("msg_type must be non-negative, got ") << getMsgType();
  }
  return success();
}

//===----------------------------------------------------------------------===//
// Collection op verifiers (Vec and HashMap)
//===----------------------------------------------------------------------===//

mlir::LogicalResult hew::VecNewOp::verify() {
  if (!llvm::isa<hew::VecType>(getResult().getType()))
    return emitOpError("result must be !hew.vec<T>, got ") << getResult().getType();
  return success();
}

mlir::LogicalResult hew::VecPushOp::verify() {
  auto vecType = llvm::dyn_cast<hew::VecType>(getVec().getType());
  if (!vecType)
    return emitOpError("vec operand must be !hew.vec<T>, got ") << getVec().getType();
  if (getValue().getType() != vecType.getElementType())
    return emitOpError("value type ") << getValue().getType() << " does not match vec element type "
                                      << vecType.getElementType();
  return success();
}

mlir::LogicalResult hew::VecGetOp::verify() {
  auto vecType = llvm::dyn_cast<hew::VecType>(getVec().getType());
  if (!vecType)
    return emitOpError("vec operand must be !hew.vec<T>, got ") << getVec().getType();
  if (getResult().getType() != vecType.getElementType())
    return emitOpError("result type ")
           << getResult().getType() << " does not match vec element type "
           << vecType.getElementType();
  return success();
}

mlir::LogicalResult hew::VecSetOp::verify() {
  auto vecType = llvm::dyn_cast<hew::VecType>(getVec().getType());
  if (!vecType)
    return emitOpError("vec operand must be !hew.vec<T>, got ") << getVec().getType();
  if (getValue().getType() != vecType.getElementType())
    return emitOpError("value type ") << getValue().getType() << " does not match vec element type "
                                      << vecType.getElementType();
  return success();
}

mlir::LogicalResult hew::VecLenOp::verify() {
  if (!llvm::isa<hew::VecType>(getVec().getType()))
    return emitOpError("vec operand must be !hew.vec<T>, got ") << getVec().getType();
  return success();
}

mlir::LogicalResult hew::VecPopOp::verify() {
  auto vecType = llvm::dyn_cast<hew::VecType>(getVec().getType());
  if (!vecType)
    return emitOpError("vec operand must be !hew.vec<T>, got ") << getVec().getType();
  if (getResult().getType() != vecType.getElementType())
    return emitOpError("result type ")
           << getResult().getType() << " does not match vec element type "
           << vecType.getElementType();
  return success();
}

mlir::LogicalResult hew::VecRemoveOp::verify() {
  auto vecType = llvm::dyn_cast<hew::VecType>(getVec().getType());
  if (!vecType)
    return emitOpError("vec operand must be !hew.vec<T>, got ") << getVec().getType();
  if (getValue().getType() != vecType.getElementType())
    return emitOpError("value type ") << getValue().getType() << " does not match vec element type "
                                      << vecType.getElementType();
  return success();
}

mlir::LogicalResult hew::HashMapNewOp::verify() {
  if (!llvm::isa<hew::HashMapType>(getResult().getType()))
    return emitOpError("result must be !hew.hashmap<K,V>, got ") << getResult().getType();
  return success();
}

mlir::LogicalResult hew::HashMapInsertOp::verify() {
  auto mapType = llvm::dyn_cast<hew::HashMapType>(getMap().getType());
  if (!mapType)
    return emitOpError("map operand must be !hew.hashmap<K,V>, got ") << getMap().getType();
  if (getKey().getType() != mapType.getKeyType())
    return emitOpError("key type ")
           << getKey().getType() << " does not match map key type " << mapType.getKeyType();
  if (getValue().getType() != mapType.getValueType())
    return emitOpError("value type ")
           << getValue().getType() << " does not match map value type " << mapType.getValueType();
  return success();
}

mlir::LogicalResult hew::HashMapGetOp::verify() {
  auto mapType = llvm::dyn_cast<hew::HashMapType>(getMap().getType());
  if (!mapType)
    return emitOpError("map operand must be !hew.hashmap<K,V>, got ") << getMap().getType();
  if (getKey().getType() != mapType.getKeyType())
    return emitOpError("key type ")
           << getKey().getType() << " does not match map key type " << mapType.getKeyType();
  if (getResult().getType() != mapType.getValueType())
    return emitOpError("result type ")
           << getResult().getType() << " does not match map value type " << mapType.getValueType();
  return success();
}

mlir::LogicalResult hew::HashMapContainsKeyOp::verify() {
  auto mapType = llvm::dyn_cast<hew::HashMapType>(getMap().getType());
  if (!mapType)
    return emitOpError("map operand must be !hew.hashmap<K,V>, got ") << getMap().getType();
  if (getKey().getType() != mapType.getKeyType())
    return emitOpError("key type ")
           << getKey().getType() << " does not match map key type " << mapType.getKeyType();
  return success();
}

mlir::LogicalResult hew::HashMapRemoveOp::verify() {
  auto mapType = llvm::dyn_cast<hew::HashMapType>(getMap().getType());
  if (!mapType)
    return emitOpError("map operand must be !hew.hashmap<K,V>, got ") << getMap().getType();
  if (getKey().getType() != mapType.getKeyType())
    return emitOpError("key type ")
           << getKey().getType() << " does not match map key type " << mapType.getKeyType();
  return success();
}

mlir::LogicalResult hew::HashMapLenOp::verify() {
  if (!llvm::isa<hew::HashMapType>(getMap().getType()))
    return emitOpError("map operand must be !hew.hashmap<K,V>, got ") << getMap().getType();
  return success();
}

mlir::LogicalResult hew::HashMapKeysOp::verify() {
  auto mapType = llvm::dyn_cast<hew::HashMapType>(getMap().getType());
  if (!mapType)
    return emitOpError("map operand must be !hew.hashmap<K,V>, got ") << getMap().getType();
  auto vecType = llvm::dyn_cast<hew::VecType>(getResult().getType());
  if (!vecType)
    return emitOpError("result must be !hew.vec<K>, got ") << getResult().getType();
  if (vecType.getElementType() != mapType.getKeyType())
    return emitOpError("result vec element type ")
           << vecType.getElementType() << " does not match map key type " << mapType.getKeyType();
  return success();
}

//===----------------------------------------------------------------------===//
// FieldGetOp — folder
//===----------------------------------------------------------------------===//

mlir::OpFoldResult hew::FieldGetOp::fold(FoldAdaptor adaptor) {
  (void)adaptor;

  const uint64_t fieldIndex = getFieldIndex();

  if (auto setOp = getStructVal().getDefiningOp<hew::FieldSetOp>()) {
    if (setOp.getFieldIndex() == fieldIndex)
      return setOp.getValue();
  }

  if (auto initOp = getStructVal().getDefiningOp<hew::StructInitOp>()) {
    auto fields = initOp.getFields();
    if (fieldIndex < fields.size())
      return fields[fieldIndex];
  }

  return nullptr;
}

//===----------------------------------------------------------------------===//
// TraitDispatchOp — custom assembly format
//===----------------------------------------------------------------------===//
//
// Format:
//   hew.trait_dispatch "Trait" "method"
//       data(%ptr) vtable(%vt) args(%a1, %a2)
//       method_index = 0
//       : (ptr_type, ptr_type, arg_types...) -> result_type
//

mlir::ParseResult hew::TraitDispatchOp::parse(OpAsmParser &parser, OperationState &result) {
  StringAttr traitName, methodName;
  OpAsmParser::UnresolvedOperand dataPtr, vtablePtr;
  SmallVector<OpAsmParser::UnresolvedOperand> extraArgs;
  IntegerAttr methodIndexAttr;
  SmallVector<Type> argTypes;
  SmallVector<Type> resultTypes;

  // Parse trait and method name strings
  if (parser.parseAttribute(traitName) || parser.parseAttribute(methodName))
    return failure();

  // Parse data(%ptr)
  if (parser.parseKeyword("data") || parser.parseLParen() || parser.parseOperand(dataPtr) ||
      parser.parseRParen())
    return failure();

  // Parse vtable(%vt)
  if (parser.parseKeyword("vtable") || parser.parseLParen() || parser.parseOperand(vtablePtr) ||
      parser.parseRParen())
    return failure();

  // Parse optional args(...)
  if (succeeded(parser.parseOptionalKeyword("args"))) {
    if (parser.parseLParen() || parser.parseOperandList(extraArgs) || parser.parseRParen())
      return failure();
  }

  // Parse method_index attribute
  if (parser.parseKeyword("method_index") || parser.parseEqual() ||
      parser.parseAttribute(methodIndexAttr))
    return failure();

  // Parse : (types...) -> result_type
  if (parser.parseColon() || parser.parseLParen() || parser.parseTypeList(argTypes) ||
      parser.parseRParen())
    return failure();
  if (succeeded(parser.parseOptionalArrow())) {
    Type resType;
    if (parser.parseType(resType))
      return failure();
    resultTypes.push_back(resType);
  }

  // Resolve operands
  if (argTypes.size() < 2)
    return parser.emitError(parser.getNameLoc(),
                            "expected at least 2 argument types (data, vtable)");
  if (parser.resolveOperand(dataPtr, argTypes[0], result.operands) ||
      parser.resolveOperand(vtablePtr, argTypes[1], result.operands))
    return failure();
  for (size_t i = 0; i < extraArgs.size(); ++i) {
    if (parser.resolveOperand(extraArgs[i], argTypes[i + 2], result.operands))
      return failure();
  }

  result.addAttribute("trait_name", traitName);
  result.addAttribute("method_name", methodName);
  result.addAttribute("method_index", methodIndexAttr);
  result.addTypes(resultTypes);
  return success();
}

void hew::TraitDispatchOp::print(OpAsmPrinter &printer) {
  printer << " " << getTraitName() << " " << getMethodName();
  printer << " data(" << getDataPtr() << ")";
  printer << " vtable(" << getVtablePtr() << ")";
  if (!getExtraArgs().empty()) {
    printer << " args(";
    llvm::interleaveComma(getExtraArgs(), printer);
    printer << ")";
  }
  printer << " method_index = " << getMethodIndex();

  // Print type signature
  printer << " : (";
  printer << getDataPtr().getType() << ", " << getVtablePtr().getType();
  for (auto arg : getExtraArgs())
    printer << ", " << arg.getType();
  printer << ")";
  if (getResult())
    printer << " -> " << getResult().getType();
}

//===----------------------------------------------------------------------===//
// EnumConstructOp — custom assembly format
//===----------------------------------------------------------------------===//
//
// Format:
//   hew.enum_construct variant <index> of <name> (<payloads>)
//       [payload_positions = [...]] : (types...) -> result_type
//

mlir::ParseResult hew::EnumConstructOp::parse(OpAsmParser &parser, OperationState &result) {
  IntegerAttr variantIndex;
  StringAttr enumName;
  SmallVector<OpAsmParser::UnresolvedOperand> payloads;
  SmallVector<Type> payloadTypes;
  Type resultType;

  // Parse: variant <index> of <name>
  if (parser.parseKeyword("variant") ||
      parser.parseAttribute(variantIndex, parser.getBuilder().getI32Type(), "variant_index",
                            result.attributes) ||
      parser.parseKeyword("of") || parser.parseAttribute(enumName, "enum_name", result.attributes))
    return failure();

  // Parse optional (payloads)
  if (succeeded(parser.parseOptionalLParen())) {
    if (parser.parseOperandList(payloads) || parser.parseRParen())
      return failure();
  }

  // Parse optional payload_positions attribute
  Attribute posAttr;
  if (succeeded(parser.parseOptionalKeyword("payload_positions"))) {
    if (parser.parseEqual() || parser.parseAttribute(posAttr))
      return failure();
    result.addAttribute("payload_positions", posAttr);
  }

  // Parse : (types...) -> result_type
  if (parser.parseColon())
    return failure();
  if (!payloads.empty()) {
    if (parser.parseLParen() || parser.parseTypeList(payloadTypes) || parser.parseRParen() ||
        parser.parseArrow())
      return failure();
  }
  if (parser.parseType(resultType))
    return failure();

  // Resolve operands
  for (size_t i = 0; i < payloads.size(); ++i) {
    if (parser.resolveOperand(payloads[i], payloadTypes[i], result.operands))
      return failure();
  }
  result.addTypes(resultType);
  return success();
}

void hew::EnumConstructOp::print(OpAsmPrinter &printer) {
  printer << " variant " << getVariantIndex() << " of " << getEnumName();
  if (!getPayloads().empty()) {
    printer << "(";
    llvm::interleaveComma(getPayloads(), printer);
    printer << ")";
  }
  if (auto pos = getPayloadPositions()) {
    printer << " payload_positions = " << *pos;
  }
  if (!getPayloads().empty()) {
    printer << " : (";
    llvm::interleaveComma(getPayloads().getTypes(), printer, [&](Type t) { printer << t; });
    printer << ") -> ";
  } else {
    printer << " : ";
  }
  printer << getResult().getType();
}

//===----------------------------------------------------------------------===//
// CastOp — folder
//===----------------------------------------------------------------------===//

mlir::OpFoldResult hew::CastOp::fold(FoldAdaptor adaptor) {
  // Identity cast: input type == result type
  if (getInput().getType() == getResult().getType())
    return getInput();

  auto inputAttr = adaptor.getInput();
  if (!inputAttr)
    return nullptr;

  auto resultType = getResult().getType();

  // Int to wider int (sign extend)
  if (auto intAttr = mlir::dyn_cast<mlir::IntegerAttr>(inputAttr)) {
    if (auto intResultType = mlir::dyn_cast<mlir::IntegerType>(resultType)) {
      auto val = intAttr.getValue().getSExtValue();
      return mlir::IntegerAttr::get(intResultType, val);
    }
    // Int to float
    if (auto floatResultType = mlir::dyn_cast<mlir::FloatType>(resultType)) {
      auto val = intAttr.getValue().getSExtValue();
      return mlir::FloatAttr::get(floatResultType, static_cast<double>(val));
    }
  }

  // Float to int
  if (auto floatAttr = mlir::dyn_cast<mlir::FloatAttr>(inputAttr)) {
    if (auto intResultType = mlir::dyn_cast<mlir::IntegerType>(resultType)) {
      auto val = static_cast<int64_t>(floatAttr.getValueAsDouble());
      return mlir::IntegerAttr::get(intResultType, val);
    }
  }

  return nullptr;
}

//===----------------------------------------------------------------------===//
// CastOp — canonicalization
//===----------------------------------------------------------------------===//

namespace {
struct FoldCastChain : public mlir::OpRewritePattern<hew::CastOp> {
  using OpRewritePattern::OpRewritePattern;

  mlir::LogicalResult matchAndRewrite(hew::CastOp op,
                                      mlir::PatternRewriter &rewriter) const override {
    // cast(cast(x, A), B) → cast(x, B)
    auto innerCast = op.getInput().getDefiningOp<hew::CastOp>();
    if (!innerCast)
      return mlir::failure();
    rewriter.replaceOpWithNewOp<hew::CastOp>(op, op.getResult().getType(), innerCast.getInput());
    return mlir::success();
  }
};
} // namespace

void hew::CastOp::getCanonicalizationPatterns(mlir::RewritePatternSet &results,
                                              mlir::MLIRContext *context) {
  results.add<FoldCastChain>(context);
}

//===----------------------------------------------------------------------===//
// StructInitOp — verifier
//===----------------------------------------------------------------------===//

mlir::LogicalResult hew::StructInitOp::verify() {
  auto resultType = llvm::dyn_cast<mlir::LLVM::LLVMStructType>(getResult().getType());
  if (!resultType) {
    return emitOpError("result must be an LLVM struct type, got ") << getResult().getType();
  }

  auto body = resultType.getBody();
  auto numFields = body.size();
  auto numOperands = getFields().size();
  if (numOperands != numFields) {
    return emitOpError("field count (")
           << numOperands << ") does not match struct field count (" << numFields << ")";
  }

  for (unsigned i = 0; i < numFields; ++i) {
    if (getFields()[i].getType() != body[i]) {
      return emitOpError("field ") << i << " type " << getFields()[i].getType()
                                   << " does not match struct field type " << body[i];
    }
  }

  return success();
}

//===----------------------------------------------------------------------===//
// FieldGetOp — verifier
//===----------------------------------------------------------------------===//

mlir::LogicalResult hew::FieldGetOp::verify() {
  auto structType = llvm::dyn_cast<mlir::LLVM::LLVMStructType>(getStructVal().getType());
  if (!structType) {
    return emitOpError("struct_val must be an LLVM struct type, got ") << getStructVal().getType();
  }

  auto body = structType.getBody();
  uint64_t idx = getFieldIndex();
  if (idx >= body.size()) {
    return emitOpError("field_index ")
           << idx << " is out of bounds for struct with " << body.size() << " fields";
  }

  if (getResult().getType() != body[idx]) {
    return emitOpError("result type ")
           << getResult().getType() << " does not match struct field type " << body[idx]
           << " at index " << idx;
  }

  return success();
}

//===----------------------------------------------------------------------===//
// FieldSetOp — verifier
//===----------------------------------------------------------------------===//

mlir::LogicalResult hew::FieldSetOp::verify() {
  auto structType = llvm::dyn_cast<mlir::LLVM::LLVMStructType>(getStructVal().getType());
  if (!structType) {
    return emitOpError("struct_val must be an LLVM struct type, got ") << getStructVal().getType();
  }

  auto body = structType.getBody();
  uint64_t idx = getFieldIndex();
  if (idx >= body.size()) {
    return emitOpError("field_index ")
           << idx << " is out of bounds for struct with " << body.size() << " fields";
  }

  if (getValue().getType() != body[idx]) {
    return emitOpError("value type ")
           << getValue().getType() << " does not match struct field type " << body[idx]
           << " at index " << idx;
  }

  if (getResult().getType() != getStructVal().getType()) {
    return emitOpError("result type ")
           << getResult().getType() << " does not match struct type " << getStructVal().getType();
  }

  return success();
}

//===----------------------------------------------------------------------===//
// EnumConstructOp — verifier
//===----------------------------------------------------------------------===//

mlir::LogicalResult hew::EnumConstructOp::verify() {
  if (getVariantIndex() < 0) {
    return emitOpError("variant_index must be non-negative, got ") << getVariantIndex();
  }

  auto structType = llvm::dyn_cast<mlir::LLVM::LLVMStructType>(getResult().getType());
  if (!structType)
    return success(); // bare i32 enum (all-unit variants) — no further checks

  auto body = structType.getBody();
  auto payloads = getPayloads();
  if (payloads.empty())
    return success();

  auto positionsAttr = getPayloadPositions();
  for (unsigned i = 0; i < payloads.size(); ++i) {
    int64_t pos;
    if (positionsAttr) {
      auto posArray = positionsAttr->getValue();
      if (i >= posArray.size()) {
        return emitOpError("payload_positions has fewer entries than payloads");
      }
      pos = llvm::cast<mlir::IntegerAttr>(posArray[i]).getInt();
    } else {
      pos = static_cast<int64_t>(i) + 1;
    }

    if (pos < 0 || static_cast<uint64_t>(pos) >= body.size()) {
      return emitOpError("payload position ")
             << pos << " is out of bounds for struct with " << body.size() << " fields";
    }

    if (payloads[i].getType() != body[pos]) {
      return emitOpError("payload ")
             << i << " type " << payloads[i].getType() << " does not match struct field type "
             << body[pos] << " at position " << pos;
    }
  }

  return success();
}

//===----------------------------------------------------------------------===//
// EnumExtractTagOp — verifier
//===----------------------------------------------------------------------===//

mlir::LogicalResult hew::EnumExtractTagOp::verify() {
  auto enumType = getEnumVal().getType();

  // Bare i32 (all-unit-variant enum) is allowed
  if (llvm::isa<mlir::IntegerType>(enumType)) {
    auto intType = llvm::cast<mlir::IntegerType>(enumType);
    if (intType.getWidth() == 32)
      return success();
    return emitOpError("integer enum_val must be i32, got ") << enumType;
  }

  // Hew dialect enum types (!hew.option<T>, !hew.result<T,E>) are valid —
  // they lower to LLVM structs with i32 tag at field 0
  if (llvm::isa<hew::OptionEnumType>(enumType) || llvm::isa<hew::ResultEnumType>(enumType))
    return success();

  // Otherwise must be LLVM struct with i32 first field
  auto structType = llvm::dyn_cast<mlir::LLVM::LLVMStructType>(enumType);
  if (!structType) {
    return emitOpError("enum_val must be i32, LLVM struct, !hew.option, or !hew.result type, got ")
           << enumType;
  }

  auto body = structType.getBody();
  if (body.empty()) {
    return emitOpError("enum struct type must have at least one field");
  }

  auto firstField = llvm::dyn_cast<mlir::IntegerType>(body[0]);
  if (!firstField || firstField.getWidth() != 32) {
    return emitOpError("first field of enum struct must be i32, got ") << body[0];
  }

  return success();
}

//===----------------------------------------------------------------------===//
// EnumExtractPayloadOp — verifier
//===----------------------------------------------------------------------===//

mlir::LogicalResult hew::EnumExtractPayloadOp::verify() {
  auto enumValType = getEnumVal().getType();

  // Hew dialect enum types (!hew.option<T>, !hew.result<T,E>) are valid —
  // they lower to LLVM structs later; skip detailed field checks here
  if (llvm::isa<hew::OptionEnumType>(enumValType) || llvm::isa<hew::ResultEnumType>(enumValType)) {
    if (getFieldIndex() < 1)
      return emitOpError("field_index must be >= 1 for payload extraction, got ")
             << getFieldIndex();
    return success();
  }

  auto structType = llvm::dyn_cast<mlir::LLVM::LLVMStructType>(enumValType);
  if (!structType) {
    return emitOpError("enum_val must be an LLVM struct, !hew.option, or !hew.result type, got ")
           << enumValType;
  }

  auto body = structType.getBody();
  uint64_t idx = getFieldIndex();
  if (idx < 1) {
    return emitOpError("field_index must be >= 1 for payload extraction, got ") << idx;
  }
  if (idx >= body.size()) {
    return emitOpError("field_index ")
           << idx << " is out of bounds for struct with " << body.size() << " fields";
  }

  if (getResult().getType() != body[idx]) {
    return emitOpError("result type ")
           << getResult().getType() << " does not match struct field type " << body[idx]
           << " at index " << idx;
  }

  return success();
}

//===----------------------------------------------------------------------===//
// TupleCreateOp — verifier
//===----------------------------------------------------------------------===//

mlir::LogicalResult hew::TupleCreateOp::verify() {
  auto tupleType = llvm::dyn_cast<hew::HewTupleType>(getResult().getType());
  if (!tupleType) {
    return emitOpError("result must be !hew.tuple type, got ") << getResult().getType();
  }

  auto elemTypes = tupleType.getElementTypes();
  auto numElements = elemTypes.size();
  auto numOperands = getElements().size();
  if (numOperands != numElements) {
    return emitOpError("element count (")
           << numOperands << ") does not match tuple element count (" << numElements << ")";
  }

  for (unsigned i = 0; i < numElements; ++i) {
    if (getElements()[i].getType() != elemTypes[i]) {
      return emitOpError("element ") << i << " type " << getElements()[i].getType()
                                     << " does not match tuple element type " << elemTypes[i];
    }
  }

  return success();
}

//===----------------------------------------------------------------------===//
// TupleExtractOp — verifier
//===----------------------------------------------------------------------===//

mlir::LogicalResult hew::TupleExtractOp::verify() {
  auto tupleType = llvm::dyn_cast<hew::HewTupleType>(getTuple().getType());
  if (!tupleType) {
    return emitOpError("tuple must be !hew.tuple type, got ") << getTuple().getType();
  }

  auto elemTypes = tupleType.getElementTypes();
  int64_t idx = getIndex();
  if (idx < 0 || static_cast<uint64_t>(idx) >= elemTypes.size()) {
    return emitOpError("index ") << idx << " is out of bounds for tuple with " << elemTypes.size()
                                 << " elements";
  }

  if (getResult().getType() != elemTypes[idx]) {
    return emitOpError("result type ")
           << getResult().getType() << " does not match tuple element type " << elemTypes[idx]
           << " at index " << idx;
  }

  return success();
}

//===----------------------------------------------------------------------===//
// ArrayCreateOp — verifier
//===----------------------------------------------------------------------===//

mlir::LogicalResult hew::ArrayCreateOp::verify() {
  auto arrayType = llvm::dyn_cast<hew::HewArrayType>(getResult().getType());
  if (!arrayType) {
    return emitOpError("result must be !hew.array type, got ") << getResult().getType();
  }

  int64_t expectedSize = arrayType.getSize();
  auto numOperands = static_cast<int64_t>(getElements().size());
  if (numOperands != expectedSize) {
    return emitOpError("element count (")
           << numOperands << ") does not match array size (" << expectedSize << ")";
  }

  auto elemType = arrayType.getElementType();
  for (unsigned i = 0; i < getElements().size(); ++i) {
    if (getElements()[i].getType() != elemType) {
      return emitOpError("element ") << i << " type " << getElements()[i].getType()
                                     << " does not match array element type " << elemType;
    }
  }

  return success();
}

//===----------------------------------------------------------------------===//
// ArrayExtractOp — verifier
//===----------------------------------------------------------------------===//

mlir::LogicalResult hew::ArrayExtractOp::verify() {
  auto arrayType = llvm::dyn_cast<hew::HewArrayType>(getArray().getType());
  if (!arrayType) {
    return emitOpError("array must be !hew.array type, got ") << getArray().getType();
  }

  int64_t idx = getIndex();
  int64_t size = arrayType.getSize();
  if (idx < 0 || idx >= size) {
    return emitOpError("index ") << idx << " is out of bounds for array of size " << size;
  }

  if (getResult().getType() != arrayType.getElementType()) {
    return emitOpError("result type ")
           << getResult().getType() << " does not match array element type "
           << arrayType.getElementType();
  }

  return success();
}

//===----------------------------------------------------------------------===//
// TraitObjectCreateOp — verifier
//===----------------------------------------------------------------------===//

mlir::LogicalResult hew::TraitObjectCreateOp::verify() {
  if (!llvm::isa<hew::HewTraitObjectType>(getResult().getType())) {
    return emitOpError("result must be !hew.trait_object type, got ") << getResult().getType();
  }

  if (!llvm::isa<mlir::LLVM::LLVMPointerType>(getData().getType())) {
    return emitOpError("data must be !llvm.ptr, got ") << getData().getType();
  }

  if (!llvm::isa<mlir::LLVM::LLVMPointerType>(getVtablePtr().getType())) {
    return emitOpError("vtable_ptr must be !llvm.ptr, got ") << getVtablePtr().getType();
  }

  return success();
}

//===----------------------------------------------------------------------===//
// TraitObjectDataOp — verifier
//===----------------------------------------------------------------------===//

mlir::LogicalResult hew::TraitObjectDataOp::verify() {
  if (!llvm::isa<hew::HewTraitObjectType>(getTraitObject().getType())) {
    return emitOpError("trait_object must be !hew.trait_object type, got ")
           << getTraitObject().getType();
  }

  if (!llvm::isa<mlir::LLVM::LLVMPointerType>(getResult().getType())) {
    return emitOpError("result must be !llvm.ptr, got ") << getResult().getType();
  }

  return success();
}

//===----------------------------------------------------------------------===//
// TraitObjectTagOp — verifier
//===----------------------------------------------------------------------===//

mlir::LogicalResult hew::TraitObjectTagOp::verify() {
  if (!llvm::isa<hew::HewTraitObjectType>(getTraitObject().getType())) {
    return emitOpError("trait_object must be !hew.trait_object type, got ")
           << getTraitObject().getType();
  }

  return success();
}

//===----------------------------------------------------------------------===//
// ClosureCreateOp — verifier
//===----------------------------------------------------------------------===//

mlir::LogicalResult hew::ClosureCreateOp::verify() {
  if (!llvm::isa<hew::ClosureType>(getResult().getType())) {
    return emitOpError("result must be !hew.closure type, got ") << getResult().getType();
  }

  if (!llvm::isa<mlir::LLVM::LLVMPointerType>(getFnPtr().getType())) {
    return emitOpError("fn_ptr must be !llvm.ptr, got ") << getFnPtr().getType();
  }

  if (!llvm::isa<mlir::LLVM::LLVMPointerType>(getEnvPtr().getType())) {
    return emitOpError("env_ptr must be !llvm.ptr, got ") << getEnvPtr().getType();
  }

  return success();
}

//===----------------------------------------------------------------------===//
// ClosureGetFnOp — verifier
//===----------------------------------------------------------------------===//

mlir::LogicalResult hew::ClosureGetFnOp::verify() {
  if (!llvm::isa<hew::ClosureType>(getClosure().getType())) {
    return emitOpError("closure must be !hew.closure type, got ") << getClosure().getType();
  }

  if (!llvm::isa<mlir::LLVM::LLVMPointerType>(getResult().getType())) {
    return emitOpError("result must be !llvm.ptr, got ") << getResult().getType();
  }

  return success();
}

//===----------------------------------------------------------------------===//
// ClosureGetEnvOp — verifier
//===----------------------------------------------------------------------===//

mlir::LogicalResult hew::ClosureGetEnvOp::verify() {
  if (!llvm::isa<hew::ClosureType>(getClosure().getType())) {
    return emitOpError("closure must be !hew.closure type, got ") << getClosure().getType();
  }

  if (!llvm::isa<mlir::LLVM::LLVMPointerType>(getResult().getType())) {
    return emitOpError("result must be !llvm.ptr, got ") << getResult().getType();
  }

  return success();
}

//===----------------------------------------------------------------------===//
// GenWrapValueOp — verifier
//===----------------------------------------------------------------------===//

mlir::LogicalResult hew::GenWrapValueOp::verify() {
  auto structType = llvm::dyn_cast<mlir::LLVM::LLVMStructType>(getResult().getType());
  if (!structType) {
    return emitOpError("result must be an LLVM struct type, got ") << getResult().getType();
  }

  auto body = structType.getBody();
  if (body.size() != 2) {
    return emitOpError("result struct must have exactly 2 fields, got ") << body.size();
  }

  auto tagType = llvm::dyn_cast<mlir::IntegerType>(body[0]);
  if (!tagType || tagType.getWidth() != 8) {
    return emitOpError("first field of wrapper struct must be i8, got ") << body[0];
  }

  if (getValue().getType() != body[1]) {
    return emitOpError("value type ")
           << getValue().getType() << " does not match wrapper struct value field type " << body[1];
  }

  return success();
}

//===----------------------------------------------------------------------===//
// GenWrapDoneOp — verifier
//===----------------------------------------------------------------------===//

mlir::LogicalResult hew::GenWrapDoneOp::verify() {
  auto structType = llvm::dyn_cast<mlir::LLVM::LLVMStructType>(getResult().getType());
  if (!structType) {
    return emitOpError("result must be an LLVM struct type, got ") << getResult().getType();
  }

  auto body = structType.getBody();
  if (body.size() != 2) {
    return emitOpError("result struct must have exactly 2 fields, got ") << body.size();
  }

  auto tagType = llvm::dyn_cast<mlir::IntegerType>(body[0]);
  if (!tagType || tagType.getWidth() != 8) {
    return emitOpError("first field of wrapper struct must be i8, got ") << body[0];
  }

  return success();
}

//===----------------------------------------------------------------------===//
// AssertEqOp — verifier
//===----------------------------------------------------------------------===//

mlir::LogicalResult hew::AssertEqOp::verify() {
  if (getLeft().getType() != getRight().getType()) {
    return emitOpError("left type ")
           << getLeft().getType() << " does not match right type " << getRight().getType();
  }

  return success();
}

//===----------------------------------------------------------------------===//
// AssertNeOp — verifier
//===----------------------------------------------------------------------===//

mlir::LogicalResult hew::AssertNeOp::verify() {
  if (getLeft().getType() != getRight().getType()) {
    return emitOpError("left type ")
           << getLeft().getType() << " does not match right type " << getRight().getType();
  }

  return success();
}

//===----------------------------------------------------------------------===//
// StringMethodOp — verifier
//===----------------------------------------------------------------------===//

mlir::LogicalResult hew::StringMethodOp::verify() {
  if (getMethod().empty()) {
    return emitOpError("method attribute must not be empty");
  }

  return success();
}

//===----------------------------------------------------------------------===//
// TupleExtractOp — folder
//===----------------------------------------------------------------------===//

mlir::OpFoldResult hew::TupleExtractOp::fold(FoldAdaptor adaptor) {
  (void)adaptor;
  if (auto createOp = getTuple().getDefiningOp<hew::TupleCreateOp>()) {
    auto elements = createOp.getElements();
    int64_t idx = getIndex();
    if (idx >= 0 && static_cast<uint64_t>(idx) < elements.size())
      return elements[idx];
  }
  return nullptr;
}

//===----------------------------------------------------------------------===//
// ArrayExtractOp — folder
//===----------------------------------------------------------------------===//

mlir::OpFoldResult hew::ArrayExtractOp::fold(FoldAdaptor adaptor) {
  (void)adaptor;
  if (auto createOp = getArray().getDefiningOp<hew::ArrayCreateOp>()) {
    auto elements = createOp.getElements();
    int64_t idx = getIndex();
    if (idx >= 0 && static_cast<uint64_t>(idx) < elements.size())
      return elements[idx];
  }
  return nullptr;
}

//===----------------------------------------------------------------------===//
// EnumExtractTagOp — folder
//===----------------------------------------------------------------------===//

mlir::OpFoldResult hew::EnumExtractTagOp::fold(FoldAdaptor adaptor) {
  (void)adaptor;
  if (auto constructOp = getEnumVal().getDefiningOp<hew::EnumConstructOp>()) {
    return mlir::IntegerAttr::get(mlir::IntegerType::get(getContext(), 32),
                                  constructOp.getVariantIndex());
  }
  return nullptr;
}

//===----------------------------------------------------------------------===//
// EnumExtractPayloadOp — folder
//===----------------------------------------------------------------------===//

mlir::OpFoldResult hew::EnumExtractPayloadOp::fold(FoldAdaptor adaptor) {
  (void)adaptor;
  auto constructOp = getEnumVal().getDefiningOp<hew::EnumConstructOp>();
  if (!constructOp)
    return nullptr;

  int64_t targetIdx = getFieldIndex();
  auto payloads = constructOp.getPayloads();
  auto positions = constructOp.getPayloadPositions();

  if (positions) {
    auto posArray = positions->getValue();
    for (size_t i = 0; i < posArray.size() && i < payloads.size(); ++i) {
      if (mlir::cast<mlir::IntegerAttr>(posArray[i]).getInt() == targetIdx)
        return payloads[i];
    }
  } else {
    int64_t payloadSlot = targetIdx - 1;
    if (payloadSlot >= 0 && static_cast<uint64_t>(payloadSlot) < payloads.size())
      return payloads[payloadSlot];
  }
  return nullptr;
}

//===----------------------------------------------------------------------===//
// ClosureGetFnOp — folder
//===----------------------------------------------------------------------===//

mlir::OpFoldResult hew::ClosureGetFnOp::fold(FoldAdaptor adaptor) {
  (void)adaptor;
  if (auto createOp = getClosure().getDefiningOp<hew::ClosureCreateOp>())
    return createOp.getFnPtr();
  return nullptr;
}

//===----------------------------------------------------------------------===//
// ClosureGetEnvOp — folder
//===----------------------------------------------------------------------===//

mlir::OpFoldResult hew::ClosureGetEnvOp::fold(FoldAdaptor adaptor) {
  (void)adaptor;
  if (auto createOp = getClosure().getDefiningOp<hew::ClosureCreateOp>())
    return createOp.getEnvPtr();
  return nullptr;
}

//===----------------------------------------------------------------------===//
// TraitObjectDataOp — folder
//===----------------------------------------------------------------------===//

mlir::OpFoldResult hew::TraitObjectDataOp::fold(FoldAdaptor adaptor) {
  (void)adaptor;
  if (auto createOp = getTraitObject().getDefiningOp<hew::TraitObjectCreateOp>())
    return createOp.getData();
  return nullptr;
}

//===----------------------------------------------------------------------===//
// TraitObjectTagOp — folder (folds to vtable_ptr when source is trait_object.create)
//===----------------------------------------------------------------------===//

mlir::OpFoldResult hew::TraitObjectTagOp::fold(FoldAdaptor adaptor) {
  (void)adaptor;
  if (auto createOp = getTraitObject().getDefiningOp<hew::TraitObjectCreateOp>())
    return createOp.getVtablePtr();
  return nullptr;
}

//===----------------------------------------------------------------------===//
// VecFreeOp — dead collection elimination
//===----------------------------------------------------------------------===//

namespace {
struct EliminateDeadVec : public mlir::OpRewritePattern<hew::VecFreeOp> {
  using OpRewritePattern::OpRewritePattern;

  mlir::LogicalResult matchAndRewrite(hew::VecFreeOp op,
                                      mlir::PatternRewriter &rewriter) const override {
    auto vecNew = op.getVec().getDefiningOp<hew::VecNewOp>();
    if (!vecNew)
      return mlir::failure();
    if (!vecNew.getResult().hasOneUse())
      return mlir::failure();
    rewriter.eraseOp(op);
    rewriter.eraseOp(vecNew);
    return mlir::success();
  }
};
} // namespace

void hew::VecFreeOp::getCanonicalizationPatterns(mlir::RewritePatternSet &results,
                                                 mlir::MLIRContext *context) {
  results.add<EliminateDeadVec>(context);
}

//===----------------------------------------------------------------------===//
// HashMapFreeOp — dead collection elimination
//===----------------------------------------------------------------------===//

namespace {
struct EliminateDeadHashMap : public mlir::OpRewritePattern<hew::HashMapFreeOp> {
  using OpRewritePattern::OpRewritePattern;

  mlir::LogicalResult matchAndRewrite(hew::HashMapFreeOp op,
                                      mlir::PatternRewriter &rewriter) const override {
    auto mapNew = op.getMap().getDefiningOp<hew::HashMapNewOp>();
    if (!mapNew)
      return mlir::failure();
    if (!mapNew.getResult().hasOneUse())
      return mlir::failure();
    rewriter.eraseOp(op);
    rewriter.eraseOp(mapNew);
    return mlir::success();
  }
};
} // namespace

void hew::HashMapFreeOp::getCanonicalizationPatterns(mlir::RewritePatternSet &results,
                                                     mlir::MLIRContext *context) {
  results.add<EliminateDeadHashMap>(context);
}

//===----------------------------------------------------------------------===//
// StringConcatOp — canonicalization
//===----------------------------------------------------------------------===//

namespace {
struct EliminateIdentityConcat : public mlir::OpRewritePattern<hew::StringConcatOp> {
  using OpRewritePattern::OpRewritePattern;

  mlir::LogicalResult matchAndRewrite(hew::StringConcatOp op,
                                      mlir::PatternRewriter &rewriter) const override {
    // Check if a value is a ConstantOp referencing a GlobalStringOp with ""
    auto isEmptyString = [&](mlir::Value v) -> bool {
      auto constOp = v.getDefiningOp<hew::ConstantOp>();
      if (!constOp)
        return false;
      auto strAttr = llvm::dyn_cast<mlir::StringAttr>(constOp.getValue());
      if (!strAttr)
        return false;
      auto module = op->getParentOfType<mlir::ModuleOp>();
      if (!module)
        return false;
      auto globalStr = module.lookupSymbol<hew::GlobalStringOp>(strAttr.getValue());
      if (!globalStr)
        return false;
      return globalStr.getValue().empty();
    };

    if (isEmptyString(op.getRhs())) {
      rewriter.replaceOp(op, op.getLhs());
      return mlir::success();
    }
    if (isEmptyString(op.getLhs())) {
      rewriter.replaceOp(op, op.getRhs());
      return mlir::success();
    }
    return mlir::failure();
  }
};
} // namespace

void hew::StringConcatOp::getCanonicalizationPatterns(mlir::RewritePatternSet &results,
                                                      mlir::MLIRContext *context) {
  results.add<EliminateIdentityConcat>(context);
}

//===----------------------------------------------------------------------===//
// TableGen'd op method definitions
//===----------------------------------------------------------------------===//

#define GET_OP_CLASSES
#include "hew/mlir/HewOps.cpp.inc"
