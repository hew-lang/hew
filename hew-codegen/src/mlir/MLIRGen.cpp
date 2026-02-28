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

#include <cassert>
#include <cstdlib>
#include <iostream>
#include <string>

using namespace hew;
using namespace mlir;

#include "MLIRGenHelpers.h"

// ============================================================================
// Constructor
// ============================================================================

MLIRGen::MLIRGen(mlir::MLIRContext &context, const std::string &targetTriple)
    : context(context), builder(&context), targetTriple(targetTriple),
      currentLoc(builder.getUnknownLoc()) {
  fileIdentifier = builder.getStringAttr("<unknown>");
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
  if (targetTriple.find("wasm32") != std::string::npos) {
    return mlir::IntegerType::get(&context, 32);
  }
  return mlir::IntegerType::get(&context, 64);
}

// ============================================================================
// Location helper
// ============================================================================

mlir::Location MLIRGen::loc(const ast::Span &span) {
  return mlir::FileLineColLoc::get(fileIdentifier, static_cast<unsigned>(span.start), 0);
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

mlir::Type MLIRGen::convertType(const ast::TypeExpr &type) {
  if (auto *named = std::get_if<ast::TypeNamed>(&type.kind)) {
    const auto &name = named->name;
    // Check for active type parameter substitutions (generics monomorphization)
    auto subst = typeParamSubstitutions.find(name);
    if (subst != typeParamSubstitutions.end()) {
      // Resolve the substituted name directly using the same logic below.
      const auto &resolvedName = subst->second;
      // Check for further alias
      auto alias = typeAliases.find(resolvedName);
      if (alias != typeAliases.end()) {
        return convertType(*alias->second);
      }
      // Resolve primitives/builtins for the substituted name
      if (resolvedName == "i8")
        return builder.getIntegerType(8);
      if (resolvedName == "i16")
        return builder.getIntegerType(16);
      if (resolvedName == "i32")
        return builder.getI32Type();
      if (resolvedName == "i64" || resolvedName == "int" || resolvedName == "Int")
        return builder.getI64Type();
      // Unsigned integers use signless MLIR types (arith ops require signless)
      if (resolvedName == "u8" || resolvedName == "byte")
        return builder.getIntegerType(8);
      if (resolvedName == "u16")
        return builder.getIntegerType(16);
      if (resolvedName == "u32")
        return builder.getIntegerType(32);
      if (resolvedName == "u64")
        return builder.getIntegerType(64);
      if (resolvedName == "f32")
        return builder.getF32Type();
      if (resolvedName == "f64" || resolvedName == "float")
        return builder.getF64Type();
      if (resolvedName == "bool")
        return builder.getI1Type();
      if (resolvedName == "char")
        return builder.getI32Type();
      if (resolvedName == "String" || resolvedName == "string" || resolvedName == "str")
        return hew::StringRefType::get(&context);
      // bytes = mutable byte buffer; stored as Vec<i32> to reuse existing runtime
      if (resolvedName == "bytes")
        return hew::VecType::get(&context, builder.getI32Type());
      if (resolvedName == "ActorRef" || resolvedName == "Actor")
        return hew::ActorRefType::get(&context);
      if (resolvedName == "Task" || resolvedName == "scope.Task")
        return hew::HandleType::get(&context, builder.getStringAttr("Task"));
      if (actorRegistry.count(resolvedName))
        return hew::TypedActorRefType::get(&context, builder.getStringAttr(resolvedName));
      auto it = structTypes.find(resolvedName);
      if (it != structTypes.end())
        return it->second.mlirType;
      auto enumIt = enumTypes.find(resolvedName);
      if (enumIt != enumTypes.end())
        return enumIt->second.mlirType;
      ++errorCount_;
      emitError(builder.getUnknownLoc())
          << "unresolved type substitution '" << resolvedName << "' for type parameter '" << name
          << "' — no builtin, struct, enum, or actor with this name is defined";
      return mlir::NoneType::get(&context);
    }
    // Check for type aliases
    auto alias = typeAliases.find(name);
    if (alias != typeAliases.end()) {
      return convertType(*alias->second);
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
        emitError(builder.getUnknownLoc()) << "Range type requires a type argument";
        return mlir::NoneType::get(&context);
      }
      auto elemType = convertType((*named->type_args)[0].value);
      if (!elemType)
        return mlir::NoneType::get(&context);
      return hew::HewTupleType::get(&context, {elemType, elemType});
    }
    if (name == "char")
      return builder.getI32Type();
    if (name == "String" || name == "string" || name == "str")
      return hew::StringRefType::get(&context);
    // bytes = mutable byte buffer; stored as Vec<i32> to reuse existing runtime
    if (name == "bytes")
      return hew::VecType::get(&context, builder.getI32Type());
    // Vec<T>: extract element type from generic args
    if (name == "Vec") {
      if (!(named->type_args && !named->type_args->empty())) {
        ++errorCount_;
        emitError(currentLoc)
            << "cannot determine element type for Vec; add explicit type annotation";
        return nullptr;
      }
      mlir::Type elemType = convertType((*named->type_args)[0].value);
      return hew::VecType::get(&context, elemType);
    }
    // HashMap<K,V>: extract key/value types from generic args
    if (name == "HashMap") {
      if (!(named->type_args && named->type_args->size() >= 2)) {
        ++errorCount_;
        emitError(currentLoc)
            << "cannot determine key/value types for HashMap; add explicit type annotation";
        return nullptr;
      }
      mlir::Type keyType = convertType((*named->type_args)[0].value);
      mlir::Type valType = convertType((*named->type_args)[1].value);
      return hew::HashMapType::get(&context, keyType, valType);
    }
    // HashSet<T>: opaque pointer (backed by HashMap<T, ()> in runtime)
    if (name == "HashSet") {
      if (!(named->type_args && !named->type_args->empty())) {
        ++errorCount_;
        emitError(currentLoc)
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
          typeParamSubstitutions = std::move(prevSubstitutions);
          return mangledStructType;
        }
        typeParamSubstitutions = std::move(prevSubstitutions);
      }
    }
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
    emitError(builder.getUnknownLoc())
        << "unresolved type '" << name << "' — no struct, enum, or actor with this name is defined";
    return mlir::NoneType::get(&context);
  }
  // Tuple types
  if (auto *tuple = std::get_if<ast::TypeTuple>(&type.kind)) {
    if (tuple->elements.empty())
      return mlir::NoneType::get(&context); // unit type
    llvm::SmallVector<mlir::Type, 4> elemTypes;
    for (const auto &elem : tuple->elements) {
      elemTypes.push_back(convertType(elem.value));
    }
    return hew::HewTupleType::get(&context, elemTypes);
  }

  // Array types
  if (auto *array = std::get_if<ast::TypeArray>(&type.kind)) {
    auto elemType = convertType(array->element->value);
    return hew::HewArrayType::get(&context, elemType, array->size);
  }

  // Option<T> → !hew.option<T>
  if (auto *option = std::get_if<ast::TypeOption>(&type.kind)) {
    auto innerType = convertType(option->inner->value);
    return hew::OptionEnumType::get(&context, innerType);
  }

  // Result<T, E> → !hew.result<T, E>
  if (auto *result = std::get_if<ast::TypeResult>(&type.kind)) {
    auto okType = convertType(result->ok->value);
    auto errType = convertType(result->err->value);
    return hew::ResultEnumType::get(&context, okType, errType);
  }

  // Function types: fn(i32, i32) -> i32  →  !hew.closure<(i32, i32) -> i32>
  // All fn(T...) -> R type annotations produce a closure fat pointer so that
  // lambdas with captures can be passed as first-class values.
  if (auto *function = std::get_if<ast::TypeFunction>(&type.kind)) {
    llvm::SmallVector<mlir::Type, 4> paramTypes;
    for (const auto &pt : function->params) {
      paramTypes.push_back(convertType(pt.value));
    }
    // Use NoneType as sentinel for void return (no return type)
    mlir::Type retType = function->return_type ? convertType(function->return_type->value)
                                               : mlir::NoneType::get(&context);
    return hew::ClosureType::get(&context, paramTypes, retType);
  }

  // Trait object types: dyn Trait → fat pointer {data_ptr, type_tag}
  if (auto *to = std::get_if<ast::TypeTraitObject>(&type.kind)) {
    std::string traitName = !to->bounds.empty() ? to->bounds[0].name : "Unknown";
    return hew::HewTraitObjectType::get(&context, traitName);
  }

  ++errorCount_;
  emitError(builder.getUnknownLoc())
      << "unsupported type expression in MLIR codegen (kind index " << type.kind.index() << ")";
  return mlir::NoneType::get(&context);
}

// ============================================================================
// Type coercion
// ============================================================================

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
      auto elem = builder.create<hew::TupleExtractOp>(location, srcTuple.getElementTypes()[i],
                                                      value, builder.getI64IntegerAttr(i));
      elements.push_back(coerceType(elem, dstTuple.getElementTypes()[i], location));
    }
    return builder.create<hew::TupleCreateOp>(location, dstTuple, elements);
  }

  bool srcIsInt = llvm::isa<mlir::IntegerType>(value.getType());
  bool dstIsFloat = llvm::isa<mlir::FloatType>(targetType);
  bool srcIsFloat = llvm::isa<mlir::FloatType>(value.getType());
  bool dstIsInt = llvm::isa<mlir::IntegerType>(targetType);

  // Numeric conversions go through hew.cast so the dialect folder /
  // canonicalizer can optimise cast chains before lowering.
  if ((srcIsInt && dstIsFloat) || (srcIsFloat && dstIsInt)) {
    auto castOp = builder.create<hew::CastOp>(location, targetType, value);
    if (isUnsigned)
      castOp->setAttr("is_unsigned", builder.getBoolAttr(true));
    return castOp;
  }
  // int -> int width conversion (e.g. i64 literal to i32 field, or i32 to i64)
  if (srcIsInt && dstIsInt) {
    auto srcWidth = mlir::cast<mlir::IntegerType>(value.getType()).getWidth();
    auto dstWidth = mlir::cast<mlir::IntegerType>(targetType).getWidth();
    if (srcWidth != dstWidth) {
      auto castOp = builder.create<hew::CastOp>(location, targetType, value);
      if (isUnsigned)
        castOp->setAttr("is_unsigned", builder.getBoolAttr(true));
      return castOp;
    }
  }
  // float -> float width conversion (f32 ↔ f64)
  if (srcIsFloat && dstIsFloat) {
    auto srcWidth = mlir::cast<mlir::FloatType>(value.getType()).getWidth();
    auto dstWidth = mlir::cast<mlir::FloatType>(targetType).getWidth();
    if (srcWidth != dstWidth) {
      auto castOp = builder.create<hew::CastOp>(location, targetType, value);
      return castOp;
    }
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
    return builder.create<hew::BitcastOp>(location, targetType, value);
  }
  // !llvm.ptr → Hew pointer-like type: bitcast on the return path.
  if (mlir::isa<mlir::LLVM::LLVMPointerType>(value.getType()) && isPointerLikeType(targetType)) {
    return builder.create<hew::BitcastOp>(location, targetType, value);
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
        auto thunkOp = builder.create<mlir::func::FuncOp>(location, thunkName, thunkFuncType);
        thunkOp.setVisibility(mlir::SymbolTable::Visibility::Private);

        auto &entry = *thunkOp.addEntryBlock();
        builder.setInsertionPointToStart(&entry);

        // Forward user args (skip env at index 0) to the real function
        llvm::SmallVector<mlir::Value, 8> forwardArgs;
        for (unsigned i = 1; i < entry.getNumArguments(); ++i)
          forwardArgs.push_back(entry.getArgument(i));

        auto realFunc = module.lookupSymbol<mlir::func::FuncOp>(funcName);
        assert(realFunc && "thunk target function must exist");
        auto callOp = builder.create<mlir::func::CallOp>(location, realFunc, forwardArgs);
        if (callOp.getNumResults() > 0)
          builder.create<mlir::func::ReturnOp>(location, callOp.getResults());
        else
          builder.create<mlir::func::ReturnOp>(location);

        builder.restoreInsertionPoint(savedIP);
        closureThunkCache[funcName] = thunkName;
      }

      auto &cachedThunkName = closureThunkCache[funcName];

      // Look up the thunk and create a closure with null env
      auto thunkFunc = module.lookupSymbol<mlir::func::FuncOp>(cachedThunkName);
      assert(thunkFunc && "thunk must exist after cache insert");
      auto fnPtrVal = builder.create<hew::FuncPtrOp>(
          location, ptrType, mlir::SymbolRefAttr::get(&context, cachedThunkName));
      auto nullEnv = builder.create<mlir::LLVM::ZeroOp>(location, ptrType);
      return builder.create<hew::ClosureCreateOp>(location, closureType, fnPtrVal, nullEnv);
    }
  }
  emitWarning(location) << "coerceType: no known conversion from " << value.getType() << " to "
                        << targetType;
  return value;
}

// ============================================================================
// Symbol table operations
// ============================================================================

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
    auto type = value.getType();
    bool canPromote = mlir::isa<mlir::IntegerType>(type) || mlir::isa<mlir::FloatType>(type) ||
                      mlir::isa<mlir::LLVM::LLVMPointerType>(type) ||
                      mlir::isa<mlir::IndexType>(type) || isPointerLikeType(type);
    if (canPromote) {
      auto memrefType = mlir::MemRefType::get({}, type);
      // Insert alloca at function entry block start (dominates everything)
      auto savedIP = builder.saveInsertionPoint();
      auto &entryBlock = currentFunction.front();
      builder.setInsertionPointToStart(&entryBlock);
      auto alloca = builder.create<mlir::memref::AllocaOp>(builder.getUnknownLoc(), memrefType);
      // Zero-initialize pointer-like allocas so that unconditional drops
      // at function exit are safe even when the variable's definition was
      // skipped (e.g. early return before the let-binding).
      // hew_vec_free/hew_hashmap_free_impl/hew_string_drop all accept null.
      if (mlir::isa<mlir::LLVM::LLVMPointerType>(type) || isPointerLikeType(type)) {
        auto zero = createDefaultValue(builder, builder.getUnknownLoc(), type);
        builder.create<mlir::memref::StoreOp>(builder.getUnknownLoc(), zero, alloca);
      }
      builder.restoreInsertionPoint(savedIP);
      // Store the value at the current insertion point
      builder.create<mlir::memref::StoreOp>(builder.getUnknownLoc(), value, alloca);
      mutableVars.insert(name, alloca);
      return;
    }
  }
  symbolTable.insert(name, value);
}

void MLIRGen::declareMutableVariable(llvm::StringRef name, mlir::Type type,
                                     mlir::Value initialValue) {
  name = intern(name.str());
  auto memrefType = mlir::MemRefType::get({}, type);
  mlir::Value alloca;
  // When return guards are active, hoist alloca to function entry block
  // so it dominates all uses across sibling guard regions.
  if (returnFlag && currentFunction) {
    auto savedIP = builder.saveInsertionPoint();
    auto &entryBlock = currentFunction.front();
    builder.setInsertionPointToStart(&entryBlock);
    alloca = builder.create<mlir::memref::AllocaOp>(builder.getUnknownLoc(), memrefType);
    // Zero-initialize pointer-like allocas so that unconditional drops
    // at function exit are safe even when the variable's definition was
    // skipped (e.g. early return before the let-binding).
    if (mlir::isa<mlir::LLVM::LLVMPointerType>(type) || isPointerLikeType(type)) {
      auto zero = createDefaultValue(builder, builder.getUnknownLoc(), type);
      builder.create<mlir::memref::StoreOp>(builder.getUnknownLoc(), zero, alloca);
    }
    builder.restoreInsertionPoint(savedIP);
  } else {
    alloca = builder.create<mlir::memref::AllocaOp>(builder.getUnknownLoc(), memrefType);
  }
  if (initialValue) {
    builder.create<mlir::memref::StoreOp>(builder.getUnknownLoc(), initialValue, alloca);
  }
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
    auto cellPtr = builder.create<mlir::memref::LoadOp>(builder.getUnknownLoc(), it);
    builder.create<mlir::LLVM::StoreOp>(builder.getUnknownLoc(), value, cellPtr);
    return;
  }
  builder.create<mlir::memref::StoreOp>(builder.getUnknownLoc(), value, it);
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
      auto cellPtr = builder.create<mlir::memref::LoadOp>(builder.getUnknownLoc(), mutVal);
      auto origType = cellIt->second;
      auto loadType = toLLVMStorageType(origType);
      mlir::Value loaded =
          builder.create<mlir::LLVM::LoadOp>(builder.getUnknownLoc(), loadType, cellPtr);
      // Bitcast back to the original dialect type so callers see the
      // expected type (e.g. !hew.handle<...>).
      if (loadType != origType)
        loaded = builder.create<hew::BitcastOp>(builder.getUnknownLoc(), origType, loaded);
      return loaded;
    }
    return builder.create<mlir::memref::LoadOp>(builder.getUnknownLoc(), mutVal);
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
  builder.create<hew::GlobalStringOp>(builder.getUnknownLoc(), builder.getStringAttr(symName),
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
  auto funcOp = builder.create<mlir::func::FuncOp>(builder.getUnknownLoc(), name, type);
  funcOp.setVisibility(mlir::SymbolTable::Visibility::Private);
  builder.restoreInsertionPoint(savedIP);
  return funcOp;
}

// ============================================================================
// Extern block generation
// ============================================================================

void MLIRGen::generateExternBlock(const ast::ExternBlock &block) {
  for (const auto &fn : block.functions) {
    llvm::SmallVector<mlir::Type, 4> paramTypes;
    for (const auto &param : fn.params) {
      // Extern "C" functions always use LLVM-level types — convert any
      // Hew dialect types (handles, strings, vecs, …) to !llvm.ptr so
      // that the type conversion framework doesn't have to chase them.
      paramTypes.push_back(toLLVMStorageType(convertType(param.ty.value)));
    }

    mlir::Type resultType = nullptr;
    mlir::Type semanticResultType = nullptr;
    if (fn.return_type) {
      semanticResultType = convertType(fn.return_type->value);
      resultType = toLLVMStorageType(semanticResultType);
    }

    auto funcType = resultType ? mlir::FunctionType::get(&context, paramTypes, {resultType})
                               : mlir::FunctionType::get(&context, paramTypes, {});

    // If variadic, we need to use LLVM-level variadic support
    // For now, create a regular extern declaration
    getOrCreateExternFunc(fn.name, funcType);
    if (semanticResultType && mlir::isa<hew::VecType, hew::HashMapType>(semanticResultType)) {
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
    builder.create<hew::PrintOp>(location, val, builder.getBoolAttr(newline));
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
    auto printOp = builder.create<hew::PrintOp>(location, val, builder.getBoolAttr(newline));
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
    builder.create<hew::PrintOp>(location, val, builder.getBoolAttr(newline));
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
    builder.create<hew::PrintOp>(location, val, builder.getBoolAttr(newline));
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
    return builder.create<mlir::func::CallOp>(location, sqrtFunc, mlir::ValueRange{arg})
        .getResult(0);
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
    auto zero = builder.create<mlir::arith::ConstantIntOp>(location, i64Type, 0);
    auto neg = builder.create<mlir::arith::SubIOp>(location, zero, arg);
    auto cmp =
        builder.create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::sgt, arg, zero);
    return builder.create<mlir::arith::SelectOp>(location, cmp, arg, neg).getResult();
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
    auto cmp = builder.create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::slt, a, b);
    return builder.create<mlir::arith::SelectOp>(location, cmp, a, b).getResult();
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
    auto cmp = builder.create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::sgt, a, b);
    return builder.create<mlir::arith::SelectOp>(location, cmp, a, b).getResult();
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
    return builder.create<hew::StringConcatOp>(location, hew::StringRefType::get(&context), a, b)
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
    return builder
        .create<hew::StringMethodOp>(location, builder.getI32Type(),
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
    return builder
        .create<hew::StringMethodOp>(location, builder.getI32Type(),
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
    builder.create<hew::SleepOp>(location, ms);
    return nullptr;
  }

  // cooperate() -> void
  if (name == "cooperate") {
    builder.create<hew::CooperateOp>(location);
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
    auto vec = builder.create<hew::VecNewOp>(location, bytesType).getResult();
    // The argument should be an array literal — push each element
    if (auto *arr = std::get_if<ast::ExprArray>(&ast::callArgExpr(args[0]).value.kind)) {
      for (const auto &elem : arr->elements) {
        auto val = generateExpression(elem->value);
        if (!val)
          return nullptr;
        val = coerceType(val, builder.getI32Type(), location);
        builder.create<hew::VecPushOp>(location, vec, val);
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
      auto vec = builder.create<hew::VecNewOp>(location, vecType).getResult();
      for (auto val : values) {
        val = coerceType(val, elemType, location);
        builder.create<hew::VecPushOp>(location, vec, val);
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
      return builder.create<hew::VecNewOp>(location, bytesType).getResult();
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
    return builder.create<hew::VecNewOp>(location, vecType).getResult();
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
    return builder.create<hew::HashMapNewOp>(location, hmType).getResult();
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
    return builder
        .create<hew::RuntimeCallOp>(location, mlir::TypeRange{setType},
                                    mlir::SymbolRefAttr::get(&context, "hew_hashset_new"),
                                    mlir::ValueRange{})
        .getResult();
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
    builder.create<hew::ActorStopOp>(location, actorVal);
    return nullptr;
  }

  // close(actor) -> void: close an actor's mailbox
  if (name == "close") {
    if (args.empty()) {
      emitError(location) << name << " requires at least 1 argument";
      return nullptr;
    }
    auto actorVal = generateExpression(ast::callArgExpr(args[0]).value);
    if (!actorVal)
      return nullptr;
    builder.create<hew::ActorCloseOp>(location, actorVal);
    return nullptr;
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
    auto selfRef = builder.create<hew::ActorSelfOp>(location, ptrType);
    builder.create<hew::ActorLinkOp>(location, selfRef, targetVal);
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
    auto selfRef = builder.create<hew::ActorSelfOp>(location, ptrType);
    builder.create<hew::ActorUnlinkOp>(location, selfRef, targetVal);
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
    auto selfRef = builder.create<hew::ActorSelfOp>(location, ptrType);
    return builder.create<hew::ActorMonitorOp>(location, builder.getI64Type(), selfRef, targetVal)
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
    builder.create<hew::ActorDemonitorOp>(location, refIdVal);
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
      idxVal = builder.create<mlir::arith::TruncIOp>(location, i32Type, idxVal);
    }
    auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);

    if (childIsSupervisor) {
      return builder
          .create<hew::RuntimeCallOp>(
              location, mlir::TypeRange{ptrType},
              mlir::SymbolRefAttr::get(&context, "hew_supervisor_get_child_supervisor"),
              mlir::ValueRange{supVal, idxVal})
          .getResult();
    }
    return builder
        .create<hew::RuntimeCallOp>(location, mlir::TypeRange{ptrType},
                                    mlir::SymbolRefAttr::get(&context, "hew_supervisor_get_child"),
                                    mlir::ValueRange{supVal, idxVal})
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
    builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{},
                                       mlir::SymbolRefAttr::get(&context, "hew_supervisor_stop"),
                                       mlir::ValueRange{supVal});
    return nullptr;
  }

  // panic() — crash the current actor by triggering SEGV via runtime
  if (name == "panic") {
    if (!args.empty()) {
      // panic("message") — print the message via hew_panic_msg, then crash
      auto msg = generateExpression(ast::callArgExpr(args[0]).value);
      if (msg) {
        auto panicMsgAttr = mlir::SymbolRefAttr::get(&context, "hew_panic_msg");
        builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{}, panicMsgAttr,
                                           mlir::ValueRange{msg});
        // hew_panic_msg calls hew_panic internally — it never returns.
        // Still emit PanicOp for MLIR's control flow analysis.
      }
    }
    builder.create<hew::PanicOp>(location);
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
    // Coerce idx to i32 (runtime function expects i32)
    idx = coerceType(idx, builder.getI32Type(), location);
    return builder
        .create<hew::StringMethodOp>(location, builder.getI32Type(),
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
    // Coerce start/end to i32 (runtime function expects i32)
    start = coerceType(start, builder.getI32Type(), location);
    end = coerceType(end, builder.getI32Type(), location);
    return builder
        .create<hew::StringMethodOp>(location, hew::StringRefType::get(&context),
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
    return builder
        .create<hew::StringMethodOp>(location, builder.getI32Type(), builder.getStringAttr("find"),
                                     a, mlir::ValueRange{b})
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
    return builder
        .create<hew::StringMethodOp>(location, builder.getI1Type(),
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
    return builder
        .create<hew::StringMethodOp>(location, builder.getI32Type(),
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
    return builder
        .create<hew::StringMethodOp>(location, builder.getI32Type(),
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
    return builder
        .create<hew::StringMethodOp>(location, hew::StringRefType::get(&context),
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
    return builder
        .create<hew::StringMethodOp>(location, hew::StringRefType::get(&context),
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
    return builder
        .create<hew::StringMethodOp>(location, builder.getI32Type(),
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
    auto toStr = builder.create<hew::ToStringOp>(location, hew::StringRefType::get(&context), n);
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
    return builder.create<hew::ToStringOp>(location, hew::StringRefType::get(&context), c)
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
    return builder
        .create<hew::RuntimeCallOp>(location, mlir::TypeRange{hew::StringRefType::get(&context)},
                                    mlir::SymbolRefAttr::get(&context, "hew_read_file"),
                                    mlir::ValueRange{path})
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
    builder.create<hew::AssertOp>(location, cond);
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
    builder.create<hew::AssertEqOp>(location, left, right);
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
    builder.create<hew::AssertNeOp>(location, left, right);
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
  int ptrWidth = (targetTriple.find("wasm32") != std::string::npos) ? 32 : 64;
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
    someV.payloadTypes.push_back(builder.getI32Type()); // default; overridden by actual usage
    optInfo.variants.push_back(someV);
    enumTypes["__Option"] = std::move(optInfo);

    EnumTypeInfo resInfo;
    resInfo.name = "__Result";
    resInfo.hasPayloads = true;
    EnumVariantInfo okV;
    okV.name = "Ok";
    okV.index = 0;
    okV.payloadTypes.push_back(builder.getI32Type()); // default; overridden by actual usage
    resInfo.variants.push_back(okV);
    EnumVariantInfo errV;
    errV.name = "Err";
    errV.index = 1;
    errV.payloadTypes.push_back(builder.getI32Type()); // default; overridden by actual usage
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
    bool changed = true;
    while (changed) {
      changed = false;
      for (auto &[modKey, impList] : moduleImports) {
        std::vector<std::vector<std::string>> toAdd;
        for (const auto &impPath : impList) {
          std::string impKey;
          for (const auto &seg : impPath)
            impKey += (impKey.empty() ? "" : "::") + seg;
          auto transIt = moduleImports.find(impKey);
          if (transIt != moduleImports.end()) {
            for (const auto &transPath : transIt->second) {
              bool found = false;
              for (const auto &existing : impList)
                if (existing == transPath) {
                  found = true;
                  break;
                }
              if (!found)
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
  // can reference struct/enum types.
  forEachItem([&](const auto &spannedItem) {
    const auto &item = spannedItem.value;
    if (auto *td = std::get_if<ast::TypeDecl>(&item.kind)) {
      registerTypeDecl(*td);
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
      registerFunctionSignature(*fn);
    } else if (auto *impl = std::get_if<ast::ImplDecl>(&item.kind)) {
      // Pre-register impl methods with mangled names
      std::string typeName;
      if (auto *named = std::get_if<ast::TypeNamed>(&impl->target_type.value.kind)) {
        typeName = named->name;
      }
      if (!typeName.empty()) {
        // Set Self substitution for bare self parameter resolution
        typeParamSubstitutions["Self"] = typeName;

        for (const auto &method : impl->methods) {
          std::string mangledMethod = mangleName(currentModulePath, typeName, method.name);
          registerFunctionSignature(method, mangledMethod);
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
                llvm::SmallVector<mlir::Type, 4> paramTypes;
                for (const auto &p : tm->params)
                  paramTypes.push_back(convertType(p.ty.value));
                llvm::SmallVector<mlir::Type, 1> resultTypes;
                if (tm->return_type) {
                  auto retTy = convertType(tm->return_type->value);
                  if (!llvm::isa<mlir::NoneType>(retTy))
                    resultTypes.push_back(retTy);
                }
                auto funcType = builder.getFunctionType(paramTypes, resultTypes);
                auto savedIP = builder.saveInsertionPoint();
                builder.setInsertionPointToEnd(module.getBody());
                builder.create<mlir::func::FuncOp>(builder.getUnknownLoc(), mangledName, funcType);
                builder.restoreInsertionPoint(savedIP);
              }
            }
          }
        }

        typeParamSubstitutions.erase("Self");
      }
    }
  });

  // Pass 1e: Register all actor declarations first (struct types, field tracking,
  // registry entries) so forward references between actors work.
  forEachItem([&](const auto &spannedItem) {
    const auto &item = spannedItem.value;
    if (auto *ad = std::get_if<ast::ActorDecl>(&item.kind)) {
      registerActorDecl(*ad);
    }
  });

  // Pass 1e2: Pre-register supervisor declarations so that spawn expressions
  // in function bodies (Pass 1k) can recognise supervisor names. The full
  // supervisor init functions are generated later in Pass 2.
  forEachItem([&](const auto &spannedItem) {
    const auto &item = spannedItem.value;
    if (auto *sd = std::get_if<ast::SupervisorDecl>(&item.kind)) {
      std::vector<std::string> childTypes;
      for (const auto &child : sd->children) {
        childTypes.push_back(child.actor_type);
      }
      supervisorChildren[sd->name] = std::move(childTypes);
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
        userDropFuncs[named->name] = mangleName(currentModulePath, named->name, "drop");
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
    }
  });

  // Pass 1i: Generate extern block declarations so function bodies can call
  // extern functions. getOrCreateExternFunc guards against duplicates.
  forEachItem([&](const auto &spannedItem) {
    const auto &item = spannedItem.value;
    if (auto *eb = std::get_if<ast::ExternBlock>(&item.kind)) {
      generateExternBlock(*eb);
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
    std::string traitName = impl->trait_bound ? impl->trait_bound->name : "";
    auto traitIt = traitRegistry.find(traitName);
    if (traitIt != traitRegistry.end()) {
      std::vector<std::string> methodNames;
      for (const auto *tm : traitIt->second.methods)
        methodNames.push_back(tm->name);
      registerTraitImpl(typeName, traitName, methodNames);
      // Shim functions are generated in generateImplDecl after method bodies exist
    }
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
      generateFunction(*fn);
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
        std::holds_alternative<ast::WireDecl>(item.kind))
      return; // already handled in pass 1
    generateItem(item);
  });

  // If actors were used, inject hew_sched_init/shutdown into main
  if (hasActors) {
    if (auto mainFunc = module.lookupSymbol<mlir::func::FuncOp>("main")) {
      auto &entryBlock = mainFunc.getBody().front();

      // Insert hew.sched.init at the very beginning of main
      auto savedIP = builder.saveInsertionPoint();
      builder.setInsertionPointToStart(&entryBlock);
      builder.create<hew::SchedInitOp>(builder.getUnknownLoc());

      // Insert hew.sched.shutdown before each return in main
      for (auto &block : mainFunc.getBody()) {
        if (block.empty())
          continue;
        auto &lastOp = block.back();
        if (mlir::isa<mlir::func::ReturnOp>(lastOp)) {
          builder.setInsertionPoint(&lastOp);
          builder.create<hew::SchedShutdownOp>(builder.getUnknownLoc());
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

void MLIRGen::generateItem(const ast::Item &item) {
  if (auto *fn = std::get_if<ast::FnDecl>(&item.kind)) {
    if (fn->type_params && !fn->type_params->empty()) {
      // Generic function: store for later specialization, don't generate yet
      genericFunctions[fn->name] = fn;
    } else if (fn->is_generator) {
      generateGeneratorFunction(*fn);
    } else {
      generateFunction(*fn);
    }
  } else if (auto *cd = std::get_if<ast::ConstDecl>(&item.kind)) {
    // Store for inline generation when referenced
    moduleConstants[cd->name] = &cd->value.value;
  } else if (auto *td = std::get_if<ast::TypeDecl>(&item.kind)) {
    registerTypeDecl(*td);
  } else if (auto *id = std::get_if<ast::ImplDecl>(&item.kind)) {
    generateImplDecl(*id);
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
  }
}

// ============================================================================
// Function signature pre-registration (for order-independent resolution)
// ============================================================================

void MLIRGen::registerFunctionSignature(const ast::FnDecl &fn, const std::string &nameOverride) {
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
  llvm::SmallVector<mlir::Type, 4> paramTypes;
  for (const auto &param : fn.params) {
    paramTypes.push_back(convertType(param.ty.value));
  }

  llvm::SmallVector<mlir::Type, 1> resultTypes;
  if (fn.return_type) {
    auto retTy = convertType(fn.return_type->value);
    if (!llvm::isa<mlir::NoneType>(retTy)) {
      resultTypes.push_back(retTy);
    }
  }

  auto funcType = builder.getFunctionType(paramTypes, resultTypes);
  auto location = builder.getUnknownLoc();

  // Create a declaration (no body) at module scope
  auto savedIP = builder.saveInsertionPoint();
  builder.setInsertionPointToEnd(module.getBody());
  builder.create<mlir::func::FuncOp>(location, funcName, funcType);
  builder.restoreInsertionPoint(savedIP);
}

// ============================================================================
// Type declaration registration
// ============================================================================

void MLIRGen::registerTypeDecl(const ast::TypeDecl &decl) {
  const std::string &declName = decl.name;

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

    unsigned idx = 0;
    for (const auto &bodyItem : decl.body) {
      if (auto *variantItem = std::get_if<ast::TypeBodyVariant>(&bodyItem.kind)) {
        EnumVariantInfo vi;
        vi.name = variantItem->variant.name;
        vi.index = idx++;
        if (auto *tuple = std::get_if<ast::VariantDecl::VariantTuple>(&variantItem->variant.kind)) {
          for (const auto &ty : tuple->fields) {
            vi.payloadTypes.push_back(convertType(ty.value));
          }
        } else if (auto *strct =
                       std::get_if<ast::VariantDecl::VariantStruct>(&variantItem->variant.kind)) {
          for (const auto &field : strct->fields) {
            vi.payloadTypes.push_back(convertType(field.ty.value));
            vi.fieldNames.push_back(field.name);
          }
        }
        info.variants.push_back(std::move(vi));
      }
    }

    // Register variant name → (enum, index) for quick lookup
    for (const auto &v : info.variants) {
      variantLookup[v.name] = {info.name, v.index};
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

      info.mlirType = mlir::LLVM::LLVMStructType::getLiteral(&context, structFields);
    }

    enumTypes[declName] = std::move(info);
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
      field.semanticType = convertType(fieldItem->ty.value);
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
}

// ============================================================================
// Impl declaration generation
// ============================================================================

void MLIRGen::generateImplDecl(const ast::ImplDecl &decl) {
  // Get the target type name
  std::string typeName;
  if (auto *named = std::get_if<ast::TypeNamed>(&decl.target_type.value.kind)) {
    typeName = named->name;
  }
  if (typeName.empty())
    return;

  // Set Self substitution so bare self parameters resolve to the target type
  typeParamSubstitutions["Self"] = typeName;

  // Generate each method as a free function with mangled name
  std::set<std::string> overriddenMethods;
  for (const auto &method : decl.methods) {
    std::string mangledMethod = mangleName(currentModulePath, typeName, method.name);
    generateFunction(method, mangledMethod);
    overriddenMethods.insert(method.name);
  }

  // Generate default method bodies from the trait for methods not overridden
  std::string traitName = decl.trait_bound ? decl.trait_bound->name : "";
  auto traitIt = traitRegistry.find(traitName);
  if (traitIt != traitRegistry.end()) {
    for (const auto *tm : traitIt->second.methods) {
      if (tm->body && overriddenMethods.find(tm->name) == overriddenMethods.end()) {
        std::string mangledDefault = mangleName(currentModulePath, typeName, tm->name);
        generateTraitDefaultMethod(*tm, typeName, mangledDefault);
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
      builder.create<hew::SizeOfOp>(location, sizeType(), mlir::TypeAttr::get(concreteType));

  auto dataPtr = builder.create<hew::ArenaMallocOp>(location, ptrType, sizeVal);
  builder.create<mlir::LLVM::StoreOp>(location, concreteVal, dataPtr);

  // Get vtable pointer via VtableRefOp
  llvm::SmallVector<mlir::Attribute> funcAttrs;
  for (const auto &shimName : implInfo->shimFunctions)
    funcAttrs.push_back(builder.getStringAttr(shimName));
  auto vtablePtr = builder.create<hew::VtableRefOp>(location, ptrType,
                                                    builder.getStringAttr(implInfo->vtableName),
                                                    builder.getArrayAttr(funcAttrs));

  // Build fat pointer: { data_ptr, vtable_ptr }
  mlir::Value fatPtr =
      builder.create<hew::TraitObjectCreateOp>(location, fatPtrType, dataPtr, vtablePtr);

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
  auto shimFunc = builder.create<mlir::func::FuncOp>(loc, shimName, shimType);
  auto *entryBlock = shimFunc.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  // Load concrete value from data pointer — use LLVM storage type since
  // this is an LLVM::LoadOp which requires LLVM-legal result types.
  auto origSelfType = implType.getInput(0);
  auto selfType = toLLVMStorageType(origSelfType);
  mlir::Value selfVal =
      builder.create<mlir::LLVM::LoadOp>(loc, selfType, entryBlock->getArgument(0));

  // If the impl expects a dialect type (e.g. !hew.handle), bitcast so
  // the func.call types match the impl signature before lowering.
  if (selfType != origSelfType)
    selfVal = builder.create<hew::BitcastOp>(loc, origSelfType, selfVal);

  // Build call args: loaded self + forwarded extra args
  llvm::SmallVector<mlir::Value> callArgs = {selfVal};
  for (unsigned i = 1; i < shimArgTypes.size(); ++i)
    callArgs.push_back(entryBlock->getArgument(i));

  // Call impl function
  auto call =
      builder.create<mlir::func::CallOp>(loc, implFuncName, implType.getResults(), callArgs);

  // Return
  if (implType.getNumResults() > 0)
    builder.create<mlir::func::ReturnOp>(loc, call.getResults());
  else
    builder.create<mlir::func::ReturnOp>(loc);

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
                                         const std::string &mangledName) {
  if (!method.body)
    return;
  SymbolTableScopeT varScope(symbolTable);
  MutableTableScopeT mutScope(mutableVars);

  llvm::SmallVector<mlir::Type, 4> paramTypes;
  for (const auto &param : method.params) {
    paramTypes.push_back(convertType(param.ty.value));
  }

  llvm::SmallVector<mlir::Type, 1> resultTypes;
  if (method.return_type) {
    auto retTy = convertType(method.return_type->value);
    if (!llvm::isa<mlir::NoneType>(retTy))
      resultTypes.push_back(retTy);
  }

  auto funcType = builder.getFunctionType(paramTypes, resultTypes);
  auto location = currentLoc;

  // Erase forward declaration if one was created in pass 1d
  if (auto existing = module.lookupSymbol<mlir::func::FuncOp>(mangledName)) {
    if (existing.isDeclaration())
      existing.erase();
  }

  auto savedIP = builder.saveInsertionPoint();
  builder.setInsertionPointToEnd(module.getBody());
  auto funcOp = builder.create<mlir::func::FuncOp>(location, mangledName, funcType);

  auto *entryBlock = funcOp.addEntryBlock();
  builder.setInsertionPointToStart(entryBlock);

  auto prevFunction = currentFunction;
  currentFunction = funcOp;
  auto prevReturnFlag = returnFlag;
  auto prevReturnSlot = returnSlot;
  auto prevFuncLevelDropExcludeVars = std::move(funcLevelDropExcludeVars);
  auto prevFnDefers = std::move(currentFnDefers);
  returnFlag = nullptr;
  returnSlot = nullptr;
  funcLevelDropExcludeVars.clear();
  currentFnDefers.clear();

  uint32_t paramIdx = 0;
  for (const auto &param : method.params) {
    declareVariable(param.name, entryBlock->getArgument(paramIdx));
    ++paramIdx;
  }

  // Early return support — always create returnFlag so that `return` inside
  // nested SCF regions (match arms, if branches) stores to the flag instead
  // of emitting an illegal func.return inside an scf.if.
  bool canUseReturnSlot = !resultTypes.empty() &&
                          !mlir::isa<mlir::LLVM::LLVMStructType>(resultTypes[0]) &&
                          !mlir::isa<mlir::LLVM::LLVMArrayType>(resultTypes[0]) &&
                          !mlir::isa<hew::HewTupleType>(resultTypes[0]) &&
                          !mlir::isa<hew::HewArrayType>(resultTypes[0]) &&
                          !mlir::isa<hew::HewTraitObjectType>(resultTypes[0]);
  {
    auto i1Type = builder.getI1Type();
    auto flagMemrefType = mlir::MemRefType::get({}, i1Type);
    returnFlag = builder.create<mlir::memref::AllocaOp>(location, flagMemrefType);
    auto falseVal = createIntConstant(builder, location, i1Type, 0);
    builder.create<mlir::memref::StoreOp>(location, falseVal, returnFlag);

    if (canUseReturnSlot) {
      auto slotMemrefType = mlir::MemRefType::get({}, resultTypes[0]);
      returnSlot = builder.create<mlir::memref::AllocaOp>(location, slotMemrefType);
    }
  }

  mlir::Value bodyValue = generateBlock(*method.body);

  auto *currentBlock = builder.getInsertionBlock();
  if (currentBlock &&
      (currentBlock->empty() || !currentBlock->back().hasTrait<mlir::OpTrait::IsTerminator>())) {
    if (returnFlag && returnSlot && !resultTypes.empty()) {
      auto flagVal = builder.create<mlir::memref::LoadOp>(location, returnFlag, mlir::ValueRange{});
      auto selectOp = builder.create<mlir::scf::IfOp>(location, resultTypes[0], flagVal,
                                                      /*withElseRegion=*/true);

      builder.setInsertionPointToStart(&selectOp.getThenRegion().front());
      auto slotVal = builder.create<mlir::memref::LoadOp>(location, returnSlot, mlir::ValueRange{});
      builder.create<mlir::scf::YieldOp>(location, mlir::ValueRange{slotVal});

      builder.setInsertionPointToStart(&selectOp.getElseRegion().front());
      mlir::Value normalValue = bodyValue;
      if (!normalValue)
        normalValue = createDefaultValue(builder, location, resultTypes[0]);
      normalValue = coerceType(normalValue, resultTypes[0], location);
      builder.create<mlir::scf::YieldOp>(location, mlir::ValueRange{normalValue});

      builder.setInsertionPointAfter(selectOp);
      builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{selectOp.getResult(0)});
    } else if (bodyValue && !resultTypes.empty()) {
      bodyValue = coerceType(bodyValue, resultTypes[0], location);
      builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{bodyValue});
    } else {
      builder.create<mlir::func::ReturnOp>(location);
    }
  }

  returnFlag = prevReturnFlag;
  returnSlot = prevReturnSlot;
  funcLevelDropExcludeVars = std::move(prevFuncLevelDropExcludeVars);
  currentFnDefers = std::move(prevFnDefers);
  currentFunction = prevFunction;
  builder.restoreInsertionPoint(savedIP);
}

// ============================================================================
// Function generation
// ============================================================================

mlir::func::FuncOp MLIRGen::generateFunction(const ast::FnDecl &fn,
                                             const std::string &nameOverride) {
  // Create symbol table scopes for this function
  SymbolTableScopeT varScope(symbolTable);
  MutableTableScopeT mutScope(mutableVars);

  // Determine parameter types
  llvm::SmallVector<mlir::Type, 4> paramTypes;
  for (const auto &param : fn.params) {
    paramTypes.push_back(convertType(param.ty.value));
  }

  // Determine return type
  llvm::SmallVector<mlir::Type, 1> resultTypes;
  if (fn.return_type) {
    auto retTy = convertType(fn.return_type->value);
    // Don't add NoneType to results (unit return = no results)
    if (!llvm::isa<mlir::NoneType>(retTy)) {
      resultTypes.push_back(retTy);
    }
  }

  auto location = currentLoc;

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
  if (auto existing = module.lookupSymbol<mlir::func::FuncOp>(funcName)) {
    if (existing.isDeclaration())
      existing.erase();
  }

  auto savedIP = builder.saveInsertionPoint();
  builder.setInsertionPointToEnd(module.getBody());
  auto funcOp = builder.create<mlir::func::FuncOp>(location, funcName, funcType);

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
  auto prevFuncLevelDropExcludeVars = std::move(funcLevelDropExcludeVars);
  auto prevFnDefers = std::move(currentFnDefers);
  returnFlag = nullptr;
  returnSlot = nullptr;
  funcLevelDropExcludeVars.clear();
  currentFnDefers.clear();
  uint32_t paramIdx = 0;
  for (const auto &param : fn.params) {
    const auto &paramName = param.name;
    declareVariable(paramName, entryBlock->getArgument(paramIdx));

    // Track collection/handle/actor parameter types from type annotation
    const auto &paramTy = param.ty.value;
    {
      auto resolveAlias = [this](const std::string &n) { return resolveTypeAlias(n); };
      auto collStr = typeExprToCollectionString(paramTy, resolveAlias);
      if (collStr.rfind("HashMap<", 0) == 0)
        collectionVarTypes[paramName] = collStr;
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
  // The returnSlot is only for non-void functions with memref-compatible return types.
  bool canUseReturnSlot = !resultTypes.empty() &&
                          !mlir::isa<mlir::LLVM::LLVMStructType>(resultTypes[0]) &&
                          !mlir::isa<mlir::LLVM::LLVMArrayType>(resultTypes[0]) &&
                          !mlir::isa<hew::HewTupleType>(resultTypes[0]) &&
                          !mlir::isa<hew::HewArrayType>(resultTypes[0]) &&
                          !mlir::isa<hew::HewTraitObjectType>(resultTypes[0]);
  {
    auto i1Type = builder.getI1Type();
    auto flagMemrefType = mlir::MemRefType::get({}, i1Type);
    returnFlag = builder.create<mlir::memref::AllocaOp>(location, flagMemrefType);
    auto falseVal = createIntConstant(builder, location, i1Type, 0);
    builder.create<mlir::memref::StoreOp>(location, falseVal, returnFlag);

    if (canUseReturnSlot) {
      auto slotMemrefType = mlir::MemRefType::get({}, resultTypes[0]);
      returnSlot = builder.create<mlir::memref::AllocaOp>(location, slotMemrefType);
    }
  }

  // Determine trailing expression variable names BEFORE generating the body.
  // For struct init expressions, collect all field variable references to
  // prevent double-free when the struct takes ownership of the values.
  funcLevelDropExcludeVars.clear();
  auto collectExcludeVars = [](const ast::Expr &expr, std::set<std::string> &out) {
    if (auto *identExpr = std::get_if<ast::ExprIdentifier>(&expr.kind)) {
      out.insert(identExpr->name);
    } else if (auto *si = std::get_if<ast::ExprStructInit>(&expr.kind)) {
      for (const auto &[fieldName, fieldVal] : si->fields) {
        if (auto *id = std::get_if<ast::ExprIdentifier>(&fieldVal->value.kind))
          out.insert(id->name);
      }
    }
  };
  if (fn.body.trailing_expr) {
    collectExcludeVars(fn.body.trailing_expr->value, funcLevelDropExcludeVars);
  } else if (!fn.body.stmts.empty()) {
    const auto &last = fn.body.stmts.back()->value;
    if (auto *exprStmt = std::get_if<ast::StmtExpression>(&last.kind)) {
      collectExcludeVars(exprStmt->expr.value, funcLevelDropExcludeVars);
    }
  }

  // Generate the function body
  mlir::Value bodyValue = generateBlock(fn.body);
  funcLevelDropExcludeVars.clear();

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

    // Collect variable names referenced in the trailing/return expression
    // to exclude from scope drops (ownership transfers to the return value).
    std::set<std::string> trailingVarNames;
    auto collectVarRefs = [](const ast::Expr &expr, std::set<std::string> &out) {
      // Simple identifier
      if (auto *id = std::get_if<ast::ExprIdentifier>(&expr.kind)) {
        out.insert(id->name);
        return;
      }
      // Struct init: collect all field value identifiers
      if (auto *si = std::get_if<ast::ExprStructInit>(&expr.kind)) {
        for (const auto &[fieldName, fieldVal] : si->fields) {
          if (auto *id = std::get_if<ast::ExprIdentifier>(&fieldVal->value.kind))
            out.insert(id->name);
        }
        return;
      }
    };
    if (fn.body.trailing_expr) {
      collectVarRefs(fn.body.trailing_expr->value, trailingVarNames);
    } else if (!fn.body.stmts.empty()) {
      const auto &last = fn.body.stmts.back()->value;
      if (auto *exprStmt = std::get_if<ast::StmtExpression>(&last.kind)) {
        collectVarRefs(exprStmt->expr.value, trailingVarNames);
      }
    }

    if (returnFlag && returnSlot && !resultTypes.empty()) {
      // Select between returnSlot (early return) and bodyValue (normal flow)
      auto flagVal = builder.create<mlir::memref::LoadOp>(location, returnFlag, mlir::ValueRange{});

      auto selectOp = builder.create<mlir::scf::IfOp>(location, resultTypes[0], flagVal,
                                                      /*withElseRegion=*/true);

      // Then (early return was taken): load from return slot
      builder.setInsertionPointToStart(&selectOp.getThenRegion().front());
      auto slotVal = builder.create<mlir::memref::LoadOp>(location, returnSlot, mlir::ValueRange{});
      builder.create<mlir::scf::YieldOp>(location, mlir::ValueRange{slotVal});

      // Else (normal flow): use body value or default
      builder.setInsertionPointToStart(&selectOp.getElseRegion().front());
      mlir::Value normalValue = bodyValue;
      if (!normalValue) {
        normalValue = createDefaultValue(builder, location, resultTypes[0]);
      }
      normalValue = coerceType(normalValue, resultTypes[0], location);
      builder.create<mlir::scf::YieldOp>(location, mlir::ValueRange{normalValue});

      builder.setInsertionPointAfter(selectOp);
      // Emit drops before return (excluding the returned variables)
      if (!trailingVarNames.empty())
        emitDropsExcept(trailingVarNames);
      else
        emitAllDrops();
      builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{selectOp.getResult(0)});
    } else if (bodyValue && !resultTypes.empty()) {
      // Emit drops before return (excluding the returned variables)
      if (!trailingVarNames.empty())
        emitDropsExcept(trailingVarNames);
      else
        emitAllDrops();
      auto coercedBody = coerceType(bodyValue, resultTypes[0], location);
      builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{coercedBody});
    } else {
      emitAllDrops();
      if (isImplicitMainReturn) {
        // Patch the function type to return i32 and emit return 0
        resultTypes.push_back(builder.getI32Type());
        funcOp.setFunctionType(builder.getFunctionType(paramTypes, resultTypes));
        auto zero = createIntConstant(builder, location, builder.getI32Type(), 0);
        builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{zero});
      } else {
        builder.create<mlir::func::ReturnOp>(location);
      }
    }
  }

  // Restore previous function state
  returnFlag = prevReturnFlag;
  returnSlot = prevReturnSlot;
  funcLevelDropExcludeVars = std::move(prevFuncLevelDropExcludeVars);
  currentFnDefers = std::move(prevFnDefers);
  currentFunction = prevFunction;
  builder.restoreInsertionPoint(savedIP);
  return funcOp;
}

// ============================================================================
// Generator function codegen (state machine transformation)
// ============================================================================

// Forward declaration for mutual recursion.
static void collectYieldsFromExpr(const ast::Expr &expr, std::vector<const ast::Expr *> &yields);

// Helper: recursively collect yield expressions from statements and blocks.
static void collectYields(const ast::Block &block, std::vector<const ast::Expr *> &yields) {
  for (const auto &stmtPtr : block.stmts) {
    const auto &stmt = stmtPtr->value;
    // Recurse into nested statement bodies
    if (auto *ifStmt = std::get_if<ast::StmtIf>(&stmt.kind)) {
      collectYields(ifStmt->then_block, yields);
      if (ifStmt->else_block) {
        if (ifStmt->else_block->is_if && ifStmt->else_block->if_stmt) {
          // else-if: recurse via the if_stmt
          if (auto *nestedIf = std::get_if<ast::StmtIf>(&ifStmt->else_block->if_stmt->value.kind)) {
            collectYields(nestedIf->then_block, yields);
          }
        }
        if (ifStmt->else_block->block)
          collectYields(*ifStmt->else_block->block, yields);
      }
    } else if (auto *forStmt = std::get_if<ast::StmtFor>(&stmt.kind)) {
      collectYields(forStmt->body, yields);
    } else if (auto *whileStmt = std::get_if<ast::StmtWhile>(&stmt.kind)) {
      collectYields(whileStmt->body, yields);
    } else if (auto *loopStmt = std::get_if<ast::StmtLoop>(&stmt.kind)) {
      collectYields(loopStmt->body, yields);
    } else if (auto *matchStmt = std::get_if<ast::StmtMatch>(&stmt.kind)) {
      for (const auto &arm : matchStmt->arms) {
        if (arm.body)
          collectYieldsFromExpr(arm.body->value, yields);
      }
    } else if (auto *exprStmt = std::get_if<ast::StmtExpression>(&stmt.kind)) {
      collectYieldsFromExpr(exprStmt->expr.value, yields);
    }
  }
  // Also check trailing expression
  if (block.trailing_expr)
    collectYieldsFromExpr(block.trailing_expr->value, yields);
}

// Recursively collect yield expressions from an expression tree.
static void collectYieldsFromExpr(const ast::Expr &expr, std::vector<const ast::Expr *> &yields) {
  if (std::holds_alternative<ast::ExprYield>(expr.kind)) {
    yields.push_back(&expr);
    return;
  }
  // Recurse into sub-expressions that may contain blocks
  if (auto *blockExpr = std::get_if<ast::ExprBlock>(&expr.kind)) {
    collectYields(blockExpr->block, yields);
  }
  if (auto *ifExpr = std::get_if<ast::ExprIf>(&expr.kind)) {
    if (ifExpr->then_block)
      collectYieldsFromExpr(ifExpr->then_block->value, yields);
    if (ifExpr->else_block && *ifExpr->else_block)
      collectYieldsFromExpr((*ifExpr->else_block)->value, yields);
  }
  if (auto *matchExpr = std::get_if<ast::ExprMatch>(&expr.kind)) {
    for (const auto &arm : matchExpr->arms) {
      if (arm.body)
        collectYieldsFromExpr(arm.body->value, yields);
    }
  }
}

void MLIRGen::generateGeneratorFunction(const ast::FnDecl &fn) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i32Type = builder.getI32Type();

  // Determine the yield value type from the return type annotation
  mlir::Type yieldType = i32Type; // default
  if (fn.return_type) {
    auto retTy = convertType(fn.return_type->value);
    if (!llvm::isa<mlir::NoneType>(retTy))
      yieldType = retTy;
  }

  // Collect all yield expressions from the function body
  std::vector<const ast::Expr *> yields;
  collectYields(fn.body, yields);

  if (yields.empty()) {
    emitWarning(location) << "generator function has no yield expressions";
    return;
  }

  const std::string &fnName = fn.name;

  // Register as generator function
  generatorFunctions.insert(fnName);

  // Generator state struct: { i32 __state, <yieldType> __value }
  auto stateStructType = mlir::LLVM::LLVMStructType::getLiteral(&context, {i32Type, yieldType});

  // ── Generate init function: <name>() -> !llvm.ptr ──────────────────
  {
    auto savedIP = builder.saveInsertionPoint();
    builder.setInsertionPointToEnd(module.getBody());

    auto initFuncType = builder.getFunctionType({}, {ptrType});
    auto initFunc = builder.create<mlir::func::FuncOp>(location, fnName, initFuncType);
    auto *entryBlock = initFunc.addEntryBlock();
    builder.setInsertionPointToStart(entryBlock);

    // Allocate state struct on HEAP via malloc (must outlive init function)
    // First declare malloc if not already declared
    auto mallocFunc = module.lookupSymbol<mlir::func::FuncOp>("malloc");
    if (!mallocFunc) {
      auto mallocType = builder.getFunctionType({sizeType()}, {ptrType});
      auto savedIP2 = builder.saveInsertionPoint();
      builder.setInsertionPointToStart(module.getBody());
      mallocFunc = builder.create<mlir::func::FuncOp>(location, "malloc", mallocType);
      mallocFunc.setPrivate();
      builder.restoreInsertionPoint(savedIP2);
    }
    // Calculate size
    auto sizeVal =
        builder.create<hew::SizeOfOp>(location, sizeType(), mlir::TypeAttr::get(stateStructType));
    auto mallocCall =
        builder.create<mlir::func::CallOp>(location, mallocFunc, mlir::ValueRange{sizeVal});
    auto allocPtr = mallocCall.getResult(0);

    // Set state = 0
    auto statePtr = builder.create<mlir::LLVM::GEPOp>(location, ptrType, stateStructType, allocPtr,
                                                      llvm::ArrayRef<mlir::LLVM::GEPArg>{0, 0});
    auto zero = builder.create<mlir::arith::ConstantIntOp>(location, i32Type, 0);
    builder.create<mlir::LLVM::StoreOp>(location, zero, statePtr);

    builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{allocPtr});
    builder.restoreInsertionPoint(savedIP);
  }

  // ── Generate next function: <name>__next(!llvm.ptr) -> <yieldType> ─
  {
    auto savedIP = builder.saveInsertionPoint();
    builder.setInsertionPointToEnd(module.getBody());

    std::string nextName = fnName + "__next";
    auto nextFuncType = builder.getFunctionType({ptrType}, {yieldType});
    auto nextFunc = builder.create<mlir::func::FuncOp>(location, nextName, nextFuncType);
    auto *entryBlock = nextFunc.addEntryBlock();
    builder.setInsertionPointToStart(entryBlock);

    auto genPtr = entryBlock->getArgument(0);

    // State pointer (field 0 of struct)
    auto statePtr = builder.create<mlir::LLVM::GEPOp>(location, ptrType, stateStructType, genPtr,
                                                      llvm::ArrayRef<mlir::LLVM::GEPArg>{0, 0});

    // Value pointer (field 1 of struct)
    auto valuePtr = builder.create<mlir::LLVM::GEPOp>(location, ptrType, stateStructType, genPtr,
                                                      llvm::ArrayRef<mlir::LLVM::GEPArg>{0, 1});

    // Generate switch on state: each yield becomes a case
    // Default block returns a zero/default value (generator exhausted)
    auto *defaultBlock = nextFunc.addBlock();
    builder.setInsertionPointToStart(defaultBlock);
    mlir::Value defaultVal;
    if (llvm::isa<mlir::IntegerType>(yieldType)) {
      defaultVal = builder.create<mlir::arith::ConstantIntOp>(location, yieldType, 0);
    } else if (llvm::isa<mlir::FloatType>(yieldType)) {
      defaultVal =
          builder.create<mlir::arith::ConstantOp>(location, builder.getFloatAttr(yieldType, 0.0));
    } else {
      defaultVal = builder.create<mlir::LLVM::ZeroOp>(location, yieldType);
    }
    builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{defaultVal});

    // Create case blocks for each yield
    llvm::SmallVector<int32_t> caseValues;
    llvm::SmallVector<mlir::Block *> caseBlocks;
    for (size_t i = 0; i < yields.size(); ++i) {
      caseValues.push_back(static_cast<int32_t>(i));
      auto *caseBlock = nextFunc.addBlock();
      caseBlocks.push_back(caseBlock);
    }

    // Generate the switch in the entry block
    builder.setInsertionPointToEnd(entryBlock);

    // Load current state for the switch
    auto stateVal = builder.create<mlir::LLVM::LoadOp>(location, i32Type, statePtr);

    // Use cf.switch for multi-way branch
    llvm::SmallVector<mlir::ValueRange> caseOperands(yields.size(), mlir::ValueRange{});
    llvm::SmallVector<int32_t> caseValuesForSwitch;
    llvm::SmallVector<mlir::Block *> caseDests;
    for (size_t i = 0; i < yields.size(); ++i) {
      caseValuesForSwitch.push_back(static_cast<int32_t>(i));
      caseDests.push_back(caseBlocks[i]);
    }

    builder.create<mlir::cf::SwitchOp>(location, stateVal, defaultBlock, mlir::ValueRange{},
                                       caseValuesForSwitch, caseDests, caseOperands);

    // Generate each case block: store yield value, bump state, return value
    for (size_t i = 0; i < yields.size(); ++i) {
      builder.setInsertionPointToStart(caseBlocks[i]);

      // Generate the yield value expression
      // We need a fresh symbol table scope for expression generation
      SymbolTableScopeT varScope(symbolTable);
      MutableTableScopeT mutScope(mutableVars);

      auto prevFunction = currentFunction;
      currentFunction = nextFunc;

      mlir::Value yieldVal = nullptr;
      const auto *yieldExpr = std::get_if<ast::ExprYield>(&yields[i]->kind);
      if (yieldExpr && yieldExpr->value && *yieldExpr->value) {
        yieldVal = generateExpression((*yieldExpr->value)->value);
      }
      if (!yieldVal) {
        if (llvm::isa<mlir::IntegerType>(yieldType)) {
          yieldVal = builder.create<mlir::arith::ConstantIntOp>(location, yieldType, 0);
        } else {
          yieldVal = builder.create<mlir::LLVM::ZeroOp>(location, yieldType);
        }
      }

      currentFunction = prevFunction;

      // Store value
      builder.create<mlir::LLVM::StoreOp>(location, yieldVal, valuePtr);

      // Bump state to next
      auto nextState = builder.create<mlir::arith::ConstantIntOp>(location, i32Type,
                                                                  static_cast<int64_t>(i + 1));
      // Need a fresh statePtr since we're in a different block
      auto sp = builder.create<mlir::LLVM::GEPOp>(location, ptrType, stateStructType, genPtr,
                                                  llvm::ArrayRef<mlir::LLVM::GEPArg>{0, 0});
      builder.create<mlir::LLVM::StoreOp>(location, nextState, sp);

      // Return the yielded value
      builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{yieldVal});
    }

    builder.restoreInsertionPoint(savedIP);
  }

  // ── Generate done function: <name>__done(!llvm.ptr) -> i1 ───────────
  {
    auto savedIP = builder.saveInsertionPoint();
    builder.setInsertionPointToEnd(module.getBody());

    std::string doneName = fnName + "__done";
    auto i1Type = builder.getI1Type();
    auto doneFuncType = builder.getFunctionType({ptrType}, {i1Type});
    auto doneFunc = builder.create<mlir::func::FuncOp>(location, doneName, doneFuncType);
    auto *entryBlock = doneFunc.addEntryBlock();
    builder.setInsertionPointToStart(entryBlock);

    auto genPtr = entryBlock->getArgument(0);
    auto statePtr = builder.create<mlir::LLVM::GEPOp>(location, ptrType, stateStructType, genPtr,
                                                      llvm::ArrayRef<mlir::LLVM::GEPArg>{0, 0});
    auto stateVal = builder.create<mlir::LLVM::LoadOp>(location, i32Type, statePtr);
    auto numYields = builder.create<mlir::arith::ConstantIntOp>(
        location, i32Type, static_cast<int64_t>(yields.size()));
    auto isDone = builder.create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::sge,
                                                      stateVal, numYields);
    builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{isDone});

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

void MLIRGen::popDropScope() {
  if (dropScopes.empty())
    return;
  if (builder.getInsertionBlock()) {
    auto *block = builder.getInsertionBlock();
    auto *parentOp = block->getParentOp();
    bool isFuncLevel = mlir::isa<mlir::func::FuncOp>(parentOp);

    if (isFuncLevel) {
      // Function-level scope: emit drops here while the symbol table is
      // still alive (the DropScopeGuard destructor runs before the
      // SymbolTableScope destructor in generateBlock).
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
        auto emitFuncLevelDrops = [&]() {
          if (!funcLevelDropExcludeVars.empty()) {
            auto &scope = dropScopes.back();
            auto ptrType = mlir::LLVM::LLVMPointerType::get(builder.getContext());
            for (auto it = scope.rbegin(); it != scope.rend(); ++it) {
              if (funcLevelDropExcludeVars.count(it->varName))
                continue;
              auto val = lookupVariable(it->varName);
              if (!val)
                continue;
              mlir::Value dropVal = val;
              if (mlir::isa<hew::ClosureType>(val.getType()))
                dropVal =
                    builder.create<hew::ClosureGetEnvOp>(builder.getUnknownLoc(), ptrType, val);
              if (!mlir::isa<mlir::LLVM::LLVMPointerType>(dropVal.getType()) && !it->isUserDrop)
                dropVal = builder.create<hew::BitcastOp>(builder.getUnknownLoc(), ptrType, dropVal);
              builder.create<hew::DropOp>(builder.getUnknownLoc(), dropVal, it->dropFuncName,
                                          it->isUserDrop);
            }
          } else {
            emitDropsForScope(dropScopes.back());
          }
        };
        emitDeferredCalls();
        emitFuncLevelDrops();
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
      // Guard drops with !returnFlag: when an early return stores a value
      // to returnSlot, we must not free it (or any other inner-scope var
      // that might alias it). The arena will reclaim the memory.
      if (returnFlag) {
        auto loc = builder.getUnknownLoc();
        auto flagVal = builder.create<mlir::memref::LoadOp>(loc, returnFlag, mlir::ValueRange{});
        auto trueConst = createIntConstant(builder, loc, builder.getI1Type(), 1);
        auto notReturned = builder.create<mlir::arith::XOrIOp>(loc, flagVal, trueConst);
        auto guard = builder.create<mlir::scf::IfOp>(loc, mlir::TypeRange{}, notReturned,
                                                     /*withElseRegion=*/false);
        builder.setInsertionPointToStart(&guard.getThenRegion().front());
        emitDropsForScope(dropScopes.back());
        // scf.if auto-adds yield; set insertion after guard
        builder.setInsertionPointAfter(guard);
      } else {
        emitDropsForScope(dropScopes.back());
      }
    }
  }
  dropScopes.pop_back();
}

void MLIRGen::registerDroppable(const std::string &varName, const std::string &dropFunc,
                                bool isUserDrop) {
  if (!dropScopes.empty()) {
    dropScopes.back().push_back({varName, dropFunc, isUserDrop});
  }
}

void MLIRGen::unregisterDroppable(const std::string &varName) {
  for (auto &scope : dropScopes) {
    scope.erase(std::remove_if(scope.begin(), scope.end(),
                               [&](const DropEntry &e) { return e.varName == varName; }),
                scope.end());
  }
}

void MLIRGen::emitDropsForScope(const std::vector<DropEntry> &scope) {
  if (scope.empty())
    return;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(builder.getContext());
  for (auto it = scope.rbegin(); it != scope.rend(); ++it) {
    auto val = lookupVariable(it->varName);
    if (!val)
      continue;
    // For closures, extract env_ptr — hew_rc_drop expects !llvm.ptr
    mlir::Value dropVal = val;
    if (mlir::isa<hew::ClosureType>(val.getType()))
      dropVal = builder.create<hew::ClosureGetEnvOp>(builder.getUnknownLoc(), ptrType, val);
    // Cast Hew dialect types (hashmap, vec, etc.) to !llvm.ptr for free fns
    if (!mlir::isa<mlir::LLVM::LLVMPointerType>(dropVal.getType()) && !it->isUserDrop)
      dropVal = builder.create<hew::BitcastOp>(builder.getUnknownLoc(), ptrType, dropVal);
    builder.create<hew::DropOp>(builder.getUnknownLoc(), dropVal, it->dropFuncName, it->isUserDrop);
  }
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
  auto ptrType = mlir::LLVM::LLVMPointerType::get(builder.getContext());
  for (auto it = scope.rbegin(); it != scope.rend(); ++it) {
    auto val = lookupVariable(it->varName);
    if (!val)
      continue;
    // For closures, extract env_ptr — hew_rc_drop expects !llvm.ptr
    mlir::Value dropVal = val;
    if (mlir::isa<hew::ClosureType>(val.getType()))
      dropVal = builder.create<hew::ClosureGetEnvOp>(builder.getUnknownLoc(), ptrType, val);
    // Cast Hew dialect types (hashmap, vec, etc.) to !llvm.ptr for free fns
    if (!mlir::isa<mlir::LLVM::LLVMPointerType>(dropVal.getType()) && !it->isUserDrop)
      dropVal = builder.create<hew::BitcastOp>(builder.getUnknownLoc(), ptrType, dropVal);
    builder.create<hew::DropOp>(builder.getUnknownLoc(), dropVal, it->dropFuncName, it->isUserDrop);
  }
}

void MLIRGen::emitAllDrops() {
  auto *parentOp = builder.getInsertionBlock()->getParentOp();
  if (!mlir::isa<mlir::func::FuncOp>(parentOp))
    return;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(builder.getContext());
  for (auto scopeIt = dropScopes.rbegin(); scopeIt != dropScopes.rend(); ++scopeIt) {
    for (auto it = scopeIt->rbegin(); it != scopeIt->rend(); ++it) {
      auto val = lookupVariable(it->varName);
      if (!val)
        continue;
      // For closures, extract env_ptr — hew_rc_drop expects !llvm.ptr
      mlir::Value dropVal = val;
      if (mlir::isa<hew::ClosureType>(val.getType()))
        dropVal = builder.create<hew::ClosureGetEnvOp>(builder.getUnknownLoc(), ptrType, val);
      // Cast Hew dialect types (hashmap, vec, etc.) to !llvm.ptr for free fns
      if (!mlir::isa<mlir::LLVM::LLVMPointerType>(dropVal.getType()) && !it->isUserDrop)
        dropVal = builder.create<hew::BitcastOp>(builder.getUnknownLoc(), ptrType, dropVal);
      builder.create<hew::DropOp>(builder.getUnknownLoc(), dropVal, it->dropFuncName,
                                  it->isUserDrop);
    }
  }
}

void MLIRGen::emitStringDrop(mlir::Value v) {
  if (!v || !builder.getInsertionBlock())
    return;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(builder.getContext());
  mlir::Value dropVal = v;
  if (!mlir::isa<mlir::LLVM::LLVMPointerType>(v.getType()))
    dropVal = builder.create<hew::BitcastOp>(builder.getUnknownLoc(), ptrType, v);
  auto funcType = mlir::FunctionType::get(builder.getContext(), {ptrType}, {});
  getOrCreateExternFunc("hew_string_drop", funcType);
  builder.create<mlir::func::CallOp>(builder.getUnknownLoc(), "hew_string_drop", mlir::TypeRange{},
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

void MLIRGen::emitDropsExcept(const std::string &excludeVar) {
  std::set<std::string> excludeSet;
  excludeSet.insert(excludeVar);
  emitDropsExcept(excludeSet);
}

void MLIRGen::emitDropsExcept(const std::set<std::string> &excludeVars) {
  auto *parentOp = builder.getInsertionBlock()->getParentOp();
  if (!mlir::isa<mlir::func::FuncOp>(parentOp))
    return;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(builder.getContext());
  for (auto scopeIt = dropScopes.rbegin(); scopeIt != dropScopes.rend(); ++scopeIt) {
    for (auto it = scopeIt->rbegin(); it != scopeIt->rend(); ++it) {
      if (excludeVars.count(it->varName))
        continue;
      auto val = lookupVariable(it->varName);
      if (!val)
        continue;
      // For closures, extract env_ptr — hew_rc_drop expects !llvm.ptr
      mlir::Value dropVal = val;
      if (mlir::isa<hew::ClosureType>(val.getType()))
        dropVal = builder.create<hew::ClosureGetEnvOp>(builder.getUnknownLoc(), ptrType, val);
      // Cast Hew dialect types (hashmap, vec, etc.) to !llvm.ptr for free fns
      if (!mlir::isa<mlir::LLVM::LLVMPointerType>(dropVal.getType()) && !it->isUserDrop)
        dropVal = builder.create<hew::BitcastOp>(builder.getUnknownLoc(), ptrType, dropVal);
      builder.create<hew::DropOp>(builder.getUnknownLoc(), dropVal, it->dropFuncName,
                                  it->isUserDrop);
    }
  }
}

// ── Defer execution ──────────────────────────────────────────

void MLIRGen::emitDeferredCalls() {
  // Execute all deferred expressions in LIFO order (last registered first).
  for (auto it = currentFnDefers.rbegin(); it != currentFnDefers.rend(); ++it) {
    generateExpression(*it->expr);
  }
}
