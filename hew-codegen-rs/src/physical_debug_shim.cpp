// LLVM debug operations without an LLVM-C entry point. Variant parts preserve
// enum payload identity; pointer-value locations need explicit storage after
// coroutine splitting so the backend keeps address and value indirection apart.

#include "llvm-c/DebugInfo.h"
#include "llvm-c/Types.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/BinaryFormat/Dwarf.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/DebugProgramInstruction.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"

using namespace llvm;

static DIBuilder *unwrapBuilder(LLVMDIBuilderRef builder) {
  return reinterpret_cast<DIBuilder *>(builder);
}

template <typename T> static T *unwrapMetadata(LLVMMetadataRef metadata) {
  if (!metadata)
    return nullptr;
  return dyn_cast<T>(reinterpret_cast<Metadata *>(metadata));
}

/// One `DW_TAG_variant`: the payload struct of a single case, selected by the
/// constant the enum's discriminant holds when that case is active.
extern "C" LLVMMetadataRef hewLLVMDIBuilderCreateVariantMemberType(
    LLVMDIBuilderRef builder, LLVMMetadataRef scope, const char *name,
    size_t name_len, LLVMMetadataRef file, uint64_t size_in_bits,
    uint32_t align_in_bits, uint64_t offset_in_bits, LLVMValueRef discriminant,
    LLVMMetadataRef type) {
  auto *scope_node = unwrapMetadata<DIScope>(scope);
  auto *file_node = unwrapMetadata<DIFile>(file);
  auto *type_node = unwrapMetadata<DIType>(type);
  auto *constant =
      dyn_cast_or_null<Constant>(reinterpret_cast<Value *>(discriminant));
  if (!builder || !scope_node || !file_node || !type_node || !constant)
    return nullptr;

  auto *member = unwrapBuilder(builder)->createVariantMemberType(
      scope_node, StringRef(name, name_len), file_node, /* LineNumber */ 0,
      size_in_bits, align_in_bits, offset_in_bits, constant, DINode::FlagPublic,
      type_node);
  return reinterpret_cast<LLVMMetadataRef>(member);
}

/// The `DW_TAG_variant_part` holding every case. `discriminator` is the
/// artificial member describing the tag; LLVM emits it as the part's own child
/// and points `DW_AT_discr` at it.
extern "C" LLVMMetadataRef hewLLVMDIBuilderCreateVariantPart(
    LLVMDIBuilderRef builder, LLVMMetadataRef scope, LLVMMetadataRef file,
    uint64_t size_in_bits, uint32_t align_in_bits,
    LLVMMetadataRef discriminator, const LLVMMetadataRef *elements,
    unsigned element_count) {
  auto *scope_node = unwrapMetadata<DIScope>(scope);
  auto *file_node = unwrapMetadata<DIFile>(file);
  auto *discriminator_node = unwrapMetadata<DIDerivedType>(discriminator);
  if (!builder || !scope_node || !file_node || !discriminator_node ||
      (element_count != 0 && !elements))
    return nullptr;

  SmallVector<Metadata *, 8> nodes;
  nodes.reserve(element_count);
  for (unsigned index = 0; index < element_count; ++index) {
    auto *node = unwrapMetadata<DINode>(elements[index]);
    if (!node)
      return nullptr;
    nodes.push_back(node);
  }

  auto *part = unwrapBuilder(builder)->createVariantPart(
      scope_node, StringRef(), file_node, /* LineNumber */ 0, size_in_bits,
      align_in_bits, DINode::FlagPublic, discriminator_node,
      unwrapBuilder(builder)->getOrCreateArray(nodes));
  return reinterpret_cast<LLVMMetadataRef>(part);
}

// LLVM 22's AArch64 backend drops the wrong dereference from a spilled frame
// address with a field offset. Materialize the field address after CoroSplit;
// remove this normalization when both backends preserve the original
// expression.
extern "C" void hewLLVMStabilizeDebugFrameLocations(LLVMModuleRef module) {
  auto *ir = reinterpret_cast<Module *>(module);
  if (!ir->getNamedMetadata("llvm.dbg.cu"))
    return;
  for (auto &function : *ir) {
    SmallVector<std::pair<DbgVariableRecord *, Instruction *>, 8> records;
    for (auto &block : function)
      for (auto &instruction : block)
        for (auto &record : instruction.getDbgRecordRange())
          if (auto *variable = dyn_cast<DbgVariableRecord>(&record))
            if (variable->isDbgValue() && !variable->isKillLocation() &&
                !variable->hasArgList() &&
                variable->getNumVariableLocationOps() == 1)
              if (variable->getVariableLocationOp(0)
                      ->getType()
                      ->isPointerTy()) {
                auto ops = variable->getExpression()->getElements();
                unsigned offset = ops.size() == 4 ? 1 : 0;
                if ((ops.size() == 3 || ops.size() == 4) &&
                    (offset == 0 || ops[0] == dwarf::DW_OP_deref) &&
                    ops[offset] == dwarf::DW_OP_plus_uconst &&
                    ops.back() == dwarf::DW_OP_deref)
                  records.push_back({variable, &instruction});
              }

    DenseMap<DILocalVariable *, AllocaInst *> slots;
    for (auto [variable, instruction] : records) {
      auto *pointer = variable->getVariableLocationOp(0);
      auto ops = variable->getExpression()->getElements();
      auto *marker = variable->getMarker();
      DbgMarker suffix;
      suffix.absorbDebugValues(make_range(variable->getIterator(),
                                          marker->getDbgRecordRange().end()),
                               *marker, false);
      IRBuilder<> location(instruction);
      location.SetCurrentDebugLocation(DebugLoc());
      Value *address = pointer;
      if (ops.size() == 4)
        address = location.CreateLoad(pointer->getType(), address);
      address = location.CreateConstGEP1_64(Type::getInt8Ty(ir->getContext()),
                                            address, ops[ops.size() - 2]);
      auto &slot = slots[variable->getVariable()];
      if (!slot) {
        IRBuilder<> entry(&function.getEntryBlock(),
                          function.getEntryBlock().getFirstInsertionPt());
        slot = entry.CreateAlloca(pointer->getType(), nullptr,
                                  "debug.local.address");
      }
      location.CreateStore(address, slot);
      variable->replaceVariableLocationOp(pointer, slot);
      variable->setExpression(DIExpression::get(
          ir->getContext(), {dwarf::DW_OP_deref, dwarf::DW_OP_deref}));
      instruction->getParent()
          ->createMarker(instruction)
          ->absorbDebugValues(suffix, false);
    }
  }
}
