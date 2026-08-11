#include "llvm-c/DebugInfo.h"
#include "llvm-c/Types.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/Metadata.h"
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

extern "C" LLVMMetadataRef hewLLVMDIBuilderCreateVariantMemberType(
    LLVMDIBuilderRef builder, LLVMMetadataRef scope, const char *name,
    size_t name_len, LLVMMetadataRef file, unsigned line_number,
    uint64_t size_in_bits, uint32_t align_in_bits, uint64_t offset_in_bits,
    LLVMValueRef discriminant, LLVMDIFlags flags, LLVMMetadataRef type) {
  auto *scope_node = unwrapMetadata<DIScope>(scope);
  auto *file_node = unwrapMetadata<DIFile>(file);
  auto *type_node = unwrapMetadata<DIType>(type);
  auto *constant =
      dyn_cast_or_null<Constant>(reinterpret_cast<Value *>(discriminant));
  if (!builder || !scope_node || !file_node || !type_node || !constant)
    return nullptr;

  auto *member = unwrapBuilder(builder)->createVariantMemberType(
      scope_node, StringRef(name, name_len), file_node, line_number,
      size_in_bits, align_in_bits, offset_in_bits, constant,
      static_cast<DINode::DIFlags>(flags), type_node);
  return reinterpret_cast<LLVMMetadataRef>(member);
}

extern "C" LLVMMetadataRef hewLLVMDIBuilderCreateVariantPart(
    LLVMDIBuilderRef builder, LLVMMetadataRef scope, const char *name,
    size_t name_len, LLVMMetadataRef file, unsigned line_number,
    uint64_t size_in_bits, uint32_t align_in_bits, LLVMDIFlags flags,
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
      scope_node, StringRef(name, name_len), file_node, line_number,
      size_in_bits, align_in_bits, static_cast<DINode::DIFlags>(flags),
      discriminator_node, unwrapBuilder(builder)->getOrCreateArray(nodes));
  return reinterpret_cast<LLVMMetadataRef>(part);
}

extern "C" LLVMMetadataRef
hewLLVMDICompositeTypeSetVariantPart(LLVMDIBuilderRef builder,
                                     LLVMMetadataRef composite,
                                     LLVMMetadataRef variant_part) {
  auto *composite_node = unwrapMetadata<DICompositeType>(composite);
  auto *variant_part_node = unwrapMetadata<DICompositeType>(variant_part);
  if (!builder || !composite_node || !variant_part_node)
    return nullptr;

  Metadata *elements[] = {
      variant_part_node,
  };
  unwrapBuilder(builder)->replaceArrays(
      composite_node, unwrapBuilder(builder)->getOrCreateArray(elements));
  return reinterpret_cast<LLVMMetadataRef>(composite_node);
}
