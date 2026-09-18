// The two `DIBuilder` variant-part entry points the LLVM-C API does not
// expose. Without them an enum DIE can only list every variant's payload at
// once, and a debugger renders the inactive ones as live data.
//
// Nothing else belongs here: every other debug node this backend builds has an
// LLVM-C entry point that inkwell already wraps.

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
