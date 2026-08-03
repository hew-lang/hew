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
  return cast<T>(reinterpret_cast<Metadata *>(metadata));
}

extern "C" LLVMMetadataRef hewLLVMDIBuilderCreateVariantMemberType(
    LLVMDIBuilderRef builder, LLVMMetadataRef scope, const char *name,
    size_t name_len, LLVMMetadataRef file, unsigned line_number,
    uint64_t size_in_bits, uint32_t align_in_bits, uint64_t offset_in_bits,
    LLVMValueRef discriminant, LLVMDIFlags flags, LLVMMetadataRef type) {
  auto *constant = cast<Constant>(reinterpret_cast<Value *>(discriminant));
  auto *member = unwrapBuilder(builder)->createVariantMemberType(
      unwrapMetadata<DIScope>(scope), StringRef(name, name_len),
      unwrapMetadata<DIFile>(file), line_number, size_in_bits, align_in_bits,
      offset_in_bits, constant, static_cast<DINode::DIFlags>(flags),
      unwrapMetadata<DIType>(type));
  return reinterpret_cast<LLVMMetadataRef>(member);
}

extern "C" LLVMMetadataRef hewLLVMDIBuilderCreateVariantPart(
    LLVMDIBuilderRef builder, LLVMMetadataRef scope, const char *name,
    size_t name_len, LLVMMetadataRef file, unsigned line_number,
    uint64_t size_in_bits, uint32_t align_in_bits, LLVMDIFlags flags,
    LLVMMetadataRef discriminator, LLVMMetadataRef *elements,
    unsigned element_count) {
  SmallVector<Metadata *, 8> nodes;
  nodes.reserve(element_count);
  for (unsigned index = 0; index < element_count; ++index)
    nodes.push_back(reinterpret_cast<Metadata *>(elements[index]));

  auto *part = unwrapBuilder(builder)->createVariantPart(
      unwrapMetadata<DIScope>(scope), StringRef(name, name_len),
      unwrapMetadata<DIFile>(file), line_number, size_in_bits, align_in_bits,
      static_cast<DINode::DIFlags>(flags),
      unwrapMetadata<DIDerivedType>(discriminator),
      unwrapBuilder(builder)->getOrCreateArray(nodes));
  return reinterpret_cast<LLVMMetadataRef>(part);
}

extern "C" void hewLLVMDICompositeTypeAppendVariantPart(
    LLVMDIBuilderRef builder, LLVMMetadataRef composite,
    LLVMMetadataRef existing_member, LLVMMetadataRef variant_part) {
  Metadata *elements[] = {
      reinterpret_cast<Metadata *>(existing_member),
      reinterpret_cast<Metadata *>(variant_part),
  };
  unwrapMetadata<DICompositeType>(composite)->replaceElements(
      unwrapBuilder(builder)->getOrCreateArray(elements));
}
