// LLVM debug operations without an LLVM-C entry point. Pointer-value locations
// need explicit storage after coroutine splitting so the backend keeps address
// and value indirection apart.

#include "llvm-c/DebugInfo.h"
#include "llvm-c/TargetMachine.h"
#include "llvm-c/Types.h"
#include "llvm/ADT/APInt.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallPtrSet.h"
#include "llvm/ADT/SmallString.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/BinaryFormat/Dwarf.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/DataLayout.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/DebugProgramInstruction.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"
#include "llvm/Support/MD5.h"
#include "llvm/Target/TargetMachine.h"

using namespace llvm;

static DIBuilder *unwrapBuilder(LLVMDIBuilderRef builder) {
  return reinterpret_cast<DIBuilder *>(builder);
}

template <typename T> static T *unwrapMetadata(LLVMMetadataRef metadata) {
  if (!metadata)
    return nullptr;
  return dyn_cast<T>(reinterpret_cast<Metadata *>(metadata));
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
    // CoroSplit reloads a spilled value at the start of each block that uses
    // it, after the block's debug records are placed and under the user's
    // line. A breakpoint on that line would then stop before the records
    // that say which locals hold a value. The reload is plumbing, so it
    // belongs to line 0.
    for (auto &block : function)
      for (auto &instruction : block)
        if (instruction.getName().contains(".reload") &&
            instruction.getDebugLoc() && instruction.getDebugLoc().getLine())
          instruction.setDebugLoc(DILocation::get(
              ir->getContext(), 0, 0, instruction.getDebugLoc()->getScope()));
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
      // The address store belongs to the variable's scope, on line 0, so the
      // scope exists in the code even when no source statement lies directly
      // in it; a scope with no code would drop its variables.
      location.SetCurrentDebugLocation(DILocation::get(
          ir->getContext(), 0, 0, variable->getVariable()->getScope()));
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

// An unoptimized coroutine ramp addresses its frame through the SSA value
// `llvm.coro.begin` returned. Without the register allocator's variable
// tracking that value is only described in the block that defines it, so a
// parameter or local living in the frame reads as optimized out everywhere
// else in the ramp. Give each such frame address one stack home, the way
// CoroSplit already does for the resume clones' frame argument, and read
// the variable through it.
extern "C" void hewLLVMHomeRampFrameAddresses(LLVMModuleRef module) {
  auto *ir = reinterpret_cast<Module *>(module);
  if (!ir->getNamedMetadata("llvm.dbg.cu"))
    return;
  for (auto &function : *ir) {
    SmallVector<DbgVariableRecord *, 8> records;
    for (auto &block : function)
      for (auto &instruction : block)
        for (auto &record : instruction.getDbgRecordRange())
          if (auto *variable = dyn_cast<DbgVariableRecord>(&record))
            if (variable->isDbgDeclare())
              if (auto *address =
                      dyn_cast<Instruction>(variable->getVariableLocationOp(0)))
                if (!isa<AllocaInst>(address))
                  records.push_back(variable);
    DenseMap<Value *, AllocaInst *> homes;
    for (auto *variable : records) {
      auto *address = cast<Instruction>(variable->getVariableLocationOp(0));
      auto &home = homes[address];
      if (!home) {
        IRBuilder<> entry(&function.getEntryBlock(),
                          function.getEntryBlock().getFirstInsertionPt());
        home = entry.CreateAlloca(address->getType(), nullptr,
                                  "debug.frame.address");
        IRBuilder<> store(address->getParent(),
                          isa<PHINode>(address)
                              ? address->getParent()->getFirstInsertionPt()
                              : std::next(address->getIterator()));
        store.SetCurrentDebugLocation(DebugLoc());
        store.CreateStore(address, home);
      }
      variable->replaceVariableLocationOp(address, home);
      variable->setExpression(DIExpression::prepend(variable->getExpression(),
                                                    DIExpression::DerefBefore));
    }
  }
}

// A DWARF 5 line table names each file with its MD5 so a debugger can tell
// the text it shows from the text compiled. LLVM-C creates files without a
// checksum, so the unit's file is replaced with one that carries it.
extern "C" LLVMMetadataRef
hewLLVMDICompileUnitChecksumFile(LLVMDIBuilderRef builder, LLVMMetadataRef unit,
                                 const char *name, size_t name_len,
                                 const char *directory, size_t directory_len,
                                 const char *text, size_t text_len) {
  auto *compile_unit = unwrapMetadata<DICompileUnit>(unit);
  if (!builder || !compile_unit)
    return nullptr;
  MD5 hash;
  hash.update(StringRef(text, text_len));
  MD5::MD5Result digest;
  hash.final(digest);
  SmallString<32> hex = digest.digest();
  auto *file = unwrapBuilder(builder)->createFile(
      StringRef(name, name_len), StringRef(directory, directory_len),
      DIFile::ChecksumInfo<StringRef>(DIFile::CSK_MD5, hex));
  compile_unit->replaceOperandWith(0, file);
  return reinterpret_cast<LLVMMetadataRef>(file);
}

// Whether `machine` optimizes: the compile unit and every subprogram state it
// so a debugger expects partial locations instead of reporting a fault.
extern "C" bool hewLLVMTargetMachineOptimizes(LLVMTargetMachineRef machine) {
  return machine && reinterpret_cast<TargetMachine *>(machine)->getOptLevel() !=
                        CodeGenOptLevel::None;
}

// Switch-lowered coroutine splitting clones a body into a ramp, a `.resume`
// continuation and destroy/cleanup parts. The ramp and continuation run the
// source; the destroy parts only release a suspended frame. The ramp stores
// the continuation at frame offset 0 and the destroy part (or a select of
// destroy and cleanup) one pointer later, which is how this finds them
// without reading symbol names. Each destroy part becomes an artificial
// subprogram on line 0 named by its own symbol, so a debugger never resolves
// a source name or line to it, and its variables and labels are dropped.
extern "C" void hewLLVMMarkCoroutineDestroyParts(LLVMModuleRef module) {
  auto *ir = reinterpret_cast<Module *>(module);
  if (!ir->getNamedMetadata("llvm.dbg.cu"))
    return;
  const DataLayout &layout = ir->getDataLayout();
  SmallPtrSet<Function *, 16> parts;
  auto note = [&](Value *stored) {
    if (auto *function = dyn_cast<Function>(stored)) {
      parts.insert(function);
    } else if (auto *select = dyn_cast<SelectInst>(stored)) {
      for (Value *arm : {select->getTrueValue(), select->getFalseValue()})
        if (auto *function = dyn_cast<Function>(arm))
          parts.insert(function);
    }
  };
  for (auto &function : *ir)
    for (auto &block : function)
      for (auto &instruction : block) {
        auto *store = dyn_cast<StoreInst>(&instruction);
        if (!store || !store->getValueOperand()->getType()->isPointerTy())
          continue;
        Value *stored = store->getValueOperand();
        if (!isa<Function>(stored) && !isa<SelectInst>(stored))
          continue;
        APInt offset(layout.getIndexTypeSizeInBits(
                         store->getPointerOperand()->getType()),
                     0);
        store->getPointerOperand()->stripAndAccumulateConstantOffsets(
            layout, offset, /* AllowNonInbounds */ true);
        // Only a ramp stores its own parts: the part's subprogram is the
        // ramp's, cloned by the split.
        DISubprogram *ramp = function.getSubprogram();
        if (offset != layout.getPointerSize() || !ramp)
          continue;
        auto same = [&](Value *candidate) {
          auto *part = dyn_cast<Function>(candidate);
          DISubprogram *cloned = part ? part->getSubprogram() : nullptr;
          return cloned && cloned != ramp &&
                 cloned->getRawFile() == ramp->getRawFile() &&
                 cloned->getLine() == ramp->getLine() &&
                 cloned->getRawType() == ramp->getRawType();
        };
        if (auto *select = dyn_cast<SelectInst>(stored)) {
          if (same(select->getTrueValue()) && same(select->getFalseValue()))
            note(stored);
        } else if (same(stored)) {
          note(stored);
        }
      }
  for (Function *function : parts) {
    DISubprogram *old = function->getSubprogram();
    if (!old || function->isDeclaration())
      continue;
    LLVMContext &context = function->getContext();
    auto *part = DISubprogram::getDistinct(
        context, old->getRawScope(),
        MDString::get(context, function->getName()), nullptr, old->getRawFile(),
        0, old->getRawType(), 0, old->getRawContainingType(),
        old->getVirtualIndex(), old->getThisAdjustment(),
        old->getFlags() | DINode::FlagArtificial, old->getSPFlags(),
        old->getRawUnit());
    function->setSubprogram(part);
    DebugLoc unattributed = DILocation::get(context, 0, 0, part);
    for (auto &block : *function)
      for (auto &instruction : block) {
        instruction.dropDbgRecords();
        if (instruction.getDebugLoc())
          instruction.setDebugLoc(unattributed);
      }
  }
}
