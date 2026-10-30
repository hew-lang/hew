// LLVM debug operations without an LLVM-C entry point. Pointer-value locations
// need explicit storage after coroutine splitting so the backend keeps address
// and value indirection apart.

#include "llvm-c/DebugInfo.h"
#include "llvm-c/Types.h"
#include "llvm/ADT/DenseMap.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/BinaryFormat/Dwarf.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/DebugProgramInstruction.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Metadata.h"
#include "llvm/IR/Module.h"
#include "llvm/IR/Value.h"
#include "llvm/Support/Casting.h"

using namespace llvm;

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
