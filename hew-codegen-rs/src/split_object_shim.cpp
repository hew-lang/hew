// ELF object splitting after whole-module optimization. Keep each image symbol's end in the
// same text run as its start so the final linker can place units independently.
#include "llvm-c/Types.h"
#include "llvm/ADT/SmallVector.h"
#include "llvm/ADT/StringMap.h"
#include "llvm/ADT/Twine.h"
#include "llvm/Bitcode/BitcodeWriter.h"
#include "llvm/IR/Constants.h"
#include "llvm/IR/Function.h"
#include "llvm/IR/IRBuilder.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"
#include "llvm/Transforms/Utils/SplitModule.h"

#include <cstdint>
#include <memory>
#include <utility>
#include <string>

using namespace llvm;

static Function *rangeStart(Constant *slot) {
  const unsigned Ops[] = {Instruction::Trunc, Instruction::Sub,
                          Instruction::PtrToInt};
  Value *operand = slot;
  for (unsigned opcode : Ops) {
    auto *expr = dyn_cast_or_null<ConstantExpr>(operand);
    if (!expr || expr->getOpcode() != opcode)
      return nullptr;
    operand = expr->getOperand(0);
  }
  return dyn_cast<Function>(operand);
}

extern "C" bool hewLLVMSplitObjectModule(
    LLVMModuleRef input, unsigned count, const unsigned *slots, size_t rowCount,
    const char *localPrefix, size_t prefixSize, void *output,
    void (*take)(void *, unsigned, const uint8_t *, size_t)) {
  auto &module = *reinterpret_cast<Module *>(input);
  if (!module.getModuleInlineAsm().empty())
    return false;
  for (const Function &function : module)
    if (!function.isDeclarationForLinker() && function.hasSection())
      return false;

  const StringRef prefix(localPrefix, prefixSize);
  for (const GlobalValue &value : module.global_values())
    if (value.getName().starts_with(prefix))
      return false;
  // SplitModule externalizes local definitions so parts can refer to one
  // another. Mark just those definitions so the joined object can restore
  // their locality before it is linked with native code and the runtime.
  unsigned localId = 0;
  for (GlobalValue &value : module.global_values())
    if (value.hasLocalLinkage())
      value.setName((Twine(prefix) + Twine(localId++)).str());

  auto &context = module.getContext();
  auto *markerType = FunctionType::get(Type::getVoidTy(context), false);
  StringMap<std::string> successors;
  std::unique_ptr<Module> tablePart;
  unsigned tableIndex = 0;
  unsigned nextIndex = 0;
  bool valid = true;
  const auto write = [&](const Module &part, unsigned index) {
    SmallVector<char, 0> bytes;
    raw_svector_ostream stream(bytes);
    WriteBitcodeToFile(part, stream);
    take(output, index, reinterpret_cast<const uint8_t *>(bytes.data()),
         bytes.size());
  };
  const auto capture = [&](std::unique_ptr<Module> part) {
    if (!valid)
      return;
    const unsigned index = nextIndex++;
    Function *last = nullptr;
    for (Function &function : *part) {
      if (function.isDeclarationForLinker())
        continue;
      if (last && !successors.try_emplace(last->getName(),
                                         function.getName().str()).second) {
        valid = false;
        return;
      }
      last = &function;
    }
    if (last) {
      const auto name = (Twine(prefix) + "text_end_" + Twine(index)).str();
      auto *marker = Function::Create(markerType, GlobalValue::ExternalLinkage,
                                      name, *part);
      marker->setVisibility(GlobalValue::HiddenVisibility);
      IRBuilder<> builder(BasicBlock::Create(context, "entry", marker));
      builder.CreateRetVoid();
      if (!successors.try_emplace(last->getName(), name).second) {
        valid = false;
        return;
      }
    }
    if (const auto *table = part->getNamedGlobal("__hew_symtab");
        table && table->hasInitializer()) {
      if (tablePart) {
        valid = false;
        return;
      }
      tableIndex = index;
      tablePart = std::move(part);
    } else {
      // Only the table's partition depends on all the other layouts. Encode
      // each other partition as it arrives to release its live IR promptly.
      write(*part, index);
    }
  };
  SplitModule(module, count, capture, /*PreserveLocals=*/false, /*RoundRobin=*/true);

  if (!valid || nextIndex != count || !tablePart)
    return false;
  auto *table = tablePart->getNamedGlobal("__hew_symtab");
  auto *initializer = dyn_cast<ConstantArray>(table->getInitializer());
  if (!initializer)
    return false;
  SmallVector<Constant *, 16> words;
  words.reserve(initializer->getNumOperands());
  for (Value *word : initializer->operands())
    words.push_back(cast<Constant>(word));
  auto *wordType = Type::getInt32Ty(context);
  auto *addressType = Type::getInt64Ty(context);
  auto *tableAddress = ConstantExpr::getPtrToInt(table, addressType);
  for (size_t row = 0; row < rowCount; ++row) {
    const unsigned startSlot = slots[row * 2];
    const unsigned endSlot = slots[row * 2 + 1];
    if (startSlot >= words.size() || endSlot >= words.size())
      return false;
    auto *start = rangeStart(words[startSlot]);
    if (!start)
      return false;
    const auto successor = successors.find(start->getName());
    if (successor == successors.end())
      return false;
    const StringRef name = successor->second;
    auto *bound = table->getParent()->getFunction(name);
    if (!bound) {
      if (table->getParent()->getNamedValue(name))
        return false;
      // A new reference may name a function that this partition did not
      // previously need or an end marker that was added after splitting.
      const auto *original = module.getFunction(name);
      auto *type = original ? original->getFunctionType() : markerType;
      bound = Function::Create(type, GlobalValue::ExternalLinkage, name,
                               *table->getParent());
      if (original)
        bound->setCallingConv(original->getCallingConv());
      bound->setVisibility(original ? original->getVisibility()
                                    : GlobalValue::HiddenVisibility);
    }
    auto *endAddress = ConstantExpr::getPtrToInt(bound, addressType);
    words[endSlot] = ConstantExpr::getTrunc(
        ConstantExpr::getSub(endAddress, tableAddress), wordType);
  }
  table->setInitializer(ConstantArray::get(initializer->getType(), words));

  write(*tablePart, tableIndex);
  return true;
}
