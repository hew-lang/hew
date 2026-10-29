// Trusted bridge: LLVM's parser/verifier -> explicit instruction/CFG events.
#include <llvm/IR/CFG.h>
#include <llvm/IR/Constants.h>
#include <llvm/IR/Instructions.h>
#include <llvm/IR/LLVMContext.h>
#include <llvm/IR/Module.h>
#include <llvm/IR/Verifier.h>
#include <llvm/IRReader/IRReader.h>
#include <llvm/Support/JSON.h>
#include <llvm/Support/SourceMgr.h>
#include <llvm/Support/raw_ostream.h>
#include <map>
using namespace llvm;

int main(int argc, char **argv) {
  if (argc != 2) {
    errs() << "usage: extract module.ll\n";
    return 2;
  }
  LLVMContext context;
  SMDiagnostic diagnostic;
  auto module = parseIRFile(argv[1], diagnostic, context);
  if (!module) {
    diagnostic.print(argv[0], errs());
    return 2;
  }
  if (verifyModule(*module, &errs()))
    return 2;
  auto *release = module->getFunction("hew_cont_frame_free");
  json::Array unknown;
  if (release) {
    if (!release->isDeclaration())
      unknown.push_back("frame release must have the external runtime ABI");
    if (!release->getReturnType()->isVoidTy() || release->arg_size() != 1 ||
        release->isVarArg() || !release->getArg(0)->getType()->isPointerTy())
      unknown.push_back("unexpected frame release signature");
    for (auto *user : release->users()) {
      auto *call = dyn_cast<CallInst>(user);
      if (!call || call->getCalledFunction() != release)
        unknown.push_back(
            "frame release address escapes or has an unsupported invocation");
    }
  }
  int64_t allocations = 0, releases = 0, definitions = 0;
  json::Array functions;
  for (auto &function : *module) {
    if (function.isDeclaration())
      continue;
    ++definitions;
    int64_t count = 0;
    for (auto &block : function)
      for (auto &instruction : block) {
        if (auto *call = dyn_cast<CallBase>(&instruction)) {
          auto *callee = call->getCalledFunction();
          if (callee && callee->getName() == "hew_cont_frame_alloc")
            ++allocations;
          if (callee && callee->getName() == "hew_cont_frame_free")
            ++count;
        }
      }
    if (count == 0)
      continue;
    releases += count;
    std::map<const BasicBlock *, int64_t> ids;
    int64_t next = 0;
    for (auto &block : function)
      ids[&block] = next++;
    json::Array blocks;
    for (auto &block : function) {
      json::Array events, instructions, edges, pointerReturns;
      for (auto &instruction : block) {
        // Quiet is intentionally restrictive. Everything other than branch,
        // switch, return, or unreachable is an effect by default.
        int64_t event = 1;
        if (isa<BranchInst>(instruction) || isa<SwitchInst>(instruction) ||
            isa<UnreachableInst>(instruction))
          event = 0;
        if (auto *ret = dyn_cast<ReturnInst>(&instruction)) {
          event = 0;
          if (ret->getReturnValue() &&
              ret->getReturnValue()->getType()->isPointerTy() &&
              !isa<ConstantPointerNull>(ret->getReturnValue()))
            pointerReturns.push_back(static_cast<int64_t>(events.size()));
        }
        if (auto *call = dyn_cast<CallInst>(&instruction))
          if (release && call->getCalledFunction() == release)
            event = 2;
        std::string text;
        raw_string_ostream stream(text);
        instruction.print(stream);
        events.push_back(event);
        instructions.push_back(text);
      }
      for (auto *successor : successors(&block))
        edges.push_back(ids.at(successor));
      blocks.push_back(
          json::Object{{"name", block.getName().str()},
                       {"events", std::move(events)},
                       {"instructions", std::move(instructions)},
                       {"successors", std::move(edges)},
                       {"pointer_returns", std::move(pointerReturns)}});
    }
    functions.push_back(json::Object{{"name", function.getName().str()},
                                     {"release_sites", count},
                                     {"blocks", std::move(blocks)}});
  }
  outs() << json::Value(json::Object{{"functions", std::move(functions)},
                                     {"defined_functions", definitions},
                                     {"allocation_sites", allocations},
                                     {"release_sites", releases},
                                     {"unknown", std::move(unknown)}})
         << "\n";
}
