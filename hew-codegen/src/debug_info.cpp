//===- debug_info.cpp - DWARF debug info emission --------------------------===//
//
// Creates proper DWARF metadata (DICompileUnit, DISubprogram, etc.) around
// the raw debug locations that MLIR's translateModuleToLLVMIR preserves from
// FileLineColLoc metadata. Without this, LLVM strips those locations because
// they aren't wrapped in a valid debug info hierarchy.
//
//===----------------------------------------------------------------------===//

#include "hew/debug_info.h"

#include "llvm/BinaryFormat/Dwarf.h"
#include "llvm/IR/DebugInfoMetadata.h"
#include "llvm/IR/DIBuilder.h"
#include "llvm/IR/Instructions.h"
#include "llvm/IR/LLVMContext.h"
#include "llvm/IR/Module.h"
#include "llvm/Support/raw_ostream.h"

#include <filesystem>
#include <unordered_map>

namespace hew {

namespace {

class DebugTypeEmitter {
public:
  DebugTypeEmitter(llvm::DIBuilder &dib, llvm::Module &module)
      : dib_(dib), module_(module) {}

  llvm::DIType *emit(llvm::Type *type) {
    std::string key;
    llvm::raw_string_ostream keyStream(key);
    if (type) {
      type->print(keyStream);
    } else {
      keyStream << "void";
    }
    keyStream.flush();

    auto existing = cache_.find(key);
    if (existing != cache_.end())
      return existing->second;

    llvm::DIType *diType = create(type);
    cache_.emplace(std::move(key), diType);
    return diType;
  }

private:
  llvm::DIType *create(llvm::Type *type) {
    if (!type || type->isVoidTy())
      return dib_.createUnspecifiedType("void");

    if (auto *intType = llvm::dyn_cast<llvm::IntegerType>(type)) {
      if (intType->getBitWidth() == 1)
        return dib_.createBasicType("bool", 1, llvm::dwarf::DW_ATE_boolean);
      return dib_.createBasicType("i" + std::to_string(intType->getBitWidth()),
                                  intType->getBitWidth(), llvm::dwarf::DW_ATE_signed);
    }

    if (auto *pointerType = llvm::dyn_cast<llvm::PointerType>(type)) {
      return dib_.createPointerType(dib_.createUnspecifiedType("ptr"),
                                    module_.getDataLayout().getPointerSizeInBits(
                                        pointerType->getAddressSpace()));
    }

    if (auto *structType = llvm::dyn_cast<llvm::StructType>(type)) {
      std::string structName =
          structType->hasName() ? structType->getName().str() : std::string("anon");
      return dib_.createUnspecifiedType(structName);
    }

    return dib_.createUnspecifiedType("value");
  }

  llvm::DIBuilder &dib_;
  llvm::Module &module_;
  std::unordered_map<std::string, llvm::DIType *> cache_;
};

} // namespace

/// Demangle a Hew mangled name into a human-readable form.
///
/// Hew mangling scheme: _H [M<len>module]* [T<len>type] F<len>func
/// Example: _HM4mainT6WorkerF4ping  →  Worker.ping
///
/// Returns the original name unchanged when it does not start with "_H"
/// (e.g. "main" which is kept unmangled).
static std::string demangleHewName(llvm::StringRef mangled) {
  if (!mangled.starts_with("_H"))
    return mangled.str();

  llvm::StringRef rest = mangled.drop_front(2); // skip "_H"
  std::string typeName;
  std::string funcName;

  auto parseSegment = [](llvm::StringRef &s) -> std::string {
    // Skip the tag character (M, T, or F), then read decimal length, then text.
    s = s.drop_front(1); // skip tag
    size_t numLen = 0;
    while (numLen < s.size() && s[numLen] >= '0' && s[numLen] <= '9')
      ++numLen;
    if (numLen == 0)
      return {};
    unsigned len = 0;
    s.substr(0, numLen).getAsInteger(10, len);
    s = s.drop_front(numLen);
    if (len > s.size())
      return {};
    std::string result = s.substr(0, len).str();
    s = s.drop_front(len);
    return result;
  };

  while (!rest.empty()) {
    if (rest.starts_with("M")) {
      parseSegment(rest); // skip module segments for now
    } else if (rest.starts_with("T")) {
      typeName = parseSegment(rest);
    } else if (rest.starts_with("F")) {
      funcName = parseSegment(rest);
      break; // function name is the last segment
    } else {
      return mangled.str(); // unrecognised tag — return as-is
    }
  }

  if (funcName.empty())
    return mangled.str();
  if (typeName.empty())
    return funcName;
  return typeName + "." + funcName;
}

void emitDebugInfo(llvm::Module &module, const std::string &sourcePath,
                   const std::vector<size_t> &lineMap,
                   const std::unordered_map<std::string, unsigned> &functionDeclLines,
                   const std::unordered_map<std::string, std::vector<std::string>>
                       &functionParamNames) {
  llvm::DIBuilder dib(module);
  DebugTypeEmitter debugTypes(dib, module);

  // Split source path into directory and filename.
  std::filesystem::path p(sourcePath);
  std::string directory = p.parent_path().string();
  std::string filename = p.filename().string();

  // Create the file and compile-unit metadata.
  llvm::DIFile *diFile = dib.createFile(filename, directory);
  // The compile unit must exist for DWARF emission even though we don't
  // reference the pointer directly — DIBuilder owns it and attaches it to
  // the module metadata graph.
  (void)dib.createCompileUnit(
      llvm::dwarf::DW_LANG_C, // closest match; DWARF has no Hew language code
      diFile,
      "hew", // producer
      false, // isOptimized — debug builds are -O0
      "",    // flags
      0      // runtime version
  );

  // Walk every function and attach DISubprogram + per-instruction locations.
  for (llvm::Function &fn : module) {
    if (fn.isDeclaration())
      continue;

    auto declLineIt = functionDeclLines.find(std::string(fn.getName()));

    // Prefer the function declaration line when we captured one from MLIR.
    // Fall back to the first instruction location only when the function did
    // not carry a usable declaration line through lowering.
    unsigned startLine = declLineIt != functionDeclLines.end() ? declLineIt->second : 1;
    if (declLineIt == functionDeclLines.end()) {
      for (const llvm::BasicBlock &bb : fn) {
        for (const llvm::Instruction &inst : bb) {
          if (const auto &dl = inst.getDebugLoc()) {
            startLine = dl.getLine();
            goto found_line; // break out of nested loops
          }
        }
      }
    }
  found_line:

    llvm::SmallVector<llvm::Metadata *, 8> signatureTypes;
    signatureTypes.push_back(debugTypes.emit(fn.getReturnType()));
    for (llvm::Argument &arg : fn.args())
      signatureTypes.push_back(debugTypes.emit(arg.getType()));
    llvm::DISubroutineType *funcTy = dib.createSubroutineType(dib.getOrCreateTypeArray(signatureTypes));

    // Create the subprogram.
    // DW_AT_name gets the human-readable demangled form so debuggers show
    // e.g. "Worker.ping" instead of "_HM4mainT6WorkerF4ping".
    // DW_AT_linkage_name keeps the mangled symbol for addr2line / profilers.
    std::string humanName = demangleHewName(fn.getName());
    llvm::DISubprogram *sp =
        dib.createFunction(diFile,                     // scope — the file
                           humanName,                  // name (DW_AT_name)
                           fn.getName(),               // linkage name (DW_AT_linkage_name)
                           diFile,                     // file
                           startLine,                  // line number
                           funcTy,                     // subroutine type
                           startLine,                  // scope line (same as start)
                           llvm::DINode::FlagPrototyped, llvm::DISubprogram::SPFlagDefinition);

    fn.setSubprogram(sp);

    if (auto paramIt = functionParamNames.find(std::string(fn.getName()));
        paramIt != functionParamNames.end() && !fn.empty()) {
      llvm::DebugLoc paramLoc(
          llvm::DILocation::get(module.getContext(), startLine, 0, sp));
      auto insertPoint = fn.getEntryBlock().getFirstInsertionPt();
      if (insertPoint != fn.getEntryBlock().end()) {
        for (size_t i = 0; i < paramIt->second.size() && i < fn.arg_size(); ++i) {
          llvm::Argument *arg = fn.getArg(i);
          const std::string &paramName = paramIt->second[i];
          if (paramName.empty())
            continue;
          auto *variable = dib.createParameterVariable(
              sp, paramName, static_cast<unsigned>(i) + 1, diFile, startLine,
              debugTypes.emit(arg->getType()), true);
          dib.insertDbgValueIntrinsic(arg, variable, dib.createExpression(), paramLoc,
                                      insertPoint);
        }
      }
    }

    // Re-scope every instruction's debug location under this subprogram.
    for (llvm::BasicBlock &bb : fn) {
      for (llvm::Instruction &inst : bb) {
        if (const auto &dl = inst.getDebugLoc()) {
          // Instruction already has a location from MLIR — keep line/col
          // but re-scope it under our new subprogram.
          auto *newLoc = llvm::DILocation::get(module.getContext(), dl.getLine(), dl.getCol(), sp);
          inst.setDebugLoc(llvm::DebugLoc(newLoc));
        } else {
          // No location — assign the function's start line so the debugger
          // doesn't lose track.
          auto *defaultLoc = llvm::DILocation::get(module.getContext(), startLine, 0, sp);
          inst.setDebugLoc(llvm::DebugLoc(defaultLoc));
        }
      }
    }
  }

  // Finalize the debug info — builds the DWARF CU tree and attaches it.
  dib.finalize();

  // Set module-level DWARF flags so the backend emits .debug_info sections.
  // Use Warning behaviour: if a flag already exists, keep it (no error).
  module.addModuleFlag(llvm::Module::Warning, "Dwarf Version", 4);
  module.addModuleFlag(llvm::Module::Warning, "Debug Info Version", llvm::DEBUG_METADATA_VERSION);
}

} // namespace hew
