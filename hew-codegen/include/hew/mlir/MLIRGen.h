//===- MLIRGen.h - AST-to-MLIR lowering for Hew ----------------*- C++ -*-===//
//
// Declares the MLIRGen class that walks the Hew AST and emits MLIR operations
// using func, arith, scf, memref, and the custom Hew dialect.
//
//===----------------------------------------------------------------------===//

#ifndef HEW_MLIR_MLIRGEN_H
#define HEW_MLIR_MLIRGEN_H

#include "hew/ast_types.h"

#include "mlir/Dialect/Func/IR/FuncOps.h"
#include "mlir/Dialect/LLVMIR/LLVMTypes.h"
#include "mlir/IR/Builders.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/MLIRContext.h"
#include "mlir/IR/Value.h"

#include "llvm/ADT/ScopedHashTable.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/ADT/StringSet.h"

#include <cstdint>
#include <optional>
#include <set>
#include <string>
#include <unordered_map>
#include <unordered_set>
#include <vector>

namespace hew {

/// Information about a single struct field.
struct StructFieldInfo {
  std::string name;
  mlir::Type type;
  mlir::Type semanticType;
  unsigned index;
  std::string typeExprStr; // Original type expression string for collection dispatch
};

/// Information about a registered struct type.
struct StructTypeInfo {
  std::string name;
  std::vector<StructFieldInfo> fields;
  mlir::LLVM::LLVMStructType mlirType;
};

/// MLIRGen walks the Hew AST and produces an MLIR module using standard
/// dialects (func, arith, scf, memref) plus the Hew dialect.
class MLIRGen {
public:
  explicit MLIRGen(mlir::MLIRContext &context, const std::string &targetTriple = "");

  /// Main entry point: lower a complete Hew Program AST to an MLIR module.
  /// Returns nullptr on failure.
  mlir::ModuleOp generate(const ast::Program &program);

private:
  // ── Type conversion ──────────────────────────────────────────────
  mlir::Type convertType(const ast::TypeExpr &type);
  mlir::Type defaultIntType();
  mlir::Type defaultFloatType();

  // ── Top-level items ──────────────────────────────────────────────
  void generateItem(const ast::Item &item);
  void registerTypeDecl(const ast::TypeDecl &decl);
  void registerFunctionSignature(const ast::FnDecl &fn, const std::string &nameOverride = "");
  void generateImplDecl(const ast::ImplDecl &decl);
  void registerTraitDecl(const ast::TraitDecl &decl);
  void generateTraitDefaultMethod(const ast::TraitMethod &method, const std::string &targetTypeName,
                                  const std::string &mangledName);
  void generateExternBlock(const ast::ExternBlock &block);
  void generateImport(const ast::ImportDecl &decl);
  mlir::func::FuncOp generateFunction(const ast::FnDecl &fn, const std::string &nameOverride = "");
  void generateGeneratorFunction(const ast::FnDecl &fn);
  void registerActorDecl(const ast::ActorDecl &decl);
  void generateActorDecl(const ast::ActorDecl &decl);
  void generateWireDecl(const ast::WireDecl &decl);
  void generateWireToJson(const ast::WireDecl &decl);
  void generateWireFromJson(const ast::WireDecl &decl);
  void generateWireToYaml(const ast::WireDecl &decl);
  void generateWireFromYaml(const ast::WireDecl &decl);
  /// Return an !llvm.ptr to a NUL-terminated global string constant.
  mlir::Value wireStringPtr(mlir::Location location, llvm::StringRef value);
  void generateSupervisorDecl(const ast::SupervisorDecl &decl);

  // ── Actor type registry (declared early for method signatures) ────
  struct ActorReceiveInfo {
    std::string name;
    std::vector<std::string> paramNames;  // message parameter names
    std::vector<mlir::Type> paramTypes;   // message parameter types
    std::optional<mlir::Type> returnType; // return type (for await/ask)
    bool isGenerator = false;             // true for receive gen fn
  };
  struct ActorInfo {
    std::string name;
    mlir::LLVM::LLVMStructType stateType;     // state struct type
    std::vector<ActorReceiveInfo> receiveFns; // receive handlers in order
    std::vector<mlir::Type> fieldHewTypes;    // Hew MLIR types (before toLLVMStorageType)
    std::optional<uint32_t> mailboxCapacity;
    int8_t overflowPolicy = 0;   // 0=none,1=drop_new,2=drop_old,3=block,4=fail,5=coalesce
    std::string coalesceKey;     // field name for coalesce key
    int8_t coalesceFallback = 0; // fallback policy
  };

  // ── Actor expressions ─────────────────────────────────────────────
  mlir::Value generateSpawnExpr(const ast::ExprSpawn &expr);
  mlir::Value generateSpawnLambdaActorExpr(const ast::ExprSpawnLambdaActor &expr);
  mlir::Value generateSendExpr(const ast::ExprSend &expr);
  void generateCoalesceKeyFn(const ActorInfo &actorInfo, const std::string &fnName);
  mlir::Value generateActorMethodSend(mlir::Value actorPtr, const ActorInfo &actorInfo,
                                      const std::string &methodName,
                                      const std::vector<ast::CallArg> &args,
                                      mlir::Location location);
  mlir::Value generateActorMethodAsk(mlir::Value actorPtr, const ActorInfo &actorInfo,
                                     const std::string &methodName,
                                     const std::vector<ast::CallArg> &args,
                                     mlir::Location location);

  // ── Statements ───────────────────────────────────────────────────
  void generateStatement(const ast::Stmt &stmt);
  void generateLetStmt(const ast::StmtLet &stmt);
  void generateVarStmt(const ast::StmtVar &stmt);
  void generateAssignStmt(const ast::StmtAssign &stmt);
  void generateIfStmt(const ast::StmtIf &stmt);
  mlir::Value generateIfStmtAsExpr(const ast::StmtIf &stmt);
  void generateWhileStmt(const ast::StmtWhile &stmt);
  void generateForStmt(const ast::StmtFor &stmt);
  void generateForAwaitStmt(const ast::StmtFor &stmt);
  void generateForCollectionStmt(const ast::StmtFor &stmt);
  void generateForHashMapStmt(const ast::StmtFor &stmt, mlir::Value collection,
                              const std::string &collType);
  void generateForGeneratorStmt(const ast::StmtFor &stmt, const std::string &genFuncName);
  void generateReturnStmt(const ast::StmtReturn &stmt);
  void generateExprStmt(const ast::StmtExpression &stmt);

  // ── Expressions ──────────────────────────────────────────────────
  mlir::Value generateExpression(const ast::Expr &expr);
  mlir::Value generateLiteral(const ast::Literal &lit);
  mlir::Value generateBinaryExpr(const ast::ExprBinary &expr);
  mlir::Value generateUnaryExpr(const ast::ExprUnary &expr);
  mlir::Value generateCallExpr(const ast::ExprCall &expr);
  mlir::Value generateIfExpr(const ast::ExprIf &expr);
  mlir::Value generateBlockExpr(const ast::Block &block);
  mlir::Value generatePostfixExpr(const ast::ExprPostfixTry &expr);
  mlir::Value generateStructInit(const ast::ExprStructInit &expr);
  mlir::Value generateMethodCall(const ast::ExprMethodCall &expr);
  mlir::Value generateLogCall(const ast::ExprMethodCall &mc);
  mlir::Value generateLogEmit(const std::vector<ast::CallArg> &args, int levelInt);
  mlir::Value generateTupleExpr(const ast::ExprTuple &expr);
  mlir::Value generateArrayExpr(const ast::ExprArray &expr);
  mlir::Value generateLambdaExpr(const ast::ExprLambda &expr);
  mlir::Value generateScopeExpr(const ast::ExprScope &expr);
  mlir::Value generateScopeLaunchExpr(const ast::ExprScopeLaunch &expr);
  mlir::Value generateScopeCancelExpr();

  mlir::Value generateSelectExpr(const ast::ExprSelect &expr);
  mlir::Value generateJoinExpr(const ast::ExprJoin &expr);
  mlir::Value generateInterpolatedString(const ast::ExprInterpolatedString &expr);
  mlir::Value generateRegexLiteral(const ast::ExprRegexLiteral &expr);

  // ── Block lowering ───────────────────────────────────────────────
  /// Generates all statements in a block and returns the block's trailing
  /// expression value (or nullptr if the block has no trailing expression).
  mlir::Value generateBlock(const ast::Block &block);

  // ── Match ────────────────────────────────────────────────────────
  void generateMatchStmt(const ast::StmtMatch &stmt);
  mlir::Value generateMatchExpr(const ast::ExprMatch &expr, const ast::Span &exprSpan);
  mlir::Value generateMatchImpl(mlir::Value scrutinee, const std::vector<ast::MatchArm> &arms,
                                mlir::Type resultType, mlir::Location location);
  mlir::Value generateMatchArmsChain(mlir::Value scrutinee, const std::vector<ast::MatchArm> &arms,
                                     size_t idx, mlir::Type resultType, mlir::Location location);
  mlir::Value generateOrPatternCondition(mlir::Value scrutinee, const ast::Pattern &pattern,
                                         mlir::Location location);

  // ── Loop/Break/Continue ─────────────────────────────────────────
  void generateLoopStmt(const ast::StmtLoop &stmt);
  void generateBreakStmt(const ast::StmtBreak &stmt);
  void generateContinueStmt(const ast::StmtContinue &stmt);

  // ── Print/println built-in handling ──────────────────────────────
  mlir::Value generatePrintCall(const ast::ExprCall &expr, bool newline);

  // ── Select/Join helpers ───────────────────────────────────────────
  /// Resolve actor type name from an expression (e.g., variable holding actor).
  std::string resolveActorTypeName(const ast::Expr &expr);

  /// Pack argument values into a stack-allocated buffer for sending.
  /// Returns (data_ptr, data_size) as (!llvm.ptr, i64).
  std::pair<mlir::Value, mlir::Value> packArgsForSend(llvm::ArrayRef<mlir::Value> args,
                                                      mlir::Location location);

  // ── Helpers ──────────────────────────────────────────────────────
  mlir::Location loc(const ast::Span &span);

  /// Get or create an extern function declaration.
  mlir::func::FuncOp getOrCreateExternFunc(llvm::StringRef name, mlir::FunctionType type);

  /// Check if a name is a builtin function and handle it.
  mlir::Value generateBuiltinCall(const std::string &name, const std::vector<ast::CallArg> &args,
                                  mlir::Location location);

  /// Look up a variable name: returns the current SSA value (for immutable
  /// bindings) or loads from the memref slot (for mutable variables).
  mlir::Value lookupVariable(llvm::StringRef name);

  /// Declare an immutable binding.
  void declareVariable(llvm::StringRef name, mlir::Value value);

  /// Declare a mutable variable (allocates a memref slot).
  void declareMutableVariable(llvm::StringRef name, mlir::Type type, mlir::Value initialValue);

  /// Store to a mutable variable.
  void storeVariable(llvm::StringRef name, mlir::Value value);

  /// Get a unique name for a global string literal.
  std::string getOrCreateGlobalString(llvm::StringRef value);

  /// Coerce a value to a target type (e.g., int-to-float promotion).
  mlir::Value coerceType(mlir::Value value, mlir::Type targetType, mlir::Location location);

  /// Coerce a value to the hashmap's declared value type based on collType.
  mlir::Value coerceToHashMapValueType(mlir::Value val, const std::string &collType,
                                       mlir::Location location, mlir::Type mapType = {});

  /// Generate remaining statements with return guards (recursive).
  /// Iterates stmts[startIdx..endIdx), then generates trailingExpr.
  void
  generateStmtsWithReturnGuards(const std::vector<std::unique_ptr<ast::Spanned<ast::Stmt>>> &stmts,
                                size_t startIdx, size_t endIdx, const ast::Expr *trailingExpr,
                                mlir::Location location);

  /// Generate loop body statements with continue/break guards (recursive).
  void generateLoopBodyWithContinueGuards(
      const std::vector<std::unique_ptr<ast::Spanned<ast::Stmt>>> &stmts, size_t startIdx,
      size_t endIdx, mlir::Value contFlag, mlir::Location location);

  /// Return the integer type matching the target's pointer/size width.
  /// i32 on wasm32, i64 otherwise.
  mlir::IntegerType sizeType() const;

  // ── Name mangling ────────────────────────────────────────────────
  /// Mangle a function name with module path and optional type context.
  /// "main" is never mangled. Empty modulePath + empty typeName produces
  /// tagged length-prefixed mangling (e.g. "_HF3foo").
  static std::string mangleName(const std::vector<std::string> &modulePath,
                                const std::string &typeName, const std::string &funcName);

  // ── MLIR infrastructure ──────────────────────────────────────────
  mlir::MLIRContext &context;
  mlir::OpBuilder builder;
  mlir::ModuleOp module;
  std::string targetTriple;

  /// Current module path for name mangling (set when processing module graph).
  std::vector<std::string> currentModulePath;

  /// Maps module short names (last path segment) to their full module path.
  /// E.g. "string" → ["std", "string"], "crypto" → ["std", "crypto", "crypto"].
  /// Populated during module graph processing for module-qualified calls.
  std::unordered_map<std::string, std::vector<std::string>> moduleNameToPath;

  /// Maps alias names to (original module path, original function name).
  /// E.g. "hello" → (["mylib"], "greet") for `import mylib::{greet as hello}`.
  std::unordered_map<std::string, std::pair<std::vector<std::string>, std::string>> aliasToFunction;

  /// Maps each module path (as string key) to the list of imported module paths.
  /// Used to resolve cross-module function calls (e.g. diamond dependencies).
  std::unordered_map<std::string, std::vector<std::vector<std::string>>> moduleImports;

  // The file identifier used for source locations.
  mlir::StringAttr fileIdentifier;

  // ── String pool ──────────────────────────────────────────────────
  // The ScopedHashTable keys are StringRef (non-owning). We intern every
  // variable name here so the StringRef stays valid for the whole
  // MLIRGen lifetime.
  llvm::StringSet<> stringPool;
  llvm::StringRef intern(const std::string &s) { return stringPool.insert(s).first->getKey(); }

  // ── Symbol tables ────────────────────────────────────────────────
  // Immutable variables: name -> SSA value
  using SymbolTableScopeT = llvm::ScopedHashTableScope<llvm::StringRef, mlir::Value>;
  llvm::ScopedHashTable<llvm::StringRef, mlir::Value> symbolTable;

  // Mutable variables: name -> memref alloca value
  using MutableTableScopeT = llvm::ScopedHashTableScope<llvm::StringRef, mlir::Value>;
  llvm::ScopedHashTable<llvm::StringRef, mlir::Value> mutableVars;

  // Heap-cell-backed mutable variables: maps the memref alloca value to the
  // underlying value type.  Used for mutable variable capture in closures —
  // the memref stores an `!llvm.ptr` to a heap cell, and lookupVariable /
  // storeVariable perform double indirection through it.
  llvm::DenseMap<mlir::Value, mlir::Type> heapCellValueTypes;

  // ── Global strings deduplication ─────────────────────────────────
  std::unordered_map<std::string, std::string> globalStrings;
  unsigned globalStringCounter = 0;

  // ── Expression type map ─────────────────────────────────────────
  // Resolved types from the Rust type checker, indexed by source span.
  // Built from Program::expr_types in generate(). Pointers are valid
  // for the lifetime of the Program reference passed to generate().
  struct SpanHash {
    std::size_t operator()(std::pair<uint64_t, uint64_t> p) const {
      return std::hash<uint64_t>{}(p.first) ^ (std::hash<uint64_t>{}(p.second) << 32);
    }
  };
  std::unordered_map<std::pair<uint64_t, uint64_t>, const ast::TypeExpr *, SpanHash> exprTypeMap;

  /// Look up the resolved type for an expression by its source span.
  const ast::TypeExpr *resolvedTypeOf(const ast::Span &span) const {
    auto it = exprTypeMap.find({span.start, span.end});
    if (it != exprTypeMap.end())
      return it->second;
    return nullptr;
  }

  // ── Struct type registry ──────────────────────────────────────────
  std::unordered_map<std::string, StructTypeInfo> structTypes;

  // ── Enum type registry ────────────────────────────────────────────
  struct EnumVariantInfo {
    std::string name;
    unsigned index;
    std::vector<mlir::Type> payloadTypes; // empty for unit variants
    // Absolute struct field positions (excluding tag at index 0) used for each
    // payload element when constructing/extracting this variant.
    std::vector<int64_t> payloadPositions;
  };
  struct EnumTypeInfo {
    std::string name;
    std::vector<EnumVariantInfo> variants;
    mlir::Type mlirType;      // i32 for unit-only enums, LLVMStructType for payload enums
    bool hasPayloads = false; // true if any variant has a payload
  };
  std::unordered_map<std::string, EnumTypeInfo> enumTypes;
  // Variant name → (enum name, variant index) for quick lookup
  std::unordered_map<std::string, std::pair<std::string, unsigned>> variantLookup;

  // ── Module-level constants ─────────────────────────────────────────
  // Stored as AST expressions, generated inline when referenced.
  std::unordered_map<std::string, const ast::Expr *> moduleConstants;

  // ── Actor type registry ───────────────────────────────────────────
  std::unordered_map<std::string, ActorInfo> actorRegistry;

  // ── Supervisor names → child actor types ─────────────────────
  // Maps supervisor name → ordered list of child actor type names
  std::unordered_map<std::string, std::vector<std::string>> supervisorChildren;

  // Track which variables hold actors and their type name
  std::unordered_map<std::string, std::string> actorVarTypes;

  // Maps "ActorName.fieldName" -> target actor type name
  std::unordered_map<std::string, std::string> actorFieldTypes;

  // The actor currently being generated (for resolving self.field)
  std::string currentActorName;

  // ── Collection type tracking ───────────────────────────────────────
  // Track HashMap variables for erased-pointer fallback paths.
  // Vec/bytes dispatch uses typed hew::VecType and does not use this map.
  std::unordered_map<std::string, std::string> collectionVarTypes;
  // Track collection-typed actor fields: "ActorName.fieldName" → "Vec<i32>", etc.
  std::unordered_map<std::string, std::string> collectionFieldTypes;

  // ── Declared type context ─────────────────────────────────────────
  // Set before generating a let/var initializer expression.  Carries
  // the MLIR type from the declaration's type annotation so that
  // constructors (Vec::new, HashMap::new, None, Ok, Err) can emit the
  // correct typed result without string matching.  Consumed and reset
  // by the first builtin that uses it.
  std::optional<mlir::Type> pendingDeclaredType;

  // ── Handle type tracking ──────────────────────────────────────────
  // Track typed handle variables: varName → "http.Server", "net.Connection", etc.
  std::unordered_map<std::string, std::string> handleVarTypes;

  // ── Handle type metadata (from Rust type checker) ──────────────
  /// Set of all known handle type names for data-driven type conversion.
  /// Populated from Program.handle_types during generate().
  std::unordered_set<std::string> knownHandleTypes;
  /// Handle type name → MLIR representation. Non-default entries only
  /// (e.g., "net.Listener" → "i32"). Types not in this map use HandleType.
  std::unordered_map<std::string, std::string> handleTypeRepr;

  // Counter for anonymous lambda actor names
  unsigned lambdaActorCounter = 0;

  // Whether any actor code was generated (to wrap main with scheduler)
  bool hasActors = false;

  // ── Structured concurrency scope tracking ────────────────────────
  // When non-null, spawns inside a scope body register with this scope.
  mlir::Value currentScopePtr;     // !llvm.ptr to HewScope, nullptr outside scope
  mlir::Value currentTaskScopePtr; // !llvm.ptr to HewTaskScope, nullptr outside scope

  // ── Lambda counter for unique anonymous function names ────────────
  unsigned lambdaCounter = 0;
  // ── Task counter for unique scope.launch function names ─────────
  unsigned taskCounter = 0;

  // ── Task result type tracking ────────────────────────────────────
  // Maps variable name (let-bound to a scope.launch task) → the MLIR type
  // of the body result, so that `await` can load the correct type.
  std::unordered_map<std::string, mlir::Type> taskResultTypes;
  // Scratch: the result type of the most recently compiled scope.launch.
  std::optional<mlir::Type> lastScopeLaunchResultType;

  // ── Closure thunk cache ─────────────────────────────────────────
  // Maps top-level function name → thunk wrapper name (generated on demand
  // when a function is used as a closure value).
  std::unordered_map<std::string, std::string> closureThunkCache;

  // ── Generator type tracking ───────────────────────────────────────
  // Track which variables hold generators: varName → generator function name
  std::unordered_map<std::string, std::string> generatorVarTypes;
  // Set of generator function names (to identify generator call results)
  std::set<std::string> generatorFunctions;

  // ── Receive generator (streaming) tracking ─────────────────────────
  // Track which variables hold actor streams: varName → {actorType, nextMsgType}
  struct StreamInfo {
    std::string actorTypeName; // actor type (for actorVarTypes lookup)
    unsigned nextMsgTypeIdx;   // msg_type index for the __next handler
    mlir::Type yieldType;      // type of yielded values
  };
  std::unordered_map<std::string, StreamInfo> streamVarTypes;

  // ── First-class Stream<T> / Sink<T> tracking ───────────────────
  // Maps variable name → "Stream" or "Sink" for first-class stream handles.
  // Separate from the actor-generator streamVarTypes above.
  std::unordered_map<std::string, std::string> streamHandleVarTypes;

  void generateForStreamStmt(const ast::StmtFor &stmt);

  // Track which receive fns are generators: "ActorName.methodName" → true
  std::unordered_map<std::string, bool> receiveGenFns;
  // Hidden __gen_frame field index in actor state struct:
  // "ActorName.methodName" → struct field index (for storing HewGenCtx*)
  std::unordered_map<std::string, unsigned> genFrameFieldIdx;
  // When non-null, yield expressions emit hew_gen_yield calls
  mlir::Value currentGenCtx; // ptr to HewGenCtx, nullptr outside gen body

  // Set by generateCallExpr before generating lambda arguments;
  // consumed by generateLambdaExpr to infer parameter types from context.
  // Can be either ClosureType or FunctionType.
  std::optional<mlir::Type> pendingLambdaExpectedType;

  /// Collect identifiers used in a block that are free (not locally defined).
  void collectFreeVarsInBlock(const ast::Block &block, const std::set<std::string> &bound,
                              std::set<std::string> &freeVars);
  void collectFreeVarsInExpr(const ast::Expr &expr, const std::set<std::string> &bound,
                             std::set<std::string> &freeVars);
  void collectFreeVarsInStmt(const ast::Stmt &stmt, std::set<std::string> &bound,
                             std::set<std::string> &freeVars);

  // ── Defer tracking (function-scoped, LIFO execution) ─────────
  struct DeferInfo {
    const ast::Expr *expr;   // deferred expression (from AST)
    mlir::Location location; // source location for diagnostics
    DeferInfo(const ast::Expr *e, mlir::Location loc) : expr(e), location(loc) {}
  };
  std::vector<DeferInfo> currentFnDefers;
  void emitDeferredCalls();

  // ── Drop tracking (RAII) ───────────────────────────────────────
  struct DropEntry {
    std::string varName;
    std::string dropFuncName;
    bool isUserDrop = false;
  };
  std::vector<std::vector<DropEntry>> dropScopes;
  std::unordered_map<std::string, std::string> userDropFuncs;
  // Variable name to exclude from function-level drops (the returned variable).
  // Set by generateFunction before generateBlock, cleared after.
  std::set<std::string> funcLevelDropExcludeVars;
  void pushDropScope();
  void popDropScope();
  void registerDroppable(const std::string &varName, const std::string &dropFunc,
                         bool isUserDrop = false);
  /// Remove a variable from all drop scopes (ownership transferred, e.g. actor send).
  void unregisterDroppable(const std::string &varName);
  void emitDropsForScope(const std::vector<DropEntry> &scope);
  void emitDropsForCurrentScope();
  void emitAllDrops();
  void emitDropsExcept(const std::string &excludeVar);
  void emitDropsExcept(const std::set<std::string> &excludeVars);

  void emitStringDrop(mlir::Value v);
  /// Returns true if `v` is a temporary string (heap-allocated, not from
  /// a variable load or a constant).  Safe to drop after consumption.
  bool isTemporaryString(mlir::Value v);

  // ── Error tracking ────────────────────────────────────────────────
  unsigned errorCount_ = 0;

  // ── Current source location (set by dispatch functions) ──────────
  mlir::Location currentLoc;

  // ── Current function tracking ────────────────────────────────────
  mlir::func::FuncOp currentFunction;

  // ── Loop control (for break/continue) ──────────────────────────
  // Stack of memref<i1> values: when set to false, the loop terminates.
  llvm::SmallVector<mlir::Value, 4> loopActiveStack;
  // Stack of memref<i1> values: when set to true, skip rest of loop body.
  llvm::SmallVector<mlir::Value, 4> loopContinueStack;
  // Stack of allocas for break-with-value results.
  llvm::SmallVector<mlir::Value, 4> loopBreakValueStack;
  // After a loop finishes, holds the loaded break value (if any).
  mlir::Value lastBreakValue;
  // Labeled loop mappings: label name → active/continue flags.
  std::unordered_map<std::string, mlir::Value> labeledActiveFlags;
  std::unordered_map<std::string, mlir::Value> labeledContinueFlags;

  // ── Early return support ──────────────────────────────────────
  // Per-function flag: true when an early return has been taken.
  mlir::Value returnFlag; // memref<i1>, nullptr when not active
  // Per-function slot for storing the return value.
  mlir::Value returnSlot; // memref<ReturnType>, nullptr when not active

  // ── Try/catch context ────────────────────────────────────────
  // When non-null, PostfixTry (?) jumps here instead of func.return.
  mlir::Block *tryErrorDest = nullptr;
  // Alloca where PostfixTry stores the error value inside a try block.
  mlir::Value tryErrorSlot; // memref<ErrType>, nullptr when not in try

  // ── Trait registry ─────────────────────────────────────────────
  struct TraitInfo {
    std::vector<const ast::TraitMethod *> methods;
    std::unordered_map<std::string, unsigned> methodIndex; // name → vtable slot
  };
  std::unordered_map<std::string, TraitInfo> traitRegistry;

  // ── dyn Trait dispatch infrastructure ────────────────────────────
  struct TraitImplInfo {
    std::string typeName;
    std::string vtableName;                 // e.g. "__vtable_HT3DogF8Greetable"
    std::vector<std::string> shimFunctions; // shim function names in vtable order
  };
  struct TraitDispatchInfo {
    std::vector<TraitImplInfo> impls;     // all types implementing this trait
    std::vector<std::string> methodNames; // ordered method names
  };
  // traitName → dispatch info
  std::unordered_map<std::string, TraitDispatchInfo> traitDispatchRegistry;
  // Track dyn-trait variable types: varName → traitName
  std::unordered_map<std::string, std::string> dynTraitVarTypes;

  /// Register an impl for trait dispatch and generate shim functions.
  void registerTraitImpl(const std::string &typeName, const std::string &traitName,
                         const std::vector<std::string> &methodNames);

  /// Generate vtable dispatch shim functions for a (type, trait) pair.
  void generateTraitImplShims(const std::string &typeName, const std::string &traitName);

  /// Generate a single dyn dispatch shim function.
  void generateDynDispatchShim(const std::string &implFuncName);

  /// Coerce a concrete struct value to a dyn Trait fat pointer {data_ptr, vtable_ptr}.
  mlir::Value coerceToDynTrait(mlir::Value concreteVal, const std::string &typeName,
                               const std::string &traitName, mlir::Location location);

  // ── Generics monomorphization ──────────────────────────────────
  // Registry of generic (unspecialized) function declarations.
  std::unordered_map<std::string, const ast::FnDecl *> genericFunctions;
  // Registry of generic (unspecialized) struct declarations.
  std::unordered_map<std::string, const ast::TypeDecl *> genericStructs;
  // Set of already-specialized mangled names to avoid duplicate generation.
  std::set<std::string> specializedFunctions;
  // Active type parameter substitutions (e.g., "T" → "i32") during specialization.
  std::unordered_map<std::string, std::string> typeParamSubstitutions;
  // Type alias mappings (e.g., "Distance" → TypeNode for i32).
  std::unordered_map<std::string, const ast::TypeExpr *> typeAliases;
  // Resolve a type name through the type alias map (e.g., "TopicFilter" → "string").
  // Returns the original name unchanged if no alias is found.
  std::string resolveTypeAlias(const std::string &name) const;
  // Mangle a generic function name with concrete type arguments.
  std::string mangleGenericName(const std::string &baseName,
                                const std::vector<std::string> &typeArgs);
  // Specialize and generate a generic function for the given concrete type args.
  mlir::func::FuncOp specializeGenericFunction(const std::string &baseName,
                                               const std::vector<std::string> &typeArgs);
  // Resolve a TypeExpr to a flat mangled name for generic substitutions.
  // Recursively handles nested generics: Pair<int> → "Pair_int".
  // Also triggers convertType to ensure nested generic structs are specialized.
  std::string resolveTypeArgMangledName(const ast::TypeExpr &type);
};

} // namespace hew

#endif // HEW_MLIR_MLIRGEN_H
