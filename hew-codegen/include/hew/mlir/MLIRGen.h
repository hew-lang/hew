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

#include "llvm/ADT/DenseSet.h"
#include "llvm/ADT/DenseMap.h"
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
  explicit MLIRGen(mlir::MLIRContext &context, const std::string &targetTriple = "",
                   const std::string &sourcePath = "", const std::vector<size_t> &lineMap = {});

  /// Main entry point: lower a complete Hew Program AST to an MLIR module.
  /// Returns nullptr on failure.
  mlir::ModuleOp generate(const ast::Program &program);

  /// Return true if the given name is a builtin unsigned integer type
  /// (u8, u16, u32, u64, uint, byte).
  static bool isBuiltinUnsignedIntegerName(llvm::StringRef name) {
    return name == "u8" || name == "u16" || name == "u32" || name == "u64" || name == "uint" ||
           name == "byte";
  }

  /// Return true if the given TypeExpr refers to an unsigned integer type
  /// directly or through a type alias.
  bool isUnsignedTypeExpr(const ast::TypeExpr &type) const {
    auto *named = std::get_if<ast::TypeNamed>(&type.kind);
    if (!named)
      return false;
    return isBuiltinUnsignedIntegerName(resolveTypeAlias(named->name));
  }

private:
  // ── Type conversion ──────────────────────────────────────────────
  mlir::Type convertType(const ast::TypeExpr &type,
                         std::optional<mlir::Location> errorLoc = std::nullopt);

  /// Convert a TypeExpr to an MLIR type, returning nullptr on failure.
  /// Calls convertType() and checks the result with isValidType().
  /// On failure, emits a contextual error (in addition to any root-cause
  /// error from convertType()), increments errorCount_, and returns nullptr.
  /// Use this at call sites that consume the type — it prevents silent
  /// NoneType propagation and centralizes the validation boilerplate.
  mlir::Type convertTypeOrError(const ast::TypeExpr &type, llvm::StringRef context,
                                std::optional<mlir::Location> errorLoc = std::nullopt);

  mlir::Type defaultIntType();
  mlir::Type defaultFloatType();

  // ── Top-level items ──────────────────────────────────────────────
  void generateItem(const ast::Item &item, std::optional<mlir::Location> fallbackLoc = std::nullopt);
  void registerTypeDecl(const ast::TypeDecl &decl);
  void generateIndirectEnumDropFunc(const std::string &enumName);
  void registerFunctionSignature(const ast::FnDecl &fn, const std::string &nameOverride = "",
                                 std::optional<mlir::Location> fallbackLoc = std::nullopt);
  void generateImplDecl(const ast::ImplDecl &decl,
                        std::optional<mlir::Location> fallbackLoc = std::nullopt);
  void registerTraitDecl(const ast::TraitDecl &decl);
  void generateTraitDefaultMethod(const ast::TraitMethod &method, const std::string &targetTypeName,
                                  const std::string &mangledName,
                                  std::optional<mlir::Location> fallbackLoc = std::nullopt);
  void generateExternBlock(const ast::ExternBlock &block,
                           std::optional<mlir::Location> fallbackLoc = std::nullopt);
  void generateImport(const ast::ImportDecl &decl);
  mlir::func::FuncOp generateFunction(const ast::FnDecl &fn, const std::string &nameOverride = "",
                                      std::optional<mlir::Location> fallbackLoc = std::nullopt);
  void generateGeneratorFunction(const ast::FnDecl &fn);
  void registerActorDecl(const ast::ActorDecl &decl,
                         std::optional<mlir::Location> fallbackLoc = std::nullopt);
  void generateActorDecl(const ast::ActorDecl &decl);
  void generateWireDecl(const ast::WireDecl &decl);
  /// Pre-register wire struct type with wire-aware field types so that actor
  /// registration (pass 1e) can resolve wire struct parameter types.
  void preRegisterWireStructType(const ast::WireDecl &decl);
  /// Pre-declare (body-less) helper functions for a wire struct so that outer
  /// wire structs can reference them regardless of declaration order.
  void predeclareWireHelpers(const ast::WireDecl &decl);
  /// Generate mangled method wrappers for wire types so that method dispatch
  /// (o.to_json(), Point.from_json(), Status.from_yaml()) works through the
  /// standard type-dispatch path.
  void generateWireMethodWrappers(const ast::WireDecl &decl);
  /// Return an !llvm.ptr to a NUL-terminated global string constant.
  mlir::Value wireStringPtr(mlir::Location location, llvm::StringRef value);
  /// Generate Foo_to_{json,yaml} for wire structs and unit-only wire enums —
  /// parameterized by format and field name resolver / naming convention.
  void generateWireToSerial(
      const ast::WireDecl &decl, llvm::StringRef format,
      const std::optional<ast::NamingCase> &namingCase,
      llvm::function_ref<const std::optional<std::string> &(const ast::WireFieldDecl &)>
          fieldOverride);
  /// Generate Foo_from_{json,yaml} for wire structs and unit-only wire enums —
  /// parameterized by format and field name resolver / naming convention.
  void generateWireFromSerial(
      const ast::WireDecl &decl, llvm::StringRef format,
      const std::optional<ast::NamingCase> &namingCase,
      llvm::function_ref<const std::optional<std::string> &(const ast::WireFieldDecl &)>
          fieldOverride);
  /// Generate TypeName_to_{json,yaml,toml} for an Encode-eligible struct type.
  /// Takes struct as input, serializes each field, returns string pointer.
  void generateStructToSerial(const std::string &typeName, llvm::StringRef format);
  /// Generate TypeName_from_{json,yaml,toml} for a Decode-eligible struct type.
  /// Takes string input, parses and extracts fields, returns struct.
  void generateStructFromSerial(const std::string &typeName, llvm::StringRef format);
  /// Generate mangled method wrappers for struct encode/decode methods.
  void generateStructEncodeWrappers(const std::string &typeName);
  void generateSupervisorDecl(const ast::SupervisorDecl &decl);
  void registerMachineDecl(const ast::MachineDecl &decl);
  void generateMachineDecl(const ast::MachineDecl &decl);

  // ── Actor type registry (declared early for method signatures) ────
  struct ActorReceiveInfo {
    std::string name;
    std::vector<std::string> paramNames;  // message parameter names
    std::vector<mlir::Type> paramTypes;   // message parameter types
    std::optional<mlir::Type> returnType; // return type (for await/ask)
    std::optional<int64_t> periodicIntervalNs; // periodic interval from #[every]
  };
  struct ActorInfo {
    std::string name;
    std::optional<mlir::Location> sourceLoc;
    mlir::LLVM::LLVMStructType stateType;     // state struct type
    std::vector<ActorReceiveInfo> receiveFns; // receive handlers in order
    std::vector<mlir::Type> fieldHewTypes;    // Hew MLIR types: [user fields..., init params...]
    size_t numUserFields = 0;                 // count of user-declared state fields only
    std::vector<std::string> initParamNames;  // init parameter names (in declaration order)
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
  bool actorBoundarySenderRetainsOwnership(mlir::Type valueType) const;
  /// Generate args for an actor send/ask call, handling self-reference substitution.
  std::optional<llvm::SmallVector<mlir::Value, 4>>
  generateActorCallArgs(const std::vector<ast::CallArg> &args, mlir::Location location,
                        bool retainAllTemporaries = false);
  /// Emit the gen-next null-check, wrap, cleanup, and return sequence.
  void emitGenNextResult(mlir::Value ctx, mlir::Value selfPtr, mlir::LLVM::LLVMStructType stateType,
                         unsigned genFrameIdx, mlir::Type yieldType, mlir::Type wrapperType,
                         mlir::Location location);

  // ── Statements ───────────────────────────────────────────────────
  void generateStatement(const ast::Stmt &stmt);
  void generateLetStmt(const ast::StmtLet &stmt);
  void generateVarStmt(const ast::StmtVar &stmt);

  /// Shared drop registration for both let and var bindings.
  /// Registers Stream/Sink RAII close, collection/string drops,
  /// user-defined drops, wire struct field drops, and closure env cleanup.
  void registerDropsForVariable(
      const std::string &varName, mlir::Value value,
      const std::optional<ast::Spanned<ast::TypeExpr>> *stmtTy,
      const std::optional<ast::Spanned<ast::Expr>> *stmtValue,
      bool isMutable, mlir::Location location);
  void generateAssignStmt(const ast::StmtAssign &stmt);
  void generateIfStmt(const ast::StmtIf &stmt);
  mlir::Value generateIfStmtAsExpr(const ast::StmtIf &stmt);
  void generateWhileStmt(const ast::StmtWhile &stmt);
  void generateWhileLetStmt(const ast::StmtWhileLet &stmt);
  bool isExprLoopInvariant(const ast::Expr &expr);
  void generateForStmt(const ast::StmtFor &stmt);
  void generateForAwaitStmt(const ast::StmtFor &stmt);
  void generateForRange(const ast::StmtFor &stmt, const ast::ExprBinary &rangeExpr);
  void generateForCollectionStmt(const ast::StmtFor &stmt);
  void generateForVec(const ast::StmtFor &stmt, mlir::Value collection,
                      const std::string &collType);
  void generateForString(const ast::StmtFor &stmt, mlir::Value collection,
                         const std::string &collType);
  void generateForHashMap(const ast::StmtFor &stmt, mlir::Value collection,
                          const std::string &collType);
  void generateForGeneratorStmt(const ast::StmtFor &stmt, const std::string &genFuncName);
  void generateReturnStmt(const ast::StmtReturn &stmt);
  void generateExprStmt(const ast::StmtExpression &stmt);

  // ── Expressions ──────────────────────────────────────────────────
  mlir::Value generateExpression(const ast::Expr &expr);
  mlir::Value generateLiteral(const ast::Literal &lit, const ast::Span &span);
  mlir::Value generateBinaryExpr(const ast::ExprBinary &expr);
  mlir::Value generateUnaryExpr(const ast::ExprUnary &expr);
  mlir::Value generateCallExpr(const ast::ExprCall &expr);
  mlir::Value generateIfExpr(const ast::ExprIf &expr, const ast::Span &exprSpan);
  mlir::Value generateBlockExpr(const ast::Block &block);
  mlir::Value generatePostfixExpr(const ast::ExprPostfixTry &expr);
  mlir::Value generateStructInit(const ast::ExprStructInit &expr,
                                 const ast::Span &exprSpan);
  mlir::Value generateMethodCall(const ast::ExprMethodCall &expr);
  std::optional<mlir::Value> generateModuleMethodCall(const ast::ExprMethodCall &mc,
                                                      const ast::ExprIdentifier &ident,
                                                      mlir::Location location);
  std::optional<mlir::Value> generateHandleMethodCall(const ast::ExprMethodCall &mc,
                                                      mlir::Value receiver,
                                                      mlir::Location location);
  std::optional<mlir::Value> generateActorMethodCall(const ast::ExprMethodCall &mc,
                                                     mlir::Value receiver,
                                                     mlir::Location location);
  std::optional<mlir::Value> generateBuiltinMethodCall(const ast::ExprMethodCall &expr,
                                                       mlir::Value receiver,
                                                       mlir::Location location);
  mlir::Value generateLogCall(const ast::ExprMethodCall &mc);
  mlir::Value generateLogEmit(const std::vector<ast::CallArg> &args, int levelInt);
  mlir::Value generateTupleExpr(const ast::ExprTuple &expr);
  mlir::Value generateArrayExpr(const ast::ExprArray &expr);
  mlir::Value generateMapLiteralExpr(const ast::ExprMapLiteral &mapLit, const ast::Span &exprSpan);
  mlir::Value generateArrayRepeatExpr(const ast::ExprArrayRepeat &expr, const ast::Span &exprSpan);
  mlir::Value generateLambdaExpr(const ast::ExprLambda &expr);
  mlir::Value generateScopeExpr(const ast::ExprScope &expr);
  mlir::Value generateScopeLaunchExpr(const ast::ExprScopeLaunch &expr);
  mlir::Value generateScopeSpawnExpr(const ast::ExprScopeSpawn &expr);
  mlir::Value generateScopeLaunchImpl(const ast::Block &block);
  mlir::Value generateScopeCancelExpr();

  mlir::Value generateSelectExpr(const ast::ExprSelect &expr);
  mlir::Value generateJoinExpr(const ast::ExprJoin &expr);
  mlir::Value generateInterpolatedString(const ast::ExprInterpolatedString &expr);
  mlir::Value generateBytesLiteral(const std::vector<uint8_t> &data);
  mlir::Value generateRegexLiteral(const ast::ExprRegexLiteral &expr);

  // ── Block lowering ───────────────────────────────────────────────
  /// Generates all statements in a block and returns the block's trailing
  /// expression value (or nullptr if the block has no trailing expression).
  /// When statementPosition is true, final if/match statements are lowered as
  /// plain statements instead of implicit block results.
  /// When isFunctionBodyBlock is true, the return-guard path activates for
  /// non-void functions.  Inline sub-blocks (unsafe, block-expr) must pass
  /// false (the default) so their result is returned as an SSA value rather
  /// than stored into returnSlot.
  mlir::Value generateBlock(const ast::Block &block, bool statementPosition = false,
                            bool isFunctionBodyBlock = false);

  // ── Match ────────────────────────────────────────────────────────
  void generateMatchStmt(const ast::StmtMatch &stmt);
  mlir::Value generateMatchExpr(const ast::ExprMatch &expr, const ast::Span &exprSpan);
  // findIndirectEnumForScrutinee and derefIndirectEnumScrutinee declared below
  // (after EnumTypeInfo definition)
  mlir::Value generateMatchImpl(mlir::Value scrutinee, const std::vector<ast::MatchArm> &arms,
                                mlir::Type resultType, mlir::Location location);
  mlir::Value generateMatchArmsChain(mlir::Value scrutinee, const std::vector<ast::MatchArm> &arms,
                                     size_t idx, mlir::Type resultType, mlir::Location location);
  mlir::Value generateOrPatternCondition(mlir::Value scrutinee, const ast::Pattern &pattern,
                                         mlir::Location location);

  // ── Pattern helpers (shared by match and if-let) ──────────────────
  /// Resolve the struct field index for an enum variant payload.
  int64_t resolvePayloadFieldIndex(llvm::StringRef variantName, size_t payloadOrdinal) const;
  /// Bind tuple pattern elements to variables recursively.
  void bindTuplePatternFields(const ast::PatTuple &tp, mlir::Value tupleValue,
                              mlir::Location location);
  /// Bind constructor sub-pattern variables by extracting enum payloads.
  void bindConstructorPatternVars(const ast::PatConstructor &ctor, mlir::Value scrutinee,
                                  mlir::Location location);
  /// Recursively bind sub-patterns in let destructuring (struct fields,
  /// tuple elements, identifiers, wildcards).
  void bindLetSubPattern(const ast::Pattern &pattern, mlir::Value value,
                         mlir::Location location);
  /// Emit a tag-equality comparison: extract tag, compare with variantIndex.
  mlir::Value emitTagEqualCondition(mlir::Value scrutinee, int64_t variantIndex,
                                    mlir::Location location);

  // ── If-let ───────────────────────────────────────────────────────
  void generateIfLetStmt(const ast::StmtIfLet &stmt);
  mlir::Value generateIfLetExpr(const ast::ExprIfLet &expr, const ast::Span &exprSpan);

  // ── Loop/Break/Continue ─────────────────────────────────────────
  void generateLoopStmt(const ast::StmtLoop &stmt);
  void generateBreakStmt(const ast::StmtBreak &stmt);
  void generateContinueStmt(const ast::StmtContinue &stmt);

  // ── Print/println built-in handling ──────────────────────────────
  mlir::Value generatePrintCall(const ast::ExprCall &expr, bool newline);

  // ── Select/Join helpers ───────────────────────────────────────────
  /// Resolve actor type name from an expression (e.g., variable holding actor).
  /// Checks resolvedTypeOf (from type checker), identifier-based actorVarTypes,
  /// and field-access-based actorFieldTypes.
  std::string resolveActorTypeName(const ast::Expr &expr, const ast::Span *span = nullptr);

  /// Pack argument values into a stack-allocated buffer for sending.
  /// Returns (data_ptr, data_size) as (!llvm.ptr, i64).
  std::pair<mlir::Value, mlir::Value> packArgsForSend(llvm::ArrayRef<mlir::Value> args,
                                                      mlir::Location location);

  /// Wait for a reply channel, load the typed result, and free the reply buffer.
  mlir::Value waitOnReplyChannel(mlir::Value channel, mlir::Type resultType,
                                 mlir::Location location);

  /// Mark a reply channel as abandoned so late replies self-clean in the runtime.
  void cancelReplyChannel(mlir::Value channel, mlir::Location location);

  /// Cancel and release a reply channel that will no longer be awaited.
  void abandonReplyChannel(mlir::Value channel, mlir::Location location);

  /// Panic after cleaning up all reply channels created before a failed send.
  void panicOnReplySendFailure(mlir::Value sendStatus, llvm::ArrayRef<mlir::Value> pendingChannels,
                               mlir::Location location);

  // ── Helpers ──────────────────────────────────────────────────────
  /// Join currentModulePath into a "::" delimited key string.
  std::string currentModuleKey() const;

  /// Look up a function through imported module paths.
  mlir::func::FuncOp lookupImportedFunc(llvm::StringRef typeName, llvm::StringRef funcName);

  /// Emit a RuntimeCallOp, returning the result (or nullptr for void calls).
  mlir::Value emitRuntimeCall(llvm::StringRef callee, mlir::Type resultType, mlir::ValueRange args,
                              mlir::Location location);

  /// Emit an Option<T> wrapping: builds an scf::IfOp with Some(payload)/None branches.
  mlir::Value emitOptionWrap(mlir::Value condition, mlir::Value payload,
                             mlir::Type optionType, mlir::Location location);

  /// Allocate the returnFlag and (if the return type is memref-compatible)
  /// the returnSlot for early-return support inside SCF regions.
  void initReturnFlagAndSlot(mlir::ArrayRef<mlir::Type> resultTypes, mlir::Location location);

  /// Lazily create returnSlot for aggregate types (structs, tuples, arrays)
  /// when an early return from a nested SCF region actually needs it.
  /// Creates the alloca at the function entry block to ensure visibility.
  void ensureReturnSlot(mlir::Location location);

  /// Apply a compound assignment arithmetic operation to (lhs, rhs).
  /// Returns the result value, or nullptr on unsupported operator.
  mlir::Value emitCompoundArithOp(ast::CompoundAssignOp op, mlir::Value lhs, mlir::Value rhs,
                                  bool isFloat, bool isUnsigned, mlir::Location location);

  /// If returnFlag is set, AND the given condition with "not returned".
  /// Returns the (possibly tightened) condition.
  mlir::Value andNotReturned(mlir::Value cond, mlir::Location location);

  /// Ensure the current insertion block has a terminator; inserts scf.yield
  /// if missing.
  void ensureYieldTerminator(mlir::Location location);

  /// Allocate loop-control flags (active, continue), push onto stacks, and
  /// register optional label.  Returns {activeFlag, continueFlag}.
  struct LoopControl {
    mlir::Value activeFlag;
    mlir::Value continueFlag;
    std::string labelName;
  };
  LoopControl pushLoopControl(const std::optional<std::string> &label, mlir::Location location);

  /// Pop loop-control stacks, erase label entries, and load break-value
  /// (if any) into lastBreakValue.
  void popLoopControl(const LoopControl &lc, mlir::Operation *whileOp);

  mlir::Location loc(const ast::Span &span);

  /// Line map from serialized AST: byte offset of each line start.
  std::vector<size_t> lineMap_;

  /// Convert a byte offset to (line, column), both 1-based.
  /// Returns (0, 0) if lineMap_ is empty (no debug info).
  std::pair<unsigned, unsigned> byteOffsetToLineCol(size_t offset) const;

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

  /// Create a memref alloca for a variable slot. When return guards are active,
  /// hoists the alloca to the function entry block and zero-initialises
  /// pointer-like types so unconditional drops are safe.
  mlir::Value createHoistedAlloca(mlir::Type storageType, mlir::Type semanticType);

  /// Store to a mutable variable.
  void storeVariable(llvm::StringRef name, mlir::Value value);

  /// Lookup the memref slot for a mutable variable, applying heap-cell remaps.
  mlir::Value getMutableVarSlot(llvm::StringRef name);

  /// Get a unique name for a global string literal.
  std::string getOrCreateGlobalString(llvm::StringRef value);

  /// Coerce a value to a target type (e.g., int-to-float promotion).
  mlir::Value coerceType(mlir::Value value, mlir::Type targetType, mlir::Location location,
                         bool isUnsigned = false);
  /// Sink-hardening wrapper around coerceType(). Guarantees the returned value
  /// has targetType by substituting a typed default on coercion failure and
  /// incrementing errorCount_ so generation still fails closed.
  /// Preserves unsigned widening when the sink is the active function's
  /// declared unsigned integer return type.
  mlir::Value coerceTypeForSink(mlir::Value value, mlir::Type targetType,
                                mlir::Location location);
  bool shouldUseUnsignedReturnSinkCoercion(mlir::Type targetType);

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
  bool isWasm32_ = false;
  mlir::IntegerType cachedSizeType_;

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
  /// When a mutable variable is promoted to a heap cell, map the original
  /// slot (from the parent scope) to the heap-cell pointer slot.
  llvm::DenseMap<mlir::Value, mlir::Value> heapCellRebindings;

  // Heap-cell-backed mutable variables: maps the memref alloca value to the
  // underlying value type.  Used for mutable variable capture in closures —
  // the memref stores an `!llvm.ptr` to a heap cell, and lookupVariable /
  // storeVariable perform double indirection through it.
  llvm::DenseMap<mlir::Value, mlir::Type> heapCellValueTypes;
  // Memref-backed local slots whose element type is an LLVM storage type
  // rather than the original semantic Hew type. Loads/stores must bitcast.
  llvm::DenseMap<mlir::Value, mlir::Type> slotSemanticTypes;

  // ── Hoisted loop-invariant values ────────────────────────────────
  // Maps AST expression pointers to pre-computed MLIR values so that
  // generateExpression returns the cached result instead of re-emitting code.
  llvm::DenseMap<const ast::Expr *, mlir::Value> hoistedValues;

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

  // ── Machine transition body context ──────────────────────────────
  // Set during transition body evaluation so that ExprFieldAccess can
  // resolve `state.field` and `event.field` on machine enum values.
  std::string currentMachineSourceVariant_;
  std::string currentMachineEventVariant_;
  std::string currentMachineEventTypeName_;

  // ── Type origin tracking ─────────────────────────────────────────
  /// Maps type name → module path where the type was defined.
  /// Used to mangle impl method names with the defining module, not the
  /// importing module (prevents duplicate generation on import).
  std::unordered_map<std::string, std::vector<std::string>> typeDefModulePath;

  // ── Struct type registry ──────────────────────────────────────────
  std::unordered_map<std::string, StructTypeInfo> structTypes;

  // ── Enum type registry ────────────────────────────────────────────
  struct EnumVariantInfo {
    std::string name;
    unsigned index;
    std::vector<mlir::Type> payloadTypes; // empty for unit variants
    std::vector<std::string> fieldNames;  // for struct-like variants
    // Absolute struct field positions (excluding tag at index 0) used for each
    // payload element when constructing/extracting this variant.
    std::vector<int64_t> payloadPositions;
  };
  struct EnumTypeInfo {
    std::string name;
    std::vector<EnumVariantInfo> variants;
    mlir::Type mlirType;        // i32 for unit-only enums, LLVMStructType for payload enums
    bool hasPayloads = false;   // true if any variant has a payload
    bool isIndirect = false;    // true for `indirect enum` (heap-allocated, recursive)
    mlir::Type innerStructType; // the actual struct layout (when isIndirect)
  };
  std::unordered_map<std::string, EnumTypeInfo> enumTypes;
  // Variant name → (enum name, variant index) for quick lookup
  std::unordered_map<std::string, std::pair<std::string, unsigned>> variantLookup;
  // Names of indirect enums currently being registered (for self-reference detection)
  std::unordered_set<std::string> pendingIndirectEnums;

  // Indirect enum scrutinee dereferencing (declared here after EnumTypeInfo)
  const EnumTypeInfo *
  findIndirectEnumForScrutinee(mlir::Value scrutinee, const ast::Span &span,
                               const std::vector<ast::MatchArm> *arms = nullptr) const;
  mlir::Value derefIndirectEnumScrutinee(mlir::Value scrutinee, const ast::Span &span,
                                         mlir::Location location,
                                         const std::vector<ast::MatchArm> *arms = nullptr);

  // ── Module-level constants ─────────────────────────────────────────
  // Stored as AST expressions, generated inline when referenced.
  std::unordered_map<std::string, const ast::Expr *> moduleConstants;

  // ── Wire struct name registry ─────────────────────────────────────
  /// Maps wire struct name → (encode_wrapper, decode_wrapper) mangled names.
  /// Populated during wire codegen, used by actor codegen for wire messages.
  struct WireWrapperNames {
    std::string encodeName; // mangled encode wrapper function name
    std::string decodeName; // mangled decode wrapper function name
  };
  std::unordered_map<std::string, WireWrapperNames> wireStructNames;
  /// All declared wire struct names — populated before pass 1b2 so that
  /// preRegisterWireStructType can distinguish nested struct field references
  /// from primitive type aliases (e.g. "int", "uint") that wireTypeToMLIR
  /// handles via its default i32 fallback.
  std::unordered_set<std::string> allWireStructNames_;

  /// Non-wire struct types eligible for encode/decode wrappers (all-primitive fields,
  /// non-generic). Populated in registerTypeDecl; wrappers are generated lazily on the
  /// first call-site that demands one, rather than unconditionally for every eligible type.
  std::unordered_set<std::string> encodeEligibleStructs_;

  // ── Actor type registry ───────────────────────────────────────────
  std::unordered_map<std::string, ActorInfo> actorRegistry;
  std::unordered_set<std::string> generatedActorBodies;

  // ── Supervisor names → child actor types ─────────────────────
  // Maps supervisor name → ordered list of child actor type names
  std::unordered_map<std::string, std::vector<std::string>> supervisorChildren;
  // Maps supervisor name → ordered list of (child_name, child_actor_type)
  std::unordered_map<std::string, std::vector<std::pair<std::string, std::string>>>
      supervisorChildNames;

  // Track which variables hold actors and their type name
  std::unordered_map<std::string, std::string> actorVarTypes;

  // Maps "ActorName.fieldName" -> target actor type name
  std::unordered_map<std::string, std::string> actorFieldTypes;

  // The actor currently being generated (for resolving self.field)
  std::string currentActorName;

  // ── Collection type tracking ───────────────────────────────────────
  // Track collection-typed actor fields: "ActorName.fieldName" → "Vec<i32>", etc.
  std::unordered_map<std::string, std::string> collectionFieldTypes;
  // Extern function semantic return types before LLVM ABI erasure.
  std::unordered_map<std::string, mlir::Type> externSemanticReturnTypes;

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
  // ── Counter for unique closure return-type coercion thunk names ─
  unsigned closureCoercionCounter = 0;

  // ── Generator type tracking ───────────────────────────────────────
  // Track which variables hold generators: varName → generator function name
  std::unordered_map<std::string, std::string> generatorVarTypes;
  // Set of generator function names (to identify generator call results)
  std::set<std::string> generatorFunctions;
  // Track yield type per generator function (for coroutine codegen)
  std::unordered_map<std::string, mlir::Type> generatorYieldTypes;
  // When non-null, yield expressions store to this promise ptr and call suspend marker
  mlir::Value currentCoroPromisePtr; // ptr to yield value slot, nullptr outside coro gen body

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
  void generateForReceiverStmt(const ast::StmtFor &stmt, const ast::TypeNamed *receiverType);

  // Hidden __gen_frame field index in actor state struct:
  // "ActorName.methodName" → struct field index (for storing HewGenCtx*)
  std::unordered_map<std::string, unsigned> genFrameFieldIdx;
  // When non-null, yield expressions emit hew_gen_yield calls
  mlir::Value currentGenCtx; // ptr to HewGenCtx, nullptr outside gen body

  // Set by generateCallExpr before generating lambda arguments;
  // consumed by generateLambdaExpr to infer parameter types from context.
  // Can be either ClosureType or FunctionType.
  std::optional<mlir::Type> pendingLambdaExpectedType;

  struct CapturedVarInfo {
    std::string name;
    mlir::Value value;
    bool isMutable = false;
    mlir::Type valueType;
  };
  void gatherCapturedVars(const std::set<std::string> &freeVars,
                          std::vector<CapturedVarInfo> &capturedVars, mlir::Location location);
  /// Generate a per-closure env drop function that calls hew_rc_drop on each
  /// captured mutable RC cell.  Returns a FuncPtrOp value (or null zero if
  /// no mutable captures exist).
  mlir::Value generateEnvDropFn(
      const std::vector<CapturedVarInfo> &capturedVars,
      mlir::LLVM::LLVMStructType envStructType, mlir::Location location);
  unsigned envDropCounter = 0;

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
  using DropValueSet = llvm::DenseSet<mlir::Value>;
  struct DropEntry {
    std::string varName;
    std::string dropFuncName;
    bool isUserDrop = false;
    /// For stream/sink RAII: alloca storing the handle pointer.
    /// emitDropEntry will null-check before calling close, then null out
    /// the alloca so explicit .close() + scope-exit don't double-free.
    mlir::Value closeAlloca;
    /// Direct reference to the variable's promoted alloca slot.  When set,
    /// emitDropEntry loads from this instead of lookupVariable, which may
    /// fail after the declaring scope (e.g. match arm) has been popped.
    mlir::Value promotedSlot;
    /// Stable identity for the binding this drop entry owns. Uses the
    /// promoted slot when one exists, otherwise the original SSA value.
    mlir::Value bindingIdentity;
  };
  std::vector<std::vector<DropEntry>> dropScopes;
  std::unordered_map<std::string, std::string> userDropFuncs;
  /// Drop functions for stdlib handle types, populated from msgpack metadata.
  ///
  /// Maps qualified type name (e.g. `"http.Request"`) to C drop function
  /// (e.g. `"hew_http_request_free"`).  Replaces the hardcoded handle-kind
  /// table in `dropFuncForMLIRType` for types that have `impl Drop` in stdlib.
  std::unordered_map<std::string, std::string> stdlibDropFuncs;
  /// Defining-module-mangled user Drop symbols already processed. Prevents
  /// flattened root-module clones from re-generating imported Drop impls.
  std::unordered_set<std::string> generatedUserDropImpls;
  /// Mangled function/method symbols whose String result is just a borrowed
  /// alias of an owned field on a caller-owned struct parameter/receiver.
  std::unordered_set<std::string> borrowedFieldReturnCallees;
  // (name, scope-depth) pairs to exclude from drops.  The depth is relative
  // to funcLevelDropScopeBase so that a shadowed binding in an inner scope
  // is NOT confused with the same-named return variable at depth 0.
  std::set<std::pair<std::string, size_t>> funcLevelDropExcludeVars;
  // Flat set of variable names appearing anywhere in the return expression,
  // ignoring depth.  Used for RAII close exclusion where the depth-based
  // tracking doesn't work (variables declared in an unsafe block at depth 1
  // can appear in if/match arms at depth 2+).
  std::set<std::string> funcLevelReturnVarNames;
  // Stable binding identities resolved from funcLevelDropExcludeVars as
  // declarations are lowered. Used to exclude the exact returned binding
  // even when the same variable name is shadowed elsewhere.
  DropValueSet funcLevelDropExcludeValues;
  // Names whose function-level return exclusions have been resolved to a
  // stable binding identity.
  std::set<std::string> funcLevelDropExcludeResolvedNames;
  // Variables referenced ONLY by explicit return statements (not trailing
  // expressions). Used for path-specific drops when returnSlotIsLazy.
  std::set<std::string> funcLevelEarlyReturnVarNames;
  // Stable binding identities returned by explicit return statements.
  DropValueSet funcLevelEarlyReturnExcludeValues;
  // Names whose explicit-return exclusions have been resolved to a stable
  // binding identity.
  std::set<std::string> funcLevelEarlyReturnExcludeResolvedNames;
  // dropScopes.size() at the point where the current function body starts.
  size_t funcLevelDropScopeBase = 0;
  /// Pending parameter drops: populated before generateBlock, drained at
  /// the start of the function-level drop scope.  Each entry is
  /// (paramName, dropFuncName, isUserDrop).
  struct PendingParamDrop {
    std::string name;
    std::string dropFunc;
    bool isUserDrop = false;
  };
  std::vector<PendingParamDrop> pendingFunctionParamDrops;
  /// Determine the drop function for a type annotation, or "" if none needed.
  std::string dropFuncForType(const ast::TypeExpr &ty) const;
  /// Infer the drop function for an MLIR-typed value (used for match pattern
  /// bindings where only the MLIR type is available, not the AST type).
  /// When \p includeStructTypes is false the LLVMStructType branch is skipped,
  /// keeping receive-path registration limited to the original narrow set
  /// (String, Vec, HashMap, Closure, Handle).  Callers that own a full type
  /// context (field-drop codegen, match bindings) pass the default \c true.
  std::string dropFuncForMLIRType(mlir::Type type,
                                   bool includeStructTypes = true) const;
  /// Returns true if the named struct has at least one owned field (String,
  /// Vec, HashMap, etc.) that requires drop at scope exit.
  bool structHasOwnedFields(const std::string &name) const;
  void pushDropScope();
  void popDropScope();
  mlir::Value resolveCurrentBindingIdentity(llvm::StringRef name);
  void maybeRecordFunctionDropExclusion(const std::string &varName, mlir::Value bindingIdentity);
  void resolveFunctionDropExclusionCandidates();
  bool isFunctionDropExcluded(const DropEntry &entry, const DropValueSet &excludeValues) const;
  void collectVisibleBindingIdentities(const ast::Expr &expr, DropValueSet &out,
                                       std::set<std::string> *resolvedNames = nullptr);
  /// Emit drops for a scope, excluding variables that match the
  /// resolved function-level exclusion identities (with a legacy name-based
  /// fallback for entries that predate identity capture).
  void emitDropsWithExclusion(const std::vector<DropEntry> &scope, size_t relDepth);
  void registerDroppable(const std::string &varName, const std::string &dropFunc,
                         bool isUserDrop = false);
  /// Remove a variable from all drop scopes (ownership transferred, e.g. actor send).
  void unregisterDroppable(const std::string &varName);
  /// Emit the DropOp for a single DropEntry (lookup, closure-env extract,
  /// bitcast, drop).  No-op if the variable is not found.
  void emitDropEntry(const DropEntry &entry);
  /// After calling a user-defined Drop function, emit drops for each owned
  /// field of the struct (String, Vec, HashMap, etc.).  The user's Drop body
  /// runs first (can read field values), then we free the heap allocations.
  void emitFieldDropsForUserStruct(mlir::Value structVal, mlir::Location loc);
  /// Rebuild a struct value with all transferred handle fields nulled.
  mlir::Value nullTransferredHandlesInStructValue(mlir::Value structVal, mlir::Location loc);
  /// Store a handle-zeroed version of a retained struct back into its drop slot.
  void nullOutTransferredHandleFields(const std::string &varName, mlir::Location loc);
  /// Null out a variable's RAII close alloca (after ownership transfer).
  void nullOutRaiiAlloca(const std::string &varName);
  /// Emit a drop for a single variable if it has a registered drop function.
  void emitDropForVariable(const std::string &varName);
  void emitDropsForScope(const std::vector<DropEntry> &scope);
  void emitDropsForCurrentScope();
  void emitAllDrops();
  void emitDropsExcept(const std::string &excludeVar);
  void emitDropsExcept(const std::set<std::string> &excludeVars);
  void emitDropsExcept(const DropValueSet &excludeValues);

  void emitStringDrop(mlir::Value v);
  /// Returns true if `v` is a temporary string (heap-allocated, not from
  /// a variable load or a constant).  Safe to drop after consumption.
  bool isTemporaryString(mlir::Value v);
  void maybeRegisterBorrowedFieldReturn(const ast::FnDecl &fn, llvm::StringRef symbolName);

  // ── Temporary materialization ─────────────────────────────────────
  /// Counter for generating unique __tmp_N implicit let-binding names.
  unsigned tempMaterializationCounter = 0;
  struct DropInfo {
    std::string dropFunc;
    bool isUserDrop = false;
  };
  /// Detect whether an expression result is a heap-allocated temporary and
  /// return the appropriate drop function + user-drop flag.
  /// Returns empty dropFunc if the value is not a droppable temporary.
  DropInfo inferDropFuncForTemporary(mlir::Value val,
                                     const ast::Expr &astExpr) const;
  /// If `val` is a heap-allocated temporary (not already bound to a variable),
  /// create an implicit `__tmp_N` let-binding and register it for scope-exit
  /// drop.  Returns true if a binding was created.
  bool materializeTemporary(mlir::Value val, const ast::Expr &astExpr);

  // ── Error tracking ────────────────────────────────────────────────
  unsigned errorCount_ = 0;

  // ── Current source location (set by dispatch functions) ──────────
  mlir::Location currentLoc;

  // ── Current function tracking ────────────────────────────────────
  mlir::func::FuncOp currentFunction;
  const ast::TypeExpr *currentFunctionReturnTypeExpr = nullptr;

  // ── Loop control (for break/continue) ──────────────────────────
  // Stack of memref<i1> values: when set to false, the loop terminates.
  llvm::SmallVector<mlir::Value, 4> loopActiveStack;
  // dropScopes.size() at the time each loop was entered (for labeled break/continue).
  llvm::SmallVector<size_t, 4> loopDropScopeBase;
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
  mlir::Value returnSlot; // memref<LLVM storage of ReturnType>, nullptr when not active
  // True when returnSlot was lazily created by ensureReturnSlot (aggregate types).
  // Deferred drops only apply to lazily-created slots.
  bool returnSlotIsLazy = false;
  // Flag set ONLY by generateReturnStmt (not by trailing expressions).
  // Used by popDropScope to distinguish early return from normal flow.
  mlir::Value earlyReturnFlag; // memref<i1>, nullptr when not active

  // ── Channel recv int out-valid alloca ────────────────────────
  // Hoisted to function entry block and reused across all
  // recv_int / try_recv_int call sites to avoid stack growth in loops.
  mlir::Value channelIntOutValidAlloca; // LLVM ptr to i32, nullptr when not active

  /// RAII guard that saves/restores function generation state.
  /// On construction: saves currentFunction, returnFlag, returnSlot,
  /// currentFunctionReturnTypeExpr, channelIntOutValidAlloca; sets
  /// currentFunction to newFunc and resets the rest to nullptr.
  /// On destruction: restores all saved values.
  struct FunctionGenerationScope {
    MLIRGen &gen;
    mlir::func::FuncOp prevFunction;
    const ast::TypeExpr *prevFunctionReturnTypeExpr;
    mlir::Value prevReturnFlag;
    mlir::Value prevReturnSlot;
    bool prevReturnSlotIsLazy;
    mlir::Value prevEarlyReturnFlag;
    mlir::Value prevChannelIntOutValidAlloca;

    FunctionGenerationScope(MLIRGen &g, mlir::func::FuncOp newFunc)
        : gen(g), prevFunction(g.currentFunction),
          prevFunctionReturnTypeExpr(g.currentFunctionReturnTypeExpr), prevReturnFlag(g.returnFlag),
          prevReturnSlot(g.returnSlot), prevReturnSlotIsLazy(g.returnSlotIsLazy),
          prevEarlyReturnFlag(g.earlyReturnFlag),
          prevChannelIntOutValidAlloca(g.channelIntOutValidAlloca) {
      gen.currentFunction = newFunc;
      gen.currentFunctionReturnTypeExpr = nullptr;
      gen.returnFlag = nullptr;
      gen.returnSlot = nullptr;
      gen.returnSlotIsLazy = false;
      gen.earlyReturnFlag = nullptr;
      gen.channelIntOutValidAlloca = nullptr;
    }

    ~FunctionGenerationScope() {
      gen.currentFunction = prevFunction;
      gen.currentFunctionReturnTypeExpr = prevFunctionReturnTypeExpr;
      gen.returnFlag = prevReturnFlag;
      gen.returnSlot = prevReturnSlot;
      gen.returnSlotIsLazy = prevReturnSlotIsLazy;
      gen.earlyReturnFlag = prevEarlyReturnFlag;
      gen.channelIntOutValidAlloca = prevChannelIntOutValidAlloca;
    }

    FunctionGenerationScope(const FunctionGenerationScope &) = delete;
    FunctionGenerationScope &operator=(const FunctionGenerationScope &) = delete;
  };

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

  enum class TraitImplBodyNameMode {
    QualifyOnBaseCollision,
    PreferQualifiedIfGenerated,
  };

  /// Choose between the base mangled impl-body name and a trait-qualified
  /// fallback, preserving the existing call-site-specific lookup behavior.
  std::string resolveTraitImplBodyName(const std::string &typeName,
                                       const std::string &traitName,
                                       const std::string &methodName,
                                       TraitImplBodyNameMode mode);

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
  // Registry of let-bound generic lambda expressions keyed by binding name.
  // Populated when a let statement binds an ExprLambda with non-empty
  // type_params.  The lambda is specialized lazily when a call site with
  // concrete type_args is encountered.
  std::unordered_map<std::string, const ast::ExprLambda *> genericLambdas;
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
  // Specialize a let-bound generic lambda for the given concrete type args and
  // return a concrete FuncOp that can be called directly (no env pointer).
  mlir::func::FuncOp specializeGenericLambda(const std::string &varName,
                                             const std::vector<std::string> &typeArgs);
  // Resolve a TypeExpr to a flat mangled name for generic substitutions.
  // Recursively handles nested generics: Pair<int> → "Pair_int".
  // Also triggers convertType to ensure nested generic structs are specialized.
  std::string resolveTypeArgMangledName(const ast::TypeExpr &type);

  // ── Generic impl monomorphization ─────────────────────────────
  /// Info for a deferred generic impl block (e.g. `impl<T> Box<T> { … }`).
  struct GenericImplInfo {
    const std::vector<ast::TypeParam> *typeParams; // points into AST
    std::vector<const ast::FnDecl *> methods;
  };
  // Keyed by base type name (e.g. "Box").
  std::unordered_map<std::string, GenericImplInfo> genericImplMethods;
  // Map from monomorphized struct name → (base name, type arg names).
  // Populated during generic struct specialization in convertType.
  std::unordered_map<std::string,
                     std::pair<std::string, std::vector<std::string>>>
      structTypeOrigin;
  // Specialize a generic impl method for the given concrete type args.
  mlir::func::FuncOp specializeGenericImplMethod(
      const std::string &baseTypeName,
      const std::vector<std::string> &typeArgs,
      const std::string &methodName);
};

} // namespace hew

#endif // HEW_MLIR_MLIRGEN_H
