//===- MLIRGenStmt.cpp - Statement codegen for Hew MLIRGen ----------------===//
//
// Statement generation methods: let, var, assign, if, while, for, return,
// expression statements, block generation, return guards, loop/break/continue.
//
//===----------------------------------------------------------------------===//

#include "hew/ast_helpers.h"
#include "hew/mlir/HewDialect.h"
#include "hew/mlir/HewOps.h"
#include "hew/mlir/HewTypes.h"
#include "hew/mlir/MLIRGen.h"
#include "MLIRGenHelpers.h"

#include "mlir/Dialect/Arith/IR/Arith.h"
#include "mlir/Dialect/ControlFlow/IR/ControlFlowOps.h"
#include "mlir/Dialect/Func/IR/FuncOps.h"
#include "mlir/Dialect/LLVMIR/LLVMDialect.h"
#include "mlir/Dialect/MemRef/IR/MemRef.h"
#include "mlir/Dialect/SCF/IR/SCF.h"
#include "mlir/IR/Builders.h"
#include "mlir/IR/BuiltinAttributes.h"
#include "mlir/IR/BuiltinOps.h"
#include "mlir/IR/BuiltinTypes.h"
#include "mlir/IR/Diagnostics.h"
#include "mlir/IR/Location.h"
#include "mlir/IR/Value.h"
#include "mlir/IR/Verifier.h"

#include "llvm/ADT/ScopedHashTable.h"
#include "llvm/ADT/StringRef.h"
#include "llvm/Support/ErrorHandling.h"
#include "llvm/Support/raw_ostream.h"

#include <cassert>
#include <cstdlib>
#include <set>
#include <string>

using namespace hew;
using namespace mlir;

// ============================================================================
// Block generation
// ============================================================================

// ============================================================================
// Return-guarded statement generation
// ============================================================================

void MLIRGen::generateStmtsWithReturnGuards(
    const std::vector<std::unique_ptr<ast::Spanned<ast::Stmt>>> &stmts, size_t startIdx,
    size_t endIdx, const ast::Expr *trailingExpr, mlir::Location location) {

  for (size_t i = startIdx; i < endIdx; ++i) {
    generateStatement(stmts[i]->value);

    if (hasRealTerminator(builder.getInsertionBlock())) {
      return;
    }

    // If this statement might contain a return, guard subsequent statements.
    if (stmtMightContainReturn(stmts[i]->value)) {
      auto flagVal = builder.create<mlir::memref::LoadOp>(location, returnFlag, mlir::ValueRange{});
      auto trueConst = createIntConstant(builder, location, builder.getI1Type(), 1);
      auto notReturned = builder.create<mlir::arith::XOrIOp>(location, flagVal, trueConst);
      auto guard = builder.create<mlir::scf::IfOp>(location, mlir::TypeRange{}, notReturned,
                                                   /*withElseRegion=*/false);
      builder.setInsertionPointToStart(&guard.getThenRegion().front());

      // Recursively generate remaining statements inside the guard.
      generateStmtsWithReturnGuards(stmts, i + 1, endIdx, trailingExpr, location);

      auto *blk = builder.getInsertionBlock();
      if (blk->empty() || !blk->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
        builder.create<mlir::scf::YieldOp>(location);
      }
      builder.setInsertionPointAfter(guard);
      return;
    }
  }

  // After all guarded statements, generate the trailing expression if any.
  if (trailingExpr) {
    mlir::Value val = generateExpression(*trailingExpr);
    if (val && returnSlot) {
      auto slotType = mlir::cast<mlir::MemRefType>(returnSlot.getType()).getElementType();
      val = coerceType(val, slotType, location);
      builder.create<mlir::memref::StoreOp>(location, val, returnSlot);
      auto trueConst = createIntConstant(builder, location, builder.getI1Type(), 1);
      builder.create<mlir::memref::StoreOp>(location, trueConst, returnFlag);
    }
  }
}

// ============================================================================
// Loop body with continue guards
// ============================================================================

void MLIRGen::generateLoopBodyWithContinueGuards(
    const std::vector<std::unique_ptr<ast::Spanned<ast::Stmt>>> &stmts, size_t startIdx,
    size_t endIdx, mlir::Value contFlag, mlir::Location location) {

  for (size_t i = startIdx; i < endIdx; ++i) {
    generateStatement(stmts[i]->value);

    if (hasRealTerminator(builder.getInsertionBlock())) {
      return;
    }

    // If this statement might contain a break or continue, guard remaining.
    if (stmtMightContainBreakOrContinue(stmts[i]->value)) {
      auto flagVal = builder.create<mlir::memref::LoadOp>(location, contFlag, mlir::ValueRange{});
      auto trueConst = createIntConstant(builder, location, builder.getI1Type(), 1);
      auto notContinued = builder.create<mlir::arith::XOrIOp>(location, flagVal, trueConst);
      auto guard = builder.create<mlir::scf::IfOp>(location, mlir::TypeRange{}, notContinued,
                                                   /*withElseRegion=*/false);
      builder.setInsertionPointToStart(&guard.getThenRegion().front());

      generateLoopBodyWithContinueGuards(stmts, i + 1, endIdx, contFlag, location);

      auto *blk = builder.getInsertionBlock();
      if (blk->empty() || !blk->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
        builder.create<mlir::scf::YieldOp>(location);
      }
      builder.setInsertionPointAfter(guard);
      return;
    }
  }
}

// ============================================================================
// Block generation
// ============================================================================

mlir::Value MLIRGen::generateBlock(const ast::Block &block) {
  // Create a new scope for variables in this block
  SymbolTableScopeT varScope(symbolTable);
  MutableTableScopeT mutScope(mutableVars);

  // RAII drop scope: emit drops when block scope exits
  struct DropScopeGuard {
    MLIRGen &gen;
    DropScopeGuard(MLIRGen &g) : gen(g) { gen.pushDropScope(); }
    ~DropScopeGuard() { gen.popDropScope(); }
  } dropGuard(*this);

  // Determine if this block is the direct function body (for return guard logic).
  // Only use the returnFlag guarded path at the function body level when a
  // returnSlot exists (non-void functions), not in nested blocks or void functions.
  bool useReturnGuards = false;
  if (returnFlag && returnSlot) {
    auto *parentOp = builder.getInsertionBlock()->getParentOp();
    useReturnGuards = mlir::isa<mlir::func::FuncOp>(parentOp);
  }

  const auto &stmts = block.stmts;

  // Generate trailing expression if present (parser may store it in expression)
  if (block.trailing_expr) {
    if (useReturnGuards) {
      auto location = builder.getUnknownLoc();
      generateStmtsWithReturnGuards(stmts, 0, stmts.size(), &block.trailing_expr->value, location);
      return nullptr; // Value is in returnSlot
    }

    for (const auto &stmtPtr : stmts) {
      generateStatement(stmtPtr->value);
      if (hasRealTerminator(builder.getInsertionBlock())) {
        return nullptr;
      }
    }
    return generateExpression(block.trailing_expr->value);
  }

  // The parser often places the trailing expression (last expression before })
  // as the last ExprStmt in block.statements, with block.expression == nullptr.
  // Similarly, an IfStmt at the end of a block may be the block's value
  // (e.g., `fn foo(x: i32) -> i32 { if x > 0 { 1 } else { 0 } }`).
  //
  // We detect the last statement and try to generate it as a value.
  if (!stmts.empty()) {
    // When at function body level with returnFlag, use guarded generation
    if (useReturnGuards) {
      auto location = builder.getUnknownLoc();

      // Check if the last statement can be a trailing expression
      const auto &lastStmt = stmts.back()->value;
      const ast::Expr *trailingExpr = nullptr;
      size_t stmtCount = stmts.size();

      if (std::holds_alternative<ast::StmtExpression>(lastStmt.kind)) {
        auto *exprStmt = std::get_if<ast::StmtExpression>(&lastStmt.kind);
        trailingExpr = &exprStmt->expr.value;
        stmtCount--; // Exclude the last ExprStmt; it's the trailing expr
      }

      if (trailingExpr) {
        // Generate stmts[0..stmtCount) with guards, then trailing expr
        generateStmtsWithReturnGuards(stmts, 0, stmtCount, trailingExpr, location);
        return nullptr; // Value in returnSlot
      }

      // Handle last statement as value-producing (IfStmt or MatchStmt)
      if (auto *ifNode = std::get_if<ast::StmtIf>(&lastStmt.kind)) {
        // Generate preceding stmts with guards
        if (stmtCount > 1) {
          generateStmtsWithReturnGuards(stmts, 0, stmtCount - 1, nullptr, location);
        }
        // Guard the value-producing if-statement
        auto flagVal =
            builder.create<mlir::memref::LoadOp>(location, returnFlag, mlir::ValueRange{});
        auto trueConst = createIntConstant(builder, location, builder.getI1Type(), 1);
        auto notReturned = builder.create<mlir::arith::XOrIOp>(location, flagVal, trueConst);
        auto guard = builder.create<mlir::scf::IfOp>(location, mlir::TypeRange{}, notReturned,
                                                     /*withElseRegion=*/false);
        builder.setInsertionPointToStart(&guard.getThenRegion().front());
        auto val = generateIfStmtAsExpr(*ifNode);
        if (val && returnSlot) {
          auto slotType = mlir::cast<mlir::MemRefType>(returnSlot.getType()).getElementType();
          val = coerceType(val, slotType, location);
          builder.create<mlir::memref::StoreOp>(location, val, returnSlot);
          builder.create<mlir::memref::StoreOp>(location, trueConst, returnFlag);
        }
        auto *blk = builder.getInsertionBlock();
        if (blk->empty() || !blk->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
          builder.create<mlir::scf::YieldOp>(location);
        }
        builder.setInsertionPointAfter(guard);
        return nullptr;
      }

      if (auto *matchNode = std::get_if<ast::StmtMatch>(&lastStmt.kind)) {
        if (stmtCount > 1) {
          generateStmtsWithReturnGuards(stmts, 0, stmtCount - 1, nullptr, location);
        }
        auto flagVal =
            builder.create<mlir::memref::LoadOp>(location, returnFlag, mlir::ValueRange{});
        auto trueConst = createIntConstant(builder, location, builder.getI1Type(), 1);
        auto notReturned = builder.create<mlir::arith::XOrIOp>(location, flagVal, trueConst);
        auto guard = builder.create<mlir::scf::IfOp>(location, mlir::TypeRange{}, notReturned,
                                                     /*withElseRegion=*/false);
        builder.setInsertionPointToStart(&guard.getThenRegion().front());
        auto scrutinee = generateExpression(matchNode->scrutinee.value);
        mlir::Type resultType;
        if (currentFunction && currentFunction.getResultTypes().size() == 1) {
          resultType = currentFunction.getResultTypes()[0];
        } else {
          resultType = builder.getI32Type();
        }
        auto val = scrutinee ? generateMatchImpl(scrutinee, matchNode->arms, resultType, location)
                             : nullptr;
        if (val && returnSlot) {
          auto slotType = mlir::cast<mlir::MemRefType>(returnSlot.getType()).getElementType();
          val = coerceType(val, slotType, location);
          builder.create<mlir::memref::StoreOp>(location, val, returnSlot);
          builder.create<mlir::memref::StoreOp>(location, trueConst, returnFlag);
        }
        auto *blk = builder.getInsertionBlock();
        if (blk->empty() || !blk->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
          builder.create<mlir::scf::YieldOp>(location);
        }
        builder.setInsertionPointAfter(guard);
        return nullptr;
      }

      // No trailing expression: generate all statements with guards
      generateStmtsWithReturnGuards(stmts, 0, stmts.size(), nullptr, location);
      return nullptr;
    }

    // Generate all statements except the last one
    for (size_t i = 0; i + 1 < stmts.size(); ++i) {
      generateStatement(stmts[i]->value);
      if (hasRealTerminator(builder.getInsertionBlock())) {
        return nullptr;
      }
    }

    // Handle the last statement specially
    const auto &lastStmt = stmts.back()->value;

    // Case 1: Expression statement -- treat as trailing expression
    if (std::holds_alternative<ast::StmtExpression>(lastStmt.kind)) {
      auto *exprStmt = std::get_if<ast::StmtExpression>(&lastStmt.kind);
      if (exprStmt) {
        return generateExpression(exprStmt->expr.value);
      }
    }

    // Case 2: If statement as a value-producing block ending
    // When a block ends with `if ... { ... } else { ... }` and the enclosing
    // function returns a value, generate it as an if-expression.
    if (auto *ifStmt = std::get_if<ast::StmtIf>(&lastStmt.kind)) {
      return generateIfStmtAsExpr(*ifStmt);
    }

    // Case 3: Match statement as a value-producing block ending
    if (auto *matchNode = std::get_if<ast::StmtMatch>(&lastStmt.kind)) {
      auto location = loc(lastStmt.span);
      auto scrutinee = generateExpression(matchNode->scrutinee.value);
      if (!scrutinee)
        return nullptr;
      mlir::Type resultType;
      if (currentFunction && currentFunction.getResultTypes().size() == 1) {
        resultType = currentFunction.getResultTypes()[0];
      } else {
        resultType = builder.getI32Type();
      }
      return generateMatchImpl(scrutinee, matchNode->arms, resultType, location);
    }

    // Case 4: Loop/While/For as a value-producing block ending (via break-with-value)
    if (std::holds_alternative<ast::StmtLoop>(lastStmt.kind) ||
        std::holds_alternative<ast::StmtWhile>(lastStmt.kind) ||
        std::holds_alternative<ast::StmtFor>(lastStmt.kind)) {
      lastBreakValue = nullptr;
      generateStatement(lastStmt);
      if (lastBreakValue)
        return lastBreakValue;
      return nullptr;
    }

    // Otherwise, generate it as a normal statement
    generateStatement(lastStmt);
  }

  return nullptr;
}

// ============================================================================
// Statement generation
// ============================================================================

void MLIRGen::generateStatement(const ast::Stmt &stmt) {
  currentLoc = loc(stmt.span);
  if (auto *s = std::get_if<ast::StmtLet>(&stmt.kind)) {
    generateLetStmt(*s);
    return;
  }
  if (auto *s = std::get_if<ast::StmtVar>(&stmt.kind)) {
    generateVarStmt(*s);
    return;
  }
  if (auto *s = std::get_if<ast::StmtAssign>(&stmt.kind)) {
    generateAssignStmt(*s);
    return;
  }
  if (auto *s = std::get_if<ast::StmtIf>(&stmt.kind)) {
    generateIfStmt(*s);
    return;
  }
  if (auto *s = std::get_if<ast::StmtWhile>(&stmt.kind)) {
    generateWhileStmt(*s);
    return;
  }
  if (auto *s = std::get_if<ast::StmtFor>(&stmt.kind)) {
    if (s->is_await)
      generateForAwaitStmt(*s);
    else
      generateForStmt(*s);
    return;
  }
  if (auto *s = std::get_if<ast::StmtReturn>(&stmt.kind)) {
    generateReturnStmt(*s);
    return;
  }
  if (auto *s = std::get_if<ast::StmtExpression>(&stmt.kind)) {
    generateExprStmt(*s);
    return;
  }
  if (auto *s = std::get_if<ast::StmtLoop>(&stmt.kind)) {
    generateLoopStmt(*s);
    return;
  }
  if (auto *s = std::get_if<ast::StmtMatch>(&stmt.kind)) {
    generateMatchStmt(*s);
    return;
  }
  if (auto *s = std::get_if<ast::StmtBreak>(&stmt.kind)) {
    generateBreakStmt(*s);
    return;
  }
  if (auto *s = std::get_if<ast::StmtContinue>(&stmt.kind)) {
    generateContinueStmt(*s);
    return;
  }
  if (auto *s = std::get_if<ast::StmtDefer>(&stmt.kind)) {
    if (s->expr) {
      currentFnDefers.emplace_back(&s->expr->value, currentLoc);
    }
    return;
  }
}

void MLIRGen::generateLetStmt(const ast::StmtLet &stmt) {
  auto location = currentLoc;
  // Set the declared type so constructors (Vec::new, HashMap::new, None, Ok,
  // Err) can emit correctly typed results.
  if (stmt.ty)
    pendingDeclaredType = convertType(stmt.ty->value);

  mlir::Value value = nullptr;
  lastScopeLaunchResultType.reset();
  if (stmt.value) {
    value = generateExpression(stmt.value->value);
  }
  pendingDeclaredType.reset();
  if (!value)
    return;

  // Type coercion: if declared type doesn't match value type, try to coerce
  if (stmt.ty) {
    auto declaredType = convertType(stmt.ty->value);
    value = coerceType(value, declaredType, location);
  }

  // Extract the name from the pattern
  const auto &pattern = stmt.pattern.value;
  if (auto *identPat = std::get_if<ast::PatIdentifier>(&pattern.kind)) {
    auto varName = identPat->name;
    declareVariable(varName, value);

    // Track actor variable types for method call dispatch
    // Normal spawns: extract from type annotation (ActorRef<MyActor>)
    if (stmt.ty) {
      auto actorName = typeExprToActorName(stmt.ty->value);
      if (!actorName.empty() && actorRegistry.count(actorName))
        actorVarTypes[varName] = actorName;
    }
    // Infer actor/supervisor type from spawn expression (no type annotation needed)
    if (stmt.value) {
      if (auto *spawn = std::get_if<ast::ExprSpawn>(&stmt.value->value.kind)) {
        if (auto *ident = std::get_if<ast::ExprIdentifier>(&spawn->target->value.kind)) {
          if (actorRegistry.count(ident->name) || supervisorChildren.count(ident->name))
            actorVarTypes[varName] = ident->name;
        }
      }
    }
    // Lambda actors use generated names not in the type annotation
    if (stmt.value && std::holds_alternative<ast::ExprSpawnLambdaActor>(stmt.value->value.kind)) {
      if (lambdaActorCounter > 0)
        actorVarTypes[varName] = "__lambda_actor_" + std::to_string(lambdaActorCounter - 1);
    }

    // Track generator variables: let g = gen_func()
    if (stmt.value) {
      if (auto *callExpr = std::get_if<ast::ExprCall>(&stmt.value->value.kind)) {
        if (callExpr->function) {
          if (auto *fnIdent = std::get_if<ast::ExprIdentifier>(&callExpr->function->value.kind)) {
            auto calleeName = fnIdent->name;
            if (generatorFunctions.count(calleeName)) {
              generatorVarTypes[varName] = calleeName;
            }
            // Track supervisor_child() calls
            if (calleeName == "supervisor_child" && callExpr->args.size() >= 2) {
              if (auto *supIdent = std::get_if<ast::ExprIdentifier>(
                      &ast::callArgExpr(callExpr->args[0]).value.kind)) {
                auto supIt = actorVarTypes.find(supIdent->name);
                if (supIt != actorVarTypes.end()) {
                  auto childrenIt = supervisorChildren.find(supIt->second);
                  if (childrenIt != supervisorChildren.end()) {
                    if (auto *litExpr = std::get_if<ast::ExprLiteral>(
                            &ast::callArgExpr(callExpr->args[1]).value.kind)) {
                      if (auto *intLit = std::get_if<ast::LitInteger>(&litExpr->lit)) {
                        auto idx = intLit->value;
                        if (idx >= 0 && static_cast<size_t>(idx) < childrenIt->second.size())
                          actorVarTypes[varName] = childrenIt->second[static_cast<size_t>(idx)];
                      }
                    }
                  }
                }
              }
            }
          }
        }
      }
    }

    // Track scope.launch task result types for await
    if (stmt.value && std::holds_alternative<ast::ExprScopeLaunch>(stmt.value->value.kind) &&
        lastScopeLaunchResultType.has_value()) {
      taskResultTypes[varName] = *lastScopeLaunchResultType;
      lastScopeLaunchResultType.reset();
    }

    // Track handle variables from type annotation (filled by enrich_program)
    if (stmt.ty) {
      auto handleStr = typeExprToHandleString(stmt.ty->value);
      if (!handleStr.empty())
        handleVarTypes[varName] = handleStr;
    }

    // ── Track first-class Stream<T> / Sink<T> variables ─────────────────
    if (stmt.ty) {
      auto streamStr = typeExprStreamKind(stmt.ty->value);
      if (!streamStr.empty())
        streamHandleVarTypes[varName] = streamStr;
    }
    // Stream channel returns a Pair (not Stream or Sink)
    if (stmt.value) {
      if (auto *ce = std::get_if<ast::ExprCall>(&stmt.value->value.kind)) {
        if (ce->function) {
          if (auto *fi = std::get_if<ast::ExprIdentifier>(&ce->function->value.kind)) {
            if (fi->name == "hew_stream_channel")
              streamHandleVarTypes[varName] = "Pair";
            else if (fi->name == "hew_stream_from_file_read" || fi->name == "hew_stream_lines" ||
                     fi->name == "hew_stream_pair_stream" || fi->name == "hew_stream_chunks")
              streamHandleVarTypes[varName] = "Stream";
            else if (fi->name == "hew_stream_pair_sink" ||
                     fi->name == "hew_stream_from_file_write" ||
                     fi->name == "hew_http_respond_stream")
              streamHandleVarTypes[varName] = "Sink";
          }
        }
      }
    }

    // Track HashMap variable types from type annotation for erased-pointer fallback.
    if (stmt.ty) {
      auto resolveAlias = [this](const std::string &n) { return resolveTypeAlias(n); };
      auto collStr = typeExprToCollectionString(stmt.ty->value, resolveAlias);
      if (collStr.rfind("HashMap<", 0) == 0)
        collectionVarTypes[varName] = collStr;
    }

    bool isBorrowedGetString = false;
    if (value && mlir::isa<hew::StringRefType>(value.getType()) && value.getDefiningOp()) {
      auto *defOp = value.getDefiningOp();
      isBorrowedGetString = mlir::isa<hew::VecGetOp, hew::HashMapGetOp>(defOp);
    }

    // Register drop functions from type annotation
    if (stmt.ty) {
      if (auto *named = std::get_if<ast::TypeNamed>(&stmt.ty->value.kind)) {
        auto typeName = resolveTypeAlias(named->name);
        auto *defOp = (value && value.getDefiningOp()) ? value.getDefiningOp() : nullptr;
        bool isVecCtor = defOp && mlir::isa<hew::VecNewOp>(defOp);
        bool isHashMapCtor = defOp && mlir::isa<hew::HashMapNewOp>(defOp);
        if ((typeName == "Vec" || typeName == "bytes") && isVecCtor)
          registerDroppable(varName, "hew_vec_free");
        else if (typeName == "HashMap" && isHashMapCtor)
          registerDroppable(varName, "hew_hashmap_free_impl");
        else if ((typeName == "String" || typeName == "string" || typeName == "str") &&
                 !handleVarTypes.count(varName) && !streamHandleVarTypes.count(varName)) {
          // Don't register string drop for borrowed references from .get()
          bool isBorrowed = isBorrowedGetString;
          if (stmt.value) {
            if (auto *mc = std::get_if<ast::ExprMethodCall>(&stmt.value->value.kind))
              isBorrowed = (mc->method == "get");
          }
          if (!isBorrowed)
            registerDroppable(varName, "hew_string_drop");
        } else {
          auto dropIt = userDropFuncs.find(typeName);
          if (dropIt != userDropFuncs.end())
            registerDroppable(varName, dropIt->second, /*isUserDrop=*/true);
        }
      }
    }

    // Register string drops when VALUE is string-typed
    if (value && mlir::isa<hew::StringRefType>(value.getType())) {
      // Opaque handle variables (stream, sink, pair, http, regex, etc.) are
      // NOT strings — they have their own lifecycle (e.g. hew_stream_close,
      // hew_sink_close, hew_stream_pair_free).  Do NOT register them for
      // hew_string_drop.
      bool isHandle = handleVarTypes.count(varName) || streamHandleVarTypes.count(varName);

      bool alreadyRegistered = false;
      if (!dropScopes.empty()) {
        for (auto &e : dropScopes.back()) {
          if (e.varName == varName) {
            alreadyRegistered = true;
            break;
          }
        }
      }
      if (!alreadyRegistered && !isHandle) {
        bool isStringExpr = false;
        if (stmt.value) {
          const auto &vk = stmt.value->value.kind;
          isStringExpr = std::holds_alternative<ast::ExprInterpolatedString>(vk) ||
                         std::holds_alternative<ast::ExprCall>(vk);
          // Method calls that produce OWNED strings should be dropped.
          // But .get() on Vec/HashMap returns a borrowed reference — NOT owned.
          if (auto *mc = std::get_if<ast::ExprMethodCall>(&vk)) {
            if (mc->method != "get")
              isStringExpr = true;
          }
        }
        if (!isBorrowedGetString && isStringExpr)
          registerDroppable(varName, "hew_string_drop");
      }
    }

    // Register user-defined Drop from struct init
    if (stmt.value) {
      if (auto *si = std::get_if<ast::ExprStructInit>(&stmt.value->value.kind)) {
        bool hasTypedAnnotation =
            stmt.ty && std::holds_alternative<ast::TypeNamed>(stmt.ty->value.kind);
        if (!hasTypedAnnotation) {
          auto dropIt = userDropFuncs.find(si->name);
          if (dropIt != userDropFuncs.end())
            registerDroppable(varName, dropIt->second, /*isUserDrop=*/true);
        }
      }
    }

    // Register closure env for RAII cleanup via hew_rc_drop.
    if (mlir::isa<hew::ClosureType>(value.getType())) {
      registerDroppable(varName, "hew_rc_drop");
      if (stmt.value && std::holds_alternative<ast::ExprIdentifier>(stmt.value->value.kind)) {
        auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
        auto envPtr = builder.create<hew::ClosureGetEnvOp>(location, ptrType, value);
        builder.create<hew::RcCloneOp>(location, ptrType, envPtr);
      }
    }
    // Track dyn Trait variable types
    if (stmt.ty) {
      if (auto *traitObj = std::get_if<ast::TypeTraitObject>(&stmt.ty->value.kind)) {
        if (traitObj->bound)
          dynTraitVarTypes[varName] = traitObj->bound->name;
      }
    }

  } else if (auto *tuplePat = std::get_if<ast::PatTuple>(&pattern.kind)) {
    for (uint32_t i = 0; i < tuplePat->elements.size(); i++) {
      const auto &elem = tuplePat->elements[i];
      if (auto *ei = std::get_if<ast::PatIdentifier>(&elem->value.kind)) {
        mlir::Value elemVal;
        if (auto hewTuple = mlir::dyn_cast<hew::HewTupleType>(value.getType())) {
          elemVal = builder.create<hew::TupleExtractOp>(location, hewTuple.getElementTypes()[i],
                                                        value, static_cast<int64_t>(i));
        } else {
          elemVal = builder.create<mlir::LLVM::ExtractValueOp>(
              location, value, llvm::ArrayRef<int64_t>{static_cast<int64_t>(i)});
        }
        declareVariable(ei->name, elemVal);
      } else if (std::holds_alternative<ast::PatWildcard>(elem->value.kind)) {
        continue;
      }
    }
  } else {
    emitWarning(location) << "only simple identifier patterns supported for let in Phase 1";
  }
}

void MLIRGen::generateVarStmt(const ast::StmtVar &stmt) {
  auto location = currentLoc;
  auto varNameStr = stmt.name;
  // Set the declared type so constructors can emit correctly typed results.
  if (stmt.ty)
    pendingDeclaredType = convertType(stmt.ty->value);

  // Determine the type
  mlir::Type varType;
  mlir::Value initValue = nullptr;

  if (stmt.value) {
    initValue = generateExpression(stmt.value->value);
    if (!initValue)
      return;
    varType = initValue.getType();
  }

  if (stmt.ty) {
    varType = convertType(stmt.ty->value);
    if (initValue)
      initValue = coerceType(initValue, varType, location);
  }

  if (!varType) {
    emitError(location) << "cannot determine type for var declaration";
    return;
  }

  declareMutableVariable(varNameStr, varType, initValue);

  // Track handle variables from type annotation (filled by enrich_program)
  if (stmt.ty) {
    auto handleStr = typeExprToHandleString(stmt.ty->value);
    if (!handleStr.empty())
      handleVarTypes[varNameStr] = handleStr;
  }

  // ── Track first-class Stream<T> / Sink<T> for var statements ────────────
  if (stmt.ty) {
    auto streamStr = typeExprStreamKind(stmt.ty->value);
    if (!streamStr.empty())
      streamHandleVarTypes[varNameStr] = streamStr;
  }

  // Track HashMap variable types from type annotation for erased-pointer fallback.
  if (stmt.ty) {
    auto resolveAlias = [this](const std::string &n) { return resolveTypeAlias(n); };
    auto collStr = typeExprToCollectionString(stmt.ty->value, resolveAlias);
    if (collStr.rfind("HashMap<", 0) == 0)
      collectionVarTypes[varNameStr] = collStr;
  }

  // Register drop functions for collections and strings declared with var.
  if (stmt.ty) {
    if (auto *named = std::get_if<ast::TypeNamed>(&stmt.ty->value.kind)) {
      auto typeName = resolveTypeAlias(named->name);
      auto *defOp = (initValue && initValue.getDefiningOp()) ? initValue.getDefiningOp() : nullptr;
      bool isVecCtor = defOp && mlir::isa<hew::VecNewOp>(defOp);
      bool isHashMapCtor = defOp && mlir::isa<hew::HashMapNewOp>(defOp);
      if ((typeName == "Vec" || typeName == "bytes") && isVecCtor)
        registerDroppable(varNameStr, "hew_vec_free");
      else if (typeName == "HashMap" && isHashMapCtor)
        registerDroppable(varNameStr, "hew_hashmap_free_impl");
      else if ((typeName == "String" || typeName == "string" || typeName == "str") &&
               !handleVarTypes.count(varNameStr) && !streamHandleVarTypes.count(varNameStr))
        registerDroppable(varNameStr, "hew_string_drop");
    }
  }
}

void MLIRGen::generateAssignStmt(const ast::StmtAssign &stmt) {
  auto location = currentLoc;

  // Handle field assignment: self.field = value (pointer-based)
  if (auto *fa = std::get_if<ast::ExprFieldAccess>(&stmt.target.value.kind)) {
    auto operandVal = generateExpression(fa->object->value);
    if (!operandVal)
      return;

    mlir::Value rhs = generateExpression(stmt.value.value);
    if (!rhs)
      return;

    auto operandType = operandVal.getType();
    if (isPointerLikeType(operandType)) {
      auto fieldName = fa->field;
      for (const auto &[typeName, stInfo] : structTypes) {
        auto structType = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(stInfo.mlirType);
        if (!structType)
          continue;
        for (const auto &field : stInfo.fields) {
          if (field.name == fieldName) {
            auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
            auto fieldPtr = builder.create<mlir::LLVM::GEPOp>(
                location, ptrType, structType, operandVal,
                llvm::ArrayRef<mlir::LLVM::GEPArg>{0, static_cast<int32_t>(field.index)});
            // Handle compound assignment
            if (stmt.op) {
              auto current = builder.create<mlir::LLVM::LoadOp>(location, field.type, fieldPtr);
              bool isFloat = llvm::isa<mlir::FloatType>(field.type);
              bool isUnsigned = false;
              if (mlir::isa<mlir::IntegerType>(field.type))
                if (auto *ty = resolvedTypeOf(stmt.target.span))
                  isUnsigned = isUnsignedTypeExpr(*ty);
              mlir::Value result;
              switch (*stmt.op) {
              case ast::CompoundAssignOp::Add:
                result =
                    isFloat
                        ? (mlir::Value)builder.create<mlir::arith::AddFOp>(location, current, rhs)
                        : (mlir::Value)builder.create<mlir::arith::AddIOp>(location, current, rhs);
                break;
              case ast::CompoundAssignOp::Subtract:
                result =
                    isFloat
                        ? (mlir::Value)builder.create<mlir::arith::SubFOp>(location, current, rhs)
                        : (mlir::Value)builder.create<mlir::arith::SubIOp>(location, current, rhs);
                break;
              case ast::CompoundAssignOp::BitAnd:
                result = (mlir::Value)builder.create<mlir::arith::AndIOp>(location, current, rhs);
                break;
              case ast::CompoundAssignOp::BitOr:
                result = (mlir::Value)builder.create<mlir::arith::OrIOp>(location, current, rhs);
                break;
              case ast::CompoundAssignOp::BitXor:
                result = (mlir::Value)builder.create<mlir::arith::XOrIOp>(location, current, rhs);
                break;
              case ast::CompoundAssignOp::Shl:
                result = (mlir::Value)builder.create<mlir::arith::ShLIOp>(location, current, rhs);
                break;
              case ast::CompoundAssignOp::Shr:
                result =
                    isUnsigned
                        ? (mlir::Value)builder.create<mlir::arith::ShRUIOp>(location, current, rhs)
                        : (mlir::Value)builder.create<mlir::arith::ShRSIOp>(location, current, rhs);
                break;
              case ast::CompoundAssignOp::Multiply:
                result =
                    isFloat
                        ? (mlir::Value)builder.create<mlir::arith::MulFOp>(location, current, rhs)
                        : (mlir::Value)builder.create<mlir::arith::MulIOp>(location, current, rhs);
                break;
              case ast::CompoundAssignOp::Divide:
                result =
                    isFloat
                        ? (mlir::Value)builder.create<mlir::arith::DivFOp>(location, current, rhs)
                    : isUnsigned
                        ? (mlir::Value)builder.create<mlir::arith::DivUIOp>(location, current, rhs)
                        : (mlir::Value)builder.create<mlir::arith::DivSIOp>(location, current, rhs);
                break;
              case ast::CompoundAssignOp::Modulo:
                result =
                    isFloat
                        ? (mlir::Value)builder.create<mlir::arith::RemFOp>(location, current, rhs)
                    : isUnsigned
                        ? (mlir::Value)builder.create<mlir::arith::RemUIOp>(location, current, rhs)
                        : (mlir::Value)builder.create<mlir::arith::RemSIOp>(location, current, rhs);
                break;
              }
              rhs = result;
            }
            rhs = coerceType(rhs, field.type, location);
            builder.create<mlir::LLVM::StoreOp>(location, rhs, fieldPtr);
            return;
          }
        }
      }
      emitError(location) << "field '" << fieldName << "' not found for assignment";
      return;
    }
    // Value struct field assignment: load struct from mutable var, insertvalue, store back.
    auto *objIdent = std::get_if<ast::ExprIdentifier>(&fa->object->value.kind);
    if (!objIdent) {
      emitError(location) << "value struct field assignment requires a variable target";
      return;
    }
    auto varSlot = mutableVars.lookup(intern(objIdent->name));
    if (!varSlot) {
      emitError(location) << "cannot assign field on immutable variable '" << objIdent->name << "'";
      return;
    }

    auto fieldName = fa->field;
    auto structType = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(operandType);
    if (!structType || !structType.isIdentified()) {
      emitError(location) << "field assignment on non-struct value type";
      return;
    }
    auto stIt = structTypes.find(structType.getName().str());
    if (stIt == structTypes.end()) {
      emitError(location) << "unknown struct type '" << structType.getName() << "'";
      return;
    }
    const auto &stInfo = stIt->second;
    const StructFieldInfo *targetField = nullptr;
    for (const auto &field : stInfo.fields) {
      if (field.name == fieldName) {
        targetField = &field;
        break;
      }
    }
    if (!targetField) {
      emitError(location) << "field '" << fieldName << "' not found on struct '"
                          << structType.getName() << "'";
      return;
    }

    // Reload the struct from the mutable variable slot
    auto currentStruct = builder.create<mlir::memref::LoadOp>(location, varSlot);
    // Handle compound assignment
    if (stmt.op) {
      auto currentFieldVal = builder.create<mlir::LLVM::ExtractValueOp>(
          location, currentStruct,
          llvm::ArrayRef<int64_t>{static_cast<int64_t>(targetField->index)});
      bool isFloat = llvm::isa<mlir::FloatType>(targetField->type);
      bool isUnsigned = false;
      if (mlir::isa<mlir::IntegerType>(targetField->type))
        if (auto *ty = resolvedTypeOf(stmt.target.span))
          isUnsigned = isUnsignedTypeExpr(*ty);
      mlir::Value result;
      switch (*stmt.op) {
      case ast::CompoundAssignOp::Add:
        result =
            isFloat
                ? (mlir::Value)builder.create<mlir::arith::AddFOp>(location, currentFieldVal, rhs)
                : (mlir::Value)builder.create<mlir::arith::AddIOp>(location, currentFieldVal, rhs);
        break;
      case ast::CompoundAssignOp::Subtract:
        result =
            isFloat
                ? (mlir::Value)builder.create<mlir::arith::SubFOp>(location, currentFieldVal, rhs)
                : (mlir::Value)builder.create<mlir::arith::SubIOp>(location, currentFieldVal, rhs);
        break;
      case ast::CompoundAssignOp::Multiply:
        result =
            isFloat
                ? (mlir::Value)builder.create<mlir::arith::MulFOp>(location, currentFieldVal, rhs)
                : (mlir::Value)builder.create<mlir::arith::MulIOp>(location, currentFieldVal, rhs);
        break;
      case ast::CompoundAssignOp::Divide:
        result =
            isFloat
                ? (mlir::Value)builder.create<mlir::arith::DivFOp>(location, currentFieldVal, rhs)
            : isUnsigned
                ? (mlir::Value)builder.create<mlir::arith::DivUIOp>(location, currentFieldVal, rhs)
                : (mlir::Value)builder.create<mlir::arith::DivSIOp>(location, currentFieldVal, rhs);
        break;
      case ast::CompoundAssignOp::Modulo:
        result =
            isFloat
                ? (mlir::Value)builder.create<mlir::arith::RemFOp>(location, currentFieldVal, rhs)
            : isUnsigned
                ? (mlir::Value)builder.create<mlir::arith::RemUIOp>(location, currentFieldVal, rhs)
                : (mlir::Value)builder.create<mlir::arith::RemSIOp>(location, currentFieldVal, rhs);
        break;
      case ast::CompoundAssignOp::BitAnd:
        result = (mlir::Value)builder.create<mlir::arith::AndIOp>(location, currentFieldVal, rhs);
        break;
      case ast::CompoundAssignOp::BitOr:
        result = (mlir::Value)builder.create<mlir::arith::OrIOp>(location, currentFieldVal, rhs);
        break;
      case ast::CompoundAssignOp::BitXor:
        result = (mlir::Value)builder.create<mlir::arith::XOrIOp>(location, currentFieldVal, rhs);
        break;
      case ast::CompoundAssignOp::Shl:
        result = (mlir::Value)builder.create<mlir::arith::ShLIOp>(location, currentFieldVal, rhs);
        break;
      case ast::CompoundAssignOp::Shr:
        result =
            isUnsigned
                ? (mlir::Value)builder.create<mlir::arith::ShRUIOp>(location, currentFieldVal, rhs)
                : (mlir::Value)builder.create<mlir::arith::ShRSIOp>(location, currentFieldVal, rhs);
        break;
      }
      rhs = result;
    }
    rhs = coerceType(rhs, targetField->type, location);
    auto updated = builder.create<mlir::LLVM::InsertValueOp>(
        location, currentStruct, rhs,
        llvm::ArrayRef<int64_t>{static_cast<int64_t>(targetField->index)});
    builder.create<mlir::memref::StoreOp>(location, updated, varSlot);
    return;
  }

  // Handle indexed assignment: v[i] = x
  if (auto *idx = std::get_if<ast::ExprIndex>(&stmt.target.value.kind)) {
    auto collectionVal = generateExpression(idx->object->value);
    auto indexVal = generateExpression(idx->index->value);
    mlir::Value rhsVal = generateExpression(stmt.value.value);
    if (!collectionVal || !indexVal || !rhsVal)
      return;

    auto i64Type = builder.getI64Type();
    auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
    if (indexVal.getType() != i64Type)
      indexVal = builder.create<mlir::arith::ExtSIOp>(location, i64Type, indexVal);

    if (auto vecType = mlir::dyn_cast<hew::VecType>(collectionVal.getType())) {
      rhsVal = coerceType(rhsVal, vecType.getElementType(), location);
      builder.create<hew::VecSetOp>(location, collectionVal, indexVal, rhsVal);
      return;
    }

    if (auto hewArrayType = mlir::dyn_cast<hew::HewArrayType>(collectionVal.getType())) {
      auto *ie = std::get_if<ast::ExprIdentifier>(&idx->object->value.kind);
      if (!ie) {
        emitError(location) << "array indexed assignment requires a variable target";
        return;
      }
      auto varSlot = mutableVars.lookup(intern(ie->name));
      if (!varSlot) {
        emitError(location) << "cannot assign index on immutable variable '" << ie->name << "'";
        return;
      }

      rhsVal = coerceType(rhsVal, hewArrayType.getElementType(), location);
      auto llvmArrayType =
          mlir::LLVM::LLVMArrayType::get(hewArrayType.getElementType(), hewArrayType.getSize());
      auto llvmArray = builder.create<hew::BitcastOp>(location, llvmArrayType, collectionVal);
      auto one = builder.create<mlir::arith::ConstantIntOp>(location, 1, 64);
      auto alloca =
          builder.create<mlir::LLVM::AllocaOp>(location, ptrType, llvmArrayType, one.getResult());
      builder.create<mlir::LLVM::StoreOp>(location, llvmArray, alloca);
      auto zero = builder.create<mlir::arith::ConstantIntOp>(location, 0, 64);
      auto elemPtr = builder.create<mlir::LLVM::GEPOp>(
          location, ptrType, llvmArrayType, alloca, mlir::ValueRange{zero.getResult(), indexVal});
      builder.create<mlir::LLVM::StoreOp>(location, rhsVal, elemPtr);
      auto updatedArray = builder.create<mlir::LLVM::LoadOp>(location, llvmArrayType, alloca);
      auto updatedHewArray =
          builder.create<hew::BitcastOp>(location, hewArrayType, updatedArray.getResult());
      storeVariable(ie->name, updatedHewArray);
      return;
    }

    emitError(location) << "unsupported indexed assignment target";
    return;
  }

  // Get the target variable name
  auto *targetIdent = std::get_if<ast::ExprIdentifier>(&stmt.target.value.kind);
  if (!targetIdent) {
    emitWarning(location) << "only simple identifier targets supported for assignment";
    return;
  }

  auto name = targetIdent->name;

  mlir::Value rhs = generateExpression(stmt.value.value);
  if (!rhs)
    return;

  // Handle compound assignment operators
  if (stmt.op) {
    mlir::Value current = lookupVariable(name);
    if (!current)
      return;

    rhs = coerceType(rhs, current.getType(), location);
    mlir::Value result;
    auto type = current.getType();
    bool isFloat = llvm::isa<mlir::FloatType>(type);
    bool isUnsigned = false;
    if (mlir::isa<mlir::IntegerType>(type))
      if (auto *ty = resolvedTypeOf(stmt.target.span))
        isUnsigned = isUnsignedTypeExpr(*ty);

    switch (*stmt.op) {
    case ast::CompoundAssignOp::Add:
      result = isFloat ? builder.create<mlir::arith::AddFOp>(location, current, rhs).getResult()
                       : builder.create<mlir::arith::AddIOp>(location, current, rhs).getResult();
      break;
    case ast::CompoundAssignOp::Subtract:
      result = isFloat ? builder.create<mlir::arith::SubFOp>(location, current, rhs).getResult()
                       : builder.create<mlir::arith::SubIOp>(location, current, rhs).getResult();
      break;
    case ast::CompoundAssignOp::Multiply:
      result = isFloat ? builder.create<mlir::arith::MulFOp>(location, current, rhs).getResult()
                       : builder.create<mlir::arith::MulIOp>(location, current, rhs).getResult();
      break;
    case ast::CompoundAssignOp::Divide:
      result = isFloat ? builder.create<mlir::arith::DivFOp>(location, current, rhs).getResult()
               : isUnsigned
                   ? builder.create<mlir::arith::DivUIOp>(location, current, rhs).getResult()
                   : builder.create<mlir::arith::DivSIOp>(location, current, rhs).getResult();
      break;
    case ast::CompoundAssignOp::Modulo:
      result = isFloat ? builder.create<mlir::arith::RemFOp>(location, current, rhs).getResult()
               : isUnsigned
                   ? builder.create<mlir::arith::RemUIOp>(location, current, rhs).getResult()
                   : builder.create<mlir::arith::RemSIOp>(location, current, rhs).getResult();
      break;
    case ast::CompoundAssignOp::BitAnd:
      result = builder.create<mlir::arith::AndIOp>(location, current, rhs).getResult();
      break;
    case ast::CompoundAssignOp::BitOr:
      result = builder.create<mlir::arith::OrIOp>(location, current, rhs).getResult();
      break;
    case ast::CompoundAssignOp::BitXor:
      result = builder.create<mlir::arith::XOrIOp>(location, current, rhs).getResult();
      break;
    case ast::CompoundAssignOp::Shl:
      result = builder.create<mlir::arith::ShLIOp>(location, current, rhs).getResult();
      break;
    case ast::CompoundAssignOp::Shr:
      result = isUnsigned
                   ? builder.create<mlir::arith::ShRUIOp>(location, current, rhs).getResult()
                   : builder.create<mlir::arith::ShRSIOp>(location, current, rhs).getResult();
      break;
    }
    storeVariable(name, result);
  } else {
    mlir::Value current = lookupVariable(name);
    if (current)
      rhs = coerceType(rhs, current.getType(), location);
    storeVariable(name, rhs);
  }
}

void MLIRGen::generateIfStmt(const ast::StmtIf &stmt) {
  auto location = currentLoc;

  mlir::Value cond = generateExpression(stmt.condition.value);
  if (!cond)
    return;

  if (cond.getType() != builder.getI1Type()) {
    auto zero = createIntConstant(builder, location, cond.getType(), 0);
    cond =
        builder.create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::ne, cond, zero);
  }

  bool hasElse = stmt.else_block.has_value();

  auto ifOp =
      builder.create<mlir::scf::IfOp>(location, /*resultTypes=*/mlir::TypeRange{}, cond, hasElse);

  builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
  generateBlock(stmt.then_block);
  auto *thenBlock = builder.getInsertionBlock();
  if (thenBlock->empty() || !thenBlock->back().hasTrait<mlir::OpTrait::IsTerminator>())
    builder.create<mlir::scf::YieldOp>(location);

  if (hasElse) {
    builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
    const auto &elseBlock = *stmt.else_block;
    if (elseBlock.is_if && elseBlock.if_stmt) {
      if (auto *innerIf = std::get_if<ast::StmtIf>(&elseBlock.if_stmt->value.kind))
        generateIfStmt(*innerIf);
    } else if (elseBlock.block) {
      generateBlock(*elseBlock.block);
    }
    auto *elseBlk = builder.getInsertionBlock();
    if (elseBlk->empty() || !elseBlk->back().hasTrait<mlir::OpTrait::IsTerminator>())
      builder.create<mlir::scf::YieldOp>(location);
  }

  builder.setInsertionPointAfter(ifOp);
}

// ============================================================================
// If statement as expression (value-producing if at end of block)
// ============================================================================

mlir::Value MLIRGen::generateIfStmtAsExpr(const ast::StmtIf &stmt) {
  auto location = currentLoc;

  mlir::Value cond = generateExpression(stmt.condition.value);
  if (!cond)
    return nullptr;

  if (cond.getType() != builder.getI1Type()) {
    auto zero = createIntConstant(builder, location, cond.getType(), 0);
    cond =
        builder.create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::ne, cond, zero);
  }

  mlir::Type resultType;
  if (currentFunction && currentFunction.getResultTypes().size() == 1)
    resultType = currentFunction.getResultTypes()[0];
  else
    resultType = defaultIntType();

  bool hasElse = stmt.else_block.has_value();
  if (!hasElse) {
    generateIfStmt(stmt);
    return nullptr;
  }

  auto ifOp = builder.create<mlir::scf::IfOp>(location, resultType, cond, /*withElseRegion=*/true);

  builder.setInsertionPointToStart(&ifOp.getThenRegion().front());
  mlir::Value thenVal = generateBlock(stmt.then_block);
  auto *thenBlock = builder.getInsertionBlock();
  if (thenBlock->empty() || !thenBlock->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
    if (thenVal) {
      thenVal = coerceType(thenVal, resultType, location);
      builder.create<mlir::scf::YieldOp>(location, mlir::ValueRange{thenVal});
    } else {
      auto defVal = createDefaultValue(builder, location, resultType);
      builder.create<mlir::scf::YieldOp>(location, mlir::ValueRange{defVal});
    }
  }

  builder.setInsertionPointToStart(&ifOp.getElseRegion().front());
  mlir::Value elseVal = nullptr;
  const auto &elseBlock = *stmt.else_block;
  if (elseBlock.is_if && elseBlock.if_stmt) {
    if (auto *innerIf = std::get_if<ast::StmtIf>(&elseBlock.if_stmt->value.kind))
      elseVal = generateIfStmtAsExpr(*innerIf);
  } else if (elseBlock.block) {
    elseVal = generateBlock(*elseBlock.block);
  }
  auto *elseBlk = builder.getInsertionBlock();
  if (elseBlk->empty() || !elseBlk->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
    if (elseVal) {
      elseVal = coerceType(elseVal, resultType, location);
      builder.create<mlir::scf::YieldOp>(location, mlir::ValueRange{elseVal});
    } else {
      auto defVal = createDefaultValue(builder, location, resultType);
      builder.create<mlir::scf::YieldOp>(location, mlir::ValueRange{defVal});
    }
  }

  builder.setInsertionPointAfter(ifOp);
  return ifOp.getResult(0);
}

void MLIRGen::generateWhileStmt(const ast::StmtWhile &stmt) {
  auto location = currentLoc;

  auto i1Type = builder.getI1Type();
  auto memrefType = mlir::MemRefType::get({}, i1Type);
  auto activeFlag = builder.create<mlir::memref::AllocaOp>(location, memrefType);
  auto trueVal = createIntConstant(builder, location, i1Type, 1);
  builder.create<mlir::memref::StoreOp>(location, trueVal, activeFlag);

  auto continueFlag = builder.create<mlir::memref::AllocaOp>(location, memrefType);
  auto falseVal = createIntConstant(builder, location, i1Type, 0);
  builder.create<mlir::memref::StoreOp>(location, falseVal, continueFlag);

  loopActiveStack.push_back(activeFlag);
  loopContinueStack.push_back(continueFlag);

  std::string labelName;
  if (stmt.label) {
    labelName = *stmt.label;
    labeledActiveFlags[labelName] = activeFlag;
    labeledContinueFlags[labelName] = continueFlag;
  }

  loopBreakValueStack.push_back(nullptr);

  auto whileOp =
      builder.create<mlir::scf::WhileOp>(location, mlir::TypeRange{}, mlir::ValueRange{});

  auto *beforeBlock = builder.createBlock(&whileOp.getBefore());
  builder.setInsertionPointToStart(beforeBlock);

  auto isActive = builder.create<mlir::memref::LoadOp>(location, activeFlag, mlir::ValueRange{});

  mlir::Value cond = generateExpression(stmt.condition.value);
  if (!cond)
    cond = createIntConstant(builder, location, builder.getI1Type(), 0);

  if (cond.getType() != builder.getI1Type()) {
    auto zero = createIntConstant(builder, location, cond.getType(), 0);
    cond =
        builder.create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::ne, cond, zero);
  }

  auto combinedCond = builder.create<mlir::arith::AndIOp>(location, isActive, cond);

  if (returnFlag) {
    auto flagVal = builder.create<mlir::memref::LoadOp>(location, returnFlag, mlir::ValueRange{});
    auto trueConst = createIntConstant(builder, location, i1Type, 1);
    auto notReturned = builder.create<mlir::arith::XOrIOp>(location, flagVal, trueConst);
    combinedCond = builder.create<mlir::arith::AndIOp>(location, combinedCond, notReturned);
  }

  builder.create<mlir::scf::ConditionOp>(location, combinedCond, mlir::ValueRange{});

  auto *afterBlock = builder.createBlock(&whileOp.getAfter());
  builder.setInsertionPointToStart(afterBlock);
  builder.create<mlir::memref::StoreOp>(location, falseVal, continueFlag);

  {
    SymbolTableScopeT bodyScope(symbolTable);
    MutableTableScopeT bodyMutScope(mutableVars);
    pushDropScope();
    generateLoopBodyWithContinueGuards(stmt.body.stmts, 0, stmt.body.stmts.size(), continueFlag,
                                       location);
    popDropScope();
  }

  auto *bodyBlock = builder.getInsertionBlock();
  if (bodyBlock->empty() || !bodyBlock->back().hasTrait<mlir::OpTrait::IsTerminator>())
    builder.create<mlir::scf::YieldOp>(location);

  auto breakValueAlloca = loopBreakValueStack.back();

  loopActiveStack.pop_back();
  loopContinueStack.pop_back();
  loopBreakValueStack.pop_back();
  if (!labelName.empty()) {
    labeledActiveFlags.erase(labelName);
    labeledContinueFlags.erase(labelName);
  }

  builder.setInsertionPointAfter(whileOp);

  if (breakValueAlloca) {
    lastBreakValue =
        builder.create<mlir::memref::LoadOp>(location, breakValueAlloca, mlir::ValueRange{});
  } else {
    lastBreakValue = nullptr;
  }
}

// ── for await: first-class Stream<T> variable iteration ─────────────────────

// ── for await: cross-actor stream iteration ─────────────────────────────

// ============================================================================
// Loop statement generation
// ============================================================================

void MLIRGen::generateForStreamStmt(const ast::StmtFor &stmt) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i1Type = builder.getI1Type();
  auto i64Type = builder.getI64Type();

  // Generate the stream pointer expression.
  mlir::Value streamPtr = generateExpression(stmt.iterable.value);
  if (!streamPtr)
    return;

  // Hoist item-pointer alloca before the while op so both before/after regions
  // can access it.  The before-region stores the fetched pointer here; the
  // after-region loads it to bind the loop variable.
  auto one64 = builder.create<mlir::arith::ConstantIntOp>(location, i64Type, 1);
  auto itemPtrAlloca = builder.create<mlir::LLVM::AllocaOp>(location, ptrType, ptrType, one64);
  auto nullPtrVal = builder.create<mlir::LLVM::ZeroOp>(location, ptrType);
  builder.create<mlir::LLVM::StoreOp>(location, nullPtrVal, itemPtrAlloca);

  // Loop-control flags (break/continue/return support).
  auto memrefI1 = mlir::MemRefType::get({}, i1Type);
  auto activeFlag = builder.create<mlir::memref::AllocaOp>(location, memrefI1);
  auto trueVal = createIntConstant(builder, location, i1Type, 1);
  auto falseVal = createIntConstant(builder, location, i1Type, 0);
  builder.create<mlir::memref::StoreOp>(location, trueVal, activeFlag);

  auto continueFlag = builder.create<mlir::memref::AllocaOp>(location, memrefI1);
  builder.create<mlir::memref::StoreOp>(location, falseVal, continueFlag);

  loopActiveStack.push_back(activeFlag);
  loopContinueStack.push_back(continueFlag);
  loopBreakValueStack.push_back(nullptr);

  auto whileOp =
      builder.create<mlir::scf::WhileOp>(location, mlir::TypeRange{}, mlir::ValueRange{});

  // ── Before region: fetch next item ──────────────────────────────
  auto *beforeBlock = builder.createBlock(&whileOp.getBefore());
  builder.setInsertionPointToStart(beforeBlock);

  // hew_stream_next(stream_ptr) → malloc'd item buffer, or null on EOF
  auto nextAttr = mlir::SymbolRefAttr::get(&context, "hew_stream_next");
  auto itemPtr = builder
                     .create<hew::RuntimeCallOp>(location, mlir::TypeRange{ptrType}, nextAttr,
                                                 mlir::ValueRange{streamPtr})
                     .getResult();

  // Stash the item pointer so the after-region can load it.
  builder.create<mlir::LLVM::StoreOp>(location, itemPtr, itemPtrAlloca);

  // Condition: itemPtr != null && active && !returned
  auto notNull = builder.create<mlir::LLVM::ICmpOp>(location, mlir::LLVM::ICmpPredicate::ne,
                                                    itemPtr, nullPtrVal);
  auto isActive = builder.create<mlir::memref::LoadOp>(location, activeFlag, mlir::ValueRange{});
  mlir::Value combinedCond = builder.create<mlir::arith::AndIOp>(location, notNull, isActive);

  if (returnFlag) {
    auto flagVal = builder.create<mlir::memref::LoadOp>(location, returnFlag, mlir::ValueRange{});
    auto trueConst = createIntConstant(builder, location, i1Type, 1);
    auto notReturned = builder.create<mlir::arith::XOrIOp>(location, flagVal, trueConst);
    combinedCond = builder.create<mlir::arith::AndIOp>(location, combinedCond, notReturned);
  }

  builder.create<mlir::scf::ConditionOp>(location, combinedCond, mlir::ValueRange{});

  // ── After region: bind loop variable, run body ──────────────────
  auto *afterBlock = builder.createBlock(&whileOp.getAfter());
  builder.setInsertionPointToStart(afterBlock);
  builder.create<mlir::memref::StoreOp>(location, falseVal, continueFlag);

  {
    SymbolTableScopeT bodyScope(symbolTable);
    MutableTableScopeT bodyMutScope(mutableVars);

    // Load the item pointer stashed by the before-region.
    auto currentItemPtr = builder.create<mlir::LLVM::LoadOp>(location, ptrType, itemPtrAlloca);

    std::string loopVarName = "_stream_item";
    if (auto *identPat = std::get_if<ast::PatIdentifier>(&stmt.pattern.value.kind)) {
      loopVarName = identPat->name;
    }
    declareVariable(loopVarName, currentItemPtr);

    pushDropScope();
    registerDroppable(loopVarName, "hew_string_drop");
    if (returnFlag) {
      auto flagVal = builder.create<mlir::memref::LoadOp>(location, returnFlag, mlir::ValueRange{});
      auto trueConst = createIntConstant(builder, location, i1Type, 1);
      auto notReturned = builder.create<mlir::arith::XOrIOp>(location, flagVal, trueConst);
      auto guard = builder.create<mlir::scf::IfOp>(location, mlir::TypeRange{}, notReturned,
                                                   /*withElseRegion=*/false);
      builder.setInsertionPointToStart(&guard.getThenRegion().front());
      generateBlock(stmt.body);
      auto *guardBlock = builder.getInsertionBlock();
      if (guardBlock->empty() || !guardBlock->back().hasTrait<mlir::OpTrait::IsTerminator>())
        builder.create<mlir::scf::YieldOp>(location);
      builder.setInsertionPointAfter(guard);
    } else {
      generateBlock(stmt.body);
    }
    popDropScope();
  }

  auto *bodyBlock = builder.getInsertionBlock();
  if (bodyBlock->empty() || !bodyBlock->back().hasTrait<mlir::OpTrait::IsTerminator>())
    builder.create<mlir::scf::YieldOp>(location);

  loopActiveStack.pop_back();
  loopContinueStack.pop_back();
  loopBreakValueStack.pop_back();

  builder.setInsertionPointAfter(whileOp);

  // Free the last fetched item if non-null (handles break case where
  // hew_stream_next returns one more item after break is signaled).
  auto lastItem = builder.create<mlir::LLVM::LoadOp>(location, ptrType, itemPtrAlloca);
  auto isNotNull = builder.create<mlir::LLVM::ICmpOp>(location, mlir::LLVM::ICmpPredicate::ne,
                                                      lastItem, nullPtrVal);
  auto cleanupIf = builder.create<mlir::scf::IfOp>(location, mlir::TypeRange{}, isNotNull,
                                                   /*withElseRegion=*/false);
  builder.setInsertionPointToStart(&cleanupIf.getThenRegion().front());
  auto dropAttr = mlir::SymbolRefAttr::get(&context, "hew_string_drop");
  builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{}, dropAttr,
                                     mlir::ValueRange{lastItem});
  // scf.if auto-adds yield; set insertion after guard
  builder.setInsertionPointAfter(cleanupIf);

  // Close the stream if it was an inline expression (e.g. `raw.lines()`).
  // Named stream variables are managed by the user via explicit `.close()`.
  bool isInlineStream = !std::get_if<ast::ExprIdentifier>(&stmt.iterable.value.kind);
  if (isInlineStream) {
    auto closeAttr = mlir::SymbolRefAttr::get(&context, "hew_stream_close");
    builder.create<hew::RuntimeCallOp>(location, mlir::TypeRange{}, closeAttr,
                                       mlir::ValueRange{streamPtr});
  }
}

// ── for await: cross-actor stream iteration ─────────────────────────────
void MLIRGen::generateForAwaitStmt(const ast::StmtFor &stmt) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i8Type = builder.getI8Type();
  auto i1Type = builder.getI1Type();

  // Fast path: if the iterable is a first-class Stream<T> variable, delegate.
  // NOTE: ExprMethodCall on actors is intentionally excluded — those use mailbox iteration.
  if (auto *identExpr = std::get_if<ast::ExprIdentifier>(&stmt.iterable.value.kind)) {
    bool isStreamVar = false;
    // Prefer resolved type from the type checker
    if (auto *typeExpr = resolvedTypeOf(stmt.iterable.span))
      isStreamVar = typeExprStreamKind(*typeExpr) == "Stream";
    // Fall back to map lookup
    if (!isStreamVar) {
      auto sit = streamHandleVarTypes.find(identExpr->name);
      isStreamVar = (sit != streamHandleVarTypes.end() && sit->second == "Stream");
    }
    if (isStreamVar) {
      generateForStreamStmt(stmt);
      return;
    }
  }

  // Also handle non-identifier expressions that produce a Stream type (e.g.
  // free-function calls like hew_stream_lines(raw)). ExprMethodCall on actors
  // is intentionally left for the mailbox iteration path below.
  if (!std::get_if<ast::ExprMethodCall>(&stmt.iterable.value.kind)) {
    if (auto *typeExpr = resolvedTypeOf(stmt.iterable.span)) {
      if (typeExprStreamKind(*typeExpr) == "Stream") {
        generateForStreamStmt(stmt);
        return;
      }
    }
    // Fall back to recognizing stream-returning C function calls by name.
    if (auto *ce = std::get_if<ast::ExprCall>(&stmt.iterable.value.kind)) {
      if (ce->function) {
        if (auto *fi = std::get_if<ast::ExprIdentifier>(&ce->function->value.kind)) {
          if (fi->name == "hew_stream_lines" || fi->name == "hew_stream_chunks" ||
              fi->name == "hew_stream_from_file_read" || fi->name == "hew_stream_pair_stream") {
            generateForStreamStmt(stmt);
            return;
          }
        }
      }
    }
  }

  // The iterable must be a method call on an actor: actor.method(args)
  auto *mc = std::get_if<ast::ExprMethodCall>(&stmt.iterable.value.kind);
  if (!mc) {
    emitError(location) << "for await requires an actor method call as iterable";
    return;
  }

  // Resolve the receiver as an actor variable
  if (!mc->receiver) {
    emitError(location) << "for await: receiver must be an actor variable";
    return;
  }
  auto *recvIdent = std::get_if<ast::ExprIdentifier>(&mc->receiver->value.kind);
  if (!recvIdent) {
    emitError(location) << "for await: receiver must be an actor variable";
    return;
  }
  std::string receiverName = recvIdent->name;
  std::string methodName = mc->method;

  auto avIt = actorVarTypes.find(receiverName);
  if (avIt == actorVarTypes.end()) {
    emitError(location) << "for await: '" << receiverName << "' is not a known actor variable";
    return;
  }
  std::string actorTypeName = avIt->second;

  auto arIt = actorRegistry.find(actorTypeName);
  if (arIt == actorRegistry.end()) {
    emitError(location) << "for await: unknown actor type '" << actorTypeName << "'";
    return;
  }
  const auto &actorInfo = arIt->second;

  // Find the init handler (the generator receive fn)
  int64_t initIdx = -1;
  const ActorReceiveInfo *initInfo = nullptr;
  for (size_t i = 0; i < actorInfo.receiveFns.size(); ++i) {
    if (actorInfo.receiveFns[i].name == methodName) {
      initIdx = static_cast<int64_t>(i);
      initInfo = &actorInfo.receiveFns[i];
      break;
    }
  }
  if (initIdx < 0 || !initInfo || !initInfo->returnType) {
    emitError(location) << "for await: '" << methodName << "' is not a generator receive fn on '"
                        << actorTypeName << "'";
    return;
  }

  // Find the __next handler (should be immediately after init)
  int64_t nextIdx = -1;
  std::string nextMethodName = methodName + "__next";
  for (size_t i = 0; i < actorInfo.receiveFns.size(); ++i) {
    if (actorInfo.receiveFns[i].name == nextMethodName) {
      nextIdx = static_cast<int64_t>(i);
      break;
    }
  }
  if (nextIdx < 0) {
    emitError(location) << "for await: missing __next handler for '" << methodName << "' on '"
                        << actorTypeName << "'";
    return;
  }

  // The wrapper return type is { i8, YieldType }
  auto wrapperType = *initInfo->returnType;
  auto wrapperStructType = mlir::dyn_cast<mlir::LLVM::LLVMStructType>(wrapperType);
  if (!wrapperStructType || wrapperStructType.getBody().size() != 2) {
    emitError(location) << "for await: unexpected wrapper type";
    return;
  }

  // Get the actor ref value
  auto actorPtr = generateExpression(mc->receiver->value);
  if (!actorPtr)
    return;

  auto actorRefType = hew::ActorRefType::get(&context);
  auto actorRef = builder.create<hew::BitcastOp>(location, actorRefType, actorPtr);

  // Generate argument values for init call
  llvm::SmallVector<mlir::Value, 4> initArgs;
  for (const auto &argPtr : mc->args) {
    auto val = generateExpression(ast::callArgExpr(argPtr).value);
    if (!val)
      return;
    initArgs.push_back(val);
  }

  // Call init: actor_ask(actor, init_msg_type, args) → { i8, YieldType }
  auto initAsk = builder.create<hew::ActorAskOp>(
      location, wrapperType, actorRef, builder.getI32IntegerAttr(static_cast<int32_t>(initIdx)),
      initArgs, /*timeout_ms=*/mlir::IntegerAttr{});
  auto initResult = initAsk.getResult();

  // Store wrapper result in alloca for mutable updates across iterations
  auto i64Type = builder.getI64Type();
  auto one = builder.create<mlir::arith::ConstantIntOp>(location, i64Type, 1);
  auto resultAlloca = builder.create<mlir::LLVM::AllocaOp>(location, ptrType, wrapperType, one);
  builder.create<mlir::LLVM::StoreOp>(location, initResult, resultAlloca);

  // Loop control flags (break/continue support)
  auto memrefType = mlir::MemRefType::get({}, i1Type);
  auto activeFlag = builder.create<mlir::memref::AllocaOp>(location, memrefType);
  auto trueVal = createIntConstant(builder, location, i1Type, 1);
  auto falseVal = createIntConstant(builder, location, i1Type, 0);
  builder.create<mlir::memref::StoreOp>(location, trueVal, activeFlag);

  auto continueFlag = builder.create<mlir::memref::AllocaOp>(location, memrefType);
  builder.create<mlir::memref::StoreOp>(location, falseVal, continueFlag);

  loopActiveStack.push_back(activeFlag);
  loopContinueStack.push_back(continueFlag);
  loopBreakValueStack.push_back(nullptr);

  // scf.while loop
  auto whileOp =
      builder.create<mlir::scf::WhileOp>(location, mlir::TypeRange{}, mlir::ValueRange{});

  // Before region: check has_value
  auto *beforeBlock = builder.createBlock(&whileOp.getBefore());
  builder.setInsertionPointToStart(beforeBlock);

  auto isActive = builder.create<mlir::memref::LoadOp>(location, activeFlag, mlir::ValueRange{});

  auto currentResult = builder.create<mlir::LLVM::LoadOp>(location, wrapperType, resultAlloca);
  auto hasValue = builder.create<mlir::LLVM::ExtractValueOp>(location, currentResult,
                                                             llvm::ArrayRef<int64_t>{0});
  // has_value is i8, convert to i1
  auto zeroI8 = builder.create<mlir::arith::ConstantIntOp>(location, i8Type, 0);
  auto hasValueBool = builder.create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::ne,
                                                          hasValue, zeroI8);

  auto combinedCond = builder.create<mlir::arith::AndIOp>(location, isActive, hasValueBool);

  if (returnFlag) {
    auto flagVal = builder.create<mlir::memref::LoadOp>(location, returnFlag, mlir::ValueRange{});
    auto trueConst = createIntConstant(builder, location, i1Type, 1);
    auto notReturned = builder.create<mlir::arith::XOrIOp>(location, flagVal, trueConst);
    combinedCond = builder.create<mlir::arith::AndIOp>(location, combinedCond, notReturned);
  }

  builder.create<mlir::scf::ConditionOp>(location, combinedCond, mlir::ValueRange{});

  // After region: extract value, run body, call next
  auto *afterBlock = builder.createBlock(&whileOp.getAfter());
  builder.setInsertionPointToStart(afterBlock);

  builder.create<mlir::memref::StoreOp>(location, falseVal, continueFlag);

  {
    SymbolTableScopeT bodyScope(symbolTable);
    MutableTableScopeT bodyMutScope(mutableVars);

    // Load current wrapper and extract the value
    auto wrapper = builder.create<mlir::LLVM::LoadOp>(location, wrapperType, resultAlloca);
    auto value =
        builder.create<mlir::LLVM::ExtractValueOp>(location, wrapper, llvm::ArrayRef<int64_t>{1});

    // Bind loop variable
    std::string loopVarName = "_await_var";
    if (auto *identPat = std::get_if<ast::PatIdentifier>(&stmt.pattern.value.kind)) {
      loopVarName = identPat->name;
    }
    declareVariable(loopVarName, value);

    // Generate loop body
    generateBlock(stmt.body);
  }

  // Guard the __next call: only call if loop is still active (no break/return)
  auto stillActive = builder.create<mlir::memref::LoadOp>(location, activeFlag, mlir::ValueRange{});
  mlir::Value nextGuard = stillActive;
  if (returnFlag) {
    auto flagVal = builder.create<mlir::memref::LoadOp>(location, returnFlag, mlir::ValueRange{});
    auto trueConst = createIntConstant(builder, location, i1Type, 1);
    auto notReturned = builder.create<mlir::arith::XOrIOp>(location, flagVal, trueConst);
    nextGuard = builder.create<mlir::arith::AndIOp>(location, nextGuard, notReturned);
  }

  auto nextIfOp = builder.create<mlir::scf::IfOp>(location, nextGuard, /*withElseRegion=*/false);
  builder.setInsertionPointToStart(&nextIfOp.getThenRegion().front());

  // Call next: actor_ask(actor, next_msg_type) → { i8, YieldType }
  auto nextAsk = builder.create<hew::ActorAskOp>(
      location, wrapperType, actorRef, builder.getI32IntegerAttr(static_cast<int32_t>(nextIdx)),
      mlir::ValueRange{}, /*timeout_ms=*/mlir::IntegerAttr{});
  builder.create<mlir::LLVM::StoreOp>(location, nextAsk.getResult(), resultAlloca);

  builder.setInsertionPointAfter(nextIfOp);

  // Ensure terminator
  auto *bodyBlock = builder.getInsertionBlock();
  if (bodyBlock->empty() || !bodyBlock->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
    builder.create<mlir::scf::YieldOp>(location);
  }

  loopActiveStack.pop_back();
  loopContinueStack.pop_back();
  loopBreakValueStack.pop_back();

  builder.setInsertionPointAfter(whileOp);
}

void MLIRGen::generateForStmt(const ast::StmtFor &stmt) {
  const ast::ExprBinary *rangeExpr = nullptr;
  if (auto *binExpr = std::get_if<ast::ExprBinary>(&stmt.iterable.value.kind)) {
    if (binExpr->op == ast::BinaryOp::Range || binExpr->op == ast::BinaryOp::RangeInclusive) {
      rangeExpr = binExpr;
    }
  }

  if (rangeExpr) {
    generateForRange(stmt, *rangeExpr);
    return;
  }

  // Collection-based for loop (for x in vec)
  generateForCollectionStmt(stmt);
}

void MLIRGen::generateForRange(const ast::StmtFor &stmt, const ast::ExprBinary &rangeExpr) {
  auto location = currentLoc;

  mlir::Value lb = nullptr;
  mlir::Value ub = nullptr;
  if (rangeExpr.left)
    lb = generateExpression(rangeExpr.left->value);
  if (rangeExpr.right)
    ub = generateExpression(rangeExpr.right->value);

  if (!lb || !ub)
    return;

  // Convert lb/ub to index type first
  auto indexType = builder.getIndexType();
  if (lb.getType() != indexType) {
    lb = builder.create<mlir::arith::IndexCastOp>(location, indexType, lb);
  }
  if (ub.getType() != indexType) {
    ub = builder.create<mlir::arith::IndexCastOp>(location, indexType, ub);
  }

  // For inclusive range, add 1 to upper bound
  if (rangeExpr.op == ast::BinaryOp::RangeInclusive) {
    auto one = builder.create<mlir::arith::ConstantIndexOp>(location, 1);
    ub = builder.create<mlir::arith::AddIOp>(location, ub, one);
  }

  // Get the loop variable name
  std::string loopVarName;
  if (auto *identPat = std::get_if<ast::PatIdentifier>(&stmt.pattern.value.kind)) {
    loopVarName = identPat->name;
  } else {
    loopVarName = "_for_var";
  }

  // Determine if the range is over an unsigned type
  bool rangeIsUnsigned = false;
  if (rangeExpr.left) {
    if (auto *ty = resolvedTypeOf(rangeExpr.left->span))
      rangeIsUnsigned = isUnsignedTypeExpr(*ty);
  }

  // Use scf.while instead of scf.for to support break/continue.
  // This mirrors the pattern in generateForCollectionStmt.
  auto i64Type = builder.getI64Type();
  auto i1Type = builder.getI1Type();

  // Cast lb/ub from index → i64 for alloca storage
  auto lbI64 = builder.create<mlir::arith::IndexCastOp>(location, i64Type, lb);
  auto ubI64 = builder.create<mlir::arith::IndexCastOp>(location, i64Type, ub);

  // Index alloca initialized to lb
  auto memrefI64 = mlir::MemRefType::get({}, i64Type);
  mlir::Value indexAlloca = builder.create<mlir::memref::AllocaOp>(location, memrefI64);
  builder.create<mlir::memref::StoreOp>(location, lbI64, indexAlloca);

  // Active flag (break sets to false)
  auto memrefI1 = mlir::MemRefType::get({}, i1Type);
  auto activeFlag = builder.create<mlir::memref::AllocaOp>(location, memrefI1);
  auto trueVal = createIntConstant(builder, location, i1Type, 1);
  builder.create<mlir::memref::StoreOp>(location, trueVal, activeFlag);

  // Continue flag (continue sets to true, reset each iteration)
  auto continueFlag = builder.create<mlir::memref::AllocaOp>(location, memrefI1);
  auto falseVal = createIntConstant(builder, location, i1Type, 0);
  builder.create<mlir::memref::StoreOp>(location, falseVal, continueFlag);

  loopActiveStack.push_back(activeFlag);
  loopContinueStack.push_back(continueFlag);
  loopBreakValueStack.push_back(nullptr);

  // Build scf.while
  auto whileOp =
      builder.create<mlir::scf::WhileOp>(location, mlir::TypeRange{}, mlir::ValueRange{});

  // Before region: check index < ub && active && !returned
  auto *beforeBlock = builder.createBlock(&whileOp.getBefore());
  builder.setInsertionPointToStart(beforeBlock);

  auto isActive = builder.create<mlir::memref::LoadOp>(location, activeFlag, mlir::ValueRange{});
  auto curIdx = builder.create<mlir::memref::LoadOp>(location, indexAlloca, mlir::ValueRange{});
  auto cond = builder.create<mlir::arith::CmpIOp>(
      location, rangeIsUnsigned ? mlir::arith::CmpIPredicate::ult : mlir::arith::CmpIPredicate::slt,
      curIdx, ubI64);
  auto combinedCond = builder.create<mlir::arith::AndIOp>(location, isActive, cond);

  if (returnFlag) {
    auto flagVal = builder.create<mlir::memref::LoadOp>(location, returnFlag, mlir::ValueRange{});
    auto trueConst = createIntConstant(builder, location, i1Type, 1);
    auto notReturned = builder.create<mlir::arith::XOrIOp>(location, flagVal, trueConst);
    combinedCond = builder.create<mlir::arith::AndIOp>(location, combinedCond, notReturned);
  }

  builder.create<mlir::scf::ConditionOp>(location, combinedCond, mlir::ValueRange{});

  // After region: loop body + index increment
  auto *afterBlock = builder.createBlock(&whileOp.getAfter());
  builder.setInsertionPointToStart(afterBlock);
  builder.create<mlir::memref::StoreOp>(location, falseVal, continueFlag);

  {
    SymbolTableScopeT loopScope(symbolTable);
    MutableTableScopeT loopMutScope(mutableVars);

    // Bind loop variable: load index as i64
    auto idx = builder.create<mlir::memref::LoadOp>(location, indexAlloca, mlir::ValueRange{});
    declareVariable(loopVarName, idx);

    // Generate body with continue guards
    pushDropScope();
    generateLoopBodyWithContinueGuards(stmt.body.stmts, 0, stmt.body.stmts.size(), continueFlag,
                                       location);
    popDropScope();

    // Increment index
    auto curI = builder.create<mlir::memref::LoadOp>(location, indexAlloca, mlir::ValueRange{});
    auto one = createIntConstant(builder, location, i64Type, 1);
    auto nextIdx = builder.create<mlir::arith::AddIOp>(location, curI, one);
    builder.create<mlir::memref::StoreOp>(location, nextIdx, indexAlloca);
  }

  // Ensure yield terminator
  auto *bodyBlock = builder.getInsertionBlock();
  if (bodyBlock->empty() || !bodyBlock->back().hasTrait<mlir::OpTrait::IsTerminator>())
    builder.create<mlir::scf::YieldOp>(location);

  // Pop loop control stacks
  auto breakValueAlloca = loopBreakValueStack.back();

  loopActiveStack.pop_back();
  loopContinueStack.pop_back();
  loopBreakValueStack.pop_back();

  builder.setInsertionPointAfter(whileOp);

  if (breakValueAlloca) {
    lastBreakValue =
        builder.create<mlir::memref::LoadOp>(location, breakValueAlloca, mlir::ValueRange{});
  } else {
    lastBreakValue = nullptr;
  }
}

void MLIRGen::generateForGeneratorStmt(const ast::StmtFor &stmt, const std::string &genFuncName) {
  auto location = currentLoc;
  auto i1Type = builder.getI1Type();

  // Generate the iterable expression to get the generator pointer
  mlir::Value genPtr = generateExpression(stmt.iterable.value);
  if (!genPtr)
    return;

  std::string nextName = genFuncName + "__next";
  std::string doneName = genFuncName + "__done";

  auto nextFuncOp = module.lookupSymbol<mlir::func::FuncOp>(nextName);
  auto doneFuncOp = module.lookupSymbol<mlir::func::FuncOp>(doneName);
  if (!nextFuncOp || !doneFuncOp) {
    emitError(location) << "generator functions not found for " << genFuncName;
    return;
  }

  std::string loopVarName;
  if (auto *identPat = std::get_if<ast::PatIdentifier>(&stmt.pattern.value.kind))
    loopVarName = identPat->name;
  else
    loopVarName = "_gen_var";

  // Loop control flags
  auto memrefI1 = mlir::MemRefType::get({}, i1Type);
  auto activeFlag = builder.create<mlir::memref::AllocaOp>(location, memrefI1);
  auto trueVal = createIntConstant(builder, location, i1Type, 1);
  builder.create<mlir::memref::StoreOp>(location, trueVal, activeFlag);

  auto continueFlag = builder.create<mlir::memref::AllocaOp>(location, memrefI1);
  auto falseVal = createIntConstant(builder, location, i1Type, 0);
  builder.create<mlir::memref::StoreOp>(location, falseVal, continueFlag);

  loopActiveStack.push_back(activeFlag);
  loopContinueStack.push_back(continueFlag);

  auto whileOp =
      builder.create<mlir::scf::WhileOp>(location, mlir::TypeRange{}, mlir::ValueRange{});

  // Before region: check !done && active
  auto *beforeBlock = builder.createBlock(&whileOp.getBefore());
  builder.setInsertionPointToStart(beforeBlock);

  auto isActive = builder.create<mlir::memref::LoadOp>(location, activeFlag, mlir::ValueRange{});
  auto doneCall =
      builder.create<mlir::func::CallOp>(location, doneFuncOp, mlir::ValueRange{genPtr});
  auto isDone = doneCall.getResult(0);
  auto notDone = builder.create<mlir::arith::XOrIOp>(location, isDone, trueVal);
  auto combinedCond = builder.create<mlir::arith::AndIOp>(location, isActive, notDone);

  if (returnFlag) {
    auto flagVal = builder.create<mlir::memref::LoadOp>(location, returnFlag, mlir::ValueRange{});
    auto trueConst = createIntConstant(builder, location, i1Type, 1);
    auto notReturned = builder.create<mlir::arith::XOrIOp>(location, flagVal, trueConst);
    combinedCond = builder.create<mlir::arith::AndIOp>(location, combinedCond, notReturned);
  }

  builder.create<mlir::scf::ConditionOp>(location, combinedCond, mlir::ValueRange{});

  // After region: call __next, bind loop var, run body
  auto *afterBlock = builder.createBlock(&whileOp.getAfter());
  builder.setInsertionPointToStart(afterBlock);

  builder.create<mlir::memref::StoreOp>(location, falseVal, continueFlag);

  auto nextCall =
      builder.create<mlir::func::CallOp>(location, nextFuncOp, mlir::ValueRange{genPtr});
  auto nextVal = nextCall.getResult(0);

  SymbolTableScopeT loopScope(symbolTable);
  MutableTableScopeT loopMutScope(mutableVars);
  declareVariable(loopVarName, nextVal);

  if (returnFlag) {
    auto flagVal = builder.create<mlir::memref::LoadOp>(location, returnFlag, mlir::ValueRange{});
    auto trueConst = createIntConstant(builder, location, i1Type, 1);
    auto notReturned = builder.create<mlir::arith::XOrIOp>(location, flagVal, trueConst);
    auto guard = builder.create<mlir::scf::IfOp>(location, mlir::TypeRange{}, notReturned,
                                                 /*withElseRegion=*/false);
    builder.setInsertionPointToStart(&guard.getThenRegion().front());
    generateBlock(stmt.body);
    auto *guardBlock = builder.getInsertionBlock();
    if (guardBlock->empty() || !guardBlock->back().hasTrait<mlir::OpTrait::IsTerminator>())
      builder.create<mlir::scf::YieldOp>(location);
    builder.setInsertionPointAfter(guard);
  } else {
    generateBlock(stmt.body);
  }

  auto *bodyBlock = builder.getInsertionBlock();
  if (bodyBlock->empty() || !bodyBlock->back().hasTrait<mlir::OpTrait::IsTerminator>())
    builder.create<mlir::scf::YieldOp>(location);

  loopActiveStack.pop_back();
  loopContinueStack.pop_back();

  builder.setInsertionPointAfter(whileOp);
}

void MLIRGen::generateForCollectionStmt(const ast::StmtFor &stmt) {
  // Check if the iterable is a generator call or variable
  std::string genFuncName;
  if (auto *callExpr = std::get_if<ast::ExprCall>(&stmt.iterable.value.kind)) {
    if (callExpr->function) {
      if (auto *funcIdent = std::get_if<ast::ExprIdentifier>(&callExpr->function->value.kind)) {
        if (generatorFunctions.count(funcIdent->name))
          genFuncName = funcIdent->name;
      }
    }
  } else if (auto *identExpr = std::get_if<ast::ExprIdentifier>(&stmt.iterable.value.kind)) {
    auto git = generatorVarTypes.find(identExpr->name);
    if (git != generatorVarTypes.end())
      genFuncName = git->second;
  }

  if (!genFuncName.empty()) {
    generateForGeneratorStmt(stmt, genFuncName);
    return;
  }

  mlir::Value collection = generateExpression(stmt.iterable.value);
  if (!collection)
    return;

  std::string collType;
  // Prefer resolved type from the type checker
  if (auto *typeExpr = resolvedTypeOf(stmt.iterable.span))
    collType = typeExprToCollectionString(
        *typeExpr, [this](const std::string &n) { return resolveTypeAlias(n); });
  // Fall back to identifier-based map lookup
  if (collType.empty()) {
    if (auto *identExpr = std::get_if<ast::ExprIdentifier>(&stmt.iterable.value.kind)) {
      auto cit = collectionVarTypes.find(identExpr->name);
      if (cit != collectionVarTypes.end())
        collType = cit->second;
    }
  }
  // Also check self.field access for actor collection fields
  if (collType.empty() && !currentActorName.empty()) {
    if (auto *fieldAccess = std::get_if<ast::ExprFieldAccess>(&stmt.iterable.value.kind)) {
      if (fieldAccess->object) {
        if (auto *objIdent = std::get_if<ast::ExprIdentifier>(&fieldAccess->object->value.kind)) {
          if (objIdent->name == "self") {
            auto key = currentActorName + "." + fieldAccess->field;
            auto cit = collectionFieldTypes.find(key);
            if (cit != collectionFieldTypes.end())
              collType = cit->second;
          }
        }
      }
    }
  }

  // Check if this is a HashMap iteration
  if (mlir::isa<hew::HashMapType>(collection.getType()) || collType.rfind("HashMap<", 0) == 0) {
    generateForHashMap(stmt, collection, collType);
    return;
  }

  bool isStringCollection = collType == "bytes" || collType == "string" || collType == "String" ||
                            collType == "str" ||
                            mlir::isa<hew::StringRefType>(collection.getType());
  if (isStringCollection) {
    generateForString(stmt, collection, collType);
    return;
  }

  generateForVec(stmt, collection, collType);
}

void MLIRGen::generateForVec(const ast::StmtFor &stmt, mlir::Value collection,
                             const std::string &collType) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i64Type = builder.getI64Type();
  auto i32Type = builder.getI32Type();
  auto i1Type = builder.getI1Type();

  mlir::Type typedVecElemType;
  if (auto vecType = mlir::dyn_cast<hew::VecType>(collection.getType()))
    typedVecElemType = vecType.getElementType();
  auto typedArrayType = mlir::dyn_cast<hew::HewArrayType>(collection.getType());
  mlir::Type typedArrayElemType;
  if (typedArrayType)
    typedArrayElemType = typedArrayType.getElementType();

  auto resolveVecElemTypeFromString = [&]() -> mlir::Type {
    if (collType == "bytes")
      return i32Type;
    if (collType.rfind("Vec<", 0) != 0)
      return {};
    auto inner = collType.substr(4);
    if (!inner.empty() && inner.back() == '>')
      inner.pop_back();
    auto start = inner.find_first_not_of(' ');
    if (start != std::string::npos)
      inner = inner.substr(start);
    auto end = inner.find_last_not_of(' ');
    if (end != std::string::npos)
      inner = inner.substr(0, end + 1);
    if (inner == "i32" || inner == "I32")
      return i32Type;
    if (inner == "i64" || inner == "I64" || inner == "int" || inner == "Int")
      return i64Type;
    if (inner == "f64" || inner == "F64" || inner == "float" || inner == "Float")
      return builder.getF64Type();
    if (inner == "string" || inner == "String" || inner == "str")
      return hew::StringRefType::get(&context);
    if (inner == "bool")
      return i1Type;
    if (inner.find("ActorRef<") == 0 || inner.find("TypedActorRef<") == 0)
      return ptrType;
    auto stIt = structTypes.find(inner);
    if (stIt != structTypes.end() && stIt->second.mlirType)
      return stIt->second.mlirType;
    return {};
  };
  mlir::Type stringVecElemType;
  if (!typedVecElemType)
    stringVecElemType = resolveVecElemTypeFromString();
  bool isVecPtr = typedVecElemType ? mlir::isa<mlir::LLVM::LLVMPointerType>(typedVecElemType)
                                   : (collType.find("Vec<ActorRef<") == 0 ||
                                      collType.find("Vec<TypedActorRef<") == 0);

  // Get collection length
  mlir::Value len;
  mlir::Value arrayAlloca;
  mlir::LLVM::LLVMArrayType arrayStorageType;
  if (typedArrayType) {
    len = createIntConstant(builder, location, i64Type, typedArrayType.getSize());
    arrayStorageType = mlir::LLVM::LLVMArrayType::get(typedArrayElemType, typedArrayType.getSize());
    auto llvmArray = builder.create<hew::BitcastOp>(location, arrayStorageType, collection);
    auto one = builder.create<mlir::arith::ConstantIntOp>(location, 1, 64);
    arrayAlloca =
        builder.create<mlir::LLVM::AllocaOp>(location, ptrType, arrayStorageType, one.getResult());
    builder.create<mlir::LLVM::StoreOp>(location, llvmArray, arrayAlloca);
  } else {
    len = builder.create<hew::VecLenOp>(location, i64Type, collection);
  }

  // Create index alloca (i64), initialized to 0
  auto memrefI64 = mlir::MemRefType::get({}, i64Type);
  mlir::Value indexAlloca = builder.create<mlir::memref::AllocaOp>(location, memrefI64);
  auto zero = createIntConstant(builder, location, i64Type, 0);
  builder.create<mlir::memref::StoreOp>(location, zero, indexAlloca);

  // Loop control flags (break/continue support)
  auto memrefI1 = mlir::MemRefType::get({}, i1Type);
  auto activeFlag = builder.create<mlir::memref::AllocaOp>(location, memrefI1);
  auto trueVal = createIntConstant(builder, location, i1Type, 1);
  builder.create<mlir::memref::StoreOp>(location, trueVal, activeFlag);

  auto continueFlag = builder.create<mlir::memref::AllocaOp>(location, memrefI1);
  auto falseVal = createIntConstant(builder, location, i1Type, 0);
  builder.create<mlir::memref::StoreOp>(location, falseVal, continueFlag);

  loopActiveStack.push_back(activeFlag);
  loopContinueStack.push_back(continueFlag);

  // scf.while loop
  auto whileOp =
      builder.create<mlir::scf::WhileOp>(location, mlir::TypeRange{}, mlir::ValueRange{});

  // Before region: check index < len && active && !returned
  auto *beforeBlock = builder.createBlock(&whileOp.getBefore());
  builder.setInsertionPointToStart(beforeBlock);

  auto isActive = builder.create<mlir::memref::LoadOp>(location, activeFlag, mlir::ValueRange{});
  mlir::Value curIdx =
      builder.create<mlir::memref::LoadOp>(location, indexAlloca, mlir::ValueRange{});
  auto cond =
      builder.create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::slt, curIdx, len);
  auto combinedCond = builder.create<mlir::arith::AndIOp>(location, isActive, cond);

  if (returnFlag) {
    auto flagVal = builder.create<mlir::memref::LoadOp>(location, returnFlag, mlir::ValueRange{});
    auto trueConst = createIntConstant(builder, location, i1Type, 1);
    auto notReturned = builder.create<mlir::arith::XOrIOp>(location, flagVal, trueConst);
    combinedCond = builder.create<mlir::arith::AndIOp>(location, combinedCond, notReturned);
  }

  builder.create<mlir::scf::ConditionOp>(location, combinedCond, mlir::ValueRange{});

  // After region: loop body
  auto *afterBlock = builder.createBlock(&whileOp.getAfter());
  builder.setInsertionPointToStart(afterBlock);

  // Reset continue flag
  builder.create<mlir::memref::StoreOp>(location, falseVal, continueFlag);

  {
    SymbolTableScopeT bodyScope(symbolTable);
    MutableTableScopeT bodyMutScope(mutableVars);

    // Load current index
    mlir::Value idx =
        builder.create<mlir::memref::LoadOp>(location, indexAlloca, mlir::ValueRange{});

    // Get element from collection based on type
    mlir::Value elem;
    {
      mlir::Type elemType;
      if (typedArrayType) {
        auto zero = builder.create<mlir::arith::ConstantIntOp>(location, 0, 64);
        auto elemPtr =
            builder.create<mlir::LLVM::GEPOp>(location, ptrType, arrayStorageType, arrayAlloca,
                                              mlir::ValueRange{zero.getResult(), idx});
        elem = builder.create<mlir::LLVM::LoadOp>(location, typedArrayElemType, elemPtr);
      } else {
        elemType = typedVecElemType ? typedVecElemType : stringVecElemType;
        if (!elemType) {
          emitError(location) << "unsupported for-loop Vec element type for iterable '" << collType
                              << "'";
          return;
        }
        elem = builder.create<hew::VecGetOp>(location, elemType, collection, idx);
      }
    }

    // Bind element to the pattern variable
    if (auto *identPat = std::get_if<ast::PatIdentifier>(&stmt.pattern.value.kind)) {
      auto patName = identPat->name;
      declareVariable(patName, elem);
      // Register loop variable for actor dispatch when iterating Vec<ActorRef<T>>
      if (isVecPtr && collType.find("Vec<ActorRef<") == 0) {
        auto start = std::string("Vec<ActorRef<").size();
        auto end = collType.rfind(">>");
        if (end != std::string::npos) {
          std::string innerActorName = collType.substr(start, end - start);
          actorVarTypes[patName] = innerActorName;
        }
      }
    }

    // Generate loop body
    pushDropScope();
    generateLoopBodyWithContinueGuards(stmt.body.stmts, 0, stmt.body.stmts.size(), continueFlag,
                                       location);
    popDropScope();

    // Increment index
    mlir::Value curI =
        builder.create<mlir::memref::LoadOp>(location, indexAlloca, mlir::ValueRange{});
    auto one = createIntConstant(builder, location, i64Type, 1);
    auto nextIdx = builder.create<mlir::arith::AddIOp>(location, curI, one);
    builder.create<mlir::memref::StoreOp>(location, nextIdx, indexAlloca);
  }

  // Ensure yield terminator
  auto *bodyBlock = builder.getInsertionBlock();
  if (bodyBlock->empty() || !bodyBlock->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
    builder.create<mlir::scf::YieldOp>(location);
  }

  loopActiveStack.pop_back();
  loopContinueStack.pop_back();

  builder.setInsertionPointAfter(whileOp);
}

void MLIRGen::generateForString(const ast::StmtFor &stmt, mlir::Value collection,
                                const std::string &collType) {
  generateForVec(stmt, collection, collType);
}

void MLIRGen::generateForHashMap(const ast::StmtFor &stmt, mlir::Value collection,
                                 const std::string &collType) {
  auto location = currentLoc;
  auto ptrType = mlir::LLVM::LLVMPointerType::get(&context);
  auto i64Type = builder.getI64Type();
  auto i1Type = builder.getI1Type();

  mlir::Type hmKeyType;
  mlir::Type hmValType;
  bool hasTypedHashMap = false;
  if (auto hmType = llvm::dyn_cast<hew::HashMapType>(collection.getType())) {
    hasTypedHashMap = true;
    hmKeyType = hmType.getKeyType();
    hmValType = hmType.getValueType();
  } else {
    hmKeyType = hew::StringRefType::get(&context);
    std::string valType;
    auto comma = collType.find(',');
    if (comma != std::string::npos) {
      auto rest = collType.substr(comma + 1);
      auto start = rest.find_first_not_of(' ');
      if (start != std::string::npos)
        rest = rest.substr(start);
      if (!rest.empty() && rest.back() == '>')
        rest.pop_back();
      valType = rest;
    }
    if (valType == "string" || valType == "String" || valType == "str") {
      hmValType = hew::StringRefType::get(&context);
    } else if (valType == "i64" || valType == "int" || valType == "Int") {
      hmValType = i64Type;
    } else if (valType == "f64" || valType == "float" || valType == "Float") {
      hmValType = builder.getF64Type();
    } else if (valType == "i32" || valType == "I32") {
      hmValType = builder.getI32Type();
    } else if (valType == "bool") {
      hmValType = i1Type;
    } else {
      emitError(location)
          << "cannot determine HashMap value type for iteration; add explicit type annotation";
      return;
    }
  }

  // Get keys as a Vec<K> via hew_hashmap_keys(map) -> !hew.vec<K>
  mlir::Type keysResultType = ptrType;
  if (hasTypedHashMap)
    keysResultType = hew::VecType::get(&context, hmKeyType);
  auto keysVec =
      builder.create<hew::HashMapKeysOp>(location, keysResultType, collection).getResult();

  // Get number of keys
  mlir::Value len = builder.create<hew::VecLenOp>(location, i64Type, keysVec);

  // Index alloca
  auto memrefI64 = mlir::MemRefType::get({}, i64Type);
  mlir::Value indexAlloca = builder.create<mlir::memref::AllocaOp>(location, memrefI64);
  auto zero = createIntConstant(builder, location, i64Type, 0);
  builder.create<mlir::memref::StoreOp>(location, zero, indexAlloca);

  // Loop control flags
  auto memrefI1 = mlir::MemRefType::get({}, i1Type);
  auto activeFlag = builder.create<mlir::memref::AllocaOp>(location, memrefI1);
  auto trueVal = createIntConstant(builder, location, i1Type, 1);
  builder.create<mlir::memref::StoreOp>(location, trueVal, activeFlag);

  auto continueFlag = builder.create<mlir::memref::AllocaOp>(location, memrefI1);
  auto falseVal = createIntConstant(builder, location, i1Type, 0);
  builder.create<mlir::memref::StoreOp>(location, falseVal, continueFlag);

  loopActiveStack.push_back(activeFlag);
  loopContinueStack.push_back(continueFlag);

  // scf.while loop
  auto whileOp =
      builder.create<mlir::scf::WhileOp>(location, mlir::TypeRange{}, mlir::ValueRange{});

  // Before region: check index < len && active && !returned
  auto *beforeBlock = builder.createBlock(&whileOp.getBefore());
  builder.setInsertionPointToStart(beforeBlock);

  auto isActive = builder.create<mlir::memref::LoadOp>(location, activeFlag, mlir::ValueRange{});
  mlir::Value curIdx =
      builder.create<mlir::memref::LoadOp>(location, indexAlloca, mlir::ValueRange{});
  auto cond =
      builder.create<mlir::arith::CmpIOp>(location, mlir::arith::CmpIPredicate::slt, curIdx, len);
  auto combinedCond = builder.create<mlir::arith::AndIOp>(location, isActive, cond);

  if (returnFlag) {
    auto flagVal = builder.create<mlir::memref::LoadOp>(location, returnFlag, mlir::ValueRange{});
    auto trueConst = createIntConstant(builder, location, i1Type, 1);
    auto notReturned = builder.create<mlir::arith::XOrIOp>(location, flagVal, trueConst);
    combinedCond = builder.create<mlir::arith::AndIOp>(location, combinedCond, notReturned);
  }

  builder.create<mlir::scf::ConditionOp>(location, combinedCond, mlir::ValueRange{});

  // After region: loop body
  auto *afterBlock = builder.createBlock(&whileOp.getAfter());
  builder.setInsertionPointToStart(afterBlock);

  builder.create<mlir::memref::StoreOp>(location, falseVal, continueFlag);

  {
    SymbolTableScopeT bodyScope(symbolTable);
    MutableTableScopeT bodyMutScope(mutableVars);

    // Load current index
    mlir::Value idx =
        builder.create<mlir::memref::LoadOp>(location, indexAlloca, mlir::ValueRange{});

    // Get key and value from hashmap
    mlir::Value key = builder.create<hew::VecGetOp>(location, hmKeyType, keysVec, idx);
    mlir::Value val =
        builder.create<hew::HashMapGetOp>(location, hmValType, collection, key).getResult();

    // Bind variables from the pattern
    if (auto *tuplePat = std::get_if<ast::PatTuple>(&stmt.pattern.value.kind)) {
      if (tuplePat->elements.size() >= 1) {
        if (auto *kIdent = std::get_if<ast::PatIdentifier>(&tuplePat->elements[0]->value.kind)) {
          declareVariable(kIdent->name, key);
        }
      }
      if (tuplePat->elements.size() >= 2) {
        if (auto *vIdent = std::get_if<ast::PatIdentifier>(&tuplePat->elements[1]->value.kind)) {
          declareVariable(vIdent->name, val);
        }
      }
    } else if (auto *identPat = std::get_if<ast::PatIdentifier>(&stmt.pattern.value.kind)) {
      declareVariable(identPat->name, key);
    }

    // Generate loop body
    pushDropScope();
    generateLoopBodyWithContinueGuards(stmt.body.stmts, 0, stmt.body.stmts.size(), continueFlag,
                                       location);
    popDropScope();

    // Increment index
    mlir::Value curI =
        builder.create<mlir::memref::LoadOp>(location, indexAlloca, mlir::ValueRange{});
    auto one = createIntConstant(builder, location, i64Type, 1);
    auto nextIdx = builder.create<mlir::arith::AddIOp>(location, curI, one);
    builder.create<mlir::memref::StoreOp>(location, nextIdx, indexAlloca);
  }

  // Ensure yield terminator
  auto *bodyBlock = builder.getInsertionBlock();
  if (bodyBlock->empty() || !bodyBlock->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
    builder.create<mlir::scf::YieldOp>(location);
  }

  loopActiveStack.pop_back();
  loopContinueStack.pop_back();

  builder.setInsertionPointAfter(whileOp);

  // Free the temporary keys vec
  builder.create<hew::VecFreeOp>(location, keysVec);
}

void MLIRGen::generateReturnStmt(const ast::StmtReturn &stmt) {
  auto location = currentLoc;

  // Check if we're directly inside the function body or nested in an SCF region
  auto *parentOp = builder.getInsertionBlock()->getParentOp();
  bool directlyInFunc = mlir::isa<mlir::func::FuncOp>(parentOp);

  if (!directlyInFunc && returnFlag) {
    // Inside an SCF region: store to return slot and set flag instead of
    // emitting func.return (which is invalid inside SCF ops).
    if (stmt.value && returnSlot) {
      auto val = generateExpression(stmt.value->value);
      if (val) {
        auto slotType = mlir::cast<mlir::MemRefType>(returnSlot.getType()).getElementType();
        val = coerceType(val, slotType, location);
        builder.create<mlir::memref::StoreOp>(location, val, returnSlot);
      }
    }
    auto trueVal = createIntConstant(builder, location, builder.getI1Type(), 1);
    builder.create<mlir::memref::StoreOp>(location, trueVal, returnFlag);
  } else {
    // At function top level: emit defers then drops before return
    emitDeferredCalls();
    if (stmt.value) {
      // Collect variable references from return expression to exclude from drops
      std::set<std::string> returnVarNames;
      if (auto *id = std::get_if<ast::ExprIdentifier>(&stmt.value->value.kind)) {
        returnVarNames.insert(id->name);
      } else if (auto *si = std::get_if<ast::ExprStructInit>(&stmt.value->value.kind)) {
        for (const auto &[fieldName, fieldVal] : si->fields) {
          if (auto *id = std::get_if<ast::ExprIdentifier>(&fieldVal->value.kind))
            returnVarNames.insert(id->name);
        }
      }
      if (!returnVarNames.empty())
        emitDropsExcept(returnVarNames);
      else
        emitAllDrops();
      auto val = generateExpression(stmt.value->value);
      if (val) {
        if (currentFunction && currentFunction.getResultTypes().size() == 1)
          val = coerceType(val, currentFunction.getResultTypes()[0], location);
        builder.create<mlir::func::ReturnOp>(location, mlir::ValueRange{val});
      } else {
        builder.create<mlir::func::ReturnOp>(location);
      }
    } else {
      emitAllDrops();
      builder.create<mlir::func::ReturnOp>(location);
    }
  }
}

void MLIRGen::generateExprStmt(const ast::StmtExpression &stmt) {
  generateExpression(stmt.expr.value);
}

// ============================================================================
// Loop statement generation
// ============================================================================

void MLIRGen::generateLoopStmt(const ast::StmtLoop &stmt) {
  auto location = currentLoc;

  // Create a mutable i1 flag for loop control
  auto i1Type = builder.getI1Type();
  auto memrefType = mlir::MemRefType::get({}, i1Type);
  auto activeFlag = builder.create<mlir::memref::AllocaOp>(location, memrefType);
  auto trueVal = createIntConstant(builder, location, i1Type, 1);
  builder.create<mlir::memref::StoreOp>(location, trueVal, activeFlag);

  // Also create a continue flag
  auto continueFlag = builder.create<mlir::memref::AllocaOp>(location, memrefType);
  auto falseVal = createIntConstant(builder, location, i1Type, 0);
  builder.create<mlir::memref::StoreOp>(location, falseVal, continueFlag);

  // Push onto loop control stacks
  loopActiveStack.push_back(activeFlag);
  loopContinueStack.push_back(continueFlag);

  // Register labeled loop flags
  std::string labelName;
  if (stmt.label) {
    labelName = *stmt.label;
    labeledActiveFlags[labelName] = activeFlag;
    labeledContinueFlags[labelName] = continueFlag;
  }

  // Break value alloca (nullptr sentinel — lazily allocated on first break-with-value)
  loopBreakValueStack.push_back(nullptr);

  // Build scf.while
  auto whileOp =
      builder.create<mlir::scf::WhileOp>(location, mlir::TypeRange{}, mlir::ValueRange{});

  // Before region: check active flag
  auto *beforeBlock = builder.createBlock(&whileOp.getBefore());
  builder.setInsertionPointToStart(beforeBlock);
  auto cond = builder.create<mlir::memref::LoadOp>(location, activeFlag, mlir::ValueRange{});
  builder.create<mlir::scf::ConditionOp>(location, cond, mlir::ValueRange{});

  // After region: loop body
  auto *afterBlock = builder.createBlock(&whileOp.getAfter());
  builder.setInsertionPointToStart(afterBlock);

  // Reset continue flag at start of each iteration
  builder.create<mlir::memref::StoreOp>(location, falseVal, continueFlag);

  {
    SymbolTableScopeT loopScope(symbolTable);
    MutableTableScopeT loopMutScope(mutableVars);
    pushDropScope();
    generateLoopBodyWithContinueGuards(stmt.body.stmts, 0, stmt.body.stmts.size(), continueFlag,
                                       location);
    popDropScope();
  }

  auto *bodyBlock = builder.getInsertionBlock();
  if (bodyBlock->empty() || !bodyBlock->back().hasTrait<mlir::OpTrait::IsTerminator>()) {
    builder.create<mlir::scf::YieldOp>(location);
  }

  // Save break value alloca before popping
  auto breakValueAlloca = loopBreakValueStack.back();

  // Pop loop control stacks
  loopActiveStack.pop_back();
  loopContinueStack.pop_back();
  loopBreakValueStack.pop_back();
  if (!labelName.empty()) {
    labeledActiveFlags.erase(labelName);
    labeledContinueFlags.erase(labelName);
  }

  builder.setInsertionPointAfter(whileOp);

  // Load break value if one was stored
  if (breakValueAlloca) {
    lastBreakValue =
        builder.create<mlir::memref::LoadOp>(location, breakValueAlloca, mlir::ValueRange{});
  } else {
    lastBreakValue = nullptr;
  }
}

void MLIRGen::generateBreakStmt(const ast::StmtBreak &stmt) {
  auto location = currentLoc;

  if (loopActiveStack.empty()) {
    emitError(location) << "break used outside of a loop";
    return;
  }

  // If break has a value, store it in the break value alloca
  if (stmt.value) {
    mlir::Value val = generateExpression(stmt.value->value);
    if (val && !loopBreakValueStack.empty()) {
      auto &breakAlloca = loopBreakValueStack.back();
      if (!breakAlloca) {
        // Lazily allocate the break value storage on first use
        auto savedIP = builder.saveInsertionPoint();
        auto &entryBlock = currentFunction.front();
        builder.setInsertionPointToStart(&entryBlock);
        auto memrefType = mlir::MemRefType::get({}, val.getType());
        breakAlloca = builder.create<mlir::memref::AllocaOp>(location, memrefType);
        builder.restoreInsertionPoint(savedIP);
      }
      builder.create<mlir::memref::StoreOp>(location, val, breakAlloca);
    }
  }

  // Determine which active/continue flags to set
  mlir::Value targetActive = loopActiveStack.back();
  mlir::Value targetContinue = loopContinueStack.empty() ? nullptr : loopContinueStack.back();

  if (stmt.label) {
    auto labelStr = *stmt.label;
    auto it = labeledActiveFlags.find(labelStr);
    if (it != labeledActiveFlags.end()) {
      targetActive = it->second;
    } else {
      emitError(location) << "unknown loop label '" << labelStr << "'";
      return;
    }
    auto cit = labeledContinueFlags.find(labelStr);
    if (cit != labeledContinueFlags.end()) {
      targetContinue = cit->second;
    }
  }

  // Drop variables in current scope before exiting loop
  if (!dropScopes.empty()) {
    emitDropsForScope(dropScopes.back());
    dropScopes.back().clear(); // Prevent double-drop on block exit
  }

  // Set the active flag to false (exit loop at condition check)
  auto i1Type = builder.getI1Type();
  auto falseVal = createIntConstant(builder, location, i1Type, 0);
  builder.create<mlir::memref::StoreOp>(location, falseVal, targetActive);

  // Also set continue flag so remaining body statements are skipped
  if (targetContinue) {
    auto trueVal = createIntConstant(builder, location, i1Type, 1);
    builder.create<mlir::memref::StoreOp>(location, trueVal, targetContinue);
  }

  // For labeled breaks, also break out of inner loops
  if (stmt.label && targetActive != loopActiveStack.back()) {
    builder.create<mlir::memref::StoreOp>(location, falseVal, loopActiveStack.back());
    if (!loopContinueStack.empty()) {
      auto trueVal = createIntConstant(builder, location, i1Type, 1);
      builder.create<mlir::memref::StoreOp>(location, trueVal, loopContinueStack.back());
    }
  }
}

void MLIRGen::generateContinueStmt(const ast::StmtContinue &stmt) {
  auto location = currentLoc;

  if (loopContinueStack.empty()) {
    emitError(location) << "continue used outside of a loop";
    return;
  }

  // Determine which continue flag to set
  mlir::Value targetContinue = loopContinueStack.back();

  if (stmt.label) {
    auto labelStr = *stmt.label;
    auto cit = labeledContinueFlags.find(labelStr);
    if (cit != labeledContinueFlags.end()) {
      targetContinue = cit->second;
    } else {
      emitError(location) << "unknown loop label '" << labelStr << "'";
      return;
    }
    // For labeled continue, also break inner loop's active flag
    if (targetContinue != loopContinueStack.back()) {
      auto i1Type = builder.getI1Type();
      auto falseVal = createIntConstant(builder, location, i1Type, 0);
      builder.create<mlir::memref::StoreOp>(location, falseVal, loopActiveStack.back());
    }
  }

  // Drop variables in current scope before continuing loop
  if (!dropScopes.empty()) {
    emitDropsForScope(dropScopes.back());
    dropScopes.back().clear(); // Prevent double-drop on block exit
  }

  // Set the continue flag to true
  auto i1Type = builder.getI1Type();
  auto trueVal = createIntConstant(builder, location, i1Type, 1);
  builder.create<mlir::memref::StoreOp>(location, trueVal, targetContinue);
  // Also set inner continue flag to skip remaining body statements
  if (targetContinue != loopContinueStack.back()) {
    builder.create<mlir::memref::StoreOp>(location, trueVal, loopContinueStack.back());
  }
}
