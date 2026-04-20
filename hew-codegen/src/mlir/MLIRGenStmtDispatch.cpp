//===- MLIRGenStmtDispatch.cpp - Statement dispatcher for Hew MLIRGen -----===//
//
// Split statement dispatch helpers for the MLIR generator.
//
//===----------------------------------------------------------------------===//

#include "hew/mlir/MLIRGen.h"

#include "llvm/Support/ErrorHandling.h"

#include <type_traits>

using namespace hew;

namespace {
template <typename T> inline constexpr bool always_false_v = false;
} // namespace

void MLIRGen::generateForStmtDispatch(const ast::StmtFor &stmt) {
  if (stmt.is_await)
    generateForAwaitStmt(stmt);
  else
    generateForStmt(stmt);
}

void MLIRGen::generateDeferStmt(const ast::StmtDefer &stmt) {
  if (stmt.expr)
    currentFnDefers.emplace_back(&stmt.expr->value, currentLoc);
}

void MLIRGen::generateStatement(const ast::Stmt &stmt) {
  currentLoc = loc(stmt.span);
  std::visit(
      [this](const auto &node) {
        using NodeT = std::decay_t<decltype(node)>;
        if constexpr (std::is_same_v<NodeT, ast::StmtLet>) {
          generateLetStmt(node);
        } else if constexpr (std::is_same_v<NodeT, ast::StmtVar>) {
          generateVarStmt(node);
        } else if constexpr (std::is_same_v<NodeT, ast::StmtAssign>) {
          generateAssignStmt(node);
        } else if constexpr (std::is_same_v<NodeT, ast::StmtIf>) {
          generateIfStmt(node);
        } else if constexpr (std::is_same_v<NodeT, ast::StmtIfLet>) {
          generateIfLetStmt(node);
        } else if constexpr (std::is_same_v<NodeT, ast::StmtMatch>) {
          generateMatchStmt(node);
        } else if constexpr (std::is_same_v<NodeT, ast::StmtLoop>) {
          generateLoopStmt(node);
        } else if constexpr (std::is_same_v<NodeT, ast::StmtFor>) {
          generateForStmtDispatch(node);
        } else if constexpr (std::is_same_v<NodeT, ast::StmtWhile>) {
          generateWhileStmt(node);
        } else if constexpr (std::is_same_v<NodeT, ast::StmtWhileLet>) {
          generateWhileLetStmt(node);
        } else if constexpr (std::is_same_v<NodeT, ast::StmtBreak>) {
          generateBreakStmt(node);
        } else if constexpr (std::is_same_v<NodeT, ast::StmtContinue>) {
          generateContinueStmt(node);
        } else if constexpr (std::is_same_v<NodeT, ast::StmtReturn>) {
          generateReturnStmt(node);
        } else if constexpr (std::is_same_v<NodeT, ast::StmtDefer>) {
          generateDeferStmt(node);
        } else if constexpr (std::is_same_v<NodeT, ast::StmtExpression>) {
          generateExprStmt(node);
        } else {
          static_assert(always_false_v<NodeT>, "unhandled statement kind in generateStatement");
        }
      },
      stmt.kind);
}
