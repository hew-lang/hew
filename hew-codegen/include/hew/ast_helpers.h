//===- ast_helpers.h - Utility helpers for Hew AST types --------*- C++ -*-===//
//
// Convenience functions for working with CallArg and other AST types.
//
//===----------------------------------------------------------------------===//

#pragma once

#include "hew/ast_types.h"

#include <string>

namespace hew {
namespace ast {

/// Extract the expression from a CallArg (positional or named).
inline const Spanned<Expr> &callArgExpr(const CallArg &arg) {
  if (auto *pos = std::get_if<CallArgPositional>(&arg))
    return *pos->expr;
  return *std::get_if<CallArgNamed>(&arg)->value;
}

/// Get the name from a CallArg, or empty string if positional.
inline std::string callArgName(const CallArg &arg) {
  if (auto *named = std::get_if<CallArgNamed>(&arg))
    return named->name;
  return {};
}

} // namespace ast
} // namespace hew
