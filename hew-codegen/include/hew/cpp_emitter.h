#pragma once

#include "hew/ast_types.h"

#include <string>

namespace hew {

/// Emit C++20 source for a subset of Hew's typed AST.
///
/// Throws `std::runtime_error` when the program uses a construct that the
/// experimental C++ backend does not yet support.
std::string emitCppSource(const ast::Program &program);

} // namespace hew
