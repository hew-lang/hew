//===- msgpack_reader.h - Deserialize msgpack AST to C++ types -----------===//
//
// Reads a msgpack-encoded Hew AST (produced by Rust's rmp_serde::to_vec_named)
// into the C++ AST types defined in ast_types.h.
//
//===----------------------------------------------------------------------===//

#pragma once

#include "hew/ast_types.h"
#include <cstdint>
#include <string>
#include <vector>

namespace hew {

/// Parse a msgpack-encoded Hew Program AST.
///
/// Returns a fully-owned ast::Program.
/// Throws std::runtime_error on parse failures.
ast::Program parseMsgpackAST(const uint8_t *data, size_t size);

/// Parse a JSON-encoded Hew Program AST.
///
/// Parses the JSON into an in-memory representation, converts it to msgpack
/// bytes, and feeds those through the existing msgpack parser. This avoids
/// duplicating the deserialization logic while providing a human-readable
/// input format for debugging.
///
/// Throws std::runtime_error on parse failures.
ast::Program parseJsonAST(const uint8_t *data, size_t size);

} // namespace hew
