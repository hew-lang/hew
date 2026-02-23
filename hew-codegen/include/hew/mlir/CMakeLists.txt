# ── TableGen targets for the Hew MLIR dialect ────────────────────────────────
#
# Generated .inc files are placed in ${CMAKE_CURRENT_BINARY_DIR} which,
# because we add ${PROJECT_BINARY_DIR}/include to the include path in
# the top-level CMakeLists.txt, resolves to:
#   ${PROJECT_BINARY_DIR}/include/hew/mlir/HewOps.h.inc  etc.
#
# This lets C++ sources do:
#   #include "hew/mlir/HewOps.h.inc"

# ── Type definitions ─────────────────────────────────────────────────────────
set(LLVM_TARGET_DEFINITIONS HewTypes.td)
mlir_tablegen(HewTypes.h.inc -gen-typedef-decls -typedefs-dialect=hew)
mlir_tablegen(HewTypes.cpp.inc -gen-typedef-defs -typedefs-dialect=hew)
add_public_tablegen_target(HewTypesIncGen)

# ── Op definitions ───────────────────────────────────────────────────────────
set(LLVM_TARGET_DEFINITIONS HewOps.td)
mlir_tablegen(HewOps.h.inc -gen-op-decls)
mlir_tablegen(HewOps.cpp.inc -gen-op-defs)
mlir_tablegen(HewOpsDialect.h.inc -gen-dialect-decls -dialect=hew)
mlir_tablegen(HewOpsDialect.cpp.inc -gen-dialect-defs -dialect=hew)
add_public_tablegen_target(HewOpsIncGen)
