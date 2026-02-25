# ── Hew MLIR dialect library ────────────────────────────────────────────────
add_library(HewMLIRDialect STATIC
  mlir/HewDialect.cpp
  mlir/HewOps.cpp
  mlir/MLIRGen.cpp
  mlir/MLIRGenStmt.cpp
  mlir/MLIRGenExpr.cpp
  mlir/MLIRGenMatch.cpp
  mlir/MLIRGenActor.cpp
  mlir/MLIRGenWire.cpp
  mlir/MLIRGenSupervisor.cpp
  msgpack_reader.cpp
)

target_include_directories(HewMLIRDialect PUBLIC
  ${PROJECT_SOURCE_DIR}/include
  ${PROJECT_BINARY_DIR}/include
)

target_link_libraries(HewMLIRDialect PUBLIC
  msgpack-cxx
  nlohmann_json::nlohmann_json
)

add_dependencies(HewMLIRDialect HewOpsIncGen HewTypesIncGen)

if(HEW_STATIC_LINK)
  # ── Static linking: use individual MLIR + LLVM static libraries ──────────
  # Get ALL MLIR static libraries
  get_property(MLIR_ALL_LIBS GLOBAL PROPERTY MLIR_ALL_LIBS)

  # LLVM components needed for codegen — backends added based on what was built
  set(HEW_LLVM_COMPONENTS
    Core Support IRReader IRPrinter
    Passes Target TransformUtils Analysis
    MC MCParser MCDisassembler
    OrcJIT native
    CodeGen AsmPrinter SelectionDAG GlobalISel
  )

  # Add backends that are available in this LLVM build
  foreach(_target X86 AArch64 ARM WebAssembly)
    if("${_target}" IN_LIST LLVM_TARGETS_TO_BUILD)
      list(APPEND HEW_LLVM_COMPONENTS
        ${_target}CodeGen ${_target}AsmParser ${_target}Desc ${_target}Info)
    endif()
  endforeach()

  llvm_map_components_to_libnames(LLVM_STATIC_LIBS ${HEW_LLVM_COMPONENTS})

  target_link_libraries(HewMLIRDialect PUBLIC
    ${MLIR_ALL_LIBS}
    ${LLVM_STATIC_LIBS}
  )
else()
  # ── Shared linking (default, for development) ────────────────────────────
  # Link against the shared libMLIR.so instead of individual static .a files.
  # Mixing static MLIR .a with shared libLLVM.so causes symbol conflicts
  # (e.g. translateModuleToLLVMIR infinite loops on llvm.fadd).
  find_library(MLIR_SHARED_LIB MLIR PATHS ${LLVM_LIBRARY_DIR} NO_DEFAULT_PATH)
  find_library(LLVM_SHARED_LIB LLVM PATHS ${LLVM_LIBRARY_DIR} NO_DEFAULT_PATH)

  target_link_libraries(HewMLIRDialect PUBLIC
    ${MLIR_SHARED_LIB}
    ${LLVM_SHARED_LIB}
  )
endif()

# ── Hew codegen library ───────────────────────────────────────────────────
add_library(HewCodegen STATIC
  codegen.cpp
)

target_include_directories(HewCodegen PUBLIC
  ${PROJECT_SOURCE_DIR}/include
  ${PROJECT_BINARY_DIR}/include
)

add_dependencies(HewCodegen HewOpsIncGen HewTypesIncGen)

target_link_libraries(HewCodegen PUBLIC
  HewMLIRDialect
)

# ── hew-codegen executable (msgpack AST → object file) ───────────────────────
add_executable(hew-codegen
  codegen_main.cpp
)

target_link_libraries(hew-codegen PRIVATE
  HewCodegen
  HewMLIRDialect
)

target_include_directories(hew-codegen PRIVATE
  ${PROJECT_SOURCE_DIR}/include
  ${PROJECT_BINARY_DIR}/include
)

if(HEW_STATIC_LINK AND CMAKE_BUILD_TYPE STREQUAL "Release")
  if(NOT MSVC)
    target_link_options(hew-codegen PRIVATE -s)
  endif()
endif()
