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
  #
  # Ubuntu's LLVM packages set LLVM_LINK_LLVM_DYLIB=ON, which means the
  # "LLVM" and "MLIR" CMake imported targets point to shared libraries
  # (libLLVM.so, libMLIR.so).  Every MLIR static library lists "LLVM" in
  # its INTERFACE_LINK_LIBRARIES, so the shared lib gets pulled in
  # transitively even when we want fully static linking.
  #
  # Fix: (1) neuter the shared LLVM/MLIR targets so they contribute nothing,
  #       (2) filter MLIR_ALL_LIBS to exclude shared/executable targets,
  #       (3) explicitly link the static LLVM component libraries.

  # (1) Filter MLIR_ALL_LIBS to only static library targets (exclude shared
  #     runtime libraries like MLIRExecutionEngineShared, mlir_c_runner_utils,
  #     and tool executables like mlir-opt).
  get_property(_all_mlir_libs GLOBAL PROPERTY MLIR_ALL_LIBS)
  set(MLIR_STATIC_LIBS "")
  foreach(_lib IN LISTS _all_mlir_libs)
    if(NOT TARGET ${_lib})
      continue()
    endif()
    get_target_property(_type ${_lib} TYPE)
    if(_type STREQUAL "STATIC_LIBRARY")
      list(APPEND MLIR_STATIC_LIBS ${_lib})
    endif()
  endforeach()

  # (2) Strip shared LLVM/MLIR targets from each static lib's transitive
  #     dependencies.  Ubuntu's packages set LLVM_LINK_LLVM_DYLIB=ON so the
  #     "LLVM" target points to libLLVM.so; we replace it with our explicit
  #     static component libraries below.
  foreach(_lib IN LISTS MLIR_STATIC_LIBS)
    get_target_property(_deps ${_lib} INTERFACE_LINK_LIBRARIES)
    if(_deps)
      list(REMOVE_ITEM _deps LLVM MLIR)
      set_target_properties(${_lib} PROPERTIES INTERFACE_LINK_LIBRARIES "${_deps}")
    endif()
  endforeach()

  # (3) LLVM components needed for codegen.  codegen.cpp calls
  #     InitializeAllTargets/AsmPrinters/AsmParsers so we need every backend
  #     that was built.  The "all-targets" pseudo-component doesn't resolve to
  #     individual static libraries when LLVM_LINK_LLVM_DYLIB=ON, so we expand
  #     it manually from LLVM_TARGETS_TO_BUILD.
  set(HEW_LLVM_COMPONENTS
    Core Support IRReader IRPrinter
    Passes Target TransformUtils Analysis
    MC MCParser MCDisassembler
    OrcJIT
    CodeGen AsmPrinter SelectionDAG GlobalISel
  )

  foreach(_target IN LISTS LLVM_TARGETS_TO_BUILD)
    list(APPEND HEW_LLVM_COMPONENTS ${_target}CodeGen ${_target}Desc ${_target}Info)
    # Not every target ships an AsmParser static library (e.g. NVPTX, SPIRV,
    # XCore).  Only add the component when the corresponding .a file exists.
    if(EXISTS "${LLVM_LIBRARY_DIR}/libLLVM${_target}AsmParser.a")
      list(APPEND HEW_LLVM_COMPONENTS ${_target}AsmParser)
    endif()
  endforeach()

  llvm_map_components_to_libnames(LLVM_STATIC_LIBS ${HEW_LLVM_COMPONENTS})

  target_link_libraries(HewMLIRDialect PUBLIC
    ${MLIR_STATIC_LIBS}
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
  if(NOT MSVC AND NOT APPLE)
    target_link_options(hew-codegen PRIVATE -s)
  endif()
  if(APPLE)
    add_custom_command(TARGET hew-codegen POST_BUILD
      COMMAND strip $<TARGET_FILE:hew-codegen>
      COMMENT "Stripping hew-codegen (macOS)")
  endif()
endif()
