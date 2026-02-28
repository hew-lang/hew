# ── Hew MLIR dialect library ────────────────────────────────────────────────
add_library(HewMLIRDialect STATIC
  mlir/HewDialect.cpp
  mlir/HewOps.cpp
  mlir/MLIRGen.cpp
  mlir/MLIRGenStmt.cpp
  mlir/MLIRGenExpr.cpp
  mlir/MLIRGenMatch.cpp
  mlir/MLIRGenIfLet.cpp
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
  # ── Static linking: curated MLIR + LLVM static libraries ────────────────
  # We link ONLY the MLIR components hew-codegen actually uses.
  # Using MLIR_ALL_LIBS pulls in MLIRJitRunner / MLIRExecutionEngine which
  # depend on shared-only runtime libraries (libmlir_c_runner_utils.so, etc.)
  # that don't have static counterparts and break portable distribution.

  # Dialects used by our MLIR codegen
  set(HEW_MLIR_LIBS
    MLIRFuncDialect
    MLIRArithDialect
    MLIRMemRefDialect
    MLIRLLVMDialect
    MLIRSCFDialect
    MLIRControlFlowDialect
    MLIRUBDialect
    MLIRMathDialect
    # Conversion passes (Hew dialect → LLVM IR)
    MLIRFuncToLLVM
    MLIRArithToLLVM
    MLIRMemRefToLLVM
    MLIRControlFlowToLLVM
    MLIRSCFToControlFlow
    MLIRReconcileUnrealizedCasts
    MLIRLLVMCommonConversion
    # Translation to LLVM IR
    MLIRTargetLLVMIRExport
    MLIRBuiltinToLLVMIRTranslation
    MLIRLLVMToLLVMIRTranslation
    # Core infrastructure
    MLIRIR
    MLIRPass
    MLIRTransforms
    MLIRTransformUtils
    MLIRAnalysis
    MLIRParser
    MLIRSupport
    MLIRRewrite
    MLIRDialect
    MLIRBytecodeWriter
    MLIRBytecodeReader
    MLIRBytecodeOpInterface
    MLIRAsmParser
    MLIRTranslateLib
    # Interfaces required by the above
    MLIRSideEffectInterfaces
    MLIRCallInterfaces
    MLIRFunctionInterfaces
    MLIRInferTypeOpInterface
    MLIRControlFlowInterfaces
    MLIRCastInterfaces
    MLIRDataLayoutInterfaces
    MLIRLoopLikeInterface
    MLIRViewLikeInterface
    MLIRDestinationStyleOpInterface
    MLIRSubsetOpInterface
    MLIRValueBoundsOpInterface
    MLIRShapedOpInterfaces
    MLIRMemorySlotInterfaces
    MLIRRuntimeVerifiableOpInterface
    MLIRInferIntRangeInterface
    MLIRInferIntRangeCommon
    MLIRDialectUtils
    # Transforms used by dialect conversions
    MLIRFuncTransforms
    MLIRSCFTransforms
    MLIRSCFUtils
    MLIRArithTransforms
    MLIRArithUtils
    MLIRMemRefTransforms
    # PDL rewrite engine (needed by some conversion passes)
    MLIRRewritePDL
    MLIRPDLToPDLInterp
    MLIRPDLInterpDialect
    MLIRPDLDialect
    # LLVM IR transforms
    MLIRLLVMIRTransforms
    MLIRArithAttrToLLVMConversion
    MLIRPresburger
  )

  # LLVM components — all backends needed since codegen calls InitializeAllTargets()
  set(HEW_LLVM_COMPONENTS
    Core Support IRReader IRPrinter
    Passes Target TransformUtils Analysis
    MC MCParser MCDisassembler
    OrcJIT
    CodeGen AsmPrinter SelectionDAG GlobalISel
    AllTargetsAsmParsers AllTargetsCodeGens AllTargetsDescs AllTargetsInfos
  )

  llvm_map_components_to_libnames(LLVM_STATIC_LIBS ${HEW_LLVM_COMPONENTS})

  # Redirect the monolithic shared LLVM target to a static archive.
  # Every MLIR target lists "LLVM" (shared) in INTERFACE_LINK_LIBRARIES.
  # We redirect it to a static LLVMSupport (a harmless duplicate since we
  # already link all needed static LLVM components) and clear its interface deps.
  #
  # IMPORTANT: We must override IMPORTED_LOCATION_<CONFIG> (e.g. _RELEASE)
  # in addition to IMPORTED_LOCATION, because CMake prefers config-specific
  # properties.  Without this, a reconfigure from shared→static silently
  # keeps linking libLLVM.dylib, producing a binary that mixes static and
  # shared LLVM symbols and segfaults at runtime (PassManager vtable
  # corruption from duplicate global state).
  if(TARGET LLVM)
    get_target_property(_llvm_type LLVM TYPE)
    if(_llvm_type STREQUAL "SHARED_LIBRARY" OR _llvm_type STREQUAL "UNKNOWN_LIBRARY")
      if(MSVC)
        set(_llvm_support_lib "${LLVM_LIBRARY_DIR}/LLVMSupport.lib")
      else()
        set(_llvm_support_lib "${LLVM_LIBRARY_DIR}/libLLVMSupport.a")
      endif()
      # Override config-specific imported locations (e.g. IMPORTED_LOCATION_RELEASE)
      # that would otherwise take precedence over the generic IMPORTED_LOCATION.
      get_target_property(_llvm_configs LLVM IMPORTED_CONFIGURATIONS)
      if(_llvm_configs)
        foreach(_cfg IN LISTS _llvm_configs)
          set_target_properties(LLVM PROPERTIES
            "IMPORTED_LOCATION_${_cfg}" "${_llvm_support_lib}"
            "IMPORTED_SONAME_${_cfg}" "")
        endforeach()
      endif()
      set_target_properties(LLVM PROPERTIES
        IMPORTED_LOCATION "${_llvm_support_lib}"
        IMPORTED_SONAME ""
        INTERFACE_LINK_LIBRARIES "")
    endif()
  endif()

  target_link_libraries(HewMLIRDialect PUBLIC
    ${HEW_MLIR_LIBS}
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
    # Statically link libstdc++ and libgcc for maximum portability.
    # Only libc, libm, libz, libzstd remain as dynamic deps (available everywhere).
    target_link_options(hew-codegen PRIVATE -s -static-libgcc -static-libstdc++)
  endif()
  if(APPLE)
    add_custom_command(TARGET hew-codegen POST_BUILD
      COMMAND strip $<TARGET_FILE:hew-codegen>
      COMMENT "Stripping hew-codegen (macOS)")
  endif()
endif()
