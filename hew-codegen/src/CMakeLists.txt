# ── Hew MLIR dialect library ────────────────────────────────────────────────
add_library(HewMLIRDialect STATIC
  mlir/HewDialect.cpp
  mlir/HewOps.cpp
  mlir/MLIRGen.cpp
  mlir/MLIRGenFunction.cpp
  mlir/MLIRGenStmt.cpp
  mlir/MLIRGenStmtDispatch.cpp
  mlir/MLIRGenExpr.cpp
  mlir/MLIRGenExprDispatch.cpp
  mlir/MLIRGenMatch.cpp
  mlir/MLIRGenIfLet.cpp
  mlir/MLIRGenWhileLet.cpp
  mlir/MLIRGenActor.cpp
  mlir/MLIRGenWire.cpp
  mlir/MLIRGenSupervisor.cpp
  msgpack_reader.cpp
)

# msgpack_reader.cpp throws std::runtime_error on parse failures.
# Ensure exceptions are enabled even when LLVM_ENABLE_EH is OFF.
target_compile_options(HewMLIRDialect PRIVATE
  $<$<NOT:$<CXX_COMPILER_ID:MSVC>>:-fexceptions>
  $<$<CXX_COMPILER_ID:MSVC>:/EHsc>
)

target_include_directories(HewMLIRDialect PUBLIC
  ${PROJECT_SOURCE_DIR}/include
  ${PROJECT_BINARY_DIR}/include
)

target_link_libraries(HewMLIRDialect PUBLIC
  msgpack-cxx
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
    MLIRMathToLLVM
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

  # Strip phantom shared-lib entries from LLVMSupport's CMake interface.
  # LLVM APT packages compile LLVMSupport with z3 enabled (unused by hew)
  # and export zlib/zstd as shared libs.  On Linux we remove all three and
  # re-link static archives so their symbols resolve from .a files rather
  # than creating NEEDED entries.  On other platforms only z3 is stripped
  # (macOS/FreeBSD bundle zlib/zstd into LLVM archives or link them via
  # the system linker and must not have those entries removed).
  #   z3:   never used — constraint passes we don't run (all platforms)
  #   zlib: needed by LLVM IR reader — replaced by libz.a on Linux
  #   zstd: needed by LLVM bitcode — replaced by libzstd.a on Linux
  if(TARGET LLVMSupport)
    get_target_property(_llvm_support_iface LLVMSupport INTERFACE_LINK_LIBRARIES)
    if(_llvm_support_iface)
      if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
        list(FILTER _llvm_support_iface EXCLUDE REGEX "libz3|ZLIB|[Zz]std|libz\\.so")
      else()
        list(FILTER _llvm_support_iface EXCLUDE REGEX "libz3")
      endif()
      set_target_properties(LLVMSupport PROPERTIES
        INTERFACE_LINK_LIBRARIES "${_llvm_support_iface}")
    endif()
  endif()

  # On Linux only, find static zlib and zstd archives and link them after the
  # LLVM/MLIR component archives so GNU ld resolves their symbols statically.
  # zlib1g-dev and libzstd-dev provide the .a files on Debian/Ubuntu.
  # On Alpine the symbols are compiled into the LLVM archives themselves
  # (LLVM built from source with zlib-static/zstd-static), so the .a find
  # is a harmless no-op there.
  # FreeBSD and macOS are excluded: FreeBSD builds LLVM with ZSTD=OFF and
  # uses libc++; macOS bundles its own zlib/zstd via Homebrew.
  if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
    set(_save_sfx "${CMAKE_FIND_LIBRARY_SUFFIXES}")
    set(CMAKE_FIND_LIBRARY_SUFFIXES ".a")
    find_library(_HEW_ZLIB_STATIC z)
    find_library(_HEW_ZSTD_STATIC zstd)
    set(CMAKE_FIND_LIBRARY_SUFFIXES "${_save_sfx}")
    unset(_save_sfx)
  endif()

  target_link_libraries(HewMLIRDialect PUBLIC
    ${HEW_MLIR_LIBS}
    ${LLVM_STATIC_LIBS}
  )

  if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
    if(_HEW_ZLIB_STATIC)
      target_link_libraries(HewMLIRDialect PUBLIC "${_HEW_ZLIB_STATIC}")
    else()
      message(WARNING "HEW_STATIC_LINK: libz.a not found — zlib will remain a shared dependency")
      target_link_libraries(HewMLIRDialect PUBLIC z)
    endif()
    if(_HEW_ZSTD_STATIC)
      target_link_libraries(HewMLIRDialect PUBLIC "${_HEW_ZSTD_STATIC}")
    else()
      message(WARNING "HEW_STATIC_LINK: libzstd.a not found — zstd will remain a shared dependency")
      target_link_libraries(HewMLIRDialect PUBLIC zstd)
    endif()
  endif()
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
set(HEW_GENERATED_JIT_STABLE_SYMBOLS
  ${PROJECT_BINARY_DIR}/include/hew/generated_jit_stable_symbols.h
)
file(GLOB_RECURSE HEW_RUNTIME_RUST_SOURCES CONFIGURE_DEPENDS
  ${CMAKE_SOURCE_DIR}/../hew-runtime/src/*.rs
)

add_custom_command(
  OUTPUT ${HEW_GENERATED_JIT_STABLE_SYMBOLS}
  COMMAND ${Python3_EXECUTABLE} ${CMAKE_SOURCE_DIR}/../scripts/verify-ffi-symbols.py
          --emit-cpp-header ${HEW_GENERATED_JIT_STABLE_SYMBOLS}
          --validate
  DEPENDS
    ${CMAKE_SOURCE_DIR}/../scripts/verify-ffi-symbols.py
    ${CMAKE_SOURCE_DIR}/../scripts/jit-symbol-classification.toml
    ${HEW_RUNTIME_RUST_SOURCES}
  WORKING_DIRECTORY ${CMAKE_SOURCE_DIR}/..
  COMMENT "Generating Hew JIT stable symbol header"
  VERBATIM
)

add_custom_target(HewJitStableSymbolHeader DEPENDS ${HEW_GENERATED_JIT_STABLE_SYMBOLS})

add_library(HewCodegen STATIC
  codegen.cpp
  debug_info.cpp
  jit_symbol_map.cpp
  jit_session.cpp
)

target_include_directories(HewCodegen PUBLIC
  ${PROJECT_SOURCE_DIR}/include
  ${PROJECT_BINARY_DIR}/include
)

add_dependencies(HewCodegen HewOpsIncGen HewTypesIncGen HewJitStableSymbolHeader)

target_link_libraries(HewCodegen PUBLIC
  HewMLIRDialect
)

# ── Embedded C API for Rust CLI cutover ─────────────────────────────────────
add_library(HewCodegenCAPI STATIC
  codegen_capi.cpp
)

# codegen_capi.cpp uses try/catch for error handling across the C FFI
# boundary. Ensure exceptions are enabled even when LLVM_ENABLE_EH is OFF.
target_compile_options(HewCodegenCAPI PRIVATE
  $<$<NOT:$<CXX_COMPILER_ID:MSVC>>:-fexceptions>
  $<$<CXX_COMPILER_ID:MSVC>:/EHsc>
)

target_link_libraries(HewCodegenCAPI PUBLIC
  HewCodegen
  HewMLIRDialect
)

target_include_directories(HewCodegenCAPI PUBLIC
  ${PROJECT_SOURCE_DIR}/include
  ${PROJECT_BINARY_DIR}/include
)

set_target_properties(HewCodegenCAPI PROPERTIES
  ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_BINARY_DIR}/lib
)

set(HEW_EMBEDDED_CARGO_FILE "${PROJECT_BINARY_DIR}/hew_embedded_codegen.cargo")
file(WRITE "${HEW_EMBEDDED_CARGO_FILE}" "")

function(hew_embedded_append line)
  file(APPEND "${HEW_EMBEDDED_CARGO_FILE}" "${line}\n")
endfunction()

function(hew_shared_lib_name out_var path)
  get_filename_component(_name "${path}" NAME)
  string(REGEX REPLACE "^lib" "" _name "${_name}")
  string(REGEX REPLACE "\\.(so|dylib|dll|lib)(\\..*)?$" "" _name "${_name}")
  set(${out_var} "${_name}" PARENT_SCOPE)
endfunction()

function(hew_emit_system_link_items)
  foreach(_item IN LISTS ARGN)
    if("${_item}" STREQUAL "")
      continue()
    endif()
    if("${_item}" MATCHES "z3")
      continue()
    endif()
    if(HEW_STATIC_LINK AND
       (CMAKE_SYSTEM_NAME STREQUAL "Linux" OR APPLE) AND
       ("${_item}" STREQUAL "-lz" OR "${_item}" STREQUAL "-lzstd"))
      # On Linux and macOS static builds, zlib and zstd are linked as
      # static archives directly — skip the dynamic -l flags from
      # llvm-config --system-libs.
      continue()
    endif()
    if("${_item}" MATCHES "^-l(.+)$")
      set(_lib "${CMAKE_MATCH_1}")
      if("${_lib}" STREQUAL "stdc++" OR "${_lib}" STREQUAL "c++"
         OR "${_lib}" STREQUAL "c++abi")
        # C++ runtime handled by hew_emit_cxx_runtime() or static linking.
        continue()
      endif()
      # On Linux static builds, system libs must be link-args (not
      # link-libs) so they appear AFTER the LLVM/MLIR archive group.
      # Cargo places link-lib entries before link-arg entries, which
      # on aarch64 causes -lm and friends to be scanned before MLIR
      # archives that reference them (tan, log2, etc.).
      if(HEW_STATIC_LINK AND CMAKE_SYSTEM_NAME STREQUAL "Linux")
        hew_embedded_append("cargo:rustc-link-arg=-l${_lib}")
      else()
        hew_embedded_append("cargo:rustc-link-lib=${_lib}")
      endif()
    elseif(IS_ABSOLUTE "${_item}")
      hew_embedded_append("cargo:rustc-link-arg=${_item}")
    else()
      hew_embedded_append("cargo:rustc-link-arg=${_item}")
    endif()
  endforeach()
endfunction()

function(hew_emit_cxx_runtime)
  if(HEW_STATIC_LINK AND NOT MSVC)
    if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU" OR
       (CMAKE_CXX_COMPILER_ID MATCHES "Clang" AND
        CMAKE_SYSTEM_NAME STREQUAL "Linux"))
      # On Linux static builds, libstdc++.a is already included inside the
      # --start-group/--end-group archive block (see above) to resolve
      # circular dependencies with MLIR/LLVM and libatomic.  Skip here.
      return()
    endif()
    if(APPLE)
      # On macOS static builds, libc++.a is linked directly (see above).
      # Do not add a dynamic link directive.
      return()
    endif()
  endif()

  if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
    hew_embedded_append("cargo:rustc-link-lib=dylib=stdc++")
  elseif(APPLE OR CMAKE_SYSTEM_NAME STREQUAL "FreeBSD")
    hew_embedded_append("cargo:rustc-link-lib=dylib=c++")
  endif()
endfunction()

find_program(LLVM_CONFIG_EXE
  NAMES llvm-config llvm-config-22
  PATHS "${LLVM_TOOLS_BINARY_DIR}" "${LLVM_DIR}/../../../bin"
  NO_DEFAULT_PATH)
if(NOT LLVM_CONFIG_EXE)
  find_program(LLVM_CONFIG_EXE NAMES llvm-config llvm-config-22)
endif()

hew_embedded_append("cargo:rustc-link-search=native=${PROJECT_BINARY_DIR}/lib")
hew_embedded_append("cargo:rustc-link-search=native=${PROJECT_BINARY_DIR}/src")
hew_embedded_append("cargo:rustc-link-lib=static=HewCodegenCAPI")
hew_embedded_append("cargo:rustc-link-lib=static=HewCodegen")
hew_embedded_append("cargo:rustc-link-lib=static=HewMLIRDialect")

if(HEW_STATIC_LINK)
  hew_embedded_append("cargo:rustc-link-search=native=${LLVM_LIBRARY_DIR}")

  # Glob all MLIR and LLVM static archives from the library directory.
  # Both llvm_map_components_to_libnames() and hand-curated MLIR lists
  # miss transitive dependencies.  With --start-group/--end-group the
  # linker resolves cross-archive references and drops unused archives,
  # so including the full set is safe and costs only link time.
  if(MSVC)
    file(GLOB _hew_all_mlir_archives "${LLVM_LIBRARY_DIR}/MLIR*.lib")
    file(GLOB _hew_all_llvm_archives "${LLVM_LIBRARY_DIR}/LLVM*.lib")
  else()
    file(GLOB _hew_all_mlir_archives "${LLVM_LIBRARY_DIR}/libMLIR*.a")
    file(GLOB _hew_all_llvm_archives "${LLVM_LIBRARY_DIR}/libLLVM*.a")
  endif()

  # On Linux, MLIR/LLVM static archives have circular dependencies
  # with each other AND with libstdc++.a (C++ runtime) and libatomic.a
  # (outline atomics on aarch64).  GNU ld requires --start-group/
  # --end-group to resolve circular references via iterative scanning.
  #
  # All mutually-dependent archives must be INSIDE the group:
  #   - MLIR/LLVM .a files (circular deps among themselves)
  #   - libstdc++.a (__cxa_guard_* references __aarch64_swp4_acq_rel
  #     from libgcc; MLIR/LLVM reference __cxa_* from libstdc++)
  #   - libgcc.a (aarch64 outline atomics: __aarch64_cas1_acq_rel,
  #     __aarch64_swp1_acq, __aarch64_ldset4_relax — these are GCC
  #     builtins for -moutline-atomics, NOT in libatomic.a)
  #
  # We use absolute paths instead of -Bstatic/-l pairs because -Bstatic
  # forces ALL symbol resolution to static mode, breaking glibc symbols
  # like __stack_chk_guard on aarch64 (lives only in ld-linux-aarch64).
  #
  # Cargo places rustc-link-lib before rustc-link-arg, so everything
  # that needs ordering control must be emitted as link-args.
  if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
    # Find absolute paths for libraries that participate in circular deps.
    # libstdc++.a: C++ runtime (__cxa_guard_* uses outline atomics on aarch64)
    # libgcc.a:    outline atomics (__aarch64_cas1_acq_rel, __aarch64_swp1_acq,
    #              __aarch64_ldset4_relax, etc.) — NOT in libatomic.a!
    execute_process(
      COMMAND "${CMAKE_CXX_COMPILER}" -print-file-name=libstdc++.a
      OUTPUT_VARIABLE _hew_libstdcpp_group
      OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_QUIET)
    execute_process(
      COMMAND "${CMAKE_CXX_COMPILER}" -print-file-name=libgcc.a
      OUTPUT_VARIABLE _hew_libgcc_group
      OUTPUT_STRIP_TRAILING_WHITESPACE ERROR_QUIET)

    hew_embedded_append("cargo:rustc-link-arg=-Wl,--start-group")
    foreach(_archive ${_hew_all_mlir_archives} ${_hew_all_llvm_archives})
      get_filename_component(_name "${_archive}" NAME)
      string(REGEX REPLACE "^lib" "" _name "${_name}")
      string(REGEX REPLACE "\\.a$" "" _name "${_name}")
      if(NOT "${_name}" MATCHES "^Polly")
        hew_embedded_append("cargo:rustc-link-arg=${_archive}")
      endif()
    endforeach()
    # Include libstdc++ and libgcc inside the group so their circular
    # dependencies with MLIR/LLVM archives resolve via iterative scanning.
    # On aarch64, MLIR/LLVM archives and libstdc++ reference outline atomic
    # builtins from libgcc.a; libstdc++ also references MLIR-provided symbols.
    if(_hew_libstdcpp_group AND NOT _hew_libstdcpp_group STREQUAL "libstdc++.a")
      hew_embedded_append("cargo:rustc-link-arg=${_hew_libstdcpp_group}")
    endif()
    if(_hew_libgcc_group AND NOT _hew_libgcc_group STREQUAL "libgcc.a")
      hew_embedded_append("cargo:rustc-link-arg=${_hew_libgcc_group}")
    endif()
    hew_embedded_append("cargo:rustc-link-arg=-Wl,--end-group")
    # Ensure libc is available for stack protector DSO on aarch64
    # (__stack_chk_guard lives in ld-linux-aarch64, resolved via -lc).
    hew_embedded_append("cargo:rustc-link-arg=-Wl,--no-as-needed")
    hew_embedded_append("cargo:rustc-link-arg=-lc")
    hew_embedded_append("cargo:rustc-link-arg=-Wl,--as-needed")
  else()
    foreach(_archive ${_hew_all_mlir_archives} ${_hew_all_llvm_archives})
      get_filename_component(_name "${_archive}" NAME)
      if(MSVC)
        string(REGEX REPLACE "\\.lib$" "" _name "${_name}")
      else()
        string(REGEX REPLACE "^lib" "" _name "${_name}")
        string(REGEX REPLACE "\\.a$" "" _name "${_name}")
      endif()
      if(NOT "${_name}" MATCHES "^Polly")
        hew_embedded_append("cargo:rustc-link-lib=static=${_name}")
      endif()
    endforeach()
  endif()

  # Platform-specific static compression libraries (Linux only).
  # These must be link-args (not link-libs) to maintain ordering after
  # the LLVM/MLIR archive group that references them.
  if(CMAKE_SYSTEM_NAME STREQUAL "Linux")
    if(_HEW_ZLIB_STATIC)
      get_filename_component(_hew_zlib_dir "${_HEW_ZLIB_STATIC}" DIRECTORY)
      hew_embedded_append("cargo:rustc-link-search=native=${_hew_zlib_dir}")
      hew_embedded_append("cargo:rustc-link-arg=-lz")
    else()
      hew_embedded_append("cargo:rustc-link-arg=-lz")
    endif()
    if(_HEW_ZSTD_STATIC)
      get_filename_component(_hew_zstd_dir "${_HEW_ZSTD_STATIC}" DIRECTORY)
      hew_embedded_append("cargo:rustc-link-search=native=${_hew_zstd_dir}")
      hew_embedded_append("cargo:rustc-link-arg=-lzstd")
    else()
      hew_embedded_append("cargo:rustc-link-arg=-lzstd")
    endif()
  endif()

  # System libraries required by LLVM (rt, dl, m, xml2, etc.)
  if(NOT LLVM_CONFIG_EXE)
    message(FATAL_ERROR
      "HEW_STATIC_LINK=ON requires llvm-config to determine system library "
      "dependencies. Set LLVM_CONFIG_EXE or ensure llvm-config is on PATH.")
  endif()
  execute_process(
    COMMAND "${LLVM_CONFIG_EXE}" --system-libs --link-static
    OUTPUT_VARIABLE HEW_LLVM_SYSTEM_LIBS_RAW
    OUTPUT_STRIP_TRAILING_WHITESPACE
    ERROR_QUIET
  )
  separate_arguments(HEW_LLVM_SYSTEM_LIBS NATIVE_COMMAND "${HEW_LLVM_SYSTEM_LIBS_RAW}")
  hew_emit_system_link_items(${HEW_LLVM_SYSTEM_LIBS})

  # macOS: statically link zstd and zlib from Homebrew so the release
  # binary has no /opt/homebrew dynamic dependencies.  xml2 uses the
  # system dylib (/usr/lib/libxml2.2.dylib) which ships with macOS.
  if(APPLE)
    set(_hew_brew_prefix "")
    foreach(_candidate "/opt/homebrew" "/usr/local")
      if(IS_DIRECTORY "${_candidate}/lib")
        set(_hew_brew_prefix "${_candidate}")
        break()
      endif()
    endforeach()
    if(_hew_brew_prefix)
      # Static zstd: link the .a file directly so no NEEDED entry is created.
      if(EXISTS "${_hew_brew_prefix}/lib/libzstd.a")
        hew_embedded_append("cargo:rustc-link-arg=${_hew_brew_prefix}/lib/libzstd.a")
      else()
        hew_embedded_append("cargo:rustc-link-search=native=${_hew_brew_prefix}/lib")
        hew_embedded_append("cargo:rustc-link-lib=zstd")
        message(WARNING "HEW_STATIC_LINK: libzstd.a not found at ${_hew_brew_prefix}/lib — zstd will be a dynamic dependency")
      endif()
      # zlib: prefer static .a if available, otherwise use system dylib.
      # macOS ships /usr/lib/libz.1.dylib which is fine for distribution.
      if(EXISTS "${_hew_brew_prefix}/lib/libz.a")
        hew_embedded_append("cargo:rustc-link-arg=${_hew_brew_prefix}/lib/libz.a")
      else()
        hew_embedded_append("cargo:rustc-link-lib=z")
      endif()
    endif()
  endif()

  # macOS: statically link libc++ from Homebrew LLVM so the release binary
  # does not depend on /opt/homebrew/opt/llvm/lib/c++/libc++.1.dylib.
  # Also link libc++abi (system dylib) for exception base classes
  # (std::logic_error, std::runtime_error, etc.) that libc++.a requires.
  if(APPLE)
    get_filename_component(_hew_llvm_libcxx_dir "${LLVM_LIBRARY_DIR}/c++" ABSOLUTE)
    if(EXISTS "${_hew_llvm_libcxx_dir}/libc++.a")
      hew_embedded_append("cargo:rustc-link-arg=${_hew_llvm_libcxx_dir}/libc++.a")
      hew_embedded_append("cargo:rustc-link-lib=dylib=c++abi")
    elseif(IS_DIRECTORY "${_hew_llvm_libcxx_dir}")
      # Fallback: dynamic link with rpath (non-portable but functional).
      hew_embedded_append("cargo:rustc-link-search=native=${_hew_llvm_libcxx_dir}")
      hew_embedded_append("cargo:rustc-link-arg=-Wl,-rpath,${_hew_llvm_libcxx_dir}")
      message(WARNING "HEW_STATIC_LINK: libc++.a not found — libc++ will be a dynamic dependency")
    endif()
  endif()
else()
  hew_embedded_append("cargo:rustc-link-search=native=${LLVM_LIBRARY_DIR}")
  if(CMAKE_SYSTEM_NAME STREQUAL "Linux" OR APPLE)
    hew_embedded_append("cargo:rustc-link-arg=-Wl,-rpath,${LLVM_LIBRARY_DIR}")
  endif()

  hew_shared_lib_name(_hew_mlir_name "${MLIR_SHARED_LIB}")
  hew_shared_lib_name(_hew_llvm_name "${LLVM_SHARED_LIB}")
  hew_embedded_append("cargo:rustc-link-lib=dylib=${_hew_mlir_name}")
  hew_embedded_append("cargo:rustc-link-lib=dylib=${_hew_llvm_name}")
endif()

hew_emit_cxx_runtime()

if(HEW_STATIC_LINK AND CMAKE_BUILD_TYPE STREQUAL "Release")
  if(NOT MSVC AND NOT APPLE)
    target_link_options(HewCodegenCAPI PRIVATE -s)
    if(CMAKE_CXX_COMPILER_ID STREQUAL "GNU" OR
       (CMAKE_CXX_COMPILER_ID MATCHES "Clang" AND
        CMAKE_SYSTEM_NAME STREQUAL "Linux"))
      # Statically link the C++ runtime and GCC intrinsics library.
      # GCC on any platform and Clang on Linux both support these flags
      # when GNU libstdc++ (libstdc++-dev / g++) is installed.  This
      # removes libstdc++.so.6 and libgcc_s.so.1 from NEEDED entries.
      # Clang on FreeBSD/BSD uses libc++ (not libstdc++) so these flags
      # are deliberately excluded there.
      target_link_options(HewCodegenCAPI PRIVATE -static-libgcc -static-libstdc++)
    endif()
  endif()
  if(APPLE)
    add_custom_command(TARGET HewCodegenCAPI POST_BUILD
      COMMAND strip $<TARGET_FILE:HewCodegenCAPI>
      COMMENT "Stripping embedded codegen library (macOS)")
  endif()
endif()
