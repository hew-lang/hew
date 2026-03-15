cmake_minimum_required(VERSION 3.20)
if(POLICY CMP0135)
  cmake_policy(SET CMP0135 NEW)
endif()
project(hew-codegen VERSION 0.2.0 LANGUAGES C CXX)

# ── Standards ────────────────────────────────────────────────────────────────
set(CMAKE_CXX_STANDARD 20)
set(CMAKE_CXX_STANDARD_REQUIRED ON)
set(CMAKE_CXX_EXTENSIONS OFF)
set(CMAKE_C_STANDARD 11)
set(CMAKE_C_STANDARD_REQUIRED ON)
set(CMAKE_EXPORT_COMPILE_COMMANDS ON)

# ── Find LLVM & MLIR ────────────────────────────────────────────────────────
# Auto-detected via CMake's standard search paths.  Override with:
#   cmake -DLLVM_DIR=/path/to/llvm/lib/cmake/llvm -DMLIR_DIR=/path/to/mlir/lib/cmake/mlir
find_package(LLVM 22.1 REQUIRED CONFIG)
find_package(MLIR REQUIRED CONFIG)

message(STATUS "Found LLVM ${LLVM_PACKAGE_VERSION}")
message(STATUS "Using LLVMConfig.cmake in: ${LLVM_DIR}")
message(STATUS "Found MLIR at: ${MLIR_DIR}")

list(APPEND CMAKE_MODULE_PATH "${LLVM_CMAKE_DIR}")
list(APPEND CMAKE_MODULE_PATH "${MLIR_CMAKE_DIR}")

include(TableGen)
include(AddLLVM)
include(AddMLIR)
include(HandleLLVMOptions)

# Ensure MLIR_TABLEGEN_EXE is an absolute path so ninja can find it.
# When installed via cmake --install with BUILD_TOOLS=OFF, the config may
# set it to a bare "mlir-tblgen" which cmake resolves to a build-local path.
if(MLIR_TABLEGEN_EXE AND NOT IS_ABSOLUTE "${MLIR_TABLEGEN_EXE}")
  find_program(_mlir_tblgen_abs "${MLIR_TABLEGEN_EXE}"
    PATHS "${LLVM_TOOLS_BINARY_DIR}" "${MLIR_DIR}/../../../bin"
    NO_DEFAULT_PATH)
  if(NOT _mlir_tblgen_abs)
    find_program(_mlir_tblgen_abs "${MLIR_TABLEGEN_EXE}")
  endif()
  if(_mlir_tblgen_abs)
    set(MLIR_TABLEGEN_EXE "${_mlir_tblgen_abs}" CACHE FILEPATH "" FORCE)
  endif()
endif()

include_directories(${LLVM_INCLUDE_DIRS})
include_directories(${MLIR_INCLUDE_DIRS})
include_directories(${PROJECT_SOURCE_DIR}/include)
include_directories(${PROJECT_BINARY_DIR}/include)

# ── Coverage instrumentation (used by `make coverage-cpp`) ───────────────────
option(HEW_COVERAGE "Build with coverage instrumentation for llvm-cov profiling" OFF)
if(HEW_COVERAGE)
  message(STATUS "Coverage instrumentation enabled")
  add_compile_options(-fprofile-instr-generate -fcoverage-mapping)
  add_link_options(-fprofile-instr-generate)
endif()

# ── Static linking option for release builds ─────────────────────────────────
option(HEW_STATIC_LINK "Statically link LLVM and MLIR for self-contained binaries" OFF)

if(HEW_STATIC_LINK)
  message(STATUS "Static linking enabled — using individual LLVM/MLIR static libraries")
endif()

# Note: HandleLLVMOptions (included above) already adds the necessary
# compile definitions (_GLIBCXX_USE_CXX11_ABI, __STDC_*_MACROS, etc.).
# Do NOT call add_definitions(${LLVM_DEFINITIONS}) — that string is
# space-separated, not semicolon-separated, causing malformed -D flags
# and duplicate macro redefinition warnings.

# Match LLVM's RTTI setting.  HandleLLVMOptions' llvm_replace_compiler_option
# only replaces an existing -frtti flag; if neither -frtti nor -fno-rtti is in
# CMAKE_CXX_FLAGS it does nothing.  Source-built LLVM defaults to RTTI=OFF,
# while distro packages (apt, Homebrew) usually enable it.
if(NOT LLVM_ENABLE_RTTI)
  if(MSVC)
    string(APPEND CMAKE_CXX_FLAGS " /GR-")
  else()
    string(APPEND CMAKE_CXX_FLAGS " -fno-rtti")
  endif()
endif()

# ── msgpack-cxx (header-only, for msgpack AST deserialization) ───────────────
include(FetchContent)
FetchContent_Declare(
  msgpack-cxx
  URL      https://github.com/msgpack/msgpack-c/archive/refs/tags/cpp-7.0.0.tar.gz
  URL_HASH SHA256=070881ebea9208cf7e731fd5a46a11404025b2f260ab9527e32dfcb7c689fbfc
)
set(MSGPACK_USE_BOOST OFF CACHE BOOL "" FORCE)
set(MSGPACK_BUILD_TESTS OFF CACHE BOOL "" FORCE)
set(MSGPACK_BUILD_EXAMPLES OFF CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(msgpack-cxx)

# ── nlohmann/json (header-only, for --input-json debug mode) ─────────────────
FetchContent_Declare(
  nlohmann_json
  URL      https://github.com/nlohmann/json/archive/refs/tags/v3.12.0.tar.gz
  # Hash omitted — GitHub tarballs are regenerated and may vary
)
set(JSON_BuildTests OFF CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(nlohmann_json)

# ── Testing ──────────────────────────────────────────────────────────────────
enable_testing()

# ── Find hew CLI binary (Rust compiler driver) ──────────────────────────────
# E2E tests use the hew CLI for full-pipeline compilation.
find_program(HEW_CLI hew
  PATHS
    ${CMAKE_SOURCE_DIR}/../target/debug
    ${CMAKE_SOURCE_DIR}/../target/release
  NO_DEFAULT_PATH
)
if(NOT HEW_CLI)
  find_program(HEW_CLI hew)
endif()
if(HEW_CLI)
  message(STATUS "Found hew CLI: ${HEW_CLI}")
else()
  message(WARNING "hew CLI not found — E2E tests will be skipped. Build with: cargo build -p hew-cli")
endif()

# ── Subdirectories ───────────────────────────────────────────────────────────
add_subdirectory(include/hew/mlir)
add_subdirectory(src)
add_subdirectory(tests)
