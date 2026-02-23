cmake_minimum_required(VERSION 3.20)
project(hew-codegen VERSION 0.1.0 LANGUAGES C CXX)

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
find_package(LLVM 21.1 REQUIRED CONFIG)
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

include_directories(${LLVM_INCLUDE_DIRS})
include_directories(${MLIR_INCLUDE_DIRS})
include_directories(${PROJECT_SOURCE_DIR}/include)
include_directories(${PROJECT_BINARY_DIR}/include)

# ── Static linking option for release builds ─────────────────────────────────
option(HEW_STATIC_LINK "Statically link LLVM and MLIR for self-contained binaries" OFF)

if(HEW_STATIC_LINK)
  message(STATUS "Static linking enabled — using individual LLVM/MLIR static libraries")
endif()

add_definitions(${LLVM_DEFINITIONS})

# ── msgpack-cxx (header-only, for msgpack AST deserialization) ───────────────
include(FetchContent)
FetchContent_Declare(
  msgpack-cxx
  GIT_REPOSITORY https://github.com/msgpack/msgpack-c.git
  GIT_TAG        cpp-7.0.0
  GIT_SHALLOW    TRUE
)
set(MSGPACK_USE_BOOST OFF CACHE BOOL "" FORCE)
set(MSGPACK_BUILD_TESTS OFF CACHE BOOL "" FORCE)
set(MSGPACK_BUILD_EXAMPLES OFF CACHE BOOL "" FORCE)
FetchContent_MakeAvailable(msgpack-cxx)

# ── nlohmann/json (header-only, for --input-json debug mode) ─────────────────
FetchContent_Declare(
  nlohmann_json
  GIT_REPOSITORY https://github.com/nlohmann/json.git
  GIT_TAG        v3.11.3
  GIT_SHALLOW    TRUE
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
