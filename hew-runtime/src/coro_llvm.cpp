// LLVM coroutine outlines use fastcc. Clang's builtins keep that convention
// at the C boundary without exposing frame layout to the runtime.
extern "C" __attribute__((visibility("hidden"))) void
hew_llvm_coro_resume(void *handle) noexcept(false) {
  __builtin_coro_resume(handle);
}

extern "C" __attribute__((visibility("hidden"))) void
hew_llvm_coro_destroy(void *handle) noexcept(false) {
  __builtin_coro_destroy(handle);
}
