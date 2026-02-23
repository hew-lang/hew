; test_coro_generator.ll — Prototype: LLVM coroutine-based generator
;
; A generator that yields 1, 2, 3 using LLVM coroutine intrinsics.
; This validates CoroSplit pass integration for Hew's future `gen fn` support.
;
; Compile: clang-21 -O1 -o test_coro_generator test_coro_generator.ll
; Expected output:
;   1
;   2
;   3
;   done

; Promise type: holds the current yielded value
%promise_type = type { i64 }

@.fmt_val = private unnamed_addr constant [5 x i8] c"%ld\0A\00"
@.fmt_done = private unnamed_addr constant [6 x i8] c"done\0A\00"

declare ptr @malloc(i64)
declare void @free(ptr)
declare i32 @printf(ptr, ...)

;; ─── Generator coroutine: yields 1, 2, 3 ─────────────────────────────────
;;
;; After CoroSplit, this becomes three functions:
;;   gen123       — ramp: allocates frame, runs until first suspend
;;   gen123.resume  — resume: continues from last suspend point
;;   gen123.destroy — destroy: deallocates frame
;;
define ptr @gen123() presplitcoroutine {
entry:
  %promise = alloca %promise_type, align 8
  %id = call token @llvm.coro.id(i32 0, ptr %promise, ptr null, ptr null)
  %need.dyn.alloc = call i1 @llvm.coro.alloc(token %id)
  br i1 %need.dyn.alloc, label %dyn.alloc, label %begin

dyn.alloc:
  %size = call i64 @llvm.coro.size.i64()
  %alloc = call ptr @malloc(i64 %size)
  br label %begin

begin:
  %phi = phi ptr [ null, %entry ], [ %alloc, %dyn.alloc ]
  %hdl = call ptr @llvm.coro.begin(token %id, ptr %phi)

  ; ── Yield 1 ──
  %val_ptr = getelementptr inbounds %promise_type, ptr %promise, i32 0, i32 0
  store i64 1, ptr %val_ptr, align 8
  %sp1 = call i8 @llvm.coro.suspend(token none, i1 false)
  switch i8 %sp1, label %suspend [i8 0, label %resume1
                                   i8 1, label %cleanup]

resume1:
  ; ── Yield 2 ──
  store i64 2, ptr %val_ptr, align 8
  %sp2 = call i8 @llvm.coro.suspend(token none, i1 false)
  switch i8 %sp2, label %suspend [i8 0, label %resume2
                                   i8 1, label %cleanup]

resume2:
  ; ── Yield 3 ──
  store i64 3, ptr %val_ptr, align 8
  %sp3 = call i8 @llvm.coro.suspend(token none, i1 false)
  switch i8 %sp3, label %suspend [i8 0, label %after_yields
                                   i8 1, label %cleanup]

after_yields:
  ; All values yielded — final suspend marks generator as "done"
  %final = call i8 @llvm.coro.suspend(token none, i1 true)
  switch i8 %final, label %suspend [i8 0, label %suspend
                                     i8 1, label %cleanup]

cleanup:
  %mem = call ptr @llvm.coro.free(token %id, ptr %hdl)
  call void @free(ptr %mem)
  br label %suspend

suspend:
  call i1 @llvm.coro.end(ptr %hdl, i1 false, token none)
  ret ptr %hdl
}

;; ─── Main: drive the generator ────────────────────────────────────────────
define i32 @main() {
entry:
  ; Create generator — ramp function executes until first suspend
  ; At that point, value 1 is stored in the promise
  %hdl = call ptr @gen123()

  ; Read yielded value 1
  %promise = call ptr @llvm.coro.promise(ptr %hdl, i32 8, i1 false)
  %val0 = load i64, ptr %promise, align 8
  call i32 (ptr, ...) @printf(ptr @.fmt_val, i64 %val0)

  ; Resume → generator stores 2, suspends again
  call void @llvm.coro.resume(ptr %hdl)
  %val1 = load i64, ptr %promise, align 8
  call i32 (ptr, ...) @printf(ptr @.fmt_val, i64 %val1)

  ; Resume → generator stores 3, suspends again
  call void @llvm.coro.resume(ptr %hdl)
  %val2 = load i64, ptr %promise, align 8
  call i32 (ptr, ...) @printf(ptr @.fmt_val, i64 %val2)

  ; Resume → generator passes final suspend, becomes "done"
  call void @llvm.coro.resume(ptr %hdl)
  %done = call i1 @llvm.coro.done(ptr %hdl)
  br i1 %done, label %print_done, label %exit

print_done:
  call i32 (ptr, ...) @printf(ptr @.fmt_done)
  br label %exit

exit:
  ; Destroy the generator (runs cleanup/deallocation)
  call void @llvm.coro.destroy(ptr %hdl)
  ret i32 0
}

;; ─── Coroutine intrinsic declarations ─────────────────────────────────────
declare token @llvm.coro.id(i32, ptr readnone, ptr nocapture readonly, ptr)
declare i1 @llvm.coro.alloc(token)
declare i64 @llvm.coro.size.i64()
declare ptr @llvm.coro.begin(token, ptr writeonly)
declare i8 @llvm.coro.suspend(token, i1)
declare i1 @llvm.coro.end(ptr, i1, token)
declare ptr @llvm.coro.free(token, ptr nocapture readonly)
declare void @llvm.coro.resume(ptr)
declare void @llvm.coro.destroy(ptr)
declare i1 @llvm.coro.done(ptr)
declare ptr @llvm.coro.promise(ptr nocapture, i32, i1)
