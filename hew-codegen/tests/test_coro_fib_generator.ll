; test_coro_fib_generator.ll — Stateful Fibonacci generator using LLVM coroutines
;
; Generates the first 8 Fibonacci numbers: 0, 1, 1, 2, 3, 5, 8, 13
; This validates coroutine frame state preservation across suspend/resume cycles.
;
; Promise layout: { i64 current_value }
; Frame state:    { i64 a, i64 b } — preserved across suspends by CoroSplit

%promise_type = type { i64 }

@.fmt_val = private unnamed_addr constant [5 x i8] c"%ld\0A\00"

declare ptr @malloc(i64)
declare void @free(ptr)
declare i32 @printf(ptr, ...)

;; ─── Fibonacci generator coroutine ────────────────────────────────────────
;; Yields Fibonacci numbers indefinitely (caller controls how many via resume count)
define ptr @fib_gen() presplitcoroutine {
entry:
  %promise = alloca %promise_type, align 8
  %id = call token @llvm.coro.id(i32 0, ptr %promise, ptr null, ptr null)
  %need.alloc = call i1 @llvm.coro.alloc(token %id)
  br i1 %need.alloc, label %do.alloc, label %begin

do.alloc:
  %size = call i64 @llvm.coro.size.i64()
  %alloc = call ptr @malloc(i64 %size)
  br label %begin

begin:
  %phi = phi ptr [ null, %entry ], [ %alloc, %do.alloc ]
  %hdl = call ptr @llvm.coro.begin(token %id, ptr %phi)
  %val_ptr = getelementptr inbounds %promise_type, ptr %promise, i32 0, i32 0

  ; State: a=0, b=1
  br label %loop

loop:
  %a = phi i64 [ 0, %begin ], [ %b, %loop.resume ]
  %b = phi i64 [ 1, %begin ], [ %next, %loop.resume ]

  ; Yield current value (a)
  store i64 %a, ptr %val_ptr, align 8
  %sp = call i8 @llvm.coro.suspend(token none, i1 false)
  switch i8 %sp, label %suspend [i8 0, label %loop.resume
                                  i8 1, label %cleanup]

loop.resume:
  ; Compute next: a <- b, b <- a + b
  %next = add i64 %a, %b
  br label %loop

cleanup:
  %mem = call ptr @llvm.coro.free(token %id, ptr %hdl)
  call void @free(ptr %mem)
  br label %suspend

suspend:
  call i1 @llvm.coro.end(ptr %hdl, i1 false, token none)
  ret ptr %hdl
}

;; ─── Main: consume 8 Fibonacci values ─────────────────────────────────────
define i32 @main() {
entry:
  %hdl = call ptr @fib_gen()
  %promise = call ptr @llvm.coro.promise(ptr %hdl, i32 8, i1 false)
  br label %loop

loop:
  %i = phi i64 [ 0, %entry ], [ %i.next, %loop.cont ]
  %cmp = icmp slt i64 %i, 8
  br i1 %cmp, label %loop.body, label %done

loop.body:
  ; Read yielded value
  %val = load i64, ptr %promise, align 8
  call i32 (ptr, ...) @printf(ptr @.fmt_val, i64 %val)
  ; Resume generator for next value
  call void @llvm.coro.resume(ptr %hdl)
  br label %loop.cont

loop.cont:
  %i.next = add i64 %i, 1
  br label %loop

done:
  ; Clean up
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
