# Debugging corpus

Source programs for the debugger, failure-report and symbolization acceptance
runs. Each program is small, deterministic and exercises one language surface
a debugger has to present. Build from this directory so relative imports
resolve:

    hew build -g NN-topic.hew -o out
    gdb -q -batch -ex 'break NN-topic.hew:LINE' -ex run -ex bt -ex 'info locals' ./out
    lldb -b -o 'b NN-topic.hew:LINE' -o run -o bt -o 'frame variable' ./out

`break` is a line inside the interesting body where every supported debugger
stops; each line below was confirmed with gdb on Linux. `expected` is stdout
(and the stderr failure line where one is printed) followed by the exit
status.

Programs 26 to 29 are unassigned.

| program                   | surface                                          | break                                     | expected                                                                                            |
| ------------------------- | ------------------------------------------------ | ----------------------------------------- | --------------------------------------------------------------------------------------------------- |
| 01-int-widths             | i8..u64 casts and arithmetic                     | 17                                        | 300, 30000, 6000000000, 9000000000000000, 18000000000000000, 500; exit 0                            |
| 02-shadowing-blocks       | shadowing in nested scopes                       | 6 (`x` is 22)                             | 22, 122, 11, 6; exit 0                                                                              |
| 03-strings-unicode        | multibyte string, len, byte_len, slice, f-string | 7                                         | `héllo wörld`, 11, 13, ...; exit 0                                                                  |
| 04-vec                    | Vec push, index, iteration                       | 9                                         | 6, 9, 55; exit 0                                                                                    |
| 05-hashmap                | HashMap<string, i64> insert, get, remove         | 7                                         | 3, 2, false, 2; exit 0                                                                              |
| 06-hashset                | HashSet dedupe                                   | 7                                         | 3, 60; exit 0                                                                                       |
| 07-records                | nested records, `..a` update                     | 14 (in `length_sq`)                       | 1, 10, diag, 64; exit 0                                                                             |
| 08-enum-match             | enum payloads and guards                         | 10 (in `describe`)                        | big circle, circle, square, rect, empty; exit 0                                                     |
| 09-option-result          | `?` in a `fails` function, Option                | 13 (in `add_digits`, `a`=3 `b`=4)         | 7, not a digit, 2, none; exit 0                                                                     |
| 10-closures               | captured value, stateful counter                 | 6 (in the counter closure)                | 105, 1, 2, 3; exit 0                                                                                |
| 11-generators             | `gen fn` with break, `gen {}` block              | 4 (generator body)                        | 15, gen:a, 1, gen:b, 2; exit 0                                                                      |
| 12-generics-dyn           | generic `Box<T>`, `dyn Named`                    | 16 (in `Dog.name`)                        | 42, boxed, dog rex, cat 9; exit 0                                                                   |
| 13-defer                  | LIFO defer, per-iteration defer                  | 7 (in `work`)                             | loop defer x2, last defer, outer defer, 1; exit 0                                                   |
| 14-recursion              | fact, fib, mutual recursion                      | 3 (in `fact`)                             | 3628800, 610, true; exit 0                                                                          |
| 15-labeled-loops          | `@outer` break and continue, loop, while         | 6                                         | 706, 7, -2; exit 0                                                                                  |
| 16-actor-reply            | actor state and handler reply                    | 6 (in `Counter.add`)                      | 42; exit 0                                                                                          |
| 17-actor-pingpong         | actor asks actor                                 | 12 (in `Relay.run`)                       | 41; exit 0                                                                                          |
| 18-supervisor-restart     | child panics, restart re-runs declared init      | 14 (in `Worker.boom`)                     | stderr crash line, then 7; exit 0                                                                   |
| 19-tasks-sleep            | scope, fork and sleep                            | 3 (in `slow_square`)                      | 25; exit 0                                                                                          |
| 20-machine                | machine events                                   | 21                                        | Idle, Running, Stopped; exit 0                                                                      |
| 21-wire-json              | `#[wire]` JSON round trip                        | 12                                        | `{"x":3,"y":4}`, 7; exit 0                                                                          |
| 22-iterators              | Vec loop with a filter                           | 10                                        | 20; exit 0                                                                                          |
| 23-tuples-destructure     | tuples, let-else                                 | 12 (`lo`=4 `hi`=9)                        | 4, 9, 5; exit 0                                                                                     |
| 24-exit-code              | main returns i64                                 | 4 (in `compute`)                          | no output; exit 42                                                                                  |
| 25-trait-static           | trait-bound static dispatch                      | 29 (in `total<Rect>`)                     | 24, 50; exit 0                                                                                      |
| 30-fail-panic             | panic three frames deep                          | 3 (in `level3`)                           | `UserPanic (212)`; exit 1                                                                           |
| 31-fail-expect            | `.expect` on Err                                 | 12 (the failing expect)                   | 1, `UserPanic: other must load: unknown key`; exit 1                                                |
| 32-fail-overflow          | i32 multiply overflow                            | 4 (in `grow`)                             | `IntegerOverflow (201)`; exit 1                                                                     |
| 33-fail-divzero           | integer divide by zero                           | 3 (in `ratio`)                            | `DivideByZero (202)`; exit 1                                                                        |
| 34-fail-index-oob         | Vec index 5 of 3                                 | 8                                         | `IndexOutOfBounds (205)`; exit 1                                                                    |
| 35-fail-unwrap-none       | `.expect` on None                                | 8 (the failing expect)                    | 100, `UserPanic: two is present`; exit 1                                                            |
| 36-fail-actor-crash       | unsupervised actor traps, ask returns Err        | 10 (in `Fragile.bad`)                     | 2, crash line for `Fragile.bad`, `bad failed (crash observed)`; exit 1                              |
| 37-fail-supervised-crash  | supervised divide by zero, restart               | 10 (in `Calc.divide`)                     | crash line, then 5; exit 0                                                                          |
| 38-fail-stack-overflow    | unbounded recursion                              | 3 (in `descend`)                          | descending, then a stack-exhaustion failure (today `UnknownFault (11)` SIGSEGV); exit 1             |
| 39-fail-assert            | `assert` on a false condition                    | 8                                         | `UserPanic` with left and right values; exit 1                                                      |
| 40-fail-string-slice      | string range out of bounds                       | 6                                         | `IndexOutOfBounds: string range out of bounds`; exit 1                                              |
| 41-stuck-ask-cycle        | two actors ask each other                        | 11 (in `Ping.go`), 33 (in `Pong.back`)    | start, `local actor wait cycle at ask: 2 -> 1 -> 2` failure, crash line for `Pong.back`, -1; exit 1 |
| 42-stuck-sleeping-handler | handler sleeps 3600 s                            | 5 (in `Slow.work`)                        | hangs: run under `timeout`, exit 124                                                                |
| 43-stuck-spin             | handler spins forever                            | 6 (in `Spinner.spin`)                     | hangs at 100% CPU: run under `timeout`, exit 124                                                    |
| 44-two-module/main.hew    | imported module function, std generic monomorph  | main.hew 8, util.hew 3 (in `util.triple`) | 18; exit 0                                                                                          |
| 45-sibling-types          | record and impl method declared in `types.hew`   | 5; types.hew 13 (in `Point.describe`)     | `point 3,4 sum 7`; exit 0                                                                           |
| 46-fail-in-import         | `.expect` fails inside `fl.hew`                  | 5; fl.hew 4 (the failing expect)          | 1, `UserPanic: lookup must find the key: missing key`; exit 1                                       |
| 47-main-fails             | `?` propagates an error out of a `fails` main    | 11                                        | 4, `error: not a digit`; exit 1                                                                     |

`types.hew` and `fl.hew` are support modules for 45 and 46 and have no `main`.

## Notes

- 41 compiles with `hew run` but `hew build -g` crashes the compiler on the
  current base; it becomes a debug-build fixture once the DILocalVariable type
  fix lands.
- Breaks in a non-root file (util.hew, types.hew, fl.hew) resolve once the
  debug metadata emits one `DIFile` per entry of the physical file table.
- A break on a function's declaration line stops before the parameters are
  stored; use a body line, as above.
