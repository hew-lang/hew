# Terminal frame cleanup in emitted LLVM

**Rule:** after an explicit `hew_cont_frame_free`, generated code may only
branch, switch, return, or terminate. Every other instruction is refused in
that region. This prevents a second explicit frame release or later memory/
call work along the represented path.

Run `make prove-llvm-frames`. It builds the current compiler and checks six
real programs at both O0 and O2, using the `.ll` retained after Hew's LLVM
pipeline. The cases exercise actor calls, selection/fork, streams, repeated
suspension with a fault, and owned/deferred locals. No program is executed.
Each program must contain actual frame allocations and releases; empty
coverage fails.

The LLVM job runs independently of runtime proofs in the daily/manual advisory
workflow. Normal PRs do not run it. It needs the normal compiler toolchain
(LLVM 22.1) and Lean 4.19, with no SMT service or Python package dependency.

## What is proved

1. LLVM's parser and module verifier read the actual `.ll`. The 90-line C++
   bridge enumerates every basic block, instruction, and CFG successor in
   every function with a direct frame release. Quiet instructions are an
   explicit allowlist; other instructions default to an effect. Escaping the
   release symbol or changing its type signature is unsupported and fails the corpus.
2. Python proposes the set of blocks reachable after release. It also reports
   the precise function, block and instruction that prevent a certificate.
3. Lean's kernel checks a concrete certificate for **each function** using
   `by decide`. The general theorem proves that acceptance permits only quiet
   events after release, inside the same block and across arbitrarily long
   paths, including cycles. Marks, block indices and closure obligations are
   checked independently of Python's proposal.

There is no path-unrolling bound and no assumption that resume and destroy
functions execute together. Each is a separate entry. Generated certificates,
extracted graphs, IR hashes and counts remain under `target/` for review.

The parser/extractor, JSON-to-Lean graph serialization, the classification of
LLVM operations, LLVM's semantics,
and the runtime meaning of `hew_cont_frame_free` remain trusted. The Lean
result is an instruction/CFG cleanup theorem for the extracted graph, rather
than a formalization of all LLVM behavior.

## What remains unresolved

Returning a pointer is a quiet event: it performs no memory access. Producer
functions in O0 output can contain syntactic free-then-pointer-return paths.
Inspection found a destroying flag initialized false in the producer, with
true writes in the split functions, but reachability under external mutation
or reentry has not been proved. The report records these paths as
`unproved_handle_returns`. They remain a separate handle-lifetime obligation.

This rule does not establish that a frame was live on entry, that calls before
release preserve it, that every frame is eventually freed, or that runtime
code cannot resume/destroy a freed frame. Arbitrary alias/escape safety and
source-to-LLVM semantic preservation remain outside the theorem. No whole
compiler or frame-lifetime completeness claim follows from the artifact count.

## Evidence that failures mean something

The runner modifies actual compiler output and requires LLVM parsing and
module verification to succeed first. Lean then proves the certificate false
for duplicate release in one block, duplicate release through a successor,
post-release store, and post-release call. Additional controls require
rejection of an escaped release symbol, forged post-release marks, and an
empty artifact. Tool failures and timeouts do not count as counterexamples.

A failed check retains the `.ll`, extraction, certificate and diagnostic.
Unsupported output fails the selected corpus; it is never silently counted
as verified. Run a smaller slice with:

```sh
python3 proofs/llvm_frames/run.py --case counter
```

A selected run writes a separate report and cannot replace full-corpus
coverage evidence. For an existing `.ll`, use `python3
proofs/llvm_frames/check.py path/to/output.ll`.
