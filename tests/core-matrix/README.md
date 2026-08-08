# tests/core-matrix/

Every core primitive crossed with every common operation, one runnable program
per cell.

The rest of the `.hew` corpus grew by accretion — a fixture per bug — so its
coverage is shaped like the bug history, not like the language. This directory
is the other shape: a systematic enumeration, where a combination is either
proven correct or explicitly recorded as unsupported.

## Layout

| Path | What it is |
|------|------------|
| `cells/<row>__<column>.hew` | One cell: a complete program with `fn main()` |
| `cells/<row>__<column>.expected` | Its exact stdout oracle |
| `cells/<row>__<column>.trap` | Present when the cell aborts by design (out-of-bounds index) |
| `matrix.tsv` | The truth table: cell → outcome class today |
| `na.tsv` | Combinations that are meaningless, with the reason |

## The oracle

Each cell prints a canonical result and is compared byte-for-byte against its
`.expected`. Where the row can carry a `#[resource]` whose `close` prints, the
expectation also carries the release trace, so **exactly-once release is part of
the same exact-stdout comparison that proves the value**. Release ORDER among
independent owners is unspecified (a frame drops LIFO, a container in insertion
order), so the runner compares value lines in order and the `close` trace as a
multiset. What must hold is exactly-once per constructed resource.

## Outcome classes

| Class | Meaning |
|-------|---------|
| `PASS` | Ran and produced exactly the expected output |
| `WRONG-ANSWER` | Ran and produced incorrect output — the worst class |
| `SILENT-UNSOUND` | Released zero or two times, with no diagnostic |
| `NYI-CLEAN` | Rejected with a user-facing message naming the limit |
| `NYI-INTERNAL` | Rejected with compiler internals in the message |
| `CRASH` | Aborted, hung, or panicked in the compiler |

## Running it

```
make test-core-matrix
```

The gate fails on drift in **either** direction. A `PASS` cell that stops
passing is a regression. A recorded failure that starts passing means the table
is stale — re-record it in the same commit that fixed the cell:

```
python3 scripts/core-matrix.py --record
```

## Adding a primitive or an operation

The corpus is generated, not hand-maintained. A new primitive is a new row
descriptor in `scripts/core-matrix-gen.py`; a new operation is a new column
function. Regenerate and re-record:

```
python3 scripts/core-matrix-gen.py
python3 scripts/core-matrix.py --record
```

`make test-core-matrix` regenerates into a scratch directory and diffs first, so
a cell cannot be hand-edited into agreement with a broken compiler.

## What the cells are not

They are not minimal reproducers for the compiler team's convenience — they are
the shapes a user writes. When a cell fails, it fails on ordinary code: a map
lookup then insert, a resource in a state machine, a `Vec` of handles going out
of scope.
