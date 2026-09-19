# Golden source fixtures

The [fixture manifest](../fixtures/manifest.json) lists the executable programs
and intentional diagnostic examples. Every executable bytecode package comes
from its adjacent `main.hew` through the public SIR compiler. Traces use the
[trace schema](trace-schema-v0.schema.json).

The corpus covers sequential values, machine transitions, actor state and
requests, pipes and selection, task joins, supervisor restart, crash actions,
checked faults, budget exhaustion, virtual time and replay inputs. A filesystem
source compiles successfully and is refused by the VM loader before it runs.

Run `make sandbox-fixtures-record` to regenerate and execute source packages.
Review stdout and final status, then run `make sandbox-parity` to compare native
and VM source behaviour and replay the recorded traces.
