"""
GDB Python extension for Hew programs.

Provides pretty-printers for Hew runtime types and convenience commands
for inspecting actor state. Load manually with:

    (gdb) source scripts/debug/hew-gdb.py

Or automatically via `hew debug <file.hew>`.
"""

import gdb  # type: ignore[import-untyped]
import re


# ---------------------------------------------------------------------------
# Pretty-printers
# ---------------------------------------------------------------------------

class HewStringPrinter:
    """Pretty-print hew_string_t (a { char* data; size_t len; } struct)."""

    def __init__(self, val):
        self.val = val

    def to_string(self):
        try:
            data = self.val["data"]
            length = int(self.val["len"])
            if length == 0:
                return '""'
            return data.string("utf-8", length=length)
        except gdb.error:
            return "<hew string>"


class HewVecPrinter:
    """Pretty-print hew_vec (opaque pointer — show address and runtime type)."""

    def __init__(self, val):
        self.val = val

    def to_string(self):
        ptr = self.val
        if int(ptr) == 0:
            return "Vec(null)"
        return f"Vec@{ptr}"


class HewHashMapPrinter:
    """Pretty-print hew_hashmap (opaque pointer)."""

    def __init__(self, val):
        self.val = val

    def to_string(self):
        ptr = self.val
        if int(ptr) == 0:
            return "HashMap(null)"
        return f"HashMap@{ptr}"


class HewActorRefPrinter:
    """Pretty-print actor reference pointers."""

    def __init__(self, val):
        self.val = val

    def to_string(self):
        ptr = self.val
        if int(ptr) == 0:
            return "ActorRef(null)"
        return f"ActorRef@{ptr}"


def hew_lookup_function(val):
    """GDB pretty-printer lookup for Hew types."""
    tag = str(val.type.strip_typedefs())

    if "hew_string" in tag:
        return HewStringPrinter(val)

    return None


# Register pretty-printers
gdb.pretty_printers.append(hew_lookup_function)


# ---------------------------------------------------------------------------
# Custom GDB commands
# ---------------------------------------------------------------------------

class HewActorsCommand(gdb.Command):
    """List active Hew actors.

    Usage: hew-actors

    Searches the runtime's actor registry (if debug symbols are available)
    and lists actor IDs, types, and mailbox sizes.
    """

    def __init__(self):
        super().__init__("hew-actors", gdb.COMMAND_DATA, gdb.COMPLETE_NONE)

    def invoke(self, arg, from_tty):
        # Try to find the runtime's global actor count
        try:
            count_sym = gdb.parse_and_eval("hew_runtime_actor_count")
            print(f"Active actors: {count_sym}")
        except gdb.error:
            print("Actor introspection requires runtime debug symbols.")
            print("The runtime was likely built in release mode.")
            print()
            print("Tip: You can still debug your Hew program:")
            print("  - Set breakpoints on your functions: (gdb) break main")
            print("  - Break on actor dispatch: (gdb) break hew_actor_send")
            print("  - Break on actor creation: (gdb) break hew_actor_spawn")


class HewBreakReceiveCommand(gdb.Command):
    """Set a breakpoint on a Hew receive handler.

    Usage: hew-break-receive <actor_name> <method_name>

    Sets a breakpoint on the generated dispatch function for the given
    actor and receive method. The function name follows the pattern:
    <actor_name>_dispatch or <actor_name>_receive_<method_name>.
    """

    def __init__(self):
        super().__init__("hew-break-receive", gdb.COMMAND_BREAKPOINTS, gdb.COMPLETE_SYMBOL)

    def invoke(self, arg, from_tty):
        args = arg.split()
        if len(args) < 1:
            print("Usage: hew-break-receive <actor_name> [method_name]")
            return

        actor = args[0]
        if len(args) >= 2:
            method = args[1]
            # Try the generated function name pattern
            patterns = [
                f"{actor}_receive_{method}",
                f"{actor}_{method}",
                f"{actor}_dispatch",
            ]
        else:
            patterns = [f"{actor}_dispatch"]

        for pattern in patterns:
            try:
                gdb.execute(f"break {pattern}", to_string=True)
                print(f"Breakpoint set on {pattern}")
                return
            except gdb.error:
                continue

        print(f"Could not find dispatch function for actor '{actor}'.")
        print("Try: (gdb) info functions .*dispatch.*")


class HewBacktraceCommand(gdb.Command):
    """Show a Hew-focused backtrace, filtering out runtime internals.

    Usage: hew-bt
    """

    def __init__(self):
        super().__init__("hew-bt", gdb.COMMAND_STACK, gdb.COMPLETE_NONE)

    def invoke(self, arg, from_tty):
        frame = gdb.newest_frame()
        idx = 0
        while frame is not None:
            name = frame.name() or "<unknown>"
            # Skip runtime internals and system frames
            if not any(skip in name for skip in [
                "hew_runtime_", "__pthread", "_start", "__libc",
                "std::rt", "std::sys", "core::ops",
            ]):
                sal = frame.find_sal()
                loc = ""
                if sal.symtab:
                    loc = f" at {sal.symtab.filename}:{sal.line}"
                print(f"  #{idx} {name}{loc}")
                idx += 1
            frame = frame.older()

        if idx == 0:
            print("  (no user frames found — try 'bt' for full backtrace)")


# Register commands
HewActorsCommand()
HewBreakReceiveCommand()
HewBacktraceCommand()

print("Hew GDB extensions loaded. Commands: hew-actors, hew-break-receive, hew-bt")
