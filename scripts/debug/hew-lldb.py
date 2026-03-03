"""
LLDB Python extension for Hew programs.

Provides type summaries for Hew runtime types and convenience commands
for inspecting actor state. Load manually with:

    (lldb) command script import scripts/debug/hew-lldb.py

Or automatically via `hew debug <file.hew>`.
"""

import lldb  # type: ignore[import-untyped]


# ---------------------------------------------------------------------------
# Type Summary Providers
# ---------------------------------------------------------------------------

def hew_string_summary(valobj, _internal_dict):
    """Pretty-print hew_string_t { data: *const char, len: usize } as "string content"."""
    try:
        data = valobj.GetChildMemberWithName("data")
        length = valobj.GetChildMemberWithName("len")
        n = length.GetValueAsUnsigned(0)
        if n == 0:
            return '""'
        error = lldb.SBError()
        content = data.GetPointeeData(0, n).GetString(error, 0)
        if error.Fail():
            # Fallback: read raw bytes from the pointer address
            process = valobj.GetProcess()
            addr = data.GetValueAsUnsigned(0)
            if addr == 0:
                return '""'
            content = process.ReadCStringFromMemory(addr, n + 1, error)
            if error.Fail():
                return "<hew string>"
        return f'"{content[:n]}"'
    except Exception:
        return "<hew string>"


def hew_vec_summary(valobj, _internal_dict):
    """Pretty-print hew_vec (opaque pointer -- show address)."""
    ptr = valobj.GetValueAsUnsigned(0)
    if ptr == 0:
        return "Vec(null)"
    return f"Vec@0x{ptr:x}"


def hew_hashmap_summary(valobj, _internal_dict):
    """Pretty-print hew_hashmap (opaque pointer)."""
    ptr = valobj.GetValueAsUnsigned(0)
    if ptr == 0:
        return "HashMap(null)"
    return f"HashMap@0x{ptr:x}"


def hew_actor_ref_summary(valobj, _internal_dict):
    """Pretty-print actor reference pointers."""
    ptr = valobj.GetValueAsUnsigned(0)
    if ptr == 0:
        return "ActorRef(null)"
    return f"ActorRef@0x{ptr:x}"


# ---------------------------------------------------------------------------
# Custom LLDB Commands
# ---------------------------------------------------------------------------

def hew_actors_command(debugger, command, result, _internal_dict):
    """List active Hew actors.

    Usage: hew-actors

    Reads the runtime's global actor count (if debug symbols are available).
    """
    target = debugger.GetSelectedTarget()
    if not target:
        result.AppendMessage("No target selected.")
        return

    # Try to evaluate the global actor count symbol
    sym_contexts = target.FindSymbols("hew_runtime_actor_count")
    if sym_contexts.GetSize() > 0:
        # Read the value from memory
        sym = sym_contexts.GetContextAtIndex(0).GetSymbol()
        addr = sym.GetStartAddress().GetLoadAddress(target)
        process = target.GetProcess()
        if process and addr != lldb.LLDB_INVALID_ADDRESS:
            error = lldb.SBError()
            # Actor count is typically a u64/usize
            ptr_size = target.GetAddressByteSize()
            count = process.ReadUnsignedFromMemory(addr, ptr_size, error)
            if error.Success():
                result.AppendMessage(f"Active actors: {count}")
                return
            else:
                result.AppendMessage(f"Could not read actor count: {error}")
                return

    result.AppendMessage("Actor introspection requires runtime debug symbols.")
    result.AppendMessage("The runtime was likely built in release mode.")
    result.AppendMessage("")
    result.AppendMessage("Tip: You can still debug your Hew program:")
    result.AppendMessage("  - Set breakpoints on your functions: (lldb) break set -n main")
    result.AppendMessage("  - Break on actor dispatch: (lldb) break set -n hew_actor_send")
    result.AppendMessage("  - Break on actor creation: (lldb) break set -n hew_actor_spawn")


def hew_break_receive_command(debugger, command, result, _internal_dict):
    """Set a breakpoint on a Hew receive handler.

    Usage: hew-break-receive <actor_name> [method_name]

    Sets a breakpoint on the generated dispatch function for the given
    actor and receive method. The function name follows the pattern:
    <actor_name>_dispatch or <actor_name>_receive_<method_name>.
    """
    args = command.strip().split()
    if len(args) < 1:
        result.AppendMessage("Usage: hew-break-receive <actor_name> [method_name]")
        return

    target = debugger.GetSelectedTarget()
    if not target:
        result.AppendMessage("No target selected.")
        return

    actor = args[0]
    if len(args) >= 2:
        method = args[1]
        patterns = [
            f"{actor}_receive_{method}",
            f"{actor}_{method}",
            f"{actor}_dispatch",
        ]
    else:
        patterns = [f"{actor}_dispatch"]

    for pattern in patterns:
        bp = target.BreakpointCreateByName(pattern)
        if bp.GetNumLocations() > 0:
            result.AppendMessage(f"Breakpoint set on {pattern}")
            return
        else:
            # Remove the empty breakpoint
            target.BreakpointDelete(bp.GetID())

    result.AppendMessage(f"Could not find dispatch function for actor '{actor}'.")
    result.AppendMessage("Try: (lldb) image lookup -r -n .*dispatch.*")


def hew_bt_command(debugger, command, result, _internal_dict):
    """Show a Hew-focused backtrace, filtering out runtime internals.

    Usage: hew-bt
    """
    target = debugger.GetSelectedTarget()
    if not target:
        result.AppendMessage("No target selected.")
        return

    process = target.GetProcess()
    if not process:
        result.AppendMessage("No process running.")
        return

    thread = process.GetSelectedThread()
    if not thread:
        result.AppendMessage("No thread selected.")
        return

    skip_patterns = [
        "hew_runtime_",
        "__pthread",
        "__libc",
        "std::rt::",
        "core::ops::",
        "tokio::",
        "__GI_",
        "_start",
        "__libc_start",
    ]

    idx = 0
    for i in range(thread.GetNumFrames()):
        frame = thread.GetFrameAtIndex(i)
        name = frame.GetFunctionName() or "<unknown>"

        if any(skip in name for skip in skip_patterns):
            continue

        line_entry = frame.GetLineEntry()
        loc = ""
        if line_entry.IsValid():
            file_spec = line_entry.GetFileSpec()
            if file_spec.IsValid():
                loc = f" at {file_spec.GetFilename()}:{line_entry.GetLine()}"

        result.AppendMessage(f"  #{idx} {name}{loc}")
        idx += 1

    if idx == 0:
        result.AppendMessage("  (no user frames found -- try 'bt' for full backtrace)")


# ---------------------------------------------------------------------------
# Registration
# ---------------------------------------------------------------------------

def __lldb_init_module(debugger, _internal_dict):
    """Called by LLDB when this script is imported."""
    # Type summaries
    debugger.HandleCommand(
        'type summary add -F hew_lldb.hew_string_summary -x "hew_string"'
    )
    debugger.HandleCommand(
        'type summary add -F hew_lldb.hew_vec_summary -x "hew_vec"'
    )
    debugger.HandleCommand(
        'type summary add -F hew_lldb.hew_hashmap_summary -x "hew_hashmap"'
    )
    debugger.HandleCommand(
        'type summary add -F hew_lldb.hew_actor_ref_summary -x "hew_actor_ref"'
    )

    # Custom commands
    debugger.HandleCommand(
        "command script add -f hew_lldb.hew_actors_command hew-actors"
    )
    debugger.HandleCommand(
        "command script add -f hew_lldb.hew_break_receive_command hew-break-receive"
    )
    debugger.HandleCommand(
        "command script add -f hew_lldb.hew_bt_command hew-bt"
    )

    print("Hew LLDB extensions loaded. Commands: hew-actors, hew-break-receive, hew-bt")
