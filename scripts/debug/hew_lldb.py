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
    result.AppendMessage(
        "  - Set breakpoints on your functions: (lldb) break set -n main"
    )
    result.AppendMessage(
        "  - Break on actor dispatch: (lldb) break set -n hew_actor_send"
    )
    result.AppendMessage(
        "  - Break on actor creation: (lldb) break set -n hew_actor_spawn"
    )


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


def _dotted_name_pattern(type_name: str, method_name: str) -> str:
    """Return the POSIX ERE regex that matches the Hew mangled symbol for
    TypeName.method.

    Hew mangles impl methods as ``_HM<len><module>T<len><Type>F<len><method>``.
    Embedding the *exact* byte lengths for both type and method prevents
    prefix false-positives: ``T4Pair`` will never match ``T8PairList``, and
    ``F3sum`` will never match ``F7summary``.

    The regex is anchored at ``$`` so no suffix can sneak in after the
    method segment.  LLDB passes this to LLVM's POSIX ERE engine, which
    does not support ``\\d``; we use literal digits instead.
    """
    import re as _re

    t_len = len(type_name)
    m_len = len(method_name)
    # re.escape covers POSIX ERE specials for the name fragments; for plain
    # identifiers it is a no-op, but guards against dots or underscores in
    # type/method names that could otherwise widen the match.
    return f"T{t_len}{_re.escape(type_name)}F{m_len}{_re.escape(method_name)}$"


def hew_break_command(debugger, command, result, _internal_dict):
    """Set a breakpoint on a Hew function or method by Hew-source name.

    Usage:
        hew-break <name>               # plain function: same as breakpoint set -n name
        hew-break <TypeName>.<method>  # impl method: resolved via mangled-symbol regex

    Plain names like ``add`` and ``main`` are passed directly to
    BreakpointCreateByName, which matches DWARF DW_AT_name entries.

    Dotted names like ``Pair.sum`` cannot be matched by plain -n because
    LLDB has no Hew demangler.  The Hew mangler encodes them as:
        _HM<len><module>T<len><TypeName>F<len><method>
    so ``hew-break Pair.sum`` uses a regex breakpoint via
    ``_dotted_name_pattern`` with exact length prefixes, which prevents
    types whose names merely contain the requested name (e.g. PairList)
    from being matched.
    """
    name = command.strip()
    if not name:
        result.AppendMessage(
            "Usage: hew-break <name>  |  hew-break <TypeName>.<method>"
        )
        return

    target = debugger.GetSelectedTarget()
    if not target:
        result.AppendMessage("No target selected.")
        return

    if "." in name:
        parts = name.split(".", 1)
        pattern = _dotted_name_pattern(parts[0], parts[1])
        bp = target.BreakpointCreateByRegex(pattern)
    else:
        bp = target.BreakpointCreateByName(name)

    n_locs = bp.GetNumLocations()
    if n_locs > 0:
        result.AppendMessage(f"Breakpoint {bp.GetID()}: {n_locs} location(s).")
        for i in range(n_locs):
            loc = bp.GetLocationAtIndex(i)
            addr = loc.GetAddress()
            line_entry = addr.GetLineEntry()
            if line_entry.IsValid():
                file_spec = line_entry.GetFileSpec()
                result.AppendMessage(
                    f"  where = {file_spec.GetFilename()}:{line_entry.GetLine()}"
                )
    else:
        target.BreakpointDelete(bp.GetID())
        result.AppendMessage(f"hew-break: no locations found for '{name}'.")
        if "." in name:
            result.AppendMessage(
                "Tip: check that the type and method names match the Hew source."
            )
        else:
            result.AppendMessage(
                "Tip: try 'image lookup -r -n <name>' to inspect symbols."
            )


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
    debugger.HandleCommand('type summary add -F hew_lldb.hew_vec_summary -x "hew_vec"')
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
    debugger.HandleCommand("command script add -f hew_lldb.hew_bt_command hew-bt")
    debugger.HandleCommand("command script add -f hew_lldb.hew_break_command hew-break")

    print(
        "Hew LLDB extensions loaded. "
        "Commands: hew-actors, hew-break, hew-break-receive, hew-bt"
    )
