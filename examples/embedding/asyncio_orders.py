#!/usr/bin/env python3
"""Run the supplied-directory Hew order copier from an asyncio event loop."""

import argparse
import asyncio
from contextlib import contextmanager
import ctypes as c
from itertools import count
from pathlib import Path


Pointer = c.c_void_p
OwnerSlot = c.POINTER(Pointer)
Callback = c.CFUNCTYPE(None, Pointer)
_contexts = {}
_context_ids = count(1)


class Waker(c.Structure):
    _fields_ = [
        ("context", Pointer),
        ("wake", Callback),
        ("retain", Callback),
        ("release", Callback),
    ]


@Callback
def _wake(identity):
    context = _contexts[identity]
    if not context.closed:
        context.loop.call_soon_threadsafe(context.event.set)


@Callback
def _retain(identity):
    # ctypes callback entries hold the GIL, including entries from I/O threads.
    _contexts[identity].references += 1


@Callback
def _release(identity):
    context = _contexts[identity]
    context.references -= 1
    if not context.references:
        del _contexts[identity]


class WakeContext:
    def __init__(self, *keepalive):
        self.loop = asyncio.get_running_loop()
        self.event = asyncio.Event()
        self.keepalive = keepalive
        self.references = 1
        self.closed = False
        self.identity = next(_context_ids)
        _contexts[self.identity] = self
        self.descriptor = Waker(self.identity, _wake, _retain, _release)


class HewFailure(Exception):
    def __init__(self, status, message):
        super().__init__(message)
        self.status = status


class Library:
    """The same-build exports used by this consumer; no background host thread."""

    def __init__(self, filename):
        self.native = c.CDLL(str(Path(filename).resolve()))
        self.runtime = self.function("hew_runtime_new", Pointer)()
        self.function("hew_runtime_shutdown", None, Pointer)
        self.function("hew_runtime_release", None, Pointer)
        self.function("hew_host_error_code", c.c_int32, Pointer)
        self.function("hew_host_error_message", Pointer, Pointer, c.POINTER(c.c_size_t))
        self.function("hew_host_error_release", None, Pointer)
        self.function(
            "hew_host_text_from_utf8",
            c.c_int32,
            Pointer,
            c.c_size_t,
            OwnerSlot,
            OwnerSlot,
        )
        self.function("hew_host_text_data", Pointer, Pointer, c.POINTER(c.c_size_t))
        self.function("hew_host_text_release", None, Pointer)

    def function(self, name, result, *arguments):
        function = getattr(self.native, name)
        function.restype = result
        function.argtypes = list(arguments)
        return function

    def check(self, status, error):
        if not status:
            assert not error.value
            return
        assert error.value
        length = c.c_size_t()
        data = self.native.hew_host_error_message(error, c.byref(length))
        message = c.string_at(data, length.value).decode("utf-8")
        assert self.native.hew_host_error_code(error) == status
        self.native.hew_host_error_release(error)
        error.value = None
        raise HewFailure(status, message)

    @contextmanager
    def text(self, value):
        data = value.encode("utf-8")
        text, error = Pointer(), Pointer()
        self.check(
            self.native.hew_host_text_from_utf8(
                data, len(data), c.byref(text), c.byref(error)
            ),
            error,
        )
        try:
            yield text
        finally:
            self.native.hew_host_text_release(text)

    def read_text(self, owner):
        length = c.c_size_t()
        data = self.native.hew_host_text_data(owner, c.byref(length))
        return c.string_at(data, length.value).decode("utf-8")

    def close(self):
        self.native.hew_runtime_shutdown(self.runtime)
        self.native.hew_runtime_release(self.runtime)
        self.runtime = None


class Call:
    """Own one generated call and its input loans until terminal cleanup."""

    def __init__(
        self,
        library,
        prefix,
        arguments,
        argument_types,
        result_type=None,
        *,
        discard=None,
    ):
        if result_type is Pointer and discard is None:
            raise ValueError(
                "owned results require asynchronous disposal after cancellation"
            )
        self.library = library
        self.discard = discard
        self.arguments = arguments
        self.context = None
        self.owner = Pointer()
        self.result = result_type() if result_type is not None else None
        self.poll = library.function(f"{prefix}_poll", c.c_int32, Pointer)
        self.cancel = library.function(f"{prefix}_cancel", None, Pointer)
        self.release = library.function(
            f"{prefix}_release", c.c_int32, OwnerSlot, OwnerSlot
        )
        result_arguments = [Pointer]
        if result_type is not None:
            result_arguments.append(c.POINTER(result_type))
        self.take = library.function(
            f"{prefix}_result", c.c_int32, *result_arguments, OwnerSlot
        )
        self.start = library.function(
            f"{prefix}_start",
            c.c_int32,
            Pointer,
            *argument_types,
            c.POINTER(Waker),
            OwnerSlot,
            OwnerSlot,
        )

    def _start(self):
        if self.context is not None:
            raise RuntimeError("a host call can be awaited only once")
        self.context = WakeContext(self.library, self.arguments)
        error = Pointer()
        try:
            self.library.check(
                self.start(
                    self.library.runtime,
                    *self.arguments,
                    c.byref(self.context.descriptor),
                    c.byref(self.owner),
                    c.byref(error),
                ),
                error,
            )
        except BaseException:
            self.context.closed = True
            raise
        finally:
            _release(self.context.identity)

    async def wait(self, *, cancellable=True):
        # No native owner exists if an asyncio task is cancelled before entering
        # this coroutine. Once started, every cancellable await is drained here.
        self._start()
        cancelled = False
        failure = None
        try:
            while True:
                self.context.event.clear()
                if self.poll(self.owner):
                    break
                try:
                    await self.context.event.wait()
                except asyncio.CancelledError:
                    cancelled = True
                    if cancellable:
                        self.cancel(self.owner)
            error = Pointer()
            arguments = [self.owner]
            if self.result is not None:
                arguments.append(c.byref(self.result))
            try:
                self.library.check(self.take(*arguments, c.byref(error)), error)
            except HewFailure as error:
                failure = error
        finally:
            # Every route above consumes the terminal result before release.
            # The retained wake context remains valid through late callback drops.
            if self.owner.value:
                error = Pointer()
                self.library.check(
                    self.release(c.byref(self.owner), c.byref(error)), error
                )
            self.context.closed = True
        if cancelled and cancellable:
            if failure is None and self.discard is not None:
                try:
                    await self.discard(self.result)
                except HewFailure as error:
                    failure = error
            raise asyncio.CancelledError() from failure
        if failure is not None:
            raise failure
        return self.result


async def copy_orders(library, input_directory, output_directory):
    input_owner, output_owner, instance = Pointer(), Pointer(), Pointer()
    error = Pointer()
    release_input = library.function(
        "HewOwned_std_24mfs_24mReadDir_release", c.c_int32, OwnerSlot, OwnerSlot
    )
    release_output = library.function(
        "HewOwned_std_24mfs_24mWriteDir_release", c.c_int32, OwnerSlot, OwnerSlot
    )

    async def shutdown(value):
        await Call(library, "orders_shutdown", (c.byref(value),), (OwnerSlot,)).wait(
            cancellable=False
        )

    try:
        for prefix, path, owner in [
            ("orders_open_input", input_directory, input_owner),
            ("orders_open_output", output_directory, output_owner),
        ]:
            open_directory = library.function(
                prefix, c.c_int32, Pointer, OwnerSlot, OwnerSlot
            )
            with library.text(str(path)) as text:
                library.check(
                    open_directory(text, c.byref(owner), c.byref(error)), error
                )
        instance = await Call(
            library,
            "orders_create",
            (c.byref(input_owner), c.byref(output_owner)),
            (OwnerSlot, OwnerSlot),
            Pointer,
            discard=shutdown,
        ).wait()
        result = await Call(
            library, "orders_run", (instance,), (Pointer,), c.c_int64
        ).wait()
        return result.value
    finally:
        if instance.value:
            await shutdown(instance)
        for release_owner, owner in [
            (release_input, input_owner),
            (release_output, output_owner),
        ]:
            library.check(release_owner(c.byref(owner), c.byref(error)), error)


async def main():
    parser = argparse.ArgumentParser(description=__doc__)
    parser.add_argument("library", type=Path)
    parser.add_argument("input", type=Path)
    parser.add_argument("output", type=Path)
    arguments = parser.parse_args()
    library = Library(arguments.library)
    try:
        print(await copy_orders(library, arguments.input, arguments.output))
    finally:
        library.close()


if __name__ == "__main__":
    asyncio.run(main())
