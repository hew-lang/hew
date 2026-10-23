#!/usr/bin/env python3
"""Exercise generated async calls through the actual asyncio consumer."""

import asyncio
import ctypes as c
import importlib.util
from pathlib import Path
import sys


ROOT = Path(__file__).resolve().parents[2]
spec = importlib.util.spec_from_file_location(
    "asyncio_orders", ROOT / "examples/embedding/asyncio_orders.py"
)
consumer = importlib.util.module_from_spec(spec)
spec.loader.exec_module(consumer)
Pointer, OwnerSlot = consumer.Pointer, consumer.OwnerSlot


async def check(library_file, directory):
    library = consumer.Library(library_file)

    async def discard_text(owner):
        library.native.hew_host_text_release(owner)

    open_marker = library.function(
        "async_open_marker", c.c_int32, Pointer, c.c_uint8, OwnerSlot, OwnerSlot
    )

    def marker(path, fail):
        owner, error = Pointer(), Pointer()
        with library.text(str(path)) as text:
            library.check(
                open_marker(text, fail, c.byref(owner), c.byref(error)), error
            )
        return owner

    try:
        value = await consumer.Call(
            library, "async_add", (20, 22), (c.c_int64, c.c_int64), c.c_int64
        ).wait()
        assert value.value == 42
        with library.text("hé\0雪") as text:
            value = await consumer.Call(
                library,
                "async_echo",
                (text,),
                (Pointer,),
                Pointer,
                discard=discard_text,
            ).wait()
            assert library.read_text(value) == "hé\0雪"
            await discard_text(value)
        for fail_close in (False, True):
            path = directory / f"cancel-{fail_close}"
            owner = marker(path, fail_close)
            if not fail_close:
                runtime, library.runtime = library.runtime, None
                try:
                    await consumer.Call(
                        library,
                        "async_wait",
                        (c.byref(owner),),
                        (OwnerSlot,),
                        Pointer,
                        discard=discard_text,
                    ).wait()
                    raise AssertionError("invalid runtime admitted consuming work")
                except consumer.HewFailure as refusal:
                    assert refusal.status == 7 and owner.value
                finally:
                    library.runtime = runtime
                unstarted = consumer.Call(
                    library,
                    "async_wait",
                    (c.byref(owner),),
                    (OwnerSlot,),
                    Pointer,
                    discard=discard_text,
                )
                cancelled_before_entry = asyncio.create_task(unstarted.wait())
                cancelled_before_entry.cancel()
                try:
                    await cancelled_before_entry
                    raise AssertionError("cancelled task entered the native call")
                except asyncio.CancelledError:
                    assert owner.value and unstarted.context is None
            call = consumer.Call(
                library,
                "async_wait",
                (c.byref(owner),),
                (OwnerSlot,),
                Pointer,
                discard=discard_text,
            )
            task = asyncio.create_task(call.wait())
            # An event-loop turn while the one-hour native wait is still pending
            # proves that start/poll did not block or wait for the timer.
            await asyncio.sleep(0)
            assert not task.done()
            assert not owner.value
            previous = call.owner.value
            error = Pointer()
            try:
                library.check(call.release(c.byref(call.owner), c.byref(error)), error)
                raise AssertionError("pending owner was released")
            except consumer.HewFailure as refusal:
                assert refusal.status == 7 and call.owner.value == previous
            assert path.read_bytes() == b""
            task.cancel()
            task.cancel()
            try:
                await task
                raise AssertionError("cancelled native work returned normally")
            except asyncio.CancelledError as cancellation:
                if fail_close:
                    assert "host cleanup failed" in str(cancellation.__cause__)
            assert path.read_bytes() == b"closed once"
        path = directory / "combined-fault"
        owner = marker(path, True)
        try:
            await consumer.Call(
                library,
                "async_fail",
                (c.byref(owner),),
                (OwnerSlot,),
                Pointer,
                discard=discard_text,
            ).wait()
            raise AssertionError("body and cleanup failures were lost")
        except consumer.HewFailure as failure:
            assert "host body failed" in str(failure)
            assert "host cleanup failed" in str(failure)
        assert not owner.value and path.read_bytes() == b"closed once"
    finally:
        library.close()
    assert not consumer._contexts


if __name__ == "__main__":
    destination = Path(sys.argv[2])
    destination.mkdir(parents=True, exist_ok=True)
    asyncio.run(check(Path(sys.argv[1]), destination))
