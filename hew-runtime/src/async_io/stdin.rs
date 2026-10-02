//! Standard input as a waiting resource.
//!
//! One process buffer holds what has been read from standard input and not
//! yet returned. A line read takes one line from it, filling it first when it
//! holds no complete line. A line handed to an operation that ends without
//! its result being taken goes back to the front of the buffer, so a cancelled
//! read loses nothing it consumed.
//!
//! On Unix the waiting task's worker reads descriptor 0 only after a
//! zero-timeout `poll` reports it readable, and otherwise waits on the stdin
//! reactor slot. Descriptor 0 stays blocking: `O_NONBLOCK` would change the
//! open file description the terminal and parent shell share. On Windows one
//! reader thread fills the buffer, bounded to [`BUFFER_LIMIT`], and reports
//! readiness on the same slot. WASI runs one thread with nothing else to
//! schedule while it waits, so there the read blocks at submission and the
//! operation is returned complete.

use std::io;
use std::sync::{Arc, Mutex};

use super::{HewAsyncIo, IoFailure, IoValue};
#[cfg(not(target_arch = "wasm32"))]
use crate::reactor::{Direction, Slot};
use crate::util::MutexExt;
use crate::wake::HewWaker;

#[derive(Default)]
struct Buffer {
    data: Vec<u8>,
    eof: bool,
    error: Option<io::Error>,
}

static STDIN: Mutex<Buffer> = Mutex::new(Buffer {
    data: Vec::new(),
    eof: false,
    error: None,
});

/// The most one read takes from the OS.
const READ_CHUNK: usize = 64 * 1024;

/// The most the Windows reader thread buffers ahead of the program.
#[cfg(windows)]
const BUFFER_LIMIT: usize = 64 * 1024;

#[cfg(windows)]
static ROOM: std::sync::Condvar = std::sync::Condvar::new();

/// One line taken from the buffer, its newline included; empty at end of
/// input. Dropped untaken, it returns to the front of the buffer.
pub(crate) struct Line(Option<Vec<u8>>);

impl Line {
    pub(super) fn bytes(&self) -> &[u8] {
        self.0.as_deref().unwrap_or_default()
    }

    /// The caller has copied the bytes out; nothing returns to the buffer.
    pub(super) fn consume(&mut self) {
        self.0 = None;
    }
}

impl Drop for Line {
    fn drop(&mut self) {
        if let Some(bytes) = self.0.take().filter(|bytes| !bytes.is_empty()) {
            let mut buffer = STDIN.lock_or_recover();
            buffer.data.splice(0..0, bytes);
        }
    }
}

fn line(bytes: Vec<u8>) -> IoValue {
    #[cfg(windows)]
    ROOM.notify_all();
    IoValue::StdinLine(Line(Some(bytes)))
}

/// Take the next line, or `None` when the buffer needs input it cannot get
/// without waiting. End of input is reported once per read that meets it, so
/// a terminal can deliver more input after an end-of-file keystroke.
fn attempt(buffer: &mut Buffer) -> Option<Result<IoValue, IoFailure>> {
    loop {
        if let Some(end) = buffer.data.iter().position(|&byte| byte == b'\n') {
            let bytes = buffer.data.drain(..=end).collect();
            return Some(Ok(line(bytes)));
        }
        if buffer.eof || buffer.error.is_some() {
            if !buffer.data.is_empty() {
                let bytes = std::mem::take(&mut buffer.data);
                return Some(Ok(line(bytes)));
            }
            if let Some(error) = buffer.error.take() {
                #[cfg(windows)]
                ROOM.notify_all();
                return Some(Err(IoFailure::from_io("read standard input", &error)));
            }
            buffer.eof = false;
            return Some(Ok(line(Vec::new())));
        }
        #[cfg(unix)]
        match fill(buffer) {
            Ok(true) => {}
            Ok(false) => return None,
            Err(error) => buffer.error = Some(error),
        }
        #[cfg(windows)]
        {
            ensure_reader();
            return None;
        }
        #[cfg(target_arch = "wasm32")]
        if let Err(error) = fill_blocking(buffer) {
            buffer.error = Some(error);
        }
    }
}

/// Read once from standard input, waiting for it.
#[cfg(target_arch = "wasm32")]
fn fill_blocking(buffer: &mut Buffer) -> io::Result<()> {
    use std::io::Read;
    let mut chunk = vec![0; READ_CHUNK];
    loop {
        match std::io::stdin().read(&mut chunk) {
            Ok(0) => buffer.eof = true,
            Ok(count) => buffer.data.extend_from_slice(&chunk[..count]),
            Err(error) if error.kind() == io::ErrorKind::Interrupted => continue,
            Err(error) => return Err(error),
        }
        return Ok(());
    }
}

/// Read once from descriptor 0 if that cannot block. Returns whether the
/// buffer changed.
#[cfg(unix)]
fn fill(buffer: &mut Buffer) -> io::Result<bool> {
    loop {
        let mut poll = libc::pollfd {
            fd: libc::STDIN_FILENO,
            events: libc::POLLIN,
            revents: 0,
        };
        // SAFETY: one valid pollfd, zero timeout.
        match unsafe { libc::poll(&raw mut poll, 1, 0) } {
            0 => return Ok(false),
            ready if ready < 0 => {
                let error = io::Error::last_os_error();
                if error.kind() == io::ErrorKind::Interrupted {
                    continue;
                }
                return Err(error);
            }
            _ => {}
        }
        let start = buffer.data.len();
        buffer.data.resize(start + READ_CHUNK, 0);
        // SAFETY: the region past `start` is READ_CHUNK initialized bytes.
        let count = unsafe {
            libc::read(
                libc::STDIN_FILENO,
                buffer.data[start..].as_mut_ptr().cast(),
                READ_CHUNK,
            )
        };
        buffer
            .data
            .truncate(start + usize::try_from(count).unwrap_or(0));
        match count {
            0 => buffer.eof = true,
            count if count < 0 => {
                let error = io::Error::last_os_error();
                // macOS poll(2) reports POLLNVAL for /dev/null, so read decides:
                // a closed descriptor 0 has no input to deliver.
                if error.raw_os_error() == Some(libc::EBADF) {
                    buffer.eof = true;
                    return Ok(true);
                }
                match error.kind() {
                    io::ErrorKind::Interrupted => continue,
                    io::ErrorKind::WouldBlock => return Ok(false),
                    _ => return Err(error),
                }
            }
            _ => {}
        }
        return Ok(true);
    }
}

/// Start the reader thread once. It fills the buffer while there is room and
/// pauses after end of input or an error until a read has reported it.
#[cfg(windows)]
pub(crate) fn ensure_reader() {
    static STARTED: std::sync::Once = std::sync::Once::new();
    STARTED.call_once(|| {
        let spawned = std::thread::Builder::new()
            .name("hew-stdin".into())
            .spawn(reader_loop);
        if let Err(error) = spawned {
            STDIN.lock_or_recover().error = Some(error);
            crate::reactor::report_stdin_ready();
        }
    });
}

#[cfg(windows)]
fn reader_loop() {
    use std::io::Read;
    let mut chunk = vec![0; READ_CHUNK];
    loop {
        {
            let mut buffer = STDIN.lock_or_recover();
            while buffer.data.len() >= BUFFER_LIMIT || buffer.eof || buffer.error.is_some() {
                buffer = ROOM
                    .wait(buffer)
                    .unwrap_or_else(std::sync::PoisonError::into_inner);
            }
        }
        let read = std::io::stdin().read(&mut chunk);
        {
            let mut buffer = STDIN.lock_or_recover();
            match read {
                Ok(0) => buffer.eof = true,
                Ok(count) => buffer.data.extend_from_slice(&chunk[..count]),
                Err(error) if error.kind() == io::ErrorKind::Interrupted => continue,
                Err(error) => buffer.error = Some(error),
            }
        }
        crate::reactor::report_stdin_ready();
    }
}

/// Take a line or wait for input. Concurrent reads take lines in the order
/// they started: a read behind another waits for its turn without
/// attempting. The buffer lock is held from the turn check until the
/// operation waits on the slot, so input that arrives in between finds the
/// waiter.
#[cfg(not(target_arch = "wasm32"))]
pub(super) fn advance(operation: &Arc<HewAsyncIo>, slot: &Arc<Slot>) {
    let mut buffer = STDIN.lock_or_recover();
    if !slot.take_turn(operation) {
        return;
    }
    let result = match attempt(&mut buffer) {
        Some(result) => result,
        None => match slot.wait(Direction::Read, operation) {
            Ok(()) => return,
            Err(error) => Err(error),
        },
    };
    drop(buffer);
    operation.complete(result);
}

/// Start reading one line of standard input. The result is the line with its
/// newline, or the final unterminated line; empty bytes mean end of input.
/// Take it with `hew_async_io_take_bytes`; a line that is never taken returns
/// to the input buffer when the operation is freed.
///
/// # Safety
/// `waker` is null or a borrowed valid `HewWaker`; its context is retained.
#[cfg(not(target_arch = "wasm32"))]
#[no_mangle]
pub unsafe extern "C" fn hew_async_stdin_read_line(waker: *const HewWaker) -> *const HewAsyncIo {
    // SAFETY: the caller supplies the borrowed readiness descriptor.
    unsafe { super::net::start_stdin_line(crate::reactor::stdin_slot(), waker) }
}

/// Read one line of standard input at submission; the operation returned is
/// already complete. The result contract matches the native entry.
///
/// # Safety
/// `waker` is null or a borrowed valid `HewWaker`; its context is retained.
#[cfg(target_arch = "wasm32")]
#[no_mangle]
pub unsafe extern "C" fn hew_async_stdin_read_line(waker: *const HewWaker) -> *const HewAsyncIo {
    // SAFETY: the caller lends the descriptor through construction.
    let operation = unsafe { HewAsyncIo::new(waker) };
    let Some(result) = attempt(&mut STDIN.lock_or_recover()) else {
        unreachable!("a blocking fill ends in a line, end of input or an error")
    };
    operation.complete(result);
    Arc::into_raw(operation)
}

#[cfg(test)]
mod tests {
    use super::*;

    fn take_line(buffer: &mut Buffer) -> Line {
        match attempt(buffer) {
            Some(Ok(IoValue::StdinLine(line))) => line,
            _ => panic!("a buffered line is ready"),
        }
    }

    #[test]
    fn an_untaken_line_returns_to_the_front_and_a_taken_one_does_not() {
        let mut buffer = STDIN.lock_or_recover();
        let saved = std::mem::take(&mut *buffer);
        buffer.data = b"first\r\nsecond\npartial".to_vec();
        let first = take_line(&mut buffer);
        assert_eq!(first.bytes(), b"first\r\n");
        drop(buffer);
        // Freed without a take, as when a deadline cancels the read.
        drop(first);
        let mut buffer = STDIN.lock_or_recover();
        let mut again = take_line(&mut buffer);
        assert_eq!(again.bytes(), b"first\r\n");
        again.consume();
        drop(buffer);
        drop(again);
        let mut buffer = STDIN.lock_or_recover();
        let mut second = take_line(&mut buffer);
        assert_eq!(second.bytes(), b"second\n");
        second.consume();
        // The unterminated tail waits for more input until end of input.
        buffer.eof = true;
        let mut tail = take_line(&mut buffer);
        assert_eq!(tail.bytes(), b"partial");
        tail.consume();
        let end = take_line(&mut buffer);
        assert!(end.bytes().is_empty());
        assert!(!buffer.eof, "end of input is reported once per read");
        *buffer = saved;
        drop(buffer);
        drop(second);
        drop(tail);
        drop(end);
    }
}
