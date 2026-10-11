use std::cell::UnsafeCell;
use std::fs::File;
use std::io;
use std::os::windows::io::{AsRawHandle, FromRawHandle};
use std::ptr;
use std::sync::Mutex;

use windows_sys::Win32::Foundation::{
    ERROR_BROKEN_PIPE, ERROR_IO_PENDING, ERROR_PIPE_CONNECTED, GENERIC_WRITE, INVALID_HANDLE_VALUE,
};
use windows_sys::Win32::Storage::FileSystem::{
    CreateFileW, ReadFile, FILE_FLAG_FIRST_PIPE_INSTANCE, FILE_FLAG_OVERLAPPED, OPEN_EXISTING,
    PIPE_ACCESS_INBOUND,
};
use windows_sys::Win32::System::Pipes::{
    ConnectNamedPipe, CreateNamedPipeW, PIPE_REJECT_REMOTE_CLIENTS,
};
use windows_sys::Win32::System::IO::{CancelIoEx, GetOverlappedResult, OVERLAPPED};

use crate::util::MutexExt;

const READ_CAPACITY: usize = 64 * 1024;

pub(super) fn output_pipe() -> io::Result<(File, File)> {
    let name: Vec<u16> = format!(
        r"\\.\pipe\hew-{}-{:032x}",
        std::process::id(),
        rand::random::<u128>()
    )
    .encode_utf16()
    .chain(Some(0))
    .collect();
    // SAFETY: the name is terminated; the parent handle is private and asynchronous.
    let parent = unsafe {
        CreateNamedPipeW(
            name.as_ptr(),
            PIPE_ACCESS_INBOUND | FILE_FLAG_OVERLAPPED | FILE_FLAG_FIRST_PIPE_INSTANCE,
            PIPE_REJECT_REMOTE_CLIENTS,
            1,
            0,
            READ_CAPACITY as u32,
            0,
            ptr::null(),
        )
    };
    if parent == INVALID_HANDLE_VALUE {
        return Err(io::Error::last_os_error());
    }
    // SAFETY: the successful creation transfers one owned handle.
    let parent = unsafe { File::from_raw_handle(parent) };
    // SAFETY: this write end is private. Command duplicates it as inheritable
    // inside its spawn lock, so unrelated children cannot retain the writer.
    let child = unsafe {
        CreateFileW(
            name.as_ptr(),
            GENERIC_WRITE,
            0,
            ptr::null(),
            OPEN_EXISTING,
            0,
            ptr::null_mut(),
        )
    };
    if child == INVALID_HANDLE_VALUE {
        return Err(io::Error::last_os_error());
    }
    // SAFETY: the successful open transfers one owned handle.
    let child = unsafe { File::from_raw_handle(child) };
    // The client connected before this call, so no connect request can remain pending.
    // SAFETY: both ends are live and already connected.
    if unsafe { ConnectNamedPipe(parent.as_raw_handle(), ptr::null_mut()) } == 0 {
        let error = io::Error::last_os_error();
        if error.raw_os_error() != Some(ERROR_PIPE_CONNECTED as i32) {
            return Err(error);
        }
    }
    Ok((parent, child))
}

#[repr(C)]
pub(crate) struct PipeRead {
    overlapped: OVERLAPPED,
    context: usize,
    buffer: [u8; READ_CAPACITY],
}

impl PipeRead {
    /// # Safety
    /// `overlapped` is this read's dequeued completion, retained by its slot.
    pub(crate) unsafe fn context(overlapped: usize) -> usize {
        // SAFETY: OVERLAPPED is the first field and its completion owns the slot.
        unsafe { (*(overlapped as *const Self)).context }
    }
}

#[derive(Default)]
struct ReadState {
    bound: bool,
    pending: bool,
    result: Option<Result<usize, i32>>,
    offset: usize,
}

pub(crate) struct WindowsPipe {
    file: File,
    state: Mutex<ReadState>,
    read: Box<UnsafeCell<PipeRead>>,
}

// SAFETY: state serializes access; pending reads expose neither kernel buffer
// nor OVERLAPPED. Each completion retains the owning reactor slot until dequeued.
unsafe impl Send for WindowsPipe {}
// SAFETY: the same synchronization and retained-completion protocol applies.
unsafe impl Sync for WindowsPipe {}

impl std::fmt::Debug for WindowsPipe {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("WindowsPipe").finish_non_exhaustive()
    }
}

impl WindowsPipe {
    pub(crate) fn new(file: File) -> Self {
        Self {
            file,
            state: Mutex::new(ReadState::default()),
            // SAFETY: zero initializes OVERLAPPED and the byte buffer.
            read: Box::new(UnsafeCell::new(unsafe { std::mem::zeroed() })),
        }
    }

    pub(crate) fn readable(&self) -> bool {
        self.state.lock_or_recover().result.is_some()
    }

    pub(crate) fn read(&self, buffer: &mut [u8]) -> io::Result<usize> {
        let mut state = self.state.lock_or_recover();
        match state.result {
            None => Err(io::ErrorKind::WouldBlock.into()),
            Some(Err(code)) if code == ERROR_BROKEN_PIPE as i32 => Ok(0),
            Some(Err(code)) => Err(io::Error::from_raw_os_error(code)),
            Some(Ok(0)) => Ok(0),
            Some(Ok(count)) => {
                let length = buffer.len().min(count - state.offset);
                // SAFETY: a result is published only after completion is dequeued.
                let read = unsafe { &*self.read.get() };
                buffer[..length].copy_from_slice(&read.buffer[state.offset..state.offset + length]);
                state.offset += length;
                if state.offset == count {
                    state.result = None;
                    state.offset = 0;
                }
                Ok(length)
            }
        }
    }

    pub(crate) fn arm(&self, poller: &crate::io_time::Poller, context: usize) -> io::Result<()> {
        let mut state = self.state.lock_or_recover();
        if state.result.is_some() {
            return poller.post_pipe_ready(context);
        }
        debug_assert!(!state.pending);
        if !state.bound {
            poller.bind_pipe(self.file.as_raw_handle())?;
            state.bound = true;
        }
        // SAFETY: no read is pending, and the caller retains the slot until completion.
        let read = unsafe { &mut *self.read.get() };
        // SAFETY: all-zero OVERLAPPED starts a new request.
        read.overlapped = unsafe { std::mem::zeroed() };
        read.context = context;
        // SAFETY: overlapped and the bounded buffer stay live and untouched until dequeued.
        let started = unsafe {
            ReadFile(
                self.file.as_raw_handle(),
                read.buffer.as_mut_ptr(),
                READ_CAPACITY as u32,
                ptr::null_mut(),
                &raw mut read.overlapped,
            )
        };
        if started != 0 {
            state.pending = true;
            return Ok(());
        }
        let error = io::Error::last_os_error();
        if error.raw_os_error() == Some(ERROR_IO_PENDING as i32) {
            state.pending = true;
            return Ok(());
        }
        state.result = Some(Err(error.raw_os_error().unwrap_or(libc::EIO)));
        poller.post_pipe_ready(context)
    }

    pub(crate) fn complete(&self) {
        let mut state = self.state.lock_or_recover();
        if !state.pending {
            return;
        }
        let mut count = 0;
        // SAFETY: the completion was dequeued; the kernel no longer accesses this read.
        let read = unsafe { &*self.read.get() };
        // SAFETY: this completed request belongs to the live file; never wait here.
        let ok = unsafe {
            GetOverlappedResult(
                self.file.as_raw_handle(),
                &raw const read.overlapped,
                &raw mut count,
                0,
            )
        };
        state.pending = false;
        state.result = Some(if ok != 0 {
            Ok(count as usize)
        } else {
            Err(io::Error::last_os_error()
                .raw_os_error()
                .unwrap_or(libc::EIO))
        });
    }

    pub(crate) fn cancel(&self) {
        let state = self.state.lock_or_recover();
        if state.pending {
            // SAFETY: cancel does not release the retained slot or its pending buffers.
            unsafe {
                CancelIoEx(self.file.as_raw_handle(), self.read.get().cast());
            }
        }
    }
}
