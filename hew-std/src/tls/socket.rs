//! A borrowed TLS operation may stop without closing its retained connection.

use hew_runtime::async_io::library::SocketCancellation;
use std::io::{self, Read, Write};
use std::net::TcpStream;
use std::sync::Arc;
use std::time::{Duration, Instant};

#[cfg(windows)]
use std::os::windows::io::AsRawSocket;
#[cfg(windows)]
use windows_sys::Win32::Networking::WinSock::{
    WSAGetLastError, WSAPoll, POLLRDNORM, POLLWRNORM, SOCKET_ERROR, WSAPOLLFD, WSAPOLL_EVENT_FLAGS,
};

const CANCEL_INTERVAL: Duration = Duration::from_millis(25);

#[derive(Debug)]
pub(super) struct TlsTcp {
    socket: TcpStream,
    read_timeout: Option<Duration>,
    write_timeout: Option<Duration>,
    cancellation: Option<Arc<SocketCancellation>>,
}

impl TlsTcp {
    pub(super) fn new(socket: TcpStream) -> io::Result<Self> {
        // A Winsock blocking timeout can leave the stream indeterminate.
        // Non-blocking I/O reports exact progress before a cancellable wait.
        #[cfg(windows)]
        socket.set_nonblocking(true)?;
        Ok(Self {
            read_timeout: socket.read_timeout()?,
            write_timeout: socket.write_timeout()?,
            socket,
            cancellation: None,
        })
    }

    pub(super) fn set_cancellation(&mut self, cancellation: Option<Arc<SocketCancellation>>) {
        self.cancellation = cancellation;
        #[cfg(not(windows))]
        if self.cancellation.is_none() {
            let _ = self.socket.set_read_timeout(self.read_timeout);
            let _ = self.socket.set_write_timeout(self.write_timeout);
        }
    }

    pub(super) fn set_read_timeout(&mut self, timeout: Option<Duration>) -> io::Result<()> {
        self.socket.set_read_timeout(timeout)?;
        self.read_timeout = timeout;
        Ok(())
    }

    fn wait(&self, deadline: Option<Instant>) -> io::Result<Duration> {
        if self
            .cancellation
            .as_ref()
            .is_some_and(|cancellation| cancellation.is_cancelled())
        {
            // WouldBlock preserves rustls's partially received and buffered
            // records. Interrupted would be retried by complete_io itself.
            return Err(io::ErrorKind::WouldBlock.into());
        }
        let remaining = deadline.map_or(CANCEL_INTERVAL, |deadline| {
            deadline.saturating_duration_since(Instant::now())
        });
        if remaining.is_zero() {
            return Err(io::ErrorKind::TimedOut.into());
        }
        Ok(remaining.min(CANCEL_INTERVAL))
    }

    #[cfg(windows)]
    fn socket_io(
        &mut self,
        timeout: Option<Duration>,
        interest: WSAPOLL_EVENT_FLAGS,
        mut operation: impl FnMut(&mut TcpStream) -> io::Result<usize>,
    ) -> io::Result<usize> {
        let deadline = timeout.map(|timeout| Instant::now() + timeout);
        loop {
            let _ = self.wait(deadline)?;
            match operation(&mut self.socket) {
                Err(error) if error.kind() == io::ErrorKind::WouldBlock => {}
                result => return result,
            }
            let timeout = if self.cancellation.is_none() && deadline.is_none() {
                -1
            } else {
                i32::try_from(self.wait(deadline)?.as_millis())
                    .expect("cancellation interval fits i32 milliseconds")
            };
            let mut poll = WSAPOLLFD {
                fd: usize::try_from(self.socket.as_raw_socket())
                    .expect("Windows socket fits its ABI"),
                events: interest,
                revents: 0,
            };
            // SAFETY: one live socket in a writable poll descriptor.
            if unsafe { WSAPoll(&raw mut poll, 1, timeout) } == SOCKET_ERROR {
                // SAFETY: reads this thread's Winsock error from the failed poll.
                return Err(io::Error::from_raw_os_error(unsafe { WSAGetLastError() }));
            }
        }
    }
}

impl std::ops::Deref for TlsTcp {
    type Target = TcpStream;

    fn deref(&self) -> &Self::Target {
        &self.socket
    }
}

impl Read for TlsTcp {
    fn read(&mut self, buffer: &mut [u8]) -> io::Result<usize> {
        #[cfg(windows)]
        {
            self.socket_io(self.read_timeout, POLLRDNORM, |socket| socket.read(buffer))
        }
        #[cfg(not(windows))]
        {
            if self.cancellation.is_none() {
                return self.socket.read(buffer);
            }
            let deadline = self.read_timeout.map(|timeout| Instant::now() + timeout);
            loop {
                self.socket.set_read_timeout(Some(self.wait(deadline)?))?;
                match self.socket.read(buffer) {
                    Err(error)
                        if matches!(
                            error.kind(),
                            io::ErrorKind::WouldBlock | io::ErrorKind::TimedOut
                        ) => {}
                    result => return result,
                }
            }
        }
    }
}

impl Write for TlsTcp {
    fn write(&mut self, buffer: &[u8]) -> io::Result<usize> {
        #[cfg(windows)]
        {
            self.socket_io(self.write_timeout, POLLWRNORM, |socket| {
                socket.write(buffer)
            })
        }
        #[cfg(not(windows))]
        {
            if self.cancellation.is_none() {
                return self.socket.write(buffer);
            }
            let deadline = self.write_timeout.map(|timeout| Instant::now() + timeout);
            loop {
                self.socket.set_write_timeout(Some(self.wait(deadline)?))?;
                match self.socket.write(buffer) {
                    Err(error)
                        if matches!(
                            error.kind(),
                            io::ErrorKind::WouldBlock | io::ErrorKind::TimedOut
                        ) => {}
                    result => return result,
                }
            }
        }
    }

    fn flush(&mut self) -> io::Result<()> {
        if self.cancellation.is_some() {
            let _ = self.wait(None)?;
        }
        self.socket.flush()
    }
}
