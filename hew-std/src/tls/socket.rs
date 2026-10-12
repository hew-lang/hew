//! A borrowed TLS operation may stop without closing its retained connection.

use hew_runtime::async_io::library::SocketCancellation;
use std::io::{self, Read, Write};
use std::net::TcpStream;
use std::sync::Arc;
use std::time::{Duration, Instant};

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
        Ok(Self {
            read_timeout: socket.read_timeout()?,
            write_timeout: socket.write_timeout()?,
            socket,
            cancellation: None,
        })
    }

    pub(super) fn set_cancellation(&mut self, cancellation: Option<Arc<SocketCancellation>>) {
        self.cancellation = cancellation;
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
}

impl std::ops::Deref for TlsTcp {
    type Target = TcpStream;

    fn deref(&self) -> &Self::Target {
        &self.socket
    }
}

impl Read for TlsTcp {
    fn read(&mut self, buffer: &mut [u8]) -> io::Result<usize> {
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

impl Write for TlsTcp {
    fn write(&mut self, buffer: &[u8]) -> io::Result<usize> {
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

    fn flush(&mut self) -> io::Result<()> {
        if self.cancellation.is_some() {
            let _ = self.wait(None)?;
        }
        self.socket.flush()
    }
}
