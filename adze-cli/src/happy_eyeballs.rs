//! Happy Eyeballs (RFC 8305) TCP connector for ureq.
//!
//! Drop-in replacement for [`ureq::unversioned::transport::TcpConnector`] that
//! races IPv6 and IPv4 connections with a 250 ms stagger.  Whichever address
//! family connects first wins.

use std::fmt;
use std::io::{self, Read, Write};
use std::net::{SocketAddr, TcpStream};
use std::sync::mpsc;
use std::time::Duration;

use ureq::unversioned::transport::time::Duration as UreqDuration;
use ureq::unversioned::transport::{
    Buffers, ConnectionDetails, Connector, Either, LazyBuffers, NextTimeout, Transport,
};
use ureq::Error;

/// RFC 8305 §5 — delay before starting the next address family.
const RESOLUTION_DELAY: Duration = Duration::from_millis(250);

/// Fallback if the caller specifies no timeout.
const DEFAULT_TIMEOUT: Duration = Duration::from_secs(30);

// ── Connector ────────────────────────────────────────────────────────────────

/// TCP connector implementing Happy Eyeballs (RFC 8305).
///
/// Interleaves IPv6 and IPv4 addresses, then races connection attempts with a
/// 250 ms stagger.  The first successful `TcpStream` is returned; all other
/// in-flight attempts are abandoned.
#[derive(Default)]
pub struct HappyEyeballsConnector;

impl<In: Transport> Connector<In> for HappyEyeballsConnector {
    type Out = Either<In, HewTcpTransport>;

    fn connect(
        &self,
        details: &ConnectionDetails,
        chained: Option<In>,
    ) -> Result<Option<Self::Out>, Error> {
        // A prior connector (e.g. SOCKS proxy) already established a
        // transport — pass it through unchanged.
        if chained.is_some() {
            return Ok(chained.map(Either::A));
        }

        let config = details.config;
        let addrs: Vec<SocketAddr> = details.addrs.iter().copied().collect();
        let timeout = details.timeout.not_zero().map_or(DEFAULT_TIMEOUT, |d| *d);

        let stream = connect_happy_eyeballs(&addrs, timeout).map_err(Error::Io)?;

        if config.no_delay() {
            stream.set_nodelay(true).map_err(Error::Io)?;
        }

        let buffers = LazyBuffers::new(config.input_buffer_size(), config.output_buffer_size());
        Ok(Some(Either::B(HewTcpTransport::new(stream, buffers))))
    }
}

impl fmt::Debug for HappyEyeballsConnector {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("HappyEyeballsConnector").finish()
    }
}

// ── Happy Eyeballs algorithm ─────────────────────────────────────────────────

/// Interleave addresses per RFC 8305 §4: alternate between families, IPv6
/// first.
fn interleave_addrs(addrs: &[SocketAddr]) -> Vec<SocketAddr> {
    let v6: Vec<_> = addrs.iter().filter(|a| a.is_ipv6()).copied().collect();
    let v4: Vec<_> = addrs.iter().filter(|a| a.is_ipv4()).copied().collect();

    let mut result = Vec::with_capacity(addrs.len());
    let (mut i6, mut i4) = (v6.into_iter(), v4.into_iter());
    loop {
        match (i6.next(), i4.next()) {
            (Some(a), Some(b)) => {
                result.push(a);
                result.push(b);
            }
            (Some(a), None) => {
                result.push(a);
                result.extend(i6);
                break;
            }
            (None, Some(b)) => {
                result.push(b);
                result.extend(i4);
                break;
            }
            (None, None) => break,
        }
    }
    result
}

/// Race TCP connections per RFC 8305.
///
/// Each successive address attempt is staggered by [`RESOLUTION_DELAY`].
/// The first successful `TcpStream` is returned immediately; remaining
/// in-flight threads are abandoned (they will finish on their own and their
/// results are silently dropped).
fn connect_happy_eyeballs(addrs: &[SocketAddr], timeout: Duration) -> io::Result<TcpStream> {
    let sorted = interleave_addrs(addrs);

    if sorted.is_empty() {
        return Err(io::Error::new(
            io::ErrorKind::InvalidInput,
            "no addresses to connect to",
        ));
    }

    // Single address — no need to race.
    if sorted.len() == 1 {
        return TcpStream::connect_timeout(&sorted[0], timeout);
    }

    let (tx, rx) = mpsc::channel();

    for (i, addr) in sorted.into_iter().enumerate() {
        let tx = tx.clone();
        std::thread::spawn(move || {
            if i > 0 {
                let stagger = u32::try_from(i).expect("address count fits in u32");
                std::thread::sleep(RESOLUTION_DELAY * stagger);
            }
            let _ = tx.send(TcpStream::connect_timeout(&addr, timeout));
        });
    }

    // Close our sender so the channel drains when all threads finish.
    drop(tx);

    let mut last_err = io::Error::new(
        io::ErrorKind::ConnectionRefused,
        "all connection attempts failed",
    );

    for result in &rx {
        match result {
            Ok(stream) => return Ok(stream),
            Err(e) => last_err = e,
        }
    }

    Err(last_err)
}

// ── Transport ────────────────────────────────────────────────────────────────
//
// `ureq::unversioned::transport::tcp::TcpTransport` is not re-exported, so we
// provide our own equivalent wrapping a `TcpStream` + `LazyBuffers`.

/// TCP transport wrapping a [`TcpStream`] with lazy I/O buffers.
pub struct HewTcpTransport {
    stream: TcpStream,
    buffers: LazyBuffers,
    timeout_write: Option<UreqDuration>,
    timeout_read: Option<UreqDuration>,
}

impl HewTcpTransport {
    fn new(stream: TcpStream, buffers: LazyBuffers) -> Self {
        Self {
            stream,
            buffers,
            timeout_read: None,
            timeout_write: None,
        }
    }
}

/// Update a stream timeout only when the new value differs from the cached one
/// to avoid unnecessary syscalls.
fn maybe_update_timeout(
    timeout: NextTimeout,
    previous: &mut Option<UreqDuration>,
    stream: &TcpStream,
    f: impl Fn(&TcpStream, Option<Duration>) -> io::Result<()>,
) -> io::Result<()> {
    let maybe_timeout = timeout.not_zero();
    if maybe_timeout != *previous {
        (f)(stream, maybe_timeout.map(|t| *t))?;
        *previous = maybe_timeout;
    }
    Ok(())
}

impl Transport for HewTcpTransport {
    fn buffers(&mut self) -> &mut dyn Buffers {
        &mut self.buffers
    }

    fn transmit_output(&mut self, amount: usize, timeout: NextTimeout) -> Result<(), Error> {
        maybe_update_timeout(
            timeout,
            &mut self.timeout_write,
            &self.stream,
            TcpStream::set_write_timeout,
        )
        .map_err(Error::Io)?;

        let output = &self.buffers.output()[..amount];
        match self.stream.write_all(output) {
            Ok(()) => Ok(()),
            Err(e) if e.kind() == io::ErrorKind::TimedOut => Err(Error::Timeout(timeout.reason)),
            Err(e) => Err(Error::Io(e)),
        }
    }

    fn await_input(&mut self, timeout: NextTimeout) -> Result<bool, Error> {
        maybe_update_timeout(
            timeout,
            &mut self.timeout_read,
            &self.stream,
            TcpStream::set_read_timeout,
        )
        .map_err(Error::Io)?;

        let input = self.buffers.input_append_buf();
        let amount = match self.stream.read(input) {
            Ok(n) => n,
            Err(e) if e.kind() == io::ErrorKind::TimedOut => {
                return Err(Error::Timeout(timeout.reason));
            }
            Err(e) => return Err(Error::Io(e)),
        };
        self.buffers.input_appended(amount);

        Ok(amount > 0)
    }

    fn is_open(&mut self) -> bool {
        self.stream
            .set_nonblocking(true)
            .and_then(|()| {
                let mut buf = [0];
                let alive = matches!(self.stream.read(&mut buf), Err(e) if e.kind() == io::ErrorKind::WouldBlock);
                self.stream.set_nonblocking(false)?;
                Ok(alive)
            })
            .unwrap_or(false)
    }
}

// The Debug impl intentionally shows only the peer address — buffers and cached
// timeouts are internal bookkeeping with no useful debug representation.
#[expect(
    clippy::missing_fields_in_debug,
    reason = "HappyEyeballs internal state is not suitable for Debug output"
)]
impl fmt::Debug for HewTcpTransport {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("HewTcpTransport")
            .field("addr", &self.stream.peer_addr().ok())
            .finish()
    }
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests {
    use super::*;
    use std::net::{Ipv4Addr, Ipv6Addr, SocketAddrV4, SocketAddrV6};

    fn v6(port: u16) -> SocketAddr {
        SocketAddr::V6(SocketAddrV6::new(Ipv6Addr::LOCALHOST, port, 0, 0))
    }

    fn v4(port: u16) -> SocketAddr {
        SocketAddr::V4(SocketAddrV4::new(Ipv4Addr::LOCALHOST, port))
    }

    #[test]
    fn interleave_mixed() {
        let addrs = vec![v6(80), v6(81), v4(80), v4(81)];
        let sorted = interleave_addrs(&addrs);
        assert!(sorted[0].is_ipv6());
        assert!(sorted[1].is_ipv4());
        assert!(sorted[2].is_ipv6());
        assert!(sorted[3].is_ipv4());
    }

    #[test]
    fn interleave_v4_only() {
        let addrs = vec![v4(80), v4(81)];
        let sorted = interleave_addrs(&addrs);
        assert_eq!(sorted.len(), 2);
        assert!(sorted.iter().all(SocketAddr::is_ipv4));
    }

    #[test]
    fn interleave_v6_only() {
        let addrs = vec![v6(80), v6(81)];
        let sorted = interleave_addrs(&addrs);
        assert_eq!(sorted.len(), 2);
        assert!(sorted.iter().all(SocketAddr::is_ipv6));
    }

    #[test]
    fn interleave_empty() {
        assert!(interleave_addrs(&[]).is_empty());
    }

    #[test]
    fn interleave_single_v4() {
        let sorted = interleave_addrs(&[v4(80)]);
        assert_eq!(sorted, vec![v4(80)]);
    }

    #[test]
    fn interleave_unequal_families() {
        let addrs = vec![v6(80), v6(81), v6(82), v4(80)];
        let sorted = interleave_addrs(&addrs);
        assert_eq!(sorted.len(), 4);
        // First pair interleaved, then remaining v6.
        assert!(sorted[0].is_ipv6());
        assert!(sorted[1].is_ipv4());
        assert!(sorted[2].is_ipv6());
        assert!(sorted[3].is_ipv6());
    }

    #[test]
    fn connect_empty_addrs() {
        let result = connect_happy_eyeballs(&[], Duration::from_secs(5));
        assert!(result.is_err());
    }
}
