//! Connection producers own the address, DNS work and any undelivered socket.

use std::ffi::c_void;
use std::net::{SocketAddr, TcpStream, ToSocketAddrs};
use std::sync::Arc;
use std::time::{Duration, Instant};

use hew_cabi::string::{string_as_str, HewString};

use super::{AcceptedConnection, HewAsyncIo, IoFailure, IoProducer, IoValue};
use crate::blocking_pool::{hew_blocking_pool_submit, shared_blocking_pool_opt, HewBlockingPool};
use crate::wake::HewWaker;

enum Address {
    Endpoint(String),
    Host(String, u16),
}

impl Address {
    fn resolve(&self) -> std::io::Result<Vec<SocketAddr>> {
        match self {
            Self::Endpoint(address) => address.to_socket_addrs().map(Iterator::collect),
            Self::Host(host, port) => (host.as_str(), *port)
                .to_socket_addrs()
                .map(Iterator::collect),
        }
    }
}

struct ConnectJob {
    address: Address,
    deadline: Option<Instant>,
    operation: IoProducer,
}

impl ConnectJob {
    fn run(&self) -> Result<IoValue, IoFailure> {
        let addresses = self
            .address
            .resolve()
            .map_err(|error| IoFailure::from_io("resolve TCP address", &error))?;
        let mut last_error = std::io::Error::new(
            std::io::ErrorKind::AddrNotAvailable,
            "no addresses resolved",
        );
        for address in addresses {
            if !self.operation.is_pending() {
                return Err(super::connect_deadline::timeout_failure());
            }
            let result = if let Some(deadline) = self.deadline {
                let remaining = deadline.saturating_duration_since(Instant::now());
                if remaining.is_zero() {
                    return Err(super::connect_deadline::timeout_failure());
                }
                TcpStream::connect_timeout(&address, remaining)
            } else {
                TcpStream::connect(address)
            };
            match result {
                Ok(stream) => {
                    // Adoption transfers the socket into an owned result. A
                    // late completion drops it through the ordinary close path.
                    return Ok(IoValue::Connection(AcceptedConnection(
                        crate::transport::tcp_adopt_connection(stream),
                    )));
                }
                Err(error) => last_error = error,
            }
        }
        Err(IoFailure::from_io("connect TCP", &last_error))
    }
}

unsafe extern "C" fn run_connect_job(context: *mut c_void) {
    // SAFETY: successful pool admission transfers this unique job to one call.
    let job = unsafe { Box::from_raw(context.cast::<ConnectJob>()) };
    if !job.operation.is_pending() {
        return;
    }
    let result = std::panic::catch_unwind(std::panic::AssertUnwindSafe(|| job.run()))
        .unwrap_or_else(|_| Err(IoFailure::invalid("TCP connection worker panicked")));
    job.operation.complete(result);
}

unsafe fn submit(
    pool: Option<*mut HewBlockingPool>,
    waker: *const HewWaker,
    address: Result<Address, IoFailure>,
    timeout: Option<Duration>,
) -> *const HewAsyncIo {
    // SAFETY: the entrypoint lends its descriptor through this call.
    let operation = unsafe { HewAsyncIo::new(waker) };
    match (pool, address) {
        (_, Err(error)) => operation.complete(Err(error)),
        (None, Ok(_)) => operation.complete(Err(IoFailure::invalid(
            "asynchronous TCP connect requires an installed runtime",
        ))),
        (Some(pool), Ok(address)) => {
            let deadline = timeout.map(|duration| Instant::now() + duration);
            if let Some(timeout) = timeout {
                operation.set_connect_timeout(timeout);
            }
            if operation.is_pending() {
                let job = Box::into_raw(Box::new(ConnectJob {
                    address,
                    deadline,
                    operation: IoProducer::new(Arc::clone(&operation)),
                }));
                // SAFETY: the installed runtime owns the pool; the admitted job
                // owns all inputs and shutdown joins its eventual completion.
                if unsafe { hew_blocking_pool_submit(pool, run_connect_job, job.cast()) } != 0 {
                    // SAFETY: failed admission did not transfer ownership.
                    drop(unsafe { Box::from_raw(job) });
                    operation.complete(Err(IoFailure::invalid("TCP connection pool is stopped")));
                }
            }
        }
    }
    Arc::into_raw(operation)
}

unsafe fn owned_address(address: *const HewString) -> Result<String, IoFailure> {
    // SAFETY: caller lends a valid managed string, with null representing empty.
    let address = unsafe { string_as_str(address) };
    if address.is_empty() || address.as_bytes().contains(&0) {
        return Err(IoFailure::invalid("TCP address is empty or contains NUL"));
    }
    Ok(address.to_owned())
}

/// Start DNS and connection setup without blocking the caller's worker.
///
/// # Safety
/// `address` is a live borrowed managed string; `waker` is null or a live
/// descriptor. The runtime outlives producer admission.
#[no_mangle]
pub unsafe extern "C" fn hew_async_tcp_connect(
    address: *const HewString,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    // SAFETY: submission copies the address and retains the borrowed waker.
    unsafe {
        submit(
            shared_blocking_pool_opt(),
            waker,
            owned_address(address).map(Address::Endpoint),
            None,
        )
    }
}

/// Start a connection with one deadline covering queueing, DNS and handshakes.
///
/// # Safety
/// `host` and `waker` obey the same borrowing contract as `hew_async_tcp_connect`.
#[no_mangle]
pub unsafe extern "C" fn hew_async_tcp_connect_timeout(
    host: *const HewString,
    port: i32,
    timeout_ms: i32,
    waker: *const HewWaker,
) -> *const HewAsyncIo {
    // SAFETY: copy the borrowed host before producer admission.
    let address = unsafe { owned_address(host) }.and_then(|host| {
        let port =
            u16::try_from(port).map_err(|_| IoFailure::invalid("TCP port is outside 0..65535"))?;
        if timeout_ms < 0 {
            return Err(IoFailure::invalid("TCP connection timeout is negative"));
        }
        Ok(Address::Host(host, port))
    });
    let timeout = u64::try_from(timeout_ms).ok().map(Duration::from_millis);
    // SAFETY: the request owns its host and submission retains the waker.
    unsafe { submit(shared_blocking_pool_opt(), waker, address, timeout) }
}
