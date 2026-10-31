//! HTTP client for fetching runtime profiler data.

#[cfg(test)]
use std::cell::RefCell;
use std::io;
#[cfg(unix)]
use std::io::{BufRead, Read, Write};
#[cfg(unix)]
use std::os::unix::net::UnixStream;
#[cfg(unix)]
use std::path::{Path, PathBuf};
use std::time::Duration;

#[cfg(test)]
pub use hew_observe_protocol::OBSERVE_SCHEMA_VERSION;
use hew_observe_protocol::{decode_envelope, DecodeError};
pub use hew_observe_protocol::{
    ActorIdentity, ActorInfo, ClusterMember, ConnectionInfo, CrashEntry, HistoryEntry, Metrics,
    RouteEntry, RoutingSnapshot, SupervisorRow, TraceBatch, TraceEvent, TraceStreamMismatch,
    TraceSubject,
};

/// Relationship between a fetched trace page and the client's previous page.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TraceContinuity {
    /// First successful attachment to this producer.
    Initial,
    /// The page continues the same producer process and trace stream.
    Continuous,
    /// The producer stayed alive but began a new trace stream epoch.
    StreamReset,
    /// The profiler process was replaced or restarted.
    ProducerRestart,
}

/// One cursor-addressed trace page and its continuity classification.
#[derive(Debug)]
pub struct TracePage {
    pub batch: TraceBatch,
    pub continuity: TraceContinuity,
}

#[derive(Debug, Clone)]
struct TraceCursor {
    producer_instance_id: String,
    stream_id: String,
    after: u64,
}

/// Validate relationships that cannot be expressed by generated field-level
/// decoders. A malformed success response must never advance the private
/// cursor: doing so could silently skip, duplicate, or reorder trace records.
fn validate_trace_identity(
    batch: &TraceBatch,
    requested_cursor: Option<&TraceCursor>,
    expected_after_mismatch: Option<&(String, String)>,
) -> Result<(), String> {
    if requested_cursor.is_some_and(|cursor| {
        batch.stream_id != cursor.stream_id
            || batch.producer_instance_id != cursor.producer_instance_id
    }) {
        return Err(
            "successful trace response changed producer or stream without a mismatch".to_owned(),
        );
    }
    if let Some((expected_stream, expected_producer)) = expected_after_mismatch {
        if batch.stream_id != *expected_stream || batch.producer_instance_id != *expected_producer {
            return Err(format!(
                "trace_stream_mismatch advertised stream {expected_stream} from producer \
                 {expected_producer}, retry returned stream {} from producer {}",
                batch.stream_id, batch.producer_instance_id
            ));
        }
    }
    Ok(())
}

fn validate_trace_batch(batch: &TraceBatch, requested_after: Option<u64>) -> Result<(), String> {
    let last_produced = validate_trace_batch_metadata(batch)?;
    let requested_next = validate_trace_batch_overrun(batch, requested_after, last_produced)?;
    validate_trace_batch_page(batch, requested_after, requested_next, last_produced)
}

fn validate_trace_batch_metadata(batch: &TraceBatch) -> Result<u64, String> {
    if batch.oldest_sequence == 0 || batch.next_sequence == 0 {
        return Err("oldest_sequence and next_sequence must be positive".to_owned());
    }
    if batch.oldest_sequence > batch.next_sequence {
        return Err(format!(
            "oldest_sequence {} exceeds next_sequence {}",
            batch.oldest_sequence, batch.next_sequence
        ));
    }

    let last_produced = batch.next_sequence - 1;
    if batch.events_recorded_total != last_produced {
        return Err(format!(
            "events_recorded_total {} does not match last produced sequence {last_produced}",
            batch.events_recorded_total
        ));
    }
    if batch.events_overwritten_total > batch.events_recorded_total {
        return Err("events_overwritten_total exceeds events_recorded_total".to_owned());
    }
    let expected_oldest = if batch.events_recorded_total == 0 {
        batch.next_sequence
    } else {
        batch
            .events_overwritten_total
            .checked_add(1)
            .ok_or_else(|| "events_overwritten_total overflow".to_owned())?
    };
    if batch.oldest_sequence != expected_oldest {
        return Err(format!(
            "oldest_sequence {} does not match overwrite history {expected_oldest}",
            batch.oldest_sequence
        ));
    }
    if batch.batch_source_monotonic_ns < batch.clock_anchor.monotonic_ns {
        return Err("batch source clock precedes its clock anchor".to_owned());
    }
    Ok(last_produced)
}

fn validate_trace_batch_overrun(
    batch: &TraceBatch,
    requested_after: Option<u64>,
    last_produced: u64,
) -> Result<u64, String> {
    let requested_next = match requested_after {
        Some(after) => {
            if after > last_produced {
                return Err(format!(
                    "response accepted future cursor {after} past {last_produced}"
                ));
            }
            after
                .checked_add(1)
                .ok_or_else(|| "requested cursor overflow".to_owned())?
        }
        None => batch.oldest_sequence,
    };
    let expected_overrun = batch.oldest_sequence.saturating_sub(requested_next);
    match (&batch.overrun, expected_overrun) {
        (None, 0) => {}
        (None, count) => {
            return Err(format!("missing overrun metadata for {count} lost events"));
        }
        (Some(_), 0) => return Err("spurious overrun metadata without a cursor gap".to_owned()),
        (Some(overrun), count) => {
            let expected_last = batch.oldest_sequence - 1;
            if overrun.first_missing_sequence != requested_next
                || overrun.last_missing_sequence != expected_last
                || overrun.count != count
            {
                return Err(format!(
                    "overrun range {}..={} count {} does not match expected {}..={} count {count}",
                    overrun.first_missing_sequence,
                    overrun.last_missing_sequence,
                    overrun.count,
                    requested_next,
                    expected_last
                ));
            }
        }
    }
    Ok(requested_next)
}

fn validate_trace_batch_page(
    batch: &TraceBatch,
    requested_after: Option<u64>,
    requested_next: u64,
    last_produced: u64,
) -> Result<(), String> {
    let expected_first = requested_next.max(batch.oldest_sequence);
    if let Some(first) = batch.events.first() {
        if first.sequence != expected_first {
            return Err(format!(
                "first event sequence {} does not match expected {expected_first}",
                first.sequence
            ));
        }
        for pair in batch.events.windows(2) {
            if pair[1].sequence != pair[0].sequence.saturating_add(1) {
                return Err(format!(
                    "event sequences are not contiguous at {} then {}",
                    pair[0].sequence, pair[1].sequence
                ));
            }
        }
        let last = batch.events.last().expect("non-empty trace page").sequence;
        if last >= batch.next_sequence {
            return Err(format!(
                "event sequence {last} is not below next_sequence {}",
                batch.next_sequence
            ));
        }
        if batch.next_cursor != last {
            return Err(format!(
                "next_cursor {} does not match last returned sequence {last}",
                batch.next_cursor
            ));
        }
    } else {
        if expected_first < batch.next_sequence {
            return Err(format!(
                "empty page omitted available sequence {expected_first}"
            ));
        }
        let expected_cursor = requested_after.unwrap_or(last_produced);
        if batch.next_cursor != expected_cursor {
            return Err(format!(
                "empty page cursor {} does not match expected {expected_cursor}",
                batch.next_cursor
            ));
        }
    }

    let expected_has_more = batch.next_cursor < last_produced;
    if batch.has_more != expected_has_more {
        return Err(format!(
            "has_more {} does not match cursor {} and last produced {last_produced}",
            batch.has_more, batch.next_cursor
        ));
    }
    Ok(())
}

#[derive(Debug)]
struct HttpResponse {
    status: String,
    status_code: u16,
    body: Vec<u8>,
}

/// Typed failure categories from a profiler client request.
///
/// Available via [`ProfilerClient::last_error`] after any `fetch_*` call
/// returns `None`, letting callers distinguish transient I/O failures from
/// socket misconfiguration or server-side issues.
#[derive(Debug)]
pub enum ClientError {
    /// Failed to build the TCP HTTP client with the configured timeout/proxy settings.
    Build {
        base_url: String,
        source: reqwest::Error,
    },
    /// Could not connect to the unix socket (stale path, permissions, etc.).
    Connect(io::Error),
    /// Failed to configure socket read/write timeouts.
    #[cfg_attr(
        not(unix),
        expect(dead_code, reason = "built only by the unix-socket transport")
    )]
    Timeout(io::Error),
    /// Failed to write the HTTP request.
    #[cfg_attr(
        not(unix),
        expect(dead_code, reason = "built only by the unix-socket transport")
    )]
    Write(io::Error),
    /// Failed to read the HTTP response (status line, headers, or body).
    Read(io::Error),
    /// Server returned a non-200 status line.
    BadStatus(String),
    /// Response body was not valid JSON for the expected type.
    Parse(serde_json::Error),
}

impl std::fmt::Display for ClientError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Build { base_url, source } => {
                write!(f, "http client build failed for {base_url}: {source}")
            }
            Self::Connect(e) => write!(f, "connect failed: {e}"),
            Self::Timeout(e) => write!(f, "timeout configure failed: {e}"),
            Self::Write(e) => write!(f, "write failed: {e}"),
            Self::Read(e) => write!(f, "read failed: {e}"),
            Self::BadStatus(s) => write!(f, "bad status: {s}"),
            Self::Parse(e) => write!(f, "parse failed: {e}"),
        }
    }
}

impl From<serde_json::Error> for ClientError {
    fn from(e: serde_json::Error) -> Self {
        Self::Parse(e)
    }
}

#[cfg(test)]
thread_local! {
    static TCP_CLIENT_BUILD_ERROR: RefCell<Option<reqwest::Error>> = const { RefCell::new(None) };
}

#[cfg(test)]
fn take_injected_tcp_client_build_error() -> Option<reqwest::Error> {
    TCP_CLIENT_BUILD_ERROR.with(|slot| slot.borrow_mut().take())
}

fn build_tcp_http_client(base_url: &str) -> Result<reqwest::blocking::Client, ClientError> {
    #[cfg(test)]
    if let Some(source) = take_injected_tcp_client_build_error() {
        return Err(ClientError::Build {
            base_url: base_url.to_owned(),
            source,
        });
    }

    reqwest::blocking::Client::builder()
        .timeout(Duration::from_millis(500))
        .build()
        .map_err(|source| ClientError::Build {
            base_url: base_url.to_owned(),
            source,
        })
}

/// Connection status.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ConnectionStatus {
    Connected,
    Disconnected,
    Connecting,
}

/// Transport backend for the profiler client.
#[derive(Debug)]
enum Transport {
    /// TCP via reqwest (for `--addr host:port` mode).
    Tcp {
        base_url: String,
        http: reqwest::blocking::Client,
    },
    /// Unix domain socket (for auto-discovered profilers).
    #[cfg(unix)]
    Unix { socket_path: PathBuf },
}

/// HTTP client for the profiler endpoint.
///
/// Connection status is determined exclusively by the metrics probe
/// (`fetch_metrics`). Other fetch methods do not affect connection
/// status — they may fail independently (e.g. a tab-specific endpoint
/// returning bad JSON) without poisoning the overall indicator.
///
/// When any `fetch_*` call returns `None`, inspect [`Self::last_error`]
/// to see the categorized failure reason.
#[derive(Debug)]
pub struct ProfilerClient {
    transport: Transport,
    pub status: ConnectionStatus,
    /// Last error from the most recent `fetch_*` call, if it failed.
    /// Cleared to `None` on a successful fetch.
    pub last_error: Option<ClientError>,
    /// Per-observer trace cursor. This is deliberately client-local: the
    /// producer never allocates or advances cursor state on our behalf.
    trace_cursor: Option<TraceCursor>,
}

impl ProfilerClient {
    /// Create a client that connects over TCP.
    pub fn new_tcp(base_url: &str) -> Result<Self, ClientError> {
        let http = build_tcp_http_client(base_url)?;
        Ok(Self {
            transport: Transport::Tcp {
                base_url: base_url.to_owned(),
                http,
            },
            status: ConnectionStatus::Connecting,
            last_error: None,
            trace_cursor: None,
        })
    }

    /// Create a client that connects over a unix domain socket.
    #[cfg(unix)]
    pub fn new_unix(socket_path: &Path) -> Self {
        Self {
            transport: Transport::Unix {
                socket_path: socket_path.to_owned(),
            },
            status: ConnectionStatus::Connecting,
            last_error: None,
            trace_cursor: None,
        }
    }

    /// Fetch current metrics and update connection status.
    ///
    /// This is the PRIMARY health-check endpoint. Connection status is
    /// set to `Connected` or `Disconnected` based solely on this call.
    pub fn fetch_metrics(&mut self) -> Option<Metrics> {
        if let Some(m) = self.get_json("/api/metrics") {
            self.status = ConnectionStatus::Connected;
            Some(m)
        } else {
            self.status = ConnectionStatus::Disconnected;
            None
        }
    }

    pub fn fetch_actors(&mut self) -> Option<Vec<ActorInfo>> {
        self.get_json("/api/actors")
    }

    pub fn fetch_history(&mut self) -> Option<Vec<HistoryEntry>> {
        self.get_json("/api/metrics/history")
    }

    pub fn fetch_cluster_members(&mut self) -> Option<Vec<ClusterMember>> {
        self.get_json("/api/cluster/members")
    }

    pub fn fetch_connections(&mut self) -> Option<Vec<ConnectionInfo>> {
        self.get_json("/api/connections")
    }

    pub fn fetch_routing(&mut self) -> Option<RoutingSnapshot> {
        self.get_json("/api/routing/table")
    }

    /// Fetch the next non-destructive trace page for this client.
    ///
    /// A stream-mismatch response is an explicit segment boundary. The old
    /// cursor is discarded and the method performs one fresh attachment with
    /// both cursor parameters omitted; events from the two epochs are never
    /// requested with the same cursor.
    pub fn fetch_traces(&mut self) -> Option<TracePage> {
        let previous = self.trace_cursor.clone();
        let mut retried_after_mismatch = false;
        let mut bootstrap_after_mismatch = false;
        let mut expected_after_mismatch: Option<(String, String)> = None;

        loop {
            let requested_after = if bootstrap_after_mismatch {
                None
            } else {
                self.trace_cursor.as_ref().map(|cursor| cursor.after)
            };
            let path = if bootstrap_after_mismatch {
                "/api/traces?limit=256".to_owned()
            } else {
                self.trace_cursor.as_ref().map_or_else(
                    || "/api/traces?limit=256".to_owned(),
                    |cursor| {
                        format!(
                            "/api/traces?stream_id={}&after={}&limit=256",
                            cursor.stream_id, cursor.after
                        )
                    },
                )
            };

            let response = match self.fetch_response_inner(&path) {
                Ok(response) => response,
                Err(error) => {
                    self.last_error = Some(error);
                    return None;
                }
            };
            let batch: TraceBatch = if (200..300).contains(&response.status_code) {
                self.decode_json_body(&response.body)?
            } else if response.status_code == 409 && !retried_after_mismatch {
                let mismatch: TraceStreamMismatch = self.decode_json_body(&response.body)?;
                let Some(cursor) = self.trace_cursor.as_ref() else {
                    self.last_error = Some(ClientError::BadStatus(
                        "409 trace_stream_mismatch without a local cursor".to_owned(),
                    ));
                    return None;
                };
                if mismatch.requested_stream_id != cursor.stream_id {
                    self.last_error = Some(ClientError::BadStatus(format!(
                        "409 trace_stream_mismatch requested {}, expected {}",
                        mismatch.requested_stream_id, cursor.stream_id
                    )));
                    return None;
                }
                if mismatch.current_stream_id == cursor.stream_id {
                    self.last_error = Some(ClientError::BadStatus(
                        "409 trace_stream_mismatch did not change stream_id".to_owned(),
                    ));
                    return None;
                }
                expected_after_mismatch = Some((
                    mismatch.current_stream_id,
                    mismatch.current_producer_instance_id,
                ));
                bootstrap_after_mismatch = true;
                retried_after_mismatch = true;
                continue;
            } else {
                self.last_error = Some(ClientError::BadStatus(response.status));
                return None;
            };
            let requested_cursor = if bootstrap_after_mismatch {
                None
            } else {
                self.trace_cursor.as_ref()
            };
            if let Err(error) =
                validate_trace_identity(&batch, requested_cursor, expected_after_mismatch.as_ref())
            {
                self.last_error = Some(ClientError::BadStatus(error));
                return None;
            }
            if let Err(error) = validate_trace_batch(&batch, requested_after) {
                self.last_error = Some(ClientError::BadStatus(format!(
                    "invalid trace batch: {error}"
                )));
                return None;
            }

            let continuity = match previous.as_ref() {
                None => TraceContinuity::Initial,
                Some(cursor) if cursor.producer_instance_id != batch.producer_instance_id => {
                    TraceContinuity::ProducerRestart
                }
                Some(cursor) if cursor.stream_id != batch.stream_id => TraceContinuity::StreamReset,
                Some(_) => TraceContinuity::Continuous,
            };
            self.trace_cursor = Some(TraceCursor {
                producer_instance_id: batch.producer_instance_id.clone(),
                stream_id: batch.stream_id.clone(),
                after: batch.next_cursor,
            });
            self.last_error = None;
            return Some(TracePage { batch, continuity });
        }
    }

    pub fn fetch_supervisors(&mut self) -> Option<Vec<SupervisorRow>> {
        self.get_json("/api/supervisors")
    }

    pub fn fetch_crashes(&mut self) -> Option<Vec<CrashEntry>> {
        self.get_json("/api/crashes")
    }

    /// Fetch JSON from an endpoint. Does NOT affect connection status.
    ///
    /// Parses the canonical observe envelope
    /// (`{"schema_version":"v1","data":<T>}`) — bare bodies emitted by a
    /// pre-envelope producer fail with `ClientError::Parse`. The envelope's
    /// `schema_version` field is checked against [`OBSERVE_SCHEMA_VERSION`];
    /// a mismatch surfaces as `ClientError::BadStatus` so the caller's
    /// `last_error` flags the version drift instead of silently returning
    /// possibly-shape-shifted data.
    fn get_json<T: serde::de::DeserializeOwned>(&mut self, path: &str) -> Option<T> {
        let body = self.get_bytes(path)?;
        self.decode_json_body(&body)
    }

    fn decode_json_body<T: serde::de::DeserializeOwned>(&mut self, body: &[u8]) -> Option<T> {
        match decode_envelope(body) {
            Ok(data) => Some(data),
            Err(DecodeError::Json(e)) => {
                self.last_error = Some(ClientError::Parse(e));
                None
            }
            Err(error @ DecodeError::SchemaVersion { .. }) => {
                self.last_error = Some(ClientError::BadStatus(error.to_string()));
                None
            }
        }
    }

    /// Raw GET request returning the response body bytes.
    ///
    /// Uses a two-phase approach: [`Self::fetch_bytes_inner`] borrows
    /// `self` immutably to perform the I/O, then this method stores any
    /// error in `self.last_error` before returning `None`.
    fn get_bytes(&mut self, path: &str) -> Option<Vec<u8>> {
        match self.fetch_bytes_inner(path) {
            Ok(body) => {
                self.last_error = None;
                Some(body)
            }
            Err(e) => {
                self.last_error = Some(e);
                None
            }
        }
    }

    /// Inner fetch that borrows `self` immutably and returns a typed `Result`.
    /// Called exclusively by [`Self::get_bytes`].
    fn fetch_bytes_inner(&self, path: &str) -> Result<Vec<u8>, ClientError> {
        let response = self.fetch_response_inner(path)?;
        if (200..300).contains(&response.status_code) {
            Ok(response.body)
        } else {
            Err(ClientError::BadStatus(response.status))
        }
    }

    fn fetch_response_inner(&self, path: &str) -> Result<HttpResponse, ClientError> {
        match &self.transport {
            Transport::Tcp { base_url, http } => {
                let url = format!("{base_url}{path}");
                let resp = http
                    .get(&url)
                    .send()
                    .map_err(|e| ClientError::Connect(io::Error::other(e)))?;
                let status_code = resp.status().as_u16();
                let status = resp.status().to_string();
                let body = resp
                    .bytes()
                    .map_err(|e| ClientError::Read(io::Error::other(e)))?
                    .to_vec();
                Ok(HttpResponse {
                    status,
                    status_code,
                    body,
                })
            }
            #[cfg(unix)]
            Transport::Unix { socket_path } => unix_get_response(socket_path, path),
        }
    }
}

#[cfg(unix)]
/// Blocking HTTP/1.1 GET over a unix domain socket.
///
/// Opens a fresh connection per request (profiler responses are small
/// JSON payloads, so connection reuse isn't worth the complexity).
///
/// Returns a typed [`ClientError`] on failure so callers can distinguish
/// connect errors (stale/missing socket path) from I/O failures and
/// unexpected server responses.
fn unix_get_response(socket_path: &Path, path: &str) -> Result<HttpResponse, ClientError> {
    let mut stream = UnixStream::connect(socket_path).map_err(ClientError::Connect)?;
    stream
        .set_read_timeout(Some(Duration::from_millis(500)))
        .map_err(ClientError::Timeout)?;
    stream
        .set_write_timeout(Some(Duration::from_millis(500)))
        .map_err(ClientError::Timeout)?;

    let request = format!("GET {path} HTTP/1.1\r\nHost: localhost\r\nConnection: close\r\n\r\n");
    stream
        .write_all(request.as_bytes())
        .map_err(ClientError::Write)?;

    // Read the HTTP response: status line, headers, blank line, body.
    let mut reader = std::io::BufReader::new(stream);

    // Status line: "HTTP/1.1 200 OK\r\n"
    let mut status_line = String::new();
    reader
        .read_line(&mut status_line)
        .map_err(ClientError::Read)?;
    let status_code = status_line
        .split_ascii_whitespace()
        .nth(1)
        .and_then(|value| value.parse::<u16>().ok())
        .ok_or_else(|| ClientError::BadStatus(status_line.trim_end().to_owned()))?;
    let status = status_line.trim_end().to_owned();

    // Headers — look for Content-Length, skip until blank line.
    let mut content_length: Option<usize> = None;
    loop {
        let mut line = String::new();
        reader.read_line(&mut line).map_err(ClientError::Read)?;
        let trimmed = line.trim();
        if trimmed.is_empty() {
            break; // End of headers.
        }
        if let Some(val) = trimmed.strip_prefix("content-length:") {
            content_length = val.trim().parse().ok();
        } else if let Some(val) = trimmed.strip_prefix("Content-Length:") {
            content_length = val.trim().parse().ok();
        }
    }

    // Read body.
    if let Some(len) = content_length {
        let mut body = vec![0u8; len];
        reader.read_exact(&mut body).map_err(ClientError::Read)?;
        Ok(HttpResponse {
            status,
            status_code,
            body,
        })
    } else {
        // No Content-Length — read until EOF (Connection: close).
        let mut body = Vec::new();
        reader.read_to_end(&mut body).map_err(ClientError::Read)?;
        Ok(HttpResponse {
            status,
            status_code,
            body,
        })
    }
}

/// Multi-node client wrapping one `ProfilerClient` per node.
#[derive(Debug)]
pub struct ClusterClient {
    pub nodes: Vec<NodeClient>,
}

#[derive(Debug)]
#[expect(
    dead_code,
    reason = "Fields used for future multi-node display features"
)]
pub struct NodeClient {
    pub client: ProfilerClient,
    pub addr: String,
    pub node_id: Option<u16>,
}

impl ClusterClient {
    /// Create a cluster client from TCP addresses.
    pub fn new(addrs: &[String]) -> Result<Self, ClientError> {
        let nodes = addrs
            .iter()
            .map(|addr| -> Result<NodeClient, ClientError> {
                let base_url = format!("http://{addr}");
                Ok(NodeClient {
                    client: ProfilerClient::new_tcp(&base_url)?,
                    addr: addr.clone(),
                    node_id: None,
                })
            })
            .collect::<Result<Vec<_>, _>>()?;
        Ok(Self { nodes })
    }

    /// Create a cluster client from unix domain sockets.
    #[cfg(unix)]
    pub fn from_unix_nodes(targets: &[(PathBuf, String)]) -> Self {
        let nodes = targets
            .iter()
            .map(|(socket_path, label)| NodeClient {
                client: ProfilerClient::new_unix(socket_path),
                addr: label.clone(),
                node_id: None,
            })
            .collect();
        Self { nodes }
    }

    /// Create a cluster client from a single unix domain socket.
    #[cfg(unix)]
    pub fn from_unix(socket_path: &Path, label: &str) -> Self {
        let nodes = vec![NodeClient {
            client: ProfilerClient::new_unix(socket_path),
            addr: label.to_owned(),
            node_id: None,
        }];
        Self { nodes }
    }

    pub fn status(&self) -> ConnectionStatus {
        if self
            .nodes
            .iter()
            .any(|n| n.client.status == ConnectionStatus::Connected)
        {
            ConnectionStatus::Connected
        } else if self
            .nodes
            .iter()
            .any(|n| n.client.status == ConnectionStatus::Connecting)
        {
            ConnectionStatus::Connecting
        } else {
            ConnectionStatus::Disconnected
        }
    }
}

#[cfg(all(test, unix))]
mod tests {
    use super::*;
    use hew_observe_protocol::Envelope;
    use std::os::unix::net::UnixListener;
    use std::sync::{Arc, Mutex};

    /// Bind a listener on `path`, accept one connection, send `response`,
    /// then close.  Spawned on a background thread so the test's
    /// `ProfilerClient` can connect from the main thread.
    /// Serve one response. The socket is bound before this returns, so a
    /// client may connect at once: the kernel queues it for the accept.
    fn serve_once(path: &Path, response: &'static str) {
        let listener = UnixListener::bind(path).expect("bind");
        std::thread::spawn(move || {
            if let Ok((mut conn, _)) = listener.accept() {
                // Drain the request (ignore it).
                let mut buf = [0u8; 512];
                let _ = conn.read(&mut buf);
                let _ = conn.write_all(response.as_bytes());
                // conn drops here, closing the socket → EOF for reader.
            }
        });
    }

    fn serve_responses(path: &Path, responses: Vec<String>, requests: Arc<Mutex<Vec<String>>>) {
        let listener = UnixListener::bind(path).expect("bind");
        std::thread::spawn(move || {
            for response in responses {
                let (mut conn, _) = listener.accept().expect("accept test request");
                let mut reader =
                    std::io::BufReader::new(conn.try_clone().expect("clone test request stream"));
                let mut request_line = String::new();
                reader
                    .read_line(&mut request_line)
                    .expect("read test request line");
                loop {
                    let mut header = String::new();
                    reader.read_line(&mut header).expect("read test header");
                    if header == "\r\n" || header.is_empty() {
                        break;
                    }
                }
                requests
                    .lock()
                    .expect("lock captured requests")
                    .push(request_line.trim().to_owned());
                conn.write_all(response.as_bytes())
                    .expect("write test response");
            }
        });
    }

    fn http_json_response(status: &str, data: &serde_json::Value) -> String {
        http_envelope_response(status, OBSERVE_SCHEMA_VERSION, data)
    }

    fn http_envelope_response(
        status: &str,
        schema_version: &str,
        data: &serde_json::Value,
    ) -> String {
        let body = serde_json::json!({
            "schema_version": schema_version,
            "data": data,
        })
        .to_string();
        format!(
            "HTTP/1.1 {status}\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{body}",
            body.len()
        )
    }

    fn empty_trace_batch(producer: &str, stream: &str, cursor: u64) -> serde_json::Value {
        serde_json::json!({
            "producer_instance_id": producer,
            "stream_id": stream,
            "oldest_sequence": (cursor + 1).to_string(),
            "next_sequence": (cursor + 1).to_string(),
            "next_cursor": cursor.to_string(),
            "has_more": false,
            "overrun": null,
            "tracing_enabled": true,
            "sampling_mode": "all",
            "events_recorded_total": cursor.to_string(),
            "events_overwritten_total": cursor.to_string(),
            "events_sampled_out_total": "0",
            "batch_source_monotonic_ns": "10",
            "clock_anchor": {
                "monotonic_ns": "10",
                "unix_epoch_ns": "1000",
                "uncertainty_ns": "1"
            },
            "events": []
        })
    }

    fn one_event_trace_batch(producer: &str, stream: &str, sequence: u64) -> serde_json::Value {
        serde_json::json!({
            "producer_instance_id": producer,
            "stream_id": stream,
            "oldest_sequence": "1",
            "next_sequence": (sequence + 1).to_string(),
            "next_cursor": sequence.to_string(),
            "has_more": false,
            "overrun": null,
            "tracing_enabled": true,
            "sampling_mode": "all",
            "events_recorded_total": sequence.to_string(),
            "events_overwritten_total": "0",
            "events_sampled_out_total": "0",
            "batch_source_monotonic_ns": "20",
            "clock_anchor": {
                "monotonic_ns": "10",
                "unix_epoch_ns": "1000",
                "uncertainty_ns": "1"
            },
            "events": [{
                "sequence": sequence.to_string(),
                "trace_id": "00000000000000000000000000000010",
                "span_id": "1",
                "parent_span_id": "0",
                "subject": null,
                "source_attribution": "external",
                "source_actor": null,
                "destination_attribution": "local_actor",
                "destination_actor": {
                    "actor_id": "9",
                    "spawn_serial": "1",
                    "actor_type": "Probe"
                },
                "event_type": "send",
                "msg_type": 1,
                "timestamp_ns": "15",
                "handler_name": "Probe.handle"
            }]
        })
    }

    // ── crash entry deserialization ──────────────────────────────────────

    /// Pins the `/api/crashes` JSON contract from the consumer side: the
    /// `trap_kind` field MUST land on `CrashEntry` populated with the
    /// canonical Hew trap-kind slug (e.g. `"DivideByZero"`), not silently
    /// dropped or defaulted to the empty string when the producer supplies
    /// it. Producer-side coverage lives in
    /// `hew-runtime/tests/crashinfo_observe_event.rs`, which round-trips a
    /// real actor trap through `snapshot_crashes_json`.
    #[test]
    fn crash_entry_deserialises_trap_kind_from_profiler_payload() {
        // Shape mirrors what `hew_runtime::crash::snapshot_crashes_json`
        // emits for an actor that trapped with `HEW_TRAP_DIVIDE_BY_ZERO`
        // (202). The runtime test asserts the exact same shape is
        // produced by a live trap; this test asserts the observe consumer
        // parses it correctly.
        let body = r#"[{"time_s":1.5,"actor_id":"42","signal":202,"trap_kind":"DivideByZero","msg_type":7,"fault_addr":"0"}]"#;
        let entries: Vec<CrashEntry> =
            serde_json::from_str(body).expect("profiler crash payload must parse");
        assert_eq!(entries.len(), 1);
        let entry = &entries[0];
        assert_eq!(entry.actor_id, 42);
        assert_eq!(entry.signal, 202);
        assert_eq!(entry.trap_kind, "DivideByZero");
        assert_eq!(entry.msg_type, 7);
    }

    /// v1 requires `trap_kind`; omission is malformed rather than an empty
    /// value with different semantics.
    #[test]
    fn crash_entry_rejects_missing_required_trap_kind() {
        let body = r#"[{"time_s":0.0,"actor_id":"1","signal":11,"msg_type":0,"fault_addr":"0"}]"#;
        let error = serde_json::from_str::<Vec<CrashEntry>>(body)
            .expect_err("missing required trap_kind must fail");
        assert!(error.to_string().contains("trap_kind"), "{error}");
    }

    // ── Schema-version envelope unwrapping ────────────────────────────────

    /// `get_json` MUST peel the canonical envelope
    /// (`{"schema_version":"v1","data":<body>}`) before handing the inner
    /// `T` back. Producer-side coverage lives in
    /// `hew_runtime::profiler::server`; this test pins the consumer half.
    #[test]
    fn get_json_unwraps_canonical_envelope_and_returns_inner_data() {
        let tmp = tempfile::tempdir().unwrap();
        let sock = tmp.path().join("envelope_ok.sock");

        let inner = r#"[{"time_s":2.5,"actor_id":"17","signal":202,"trap_kind":"DivideByZero","msg_type":3,"fault_addr":"0"}]"#;
        let body = format!(r#"{{"schema_version":"{OBSERVE_SCHEMA_VERSION}","data":{inner}}}"#);
        let body_len = body.len();
        let response: &'static str = Box::leak(
            format!("HTTP/1.1 200 OK\r\nContent-Length: {body_len}\r\n\r\n{body}").into_boxed_str(),
        );
        serve_once(&sock, response);

        let mut client = ProfilerClient::new_unix(&sock);
        let crashes = client
            .fetch_crashes()
            .expect("envelope-wrapped crash list must parse");
        assert_eq!(crashes.len(), 1);
        assert_eq!(crashes[0].actor_id, 17);
        assert_eq!(crashes[0].trap_kind, "DivideByZero");
        assert!(
            client.last_error.is_none(),
            "successful envelope fetch must clear last_error, got {:?}",
            client.last_error
        );
    }

    /// A producer that drifts to a non-canonical `schema_version` MUST be
    /// surfaced via `last_error` (not silently returned) so downstream tools
    /// can prompt the user to upgrade either side.
    #[test]
    fn get_json_rejects_envelope_with_mismatched_schema_version() {
        let tmp = tempfile::tempdir().unwrap();
        let sock = tmp.path().join("envelope_drift.sock");

        let body = r#"{"schema_version":"v9.9","data":[]}"#;
        let body_len = body.len();
        let response: &'static str = Box::leak(
            format!("HTTP/1.1 200 OK\r\nContent-Length: {body_len}\r\n\r\n{body}").into_boxed_str(),
        );
        serve_once(&sock, response);

        let mut client = ProfilerClient::new_unix(&sock);
        let result = client.fetch_crashes();
        assert!(result.is_none(), "schema drift must short-circuit fetch");
        assert!(
            matches!(
                client.last_error,
                Some(ClientError::BadStatus(ref s)) if s.contains("schema_version") && s.contains("v9.9")
            ),
            "expected schema_version drift to surface as BadStatus, got {:?}",
            client.last_error
        );
    }

    /// A bare (un-wrapped) body emitted by a pre-envelope producer fails with
    /// `ClientError::Parse` rather than silently degrading — R58 Q136 Option
    /// B is field-only with no compat shims, so version drift on the wire
    /// must be loud.
    #[test]
    fn get_json_rejects_bare_body_without_envelope() {
        let tmp = tempfile::tempdir().unwrap();
        let sock = tmp.path().join("envelope_bare.sock");

        let body = "[]";
        let body_len = body.len();
        let response: &'static str = Box::leak(
            format!("HTTP/1.1 200 OK\r\nContent-Length: {body_len}\r\n\r\n{body}").into_boxed_str(),
        );
        serve_once(&sock, response);

        let mut client = ProfilerClient::new_unix(&sock);
        let result = client.fetch_crashes();
        assert!(result.is_none(), "bare body must fail without envelope");
        assert!(
            matches!(client.last_error, Some(ClientError::Parse(_))),
            "expected ClientError::Parse for bare body, got {:?}",
            client.last_error
        );
    }

    #[test]
    fn trace_cursor_retries_stream_mismatch_as_new_segment() {
        let tmp = tempfile::tempdir().unwrap();
        let sock = tmp.path().join("trace_cursor.sock");
        let producer = "00000000000000000000000000000001";
        let old_stream = "00000000000000000000000000000002";
        let new_stream = "00000000000000000000000000000003";
        let requests = Arc::new(Mutex::new(Vec::new()));
        serve_responses(
            &sock,
            vec![
                http_json_response("200 OK", &empty_trace_batch(producer, old_stream, 7)),
                http_json_response(
                    "409 Conflict",
                    &serde_json::json!({
                        "error": "trace_stream_mismatch",
                        "requested_stream_id": old_stream,
                        "current_stream_id": new_stream,
                        "current_producer_instance_id": producer,
                    }),
                ),
                http_json_response("200 OK", &empty_trace_batch(producer, new_stream, 0)),
            ],
            Arc::clone(&requests),
        );

        let mut client = ProfilerClient::new_unix(&sock);
        let initial = client.fetch_traces().expect("initial trace attach");
        assert_eq!(initial.continuity, TraceContinuity::Initial);
        assert_eq!(initial.batch.stream_id, old_stream);

        let reset = client.fetch_traces().expect("retry after stream mismatch");
        assert_eq!(reset.continuity, TraceContinuity::StreamReset);
        assert_eq!(reset.batch.stream_id, new_stream);
        assert_eq!(
            requests.lock().expect("lock captured requests").as_slice(),
            [
                "GET /api/traces?limit=256 HTTP/1.1",
                "GET /api/traces?stream_id=00000000000000000000000000000002&after=7&limit=256 HTTP/1.1",
                "GET /api/traces?limit=256 HTTP/1.1",
            ]
        );
    }

    #[test]
    fn trace_batch_validation_pins_overrun_cursor_and_sequence_relationships() {
        let producer = "00000000000000000000000000000001";
        let stream = "00000000000000000000000000000002";
        let mut value = one_event_trace_batch(producer, stream, 8);
        value["oldest_sequence"] = serde_json::json!("8");
        value["next_sequence"] = serde_json::json!("10");
        value["has_more"] = serde_json::json!(true);
        value["events_recorded_total"] = serde_json::json!("9");
        value["events_overwritten_total"] = serde_json::json!("7");
        value["overrun"] = serde_json::json!({
            "first_missing_sequence": "5",
            "last_missing_sequence": "7",
            "count": "3"
        });

        let valid: TraceBatch = serde_json::from_value(value).expect("valid trace batch shape");
        validate_trace_batch(&valid, Some(4)).expect("valid stale-cursor page");

        let mut bad_overrun = valid.clone();
        bad_overrun.overrun.as_mut().expect("overrun").count = 2;
        assert!(validate_trace_batch(&bad_overrun, Some(4))
            .expect_err("wrong loss count")
            .contains("overrun range"));

        let mut bad_sequence = valid.clone();
        bad_sequence.events[0].sequence = 9;
        assert!(validate_trace_batch(&bad_sequence, Some(4))
            .expect_err("wrong first sequence")
            .contains("first event sequence"));

        let mut bad_cursor = valid.clone();
        bad_cursor.next_cursor = 9;
        assert!(validate_trace_batch(&bad_cursor, Some(4))
            .expect_err("wrong next cursor")
            .contains("next_cursor"));

        let mut bad_backlog = valid;
        bad_backlog.has_more = false;
        assert!(validate_trace_batch(&bad_backlog, Some(4))
            .expect_err("wrong backlog flag")
            .contains("has_more"));
    }

    #[test]
    fn invalid_success_batch_does_not_advance_private_cursor() {
        let temp = tempfile::tempdir().unwrap();
        let sock = temp.path().join("invalid_trace_batch.sock");
        let producer = "00000000000000000000000000000001";
        let stream = "00000000000000000000000000000002";
        let mut invalid = one_event_trace_batch(producer, stream, 1);
        invalid["next_cursor"] = serde_json::json!("2");
        let requests = Arc::new(Mutex::new(Vec::new()));
        serve_responses(
            &sock,
            vec![
                http_json_response("200 OK", &empty_trace_batch(producer, stream, 0)),
                http_json_response("200 OK", &invalid),
                http_json_response("200 OK", &one_event_trace_batch(producer, stream, 1)),
            ],
            Arc::clone(&requests),
        );

        let mut client = ProfilerClient::new_unix(&sock);
        client.fetch_traces().expect("initial attachment");
        assert!(client.fetch_traces().is_none());
        assert!(matches!(
            client.last_error,
            Some(ClientError::BadStatus(ref message)) if message.contains("invalid trace batch")
        ));
        let recovered = client
            .fetch_traces()
            .expect("valid retry must use preserved cursor");
        assert_eq!(recovered.batch.next_cursor, 1);
        assert_eq!(
            requests.lock().expect("captured requests").as_slice(),
            [
                "GET /api/traces?limit=256 HTTP/1.1",
                "GET /api/traces?stream_id=00000000000000000000000000000002&after=0&limit=256 HTTP/1.1",
                "GET /api/traces?stream_id=00000000000000000000000000000002&after=0&limit=256 HTTP/1.1",
            ]
        );
    }

    fn assert_invalid_trace_response_keeps_cursor(response: String) -> ClientError {
        let tmp = tempfile::tempdir().unwrap();
        let sock = tmp.path().join("invalid_trace_mismatch.sock");
        let producer = "00000000000000000000000000000001";
        let old_stream = "00000000000000000000000000000002";
        let requests = Arc::new(Mutex::new(Vec::new()));
        serve_responses(
            &sock,
            vec![
                http_json_response("200 OK", &empty_trace_batch(producer, old_stream, 7)),
                response,
                http_json_response("200 OK", &one_event_trace_batch(producer, old_stream, 8)),
            ],
            Arc::clone(&requests),
        );

        let mut client = ProfilerClient::new_unix(&sock);
        client.fetch_traces().expect("initial trace attach");
        assert!(
            client.fetch_traces().is_none(),
            "invalid trace response must fail"
        );
        let error = client
            .last_error
            .take()
            .expect("invalid trace response must surface an error");
        let continued = client
            .fetch_traces()
            .expect("invalid trace response must retain the old cursor");
        assert_eq!(continued.continuity, TraceContinuity::Continuous);
        assert_eq!(continued.batch.stream_id, old_stream);
        assert_eq!(
            requests.lock().expect("lock captured requests").as_slice(),
            [
                "GET /api/traces?limit=256 HTTP/1.1",
                "GET /api/traces?stream_id=00000000000000000000000000000002&after=7&limit=256 HTTP/1.1",
                "GET /api/traces?stream_id=00000000000000000000000000000002&after=7&limit=256 HTTP/1.1",
            ]
        );
        error
    }

    #[test]
    fn malformed_or_wrong_version_mismatch_does_not_reset_cursor() {
        let malformed = "not json";
        let malformed_response = format!(
            "HTTP/1.1 409 Conflict\r\nContent-Length: {}\r\nConnection: close\r\n\r\n{malformed}",
            malformed.len()
        );
        assert!(matches!(
            assert_invalid_trace_response_keeps_cursor(malformed_response),
            ClientError::Parse(_)
        ));

        let mismatch = serde_json::json!({
            "error": "trace_stream_mismatch",
            "requested_stream_id": "00000000000000000000000000000002",
            "current_stream_id": "00000000000000000000000000000003",
            "current_producer_instance_id": "00000000000000000000000000000001",
        });
        assert!(matches!(
            assert_invalid_trace_response_keeps_cursor(http_envelope_response(
                "409 Conflict",
                "v9",
                &mismatch,
            )),
            ClientError::BadStatus(message) if message.contains("schema_version")
        ));
    }

    #[test]
    fn inconsistent_trace_response_does_not_reset_cursor() {
        let wrong_literal = serde_json::json!({
            "error": "something_else",
            "requested_stream_id": "00000000000000000000000000000002",
            "current_stream_id": "00000000000000000000000000000003",
            "current_producer_instance_id": "00000000000000000000000000000001",
        });
        assert!(matches!(
            assert_invalid_trace_response_keeps_cursor(http_json_response(
                "409 Conflict",
                &wrong_literal,
            )),
            ClientError::Parse(_)
        ));

        let wrong_requested_stream = serde_json::json!({
            "error": "trace_stream_mismatch",
            "requested_stream_id": "00000000000000000000000000000004",
            "current_stream_id": "00000000000000000000000000000003",
            "current_producer_instance_id": "00000000000000000000000000000001",
        });
        assert!(matches!(
            assert_invalid_trace_response_keeps_cursor(http_json_response(
                "409 Conflict",
                &wrong_requested_stream,
            )),
            ClientError::BadStatus(message) if message.contains("requested")
        ));

        for (producer, stream) in [
            (
                "00000000000000000000000000000001",
                "00000000000000000000000000000003",
            ),
            (
                "00000000000000000000000000000003",
                "00000000000000000000000000000002",
            ),
        ] {
            assert!(matches!(
                assert_invalid_trace_response_keeps_cursor(http_json_response(
                    "200 OK",
                    &one_event_trace_batch(producer, stream, 8),
                )),
                ClientError::BadStatus(message) if message.contains("changed producer or stream")
            ));
        }
    }

    #[test]
    fn mismatch_retry_must_match_advertised_current_identity() {
        let tmp = tempfile::tempdir().unwrap();
        let sock = tmp.path().join("inconsistent_trace_retry.sock");
        let producer = "00000000000000000000000000000001";
        let old_stream = "00000000000000000000000000000002";
        let advertised_stream = "00000000000000000000000000000003";
        let unexpected_stream = "00000000000000000000000000000004";
        let requests = Arc::new(Mutex::new(Vec::new()));
        serve_responses(
            &sock,
            vec![
                http_json_response("200 OK", &empty_trace_batch(producer, old_stream, 7)),
                http_json_response(
                    "409 Conflict",
                    &serde_json::json!({
                        "error": "trace_stream_mismatch",
                        "requested_stream_id": old_stream,
                        "current_stream_id": advertised_stream,
                        "current_producer_instance_id": producer,
                    }),
                ),
                http_json_response("200 OK", &empty_trace_batch(producer, unexpected_stream, 0)),
                http_json_response("200 OK", &one_event_trace_batch(producer, old_stream, 8)),
            ],
            Arc::clone(&requests),
        );

        let mut client = ProfilerClient::new_unix(&sock);
        client.fetch_traces().expect("initial trace attach");
        assert!(client.fetch_traces().is_none());
        assert!(matches!(
            client.last_error,
            Some(ClientError::BadStatus(ref message)) if message.contains("advertised stream")
        ));
        let continued = client
            .fetch_traces()
            .expect("failed recovery must preserve old cursor");
        assert_eq!(continued.continuity, TraceContinuity::Continuous);
        assert_eq!(continued.batch.stream_id, old_stream);
        assert_eq!(
            requests.lock().expect("lock captured requests").as_slice(),
            [
                "GET /api/traces?limit=256 HTTP/1.1",
                "GET /api/traces?stream_id=00000000000000000000000000000002&after=7&limit=256 HTTP/1.1",
                "GET /api/traces?limit=256 HTTP/1.1",
                "GET /api/traces?stream_id=00000000000000000000000000000002&after=7&limit=256 HTTP/1.1",
            ]
        );
    }

    // ── Live profiler server round-trip ──────────────────────────────────

    #[test]
    fn live_profiler_server_snapshot_round_trips_through_observe_client() {
        let (listener, addr) = bind_ephemeral_profiler_listener();

        let ctx = std::sync::Arc::new(hew_runtime::profiler::ProfilerContext {
            ring: std::sync::Arc::new(std::sync::Mutex::new(
                hew_runtime::profiler::metrics::MetricsRing::new(),
            )),
        });

        let (shutdown, shutdown_rx) = hew_runtime::profiler::oneshot::channel();
        let server = std::thread::Builder::new()
            .name("observe-live-smoke-profiler".into())
            .spawn({
                let ctx = std::sync::Arc::clone(&ctx);
                move || hew_runtime::profiler::run_tcp_with_listener(listener, ctx, shutdown_rx)
            })
            .expect("spawn live profiler server");

        let base_url = format!("http://{addr}");
        wait_for_live_profiler(&base_url);

        let mut client = ProfilerClient::new_tcp(&base_url).expect("create observe TCP client");
        let raw_body = client
            .get_bytes("/api/metrics")
            .expect("live profiler metrics envelope must be readable");
        let envelope: Envelope<serde_json::Value> = serde_json::from_slice(&raw_body)
            .unwrap_or_else(|err| panic!("live profiler payload must parse: {err}"));
        assert_eq!(envelope.schema_version, OBSERVE_SCHEMA_VERSION);
        let data = envelope
            .data
            .as_object()
            .expect("metrics snapshot data must be a JSON object");
        for key in [
            "timestamp_secs",
            "tasks_spawned",
            "tasks_completed",
            "steals",
            "messages_sent",
            "messages_received",
            "active_workers",
            "alloc_count",
            "dealloc_count",
            "bytes_allocated",
            "bytes_freed",
            "bytes_live",
            "peak_bytes_live",
            "tcp_bytes_read",
            "tcp_bytes_written",
            "tcp_accept_count",
            "tcp_connect_count",
            "tcp_error_count",
        ] {
            assert!(
                data.contains_key(key),
                "live metrics v1 payload must include {key}"
            );
        }

        let metrics = client
            .fetch_metrics()
            .expect("observe client must parse live v1 metrics snapshot");
        assert_eq!(
            metrics.timestamp_secs.to_string(),
            data["timestamp_secs"]
                .as_str()
                .expect("v1 timestamps are decimal strings")
        );
        assert_eq!(client.status, ConnectionStatus::Connected);
        assert!(
            client.last_error.is_none(),
            "successful live fetch must leave no client error, got {:?}",
            client.last_error
        );
        shutdown.send(()).expect("signal profiler shutdown");
        server.join().expect("profiler server should exit cleanly");
    }

    /// Poll until the live profiler serves metrics; the test runner's
    /// timeout is the hang guard.
    fn wait_for_live_profiler(base_url: &str) {
        loop {
            if let Ok(mut client) = ProfilerClient::new_tcp(base_url) {
                if client.fetch_metrics().is_some() {
                    return;
                }
            }
            std::thread::sleep(Duration::from_millis(25));
        }
    }

    fn bind_ephemeral_profiler_listener() -> (std::net::TcpListener, std::net::SocketAddr) {
        for _ in 0..16 {
            let listener =
                std::net::TcpListener::bind("127.0.0.1:0").expect("bind ephemeral profiler port");
            let addr = listener.local_addr().expect("read ephemeral profiler addr");
            if addr.port() != 6060 {
                return (listener, addr);
            }
        }
        panic!("ephemeral profiler port selection kept returning forbidden :6060");
    }

    // ── connect error ─────────────────────────────────────────────────────

    #[test]
    fn connect_error_surfaces_in_last_error() {
        let tmp = tempfile::tempdir().unwrap();
        let sock = tmp.path().join("nonexistent.sock");

        let mut client = ProfilerClient::new_unix(&sock);
        let result = client.fetch_metrics();

        assert!(result.is_none(), "expected None on missing socket");
        assert!(
            matches!(client.last_error, Some(ClientError::Connect(_))),
            "expected ClientError::Connect, got {:?}",
            client.last_error
        );
    }

    // ── bad status ────────────────────────────────────────────────────────

    #[test]
    fn bad_status_surfaces_in_last_error() {
        let tmp = tempfile::tempdir().unwrap();
        let sock = tmp.path().join("bad_status.sock");

        serve_once(&sock, "HTTP/1.1 404 Not Found\r\nContent-Length: 0\r\n\r\n");

        let mut client = ProfilerClient::new_unix(&sock);
        let result = client.fetch_metrics();

        assert!(result.is_none(), "expected None on 404");
        assert!(
            matches!(client.last_error, Some(ClientError::BadStatus(ref s)) if s.contains("404")),
            "expected ClientError::BadStatus with '404', got {:?}",
            client.last_error
        );
    }

    // ── parse error ───────────────────────────────────────────────────────

    #[test]
    fn parse_error_surfaces_in_last_error() {
        let tmp = tempfile::tempdir().unwrap();
        let sock = tmp.path().join("bad_json.sock");

        serve_once(
            &sock,
            "HTTP/1.1 200 OK\r\nContent-Length: 8\r\n\r\nnot json",
        );

        let mut client = ProfilerClient::new_unix(&sock);
        let result = client.fetch_metrics();

        assert!(result.is_none(), "expected None on bad JSON");
        assert!(
            matches!(client.last_error, Some(ClientError::Parse(_))),
            "expected ClientError::Parse, got {:?}",
            client.last_error
        );
    }

    // ── success clears last_error ─────────────────────────────────────────

    #[test]
    fn success_clears_last_error() {
        let tmp = tempfile::tempdir().unwrap();
        let sock_fail = tmp.path().join("fail.sock");
        let sock_ok = tmp.path().join("ok.sock");

        // First call: missing socket → sets last_error.
        let mut client = ProfilerClient::new_unix(&sock_fail);
        client.fetch_metrics();
        assert!(client.last_error.is_some());

        // Swap to a socket that serves valid metrics JSON wrapped in the
        // canonical observe envelope (producer/consumer at v1 emit/accept
        // `{"schema_version":"v1","data":<body>}`).
        let metrics_inner = r#"{"timestamp_secs":"1","tasks_spawned":"0","tasks_completed":"0","steals":"0","messages_sent":"0","messages_received":"0","active_workers":"0","alloc_count":"0","dealloc_count":"0","bytes_allocated":"0","bytes_freed":"0","bytes_live":"0","peak_bytes_live":"0","tcp_bytes_read":"0","tcp_bytes_written":"0","tcp_accept_count":"0","tcp_connect_count":"0","tcp_error_count":"0"}"#;
        let metrics_json =
            format!(r#"{{"schema_version":"{OBSERVE_SCHEMA_VERSION}","data":{metrics_inner}}}"#);
        let body_len = metrics_json.len();
        let response =
            format!("HTTP/1.1 200 OK\r\nContent-Length: {body_len}\r\n\r\n{metrics_json}");
        let response: &'static str = Box::leak(response.into_boxed_str());
        serve_once(&sock_ok, response);

        client.transport = Transport::Unix {
            socket_path: sock_ok.clone(),
        };
        let result = client.fetch_metrics();

        assert!(result.is_some(), "expected Some on valid response");
        assert!(
            client.last_error.is_none(),
            "expected last_error cleared after success, got {:?}",
            client.last_error
        );
    }

    #[test]
    fn tcp_client_builder_failure_bubbles_up() {
        let source =
            reqwest::Proxy::all("http://127.0.0.1:bad-port").expect_err("proxy should be invalid");
        TCP_CLIENT_BUILD_ERROR.with(|slot| {
            *slot.borrow_mut() = Some(source);
        });

        let error = ProfilerClient::new_tcp("http://127.0.0.1:6060")
            .expect_err("injected reqwest build error should fail client construction");

        match error {
            ClientError::Build { base_url, .. } => {
                assert_eq!(base_url, "http://127.0.0.1:6060");
            }
            other => panic!("expected ClientError::Build, got {other:?}"),
        }
    }

    // ── ClientError::Display ──────────────────────────────────────────────

    #[test]
    fn client_error_display_includes_category() {
        let source =
            reqwest::Proxy::all("http://127.0.0.1:bad-port").expect_err("proxy should be invalid");
        let e = ClientError::Build {
            base_url: "http://127.0.0.1:6060".to_owned(),
            source,
        };
        assert!(
            e.to_string()
                .starts_with("http client build failed for http://127.0.0.1:6060:"),
            "{e}"
        );

        let e = ClientError::Connect(io::Error::new(io::ErrorKind::NotFound, "no such file"));
        assert!(e.to_string().starts_with("connect failed:"), "{e}");

        let e = ClientError::BadStatus("HTTP/1.1 503 Service Unavailable".to_owned());
        assert!(e.to_string().starts_with("bad status:"), "{e}");

        let e: ClientError = serde_json::from_str::<serde_json::Value>("bad")
            .unwrap_err()
            .into();
        assert!(e.to_string().starts_with("parse failed:"), "{e}");
    }

    // ── TraceEvent channel lifecycle round-trip tests ─────────────────────

    fn make_trace_event(event_type: &str) -> TraceEvent {
        serde_json::from_str(&format!(
            r#"{{"sequence":"1","trace_id":"00000000000000000000000000000000","span_id":"1","parent_span_id":"0","subject":{{"kind":"actor","id":"12345678","actor":{{"actor_id":"12345678","spawn_serial":"12345678","actor_type":null}}}},"source_attribution":"not_applicable","source_actor":null,"destination_attribution":"not_applicable","destination_actor":null,"event_type":"{event_type}","msg_type":0,"timestamp_ns":"9999","handler_name":null}}"#
        ))
        .expect("TraceEvent JSON must deserialise")
    }

    /// Each new channel `event_type` string deserialises into a `TraceEvent`
    /// with the `event_type` field populated (wire-contract-test-presence).
    #[test]
    fn channel_event_types_deserialise_with_populated_fields() {
        for et in crate::events::CURRENT_TRACE_EVENT_METADATA
            .iter()
            .filter(|meta| meta.is_actionable())
            .map(|meta| meta.name)
        {
            if matches!(et, "send" | "spawn" | "crash" | "stop") {
                continue;
            }
            let ev = make_trace_event(et);
            assert_eq!(ev.event_type, et, "event_type must round-trip for {et}");
            assert_eq!(
                ev.subject
                    .as_ref()
                    .and_then(|subject| subject.actor.as_ref())
                    .map(|actor| actor.actor_id),
                Some(12_345_678),
                "actor_id must be populated for {et}"
            );
            assert_eq!(
                ev.timestamp_ns, 9999,
                "timestamp_ns must be populated for {et}"
            );
        }
    }

    /// Channel lifecycle events are actionable (visible in the trace UI).
    #[test]
    fn channel_event_types_are_actionable() {
        for et in crate::events::CURRENT_TRACE_EVENT_METADATA
            .iter()
            .filter(|meta| meta.is_actionable())
            .map(|meta| meta.name)
        {
            let ev = make_trace_event(et);
            assert!(
                ev.is_actionable(),
                "channel event '{et}' must be actionable"
            );
        }
    }

    /// Internal-only span types remain non-actionable.
    #[test]
    fn internal_span_types_are_not_actionable() {
        for et in ["begin", "end", "io_accept", "io_recv", "unknown"] {
            let ev = make_trace_event(et);
            assert!(
                !ev.is_actionable(),
                "internal event '{et}' must not be actionable"
            );
        }
    }
}
