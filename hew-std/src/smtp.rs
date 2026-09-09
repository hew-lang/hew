//! Hew runtime: SMTP email sending via lettre.
//!
//! Provides SMTP client functionality for compiled Hew programs.
//! Text arguments and results use managed UTF-8 strings; connection handles
//! are `Box`-allocated so callers can free them with the corresponding free
//! function. Null is the canonical empty string.
use hew_cabi::string::{string_as_str, string_from_str, HewString};
use std::time::Duration;

use lettre::message::{header::ContentType, Mailbox};
use lettre::transport::smtp::authentication::Credentials;
use lettre::{Message, SmtpTransport, Transport};

fn set_smtp_last_error(msg: impl Into<String>) {
    hew_runtime::parse_error_slot::set_error(
        hew_runtime::parse_error_slot::ErrorSlotKind::Smtp,
        msg,
    );
}

fn clear_smtp_last_error() {
    hew_runtime::parse_error_slot::clear_error(hew_runtime::parse_error_slot::ErrorSlotKind::Smtp);
}

fn get_smtp_last_error() -> String {
    hew_runtime::parse_error_slot::get_error(hew_runtime::parse_error_slot::ErrorSlotKind::Smtp)
        .unwrap_or_default()
}

fn smtp_error_result(msg: impl Into<String>) -> i32 {
    set_smtp_last_error(msg);
    -1
}

/// Opaque SMTP connection handle.
///
/// Returned by [`hew_smtp_connect`] or [`hew_smtp_connect_tls`].
/// Must be closed with [`hew_smtp_close`].
pub struct HewSmtpConn {
    transport: SmtpTransport,
}

/// Return this actor's last SMTP client error.
///
/// Returns an empty string when no SMTP client error has been recorded.
#[no_mangle]
pub extern "C" fn hew_smtp_last_error() -> *mut HewString {
    string_from_str(&get_smtp_last_error())
}

impl std::fmt::Debug for HewSmtpConn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewSmtpConn").finish_non_exhaustive()
    }
}

struct CloseOnDrop<C: Copy, F: FnOnce(C)> {
    conn: Option<C>,
    close: Option<F>,
}

impl<C: Copy, F: FnOnce(C)> CloseOnDrop<C, F> {
    fn new(conn: C, close: F) -> Self {
        Self {
            conn: Some(conn),
            close: Some(close),
        }
    }

    fn conn(&self) -> C {
        self.conn.expect("connection should remain live until drop")
    }
}

impl<C: Copy, F: FnOnce(C)> Drop for CloseOnDrop<C, F> {
    fn drop(&mut self) {
        if let (Some(conn), Some(close)) = (self.conn.take(), self.close.take()) {
            close(conn);
        }
    }
}

fn with_connection<C: Copy, Connect, Close, Send>(connect: Connect, close: Close, send: Send) -> i32
where
    Connect: FnOnce() -> Option<C>,
    Close: FnOnce(C),
    Send: FnOnce(C) -> i32,
{
    let Some(conn) = connect() else {
        return -1;
    };
    let guard = CloseOnDrop::new(conn, close);
    send(guard.conn())
}

/// Upper bound on how long a connect probe may block.
///
/// `connect` must prove the server is reachable and speaking SMTP before it
/// hands back a connection, and proving that means a real round trip. The
/// bound keeps an unreachable host from stalling the caller indefinitely.
const SMTP_CONNECT_TIMEOUT: Duration = Duration::from_secs(10);

/// Apply optional credentials and port to an SMTP transport builder.
fn configure_builder(
    builder: lettre::transport::smtp::SmtpTransportBuilder,
    port: u16,
    user: Option<&str>,
    pass: Option<&str>,
) -> SmtpTransport {
    let builder = builder.port(port).timeout(Some(SMTP_CONNECT_TIMEOUT));
    let builder = if let (Some(u), Some(p)) = (user, pass) {
        builder.credentials(Credentials::new(u.to_owned(), p.to_owned()))
    } else {
        builder
    };
    builder.build()
}

/// Open a connection to the configured server and exchange one command.
///
/// `SmtpTransport` is lazy: building it performs no I/O, so a transport for an
/// unreachable host is indistinguishable from a working one until the first
/// send. `test_connection` opens the connection and issues a `NOOP`, which is
/// what makes a connect failure observable at connect time.
fn probe_transport(transport: &SmtpTransport, host: &str, port: u16) -> Result<(), String> {
    match transport.test_connection() {
        Ok(true) => Ok(()),
        Ok(false) => Err(format!(
            "SMTP connect failed: server at `{host}:{port}` did not accept the connection probe"
        )),
        Err(err) => Err(format!(
            "SMTP connect failed: cannot reach `{host}:{port}`: {err}"
        )),
    }
}

/// Report whether `conn` is backed by a probed connection.
///
/// A failed connect returns null, which is otherwise indistinguishable from a
/// live connection until the first send.
///
/// # Safety
///
/// `conn` must be null or a pointer previously returned by
/// [`hew_smtp_connect`] or [`hew_smtp_connect_tls`].
#[no_mangle]
pub unsafe extern "C" fn hew_smtp_conn_is_valid(conn: *const HewSmtpConn) -> bool {
    !conn.is_null()
}

fn normalize_port(port: i64) -> Option<u16> {
    u16::try_from(port).ok()
}

/// Treat the managed-string canonical empty as "not provided".
///
/// A managed empty string and a null handle are indistinguishable, so an
/// explicit empty username/password now reads as "no credentials given" too
/// — the previous C-string contract could tell an empty string from null and
/// would build `Credentials("", "")` for the former; that distinction cannot
/// exist under the managed carrier.
fn non_empty(text: &str) -> Option<&str> {
    (!text.is_empty()).then_some(text)
}

/// Connect to an SMTP server using STARTTLS.
///
/// Returns a heap-allocated [`HewSmtpConn`] on success, or null on error.
/// `user` and `pass` may be null (canonical empty) for unauthenticated
/// connections. `port` is `i64` (not `i32`) so a caller-supplied out-of-range
/// value is rejected by `normalize_port` directly instead of being narrowed
/// by a lossy cast first. The caller must close the connection with
/// [`hew_smtp_close`].
///
/// # Safety
///
/// `host`, `user`, and `pass` must each be null (canonical empty) or a live
/// managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_smtp_connect(
    host: *const HewString,
    port: i64,
    user: *const HewString,
    pass: *const HewString,
) -> *mut HewSmtpConn {
    let Some(port) = normalize_port(port) else {
        set_smtp_last_error(format!(
            "SMTP connect failed: port {port} out of range (must be 0..=65535)"
        ));
        return std::ptr::null_mut();
    };
    // SAFETY: host borrows a live managed string or canonical empty.
    let host_str = unsafe { string_as_str(host) };
    if host_str.is_empty() {
        set_smtp_last_error("SMTP connect failed: host is empty");
        return std::ptr::null_mut();
    }
    // SAFETY: user/pass borrow live managed strings or canonical empty; an
    // empty value now means "no credentials" (see `non_empty`).
    let user_str = non_empty(unsafe { string_as_str(user) });
    // SAFETY: as above.
    let pass_str = non_empty(unsafe { string_as_str(pass) });

    let Ok(builder) = SmtpTransport::starttls_relay(host_str) else {
        set_smtp_last_error(format!(
            "SMTP connect failed: could not initialize relay for host `{host_str}`"
        ));
        return std::ptr::null_mut();
    };

    let transport = configure_builder(builder, port, user_str, pass_str);
    if let Err(reason) = probe_transport(&transport, host_str, port) {
        set_smtp_last_error(reason);
        return std::ptr::null_mut();
    }
    clear_smtp_last_error();
    Box::into_raw(Box::new(HewSmtpConn { transport }))
}

/// Connect to an SMTP server using implicit TLS (typically port 465).
///
/// Returns a heap-allocated [`HewSmtpConn`] on success, or null on error.
/// `user` and `pass` may be null (canonical empty) for unauthenticated
/// connections. `port` is `i64` for the same lossless-validation reason as
/// [`hew_smtp_connect`]. The caller must close the connection with
/// [`hew_smtp_close`].
///
/// # Safety
///
/// `host`, `user`, and `pass` must each be null (canonical empty) or a live
/// managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_smtp_connect_tls(
    host: *const HewString,
    port: i64,
    user: *const HewString,
    pass: *const HewString,
) -> *mut HewSmtpConn {
    let Some(port) = normalize_port(port) else {
        set_smtp_last_error(format!(
            "SMTP connect failed: port {port} out of range (must be 0..=65535)"
        ));
        return std::ptr::null_mut();
    };
    // SAFETY: host borrows a live managed string or canonical empty.
    let host_str = unsafe { string_as_str(host) };
    if host_str.is_empty() {
        set_smtp_last_error("SMTP connect failed: host is empty");
        return std::ptr::null_mut();
    }
    // SAFETY: user/pass borrow live managed strings or canonical empty; an
    // empty value now means "no credentials" (see `non_empty`).
    let user_str = non_empty(unsafe { string_as_str(user) });
    // SAFETY: as above.
    let pass_str = non_empty(unsafe { string_as_str(pass) });

    let Ok(builder) = SmtpTransport::relay(host_str) else {
        set_smtp_last_error(format!(
            "SMTP connect failed: could not initialize relay for host `{host_str}`"
        ));
        return std::ptr::null_mut();
    };

    let transport = configure_builder(builder, port, user_str, pass_str);
    if let Err(reason) = probe_transport(&transport, host_str, port) {
        set_smtp_last_error(reason);
        return std::ptr::null_mut();
    }
    clear_smtp_last_error();
    Box::into_raw(Box::new(HewSmtpConn { transport }))
}

/// Build an email [`Message`] from managed string arguments.
///
/// Returns an error if the addresses/message cannot be parsed, or if subject
/// and body are both empty.
///
/// # Safety
///
/// Each argument must be null (canonical empty) or a live managed string handle.
unsafe fn build_message(
    from: *const HewString,
    to: *const HewString,
    subject: *const HewString,
    body: *const HewString,
    html: bool,
) -> Result<Message, String> {
    // SAFETY: each argument borrows a live managed string or canonical empty
    // for this call.
    let (from_str, to_str, subject_str, body_str) = unsafe {
        (
            string_as_str(from),
            string_as_str(to),
            string_as_str(subject),
            string_as_str(body),
        )
    };

    let from_mbox: Mailbox = match from_str.parse() {
        Ok(mailbox) => mailbox,
        Err(err) => {
            return Err(format!(
                "SMTP message build failed: could not parse from address `{from_str}`: {err}"
            ))
        }
    };
    let to_mbox: Mailbox = match to_str.parse() {
        Ok(mailbox) => mailbox,
        Err(err) => {
            return Err(format!(
                "SMTP message build failed: could not parse to address `{to_str}`: {err}"
            ))
        }
    };

    if subject_str.is_empty() && body_str.is_empty() {
        return Err("SMTP message build failed: subject and body cannot both be empty".to_string());
    }

    let builder = Message::builder()
        .from(from_mbox)
        .to(to_mbox)
        .subject(subject_str);

    if html {
        builder
            .header(ContentType::TEXT_HTML)
            .body(body_str.to_owned())
            .map_err(|err| format!("SMTP HTML message build failed: {err}"))
    } else {
        builder
            .header(ContentType::TEXT_PLAIN)
            .body(body_str.to_owned())
            .map_err(|err| format!("SMTP plain-text message build failed: {err}"))
    }
}

fn smtp_send_impl(
    conn: *mut HewSmtpConn,
    from: *const HewString,
    to: *const HewString,
    subject: *const HewString,
    body: *const HewString,
    html: bool,
    send: impl FnOnce(&HewSmtpConn, &Message) -> Result<(), String>,
) -> i32 {
    if conn.is_null() {
        return smtp_error_result("SMTP send failed: connection pointer is null");
    }
    // SAFETY: from/to/subject/body each borrow a live managed string or
    // canonical empty per caller contract.
    let message = match unsafe { build_message(from, to, subject, body, html) } {
        Ok(message) => message,
        Err(err) => return smtp_error_result(err),
    };
    // SAFETY: conn is a valid HewSmtpConn pointer per caller contract.
    let conn = unsafe { &*conn };
    match send(conn, &message) {
        Ok(()) => {
            clear_smtp_last_error();
            0
        }
        Err(err) => smtp_error_result(err),
    }
}

/// Send a plain-text email.
///
/// Returns 0 on success, -1 on error.
///
/// # Safety
///
/// - `conn` must be a valid pointer returned by [`hew_smtp_connect`] or
///   [`hew_smtp_connect_tls`].
/// - `from`, `to`, `subject`, and `body` must each be null (canonical empty)
///   or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_smtp_send(
    conn: *mut HewSmtpConn,
    from: *const HewString,
    to: *const HewString,
    subject: *const HewString,
    body: *const HewString,
) -> i32 {
    smtp_send_impl(conn, from, to, subject, body, false, |conn, message| {
        conn.transport
            .send(message)
            .map(|_| ())
            .map_err(|err| format!("hew_smtp_send: send failed: {err}"))
    })
}

/// Send an HTML email.
///
/// Returns 0 on success, -1 on error.
///
/// # Safety
///
/// - `conn` must be a valid pointer returned by [`hew_smtp_connect`] or
///   [`hew_smtp_connect_tls`].
/// - `from`, `to`, `subject`, and `html` must each be null (canonical empty)
///   or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_smtp_send_html(
    conn: *mut HewSmtpConn,
    from: *const HewString,
    to: *const HewString,
    subject: *const HewString,
    html: *const HewString,
) -> i32 {
    smtp_send_impl(conn, from, to, subject, html, true, |conn, message| {
        conn.transport
            .send(message)
            .map(|_| ())
            .map_err(|err| format!("hew_smtp_send_html: send failed: {err}"))
    })
}

unsafe fn connect_send_close(
    host: *const HewString,
    port: i64,
    user: *const HewString,
    pass: *const HewString,
    send: impl FnOnce(*mut HewSmtpConn) -> i32,
) -> i32 {
    with_connection(
        || {
            // SAFETY: host/user/pass satisfy hew_smtp_connect's contract.
            let conn = unsafe { hew_smtp_connect(host, port, user, pass) };
            (!conn.is_null()).then_some(conn)
        },
        |conn| {
            // SAFETY: conn comes from hew_smtp_connect and has not yet been freed.
            unsafe { hew_smtp_close(conn) };
        },
        send,
    )
}

/// Connect using STARTTLS, send one plain-text email, and close the connection.
///
/// Returns 0 on success, -1 on connection or send error. `port` is `i64` for
/// the same lossless-validation reason as [`hew_smtp_connect`].
///
/// # Safety
///
/// `host`, `user`, `pass`, `from`, `to`, `subject`, and `body` must each be
/// null (canonical empty) or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_smtp_send_once(
    host: *const HewString,
    port: i64,
    user: *const HewString,
    pass: *const HewString,
    from: *const HewString,
    to: *const HewString,
    subject: *const HewString,
    body: *const HewString,
) -> i32 {
    // SAFETY: all pointers satisfy the contracts of connect_send_close/hew_smtp_send.
    unsafe {
        connect_send_close(host, port, user, pass, |conn| {
            hew_smtp_send(conn, from, to, subject, body)
        })
    }
}

/// Connect using STARTTLS, send one HTML email, and close the connection.
///
/// Returns 0 on success, -1 on connection or send error. `port` is `i64` for
/// the same lossless-validation reason as [`hew_smtp_connect`].
///
/// # Safety
///
/// `host`, `user`, `pass`, `from`, `to`, `subject`, and `html` must each be
/// null (canonical empty) or a live managed string handle.
#[no_mangle]
pub unsafe extern "C" fn hew_smtp_send_html_once(
    host: *const HewString,
    port: i64,
    user: *const HewString,
    pass: *const HewString,
    from: *const HewString,
    to: *const HewString,
    subject: *const HewString,
    html: *const HewString,
) -> i32 {
    // SAFETY: all pointers satisfy the contracts of connect_send_close/hew_smtp_send_html.
    unsafe {
        connect_send_close(host, port, user, pass, |conn| {
            hew_smtp_send_html(conn, from, to, subject, html)
        })
    }
}

/// Close and free an SMTP connection.
///
/// # Safety
///
/// `conn` must be a pointer previously returned by [`hew_smtp_connect`] or
/// [`hew_smtp_connect_tls`], and must not have been freed already. Null is
/// accepted and ignored.
#[no_mangle]
pub unsafe extern "C" fn hew_smtp_close(conn: *mut HewSmtpConn) {
    if conn.is_null() {
        return;
    }
    // SAFETY: conn was allocated with Box::into_raw in hew_smtp_connect/hew_smtp_connect_tls.
    let _ = unsafe { Box::from_raw(conn) };
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_string::ManagedString;
    use hew_cabi::string::string_release;
    use lettre::transport::smtp::client::Tls;
    use std::cell::RefCell;
    use std::io::{BufRead, BufReader, Write};
    use std::net::TcpListener;
    use std::ptr;
    use std::rc::Rc;

    fn make_test_conn() -> *mut HewSmtpConn {
        Box::into_raw(Box::new(HewSmtpConn {
            transport: SmtpTransport::unencrypted_localhost(),
        }))
    }

    /// Last-error accessor returns "" (null) with no error recorded; a
    /// non-empty result is a live managed string owner this helper releases.
    fn last_error_text() -> String {
        let err = hew_smtp_last_error();
        // SAFETY: `err` is null (canonical empty) or a live managed owner
        // returned by `hew_smtp_last_error`.
        let text = unsafe { string_as_str(err) }.to_owned();
        // SAFETY: `err` is the owner this call just produced; releasing is a
        // no-op when it is null.
        unsafe { string_release(err) };
        text
    }

    #[test]
    fn connection_probe_rejects_a_reachable_server_that_refuses_noop() {
        let listener = TcpListener::bind("127.0.0.1:0").expect("bind loopback SMTP oracle");
        let port = listener.local_addr().expect("listener address").port();
        let server = std::thread::spawn(move || {
            let (mut stream, _) = listener.accept().expect("accept SMTP probe");
            stream
                .set_read_timeout(Some(Duration::from_secs(2)))
                .expect("set read timeout");
            stream
                .set_write_timeout(Some(Duration::from_secs(2)))
                .expect("set write timeout");
            stream.write_all(b"220 oracle ESMTP\r\n").expect("greeting");

            let mut reader = BufReader::new(stream.try_clone().expect("clone stream"));
            let mut line = String::new();
            reader.read_line(&mut line).expect("read EHLO");
            assert!(line.starts_with("EHLO "), "unexpected command: {line:?}");
            stream
                .write_all(b"250-oracle\r\n250 HELP\r\n")
                .expect("EHLO response");

            line.clear();
            reader.read_line(&mut line).expect("read NOOP");
            assert_eq!(line, "NOOP\r\n");
            stream
                .write_all(b"550 NOOP refused\r\n")
                .expect("NOOP rejection");

            line.clear();
            if reader.read_line(&mut line).unwrap_or(0) > 0 && line == "QUIT\r\n" {
                stream.write_all(b"221 bye\r\n").expect("QUIT response");
            }
        });

        let transport = SmtpTransport::builder_dangerous("127.0.0.1")
            .port(port)
            .tls(Tls::None)
            .timeout(Some(Duration::from_secs(2)))
            .build();
        let result = probe_transport(&transport, "127.0.0.1", port);
        assert!(
            result.is_err(),
            "NOOP refusal must reject the constructor gate"
        );
        server.join().expect("SMTP oracle thread");
    }

    #[test]
    fn connection_probe_rejects_reachable_non_smtp_endpoint() {
        let listener = TcpListener::bind("127.0.0.1:0").expect("bind loopback oracle");
        let port = listener.local_addr().expect("listener address").port();
        let server = std::thread::spawn(move || {
            let (mut stream, _) = listener.accept().expect("accept probe");
            stream
                .write_all(b"this is not SMTP\r\n")
                .expect("write junk");
        });

        let host = ManagedString::new("127.0.0.1");
        // SAFETY: host is a live managed string and optional credentials are null.
        let conn =
            unsafe { hew_smtp_connect(host.as_ptr(), i64::from(port), ptr::null(), ptr::null()) };
        assert!(
            conn.is_null(),
            "the public constructor must reject a reachable non-SMTP endpoint"
        );
        assert!(!last_error_text().is_empty());
        server.join().expect("non-SMTP oracle thread");
    }

    #[test]
    fn normalize_port_rejects_out_of_range_values() {
        assert_eq!(normalize_port(0), Some(0));
        assert_eq!(normalize_port(587), Some(587));
        assert_eq!(normalize_port(65_535), Some(65_535));
        assert_eq!(normalize_port(-1), None);
        assert_eq!(normalize_port(65_536), None);
    }

    #[test]
    fn normalize_port_rejects_oversized_i64_without_wrapping_into_a_valid_port() {
        // 4_294_967_883 == 587 + 2^32. A lossy i64->i32 truncation before this
        // check would wrap it down to the "valid" port 587; normalize_port
        // takes the full i64 directly, so it must reject the value outright.
        assert_eq!(normalize_port(4_294_967_883), None);
        assert_eq!(normalize_port(i64::MAX), None);
        assert_eq!(normalize_port(i64::MIN), None);
    }

    #[test]
    fn debug_impl() {
        let formatted = format!(
            "{:?}",
            HewSmtpConn {
                transport: SmtpTransport::unencrypted_localhost(),
            }
        );
        assert!(
            formatted.contains("HewSmtpConn"),
            "Debug output should contain struct name"
        );
    }

    #[test]
    fn build_plain_message() {
        let from = ManagedString::new("sender@example.com");
        let to = ManagedString::new("recipient@example.com");
        let subject = ManagedString::new("Test Subject");
        let body = ManagedString::new("Hello, world!");

        // SAFETY: each argument is a live managed string handle.
        let msg = unsafe {
            build_message(
                from.as_ptr(),
                to.as_ptr(),
                subject.as_ptr(),
                body.as_ptr(),
                false,
            )
        };
        assert!(msg.is_ok(), "plain-text message should build successfully");
    }

    #[test]
    fn build_html_message() {
        let from = ManagedString::new("sender@example.com");
        let to = ManagedString::new("recipient@example.com");
        let subject = ManagedString::new("HTML Test");
        let html = ManagedString::new("<h1>Hello</h1>");

        // SAFETY: each argument is a live managed string handle.
        let msg = unsafe {
            build_message(
                from.as_ptr(),
                to.as_ptr(),
                subject.as_ptr(),
                html.as_ptr(),
                true,
            )
        };
        assert!(msg.is_ok(), "HTML message should build successfully");
    }

    #[test]
    fn null_pointer_safety() {
        // hew_smtp_connect with null host returns null.
        // SAFETY: Testing null-pointer handling; no valid pointers needed.
        let conn = unsafe { hew_smtp_connect(ptr::null(), 587, ptr::null(), ptr::null()) };
        assert!(conn.is_null());

        // hew_smtp_connect_tls with null host returns null.
        // SAFETY: Testing null-pointer handling; no valid pointers needed.
        let conn = unsafe { hew_smtp_connect_tls(ptr::null(), 465, ptr::null(), ptr::null()) };
        assert!(conn.is_null());

        // hew_smtp_send with null conn returns -1.
        let from = ManagedString::new("a@b.com");
        let to = ManagedString::new("c@d.com");
        let subj = ManagedString::new("s");
        let body = ManagedString::new("b");
        // SAFETY: conn is null (tested), other pointers are live managed strings.
        let rc = unsafe {
            hew_smtp_send(
                ptr::null_mut(),
                from.as_ptr(),
                to.as_ptr(),
                subj.as_ptr(),
                body.as_ptr(),
            )
        };
        assert_eq!(rc, -1);

        // hew_smtp_send_once with null host returns -1.
        // SAFETY: host is null (tested), remaining pointers are live managed strings.
        let rc = unsafe {
            hew_smtp_send_once(
                ptr::null(),
                587,
                ptr::null(),
                ptr::null(),
                from.as_ptr(),
                to.as_ptr(),
                subj.as_ptr(),
                body.as_ptr(),
            )
        };
        assert_eq!(rc, -1);

        // hew_smtp_send_html_once with null host returns -1.
        // SAFETY: host is null (tested), remaining pointers are live managed strings.
        let rc = unsafe {
            hew_smtp_send_html_once(
                ptr::null(),
                587,
                ptr::null(),
                ptr::null(),
                from.as_ptr(),
                to.as_ptr(),
                subj.as_ptr(),
                body.as_ptr(),
            )
        };
        assert_eq!(rc, -1);

        // hew_smtp_send_html with null conn returns -1.
        // SAFETY: conn is null (tested), other pointers are live managed strings.
        let rc = unsafe {
            hew_smtp_send_html(
                ptr::null_mut(),
                from.as_ptr(),
                to.as_ptr(),
                subj.as_ptr(),
                body.as_ptr(),
            )
        };
        assert_eq!(rc, -1);

        // hew_smtp_close with null is safe (no-op).
        // SAFETY: Null pointer is explicitly handled.
        unsafe { hew_smtp_close(ptr::null_mut()) };
    }

    #[test]
    fn build_message_null_args() {
        // Null is the canonical empty managed string; an empty from/to address
        // fails Mailbox parsing the same way it did under the old contract.
        let valid = ManagedString::new("a@b.com");

        // SAFETY: null is canonical empty; `valid` is a live managed string.
        let msg = unsafe {
            build_message(
                ptr::null(),
                valid.as_ptr(),
                valid.as_ptr(),
                valid.as_ptr(),
                false,
            )
        };
        assert!(msg.is_err(), "empty from should return an error");

        // SAFETY: null is canonical empty; `valid` is a live managed string.
        let msg = unsafe {
            build_message(
                valid.as_ptr(),
                ptr::null(),
                valid.as_ptr(),
                valid.as_ptr(),
                false,
            )
        };
        assert!(msg.is_err(), "empty to should return an error");
    }

    #[test]
    fn bad_from_address_sets_last_error() {
        clear_smtp_last_error();
        let conn = make_test_conn();
        let from = ManagedString::new("not-an-email");
        let to = ManagedString::new("recipient@example.com");
        let subject = ManagedString::new("Hello");
        let body = ManagedString::new("Body");

        // SAFETY: `conn` is a valid test connection and the strings are live managed strings.
        let rc = unsafe {
            hew_smtp_send(
                conn,
                from.as_ptr(),
                to.as_ptr(),
                subject.as_ptr(),
                body.as_ptr(),
            )
        };
        assert_eq!(rc, -1);
        let err = last_error_text();
        assert!(err.contains("parse") || err.contains("address"));

        // SAFETY: `conn` came from `make_test_conn` and has not been freed yet.
        unsafe { hew_smtp_close(conn) };
    }

    #[test]
    fn empty_subject_and_body_set_distinct_last_error() {
        clear_smtp_last_error();
        let conn = make_test_conn();
        let from = ManagedString::new("sender@example.com");
        let to = ManagedString::new("recipient@example.com");
        let empty = ManagedString::new("");

        // SAFETY: `conn` is a valid test connection and the strings are live managed strings.
        let rc = unsafe {
            hew_smtp_send(
                conn,
                from.as_ptr(),
                to.as_ptr(),
                empty.as_ptr(),
                empty.as_ptr(),
            )
        };
        assert_eq!(rc, -1);
        let err = last_error_text();
        assert!(err.contains("subject"));
        assert!(err.contains("body"));
        assert!(!err.contains("parse"));

        // SAFETY: `conn` came from `make_test_conn` and has not been freed yet.
        unsafe { hew_smtp_close(conn) };
    }

    #[test]
    fn successful_send_clears_last_error() {
        clear_smtp_last_error();
        let conn = make_test_conn();
        let bad_from = ManagedString::new("not-an-email");
        let from = ManagedString::new("sender@example.com");
        let to = ManagedString::new("recipient@example.com");
        let subject = ManagedString::new("Hello");
        let body = ManagedString::new("Body");

        let rc = smtp_send_impl(
            conn,
            bad_from.as_ptr(),
            to.as_ptr(),
            subject.as_ptr(),
            body.as_ptr(),
            false,
            |_conn, _message| Ok(()),
        );
        assert_eq!(rc, -1);
        assert!(!last_error_text().is_empty());

        let rc = smtp_send_impl(
            conn,
            from.as_ptr(),
            to.as_ptr(),
            subject.as_ptr(),
            body.as_ptr(),
            false,
            |_conn, _message| Ok(()),
        );
        assert_eq!(rc, 0);
        assert_eq!(last_error_text(), "");

        // SAFETY: `conn` came from `make_test_conn` and has not been freed yet.
        unsafe { hew_smtp_close(conn) };
    }

    #[test]
    fn with_connection_closes_after_send() {
        let events = Rc::new(RefCell::new(Vec::new()));
        let send_events = Rc::clone(&events);
        let close_events = Rc::clone(&events);
        let rc = with_connection(
            || Some(7_i32),
            move |_| close_events.borrow_mut().push("close"),
            move |conn| {
                assert_eq!(conn, 7);
                send_events.borrow_mut().push("send");
                0
            },
        );
        assert_eq!(rc, 0);
        assert_eq!(events.borrow().as_slice(), ["send", "close"]);
    }

    #[test]
    fn with_connection_closes_after_send_error() {
        let events = Rc::new(RefCell::new(Vec::new()));
        let send_events = Rc::clone(&events);
        let close_events = Rc::clone(&events);
        let rc = with_connection(
            || Some(11_i32),
            move |_| close_events.borrow_mut().push("close"),
            move |conn| {
                assert_eq!(conn, 11);
                send_events.borrow_mut().push("send");
                -1
            },
        );
        assert_eq!(rc, -1);
        assert_eq!(events.borrow().as_slice(), ["send", "close"]);
    }

    #[test]
    fn with_connection_skips_close_when_connect_fails() {
        let events = Rc::new(RefCell::new(Vec::new()));
        let close_events = Rc::clone(&events);
        let send_events = Rc::clone(&events);
        let rc = with_connection(
            || None::<i32>,
            move |_| close_events.borrow_mut().push("close"),
            move |_| {
                send_events.borrow_mut().push("send");
                0
            },
        );
        assert_eq!(rc, -1);
        assert!(events.borrow().is_empty());
    }

    /// An SMTP error recorded by the REAL `hew_smtp_send` failure path —
    /// while a given actor is the installed dispatch context on OS thread A —
    /// must be readable through the REAL public `hew_smtp_last_error`
    /// accessor when that SAME actor is the installed dispatch context on a
    /// DIFFERENT OS thread B.
    ///
    /// This is the actual #2659 regression: an actor parked mid-send and
    /// resumed on another scheduler worker must not lose its recorded error.
    /// Driving the module's own producer (`hew_smtp_send`) and public
    /// accessor (`hew_smtp_last_error`) — rather than poking the shared
    /// `parse_error_slot` map directly — means this test is RED against the
    /// predecessor `thread_local!` implementation: a plain `thread_local!`
    /// slot is intrinsically per-OS-thread storage, so thread B's slot would
    /// stay empty no matter which actor either thread believes is
    /// dispatching. It is GREEN only because `set_/get_smtp_last_error` now
    /// key on actor identity via `parse_error_slot`.
    ///
    /// Run 3× to satisfy the flake gate.
    #[test]
    fn smtp_error_visible_across_worker_threads_regression_2659() {
        use crate::net_error_slot_test_support::{
            spawn_error_slot_test_actor, with_actor_context, NetErrorSlotRuntimeGuard,
        };

        // hew_actor_spawn requires an installed runtime authority; shared
        // across tls/smtp/quic so their regression tests serialize on the
        // single process-global scheduler slot instead of racing.
        let _runtime = NetErrorSlotRuntimeGuard::new();

        for run in 0..3_u32 {
            let test_actor = spawn_error_slot_test_actor();
            assert!(!test_actor.is_null(), "test actor should spawn");
            let actor_addr = test_actor as usize;

            let conn = make_test_conn();
            let from = ManagedString::new("not-an-email");
            let to = ManagedString::new("recipient@example.com");
            let subject = ManagedString::new("Hello");
            let body = ManagedString::new("Body");

            let barrier = std::sync::Arc::new(std::sync::Barrier::new(2));
            let barrier2 = barrier.clone();

            let handle = std::thread::spawn(move || {
                // Simulate: the actor resumes on thread B, a different OS
                // thread than the one that recorded the error.
                barrier2.wait();
                let actor_ptr = actor_addr as *mut hew_runtime::actor::HewActor;
                with_actor_context(actor_ptr, last_error_text)
            });

            // Thread A: install the SAME actor as the dispatch context and
            // drive the real hew_smtp_send failure path — the actual
            // producer, not a direct slot poke.
            with_actor_context(test_actor, || {
                // SAFETY: `conn` is a valid test connection and the strings
                // are live managed strings; the malformed `from` address is
                // the documented failure path.
                let rc = unsafe {
                    hew_smtp_send(
                        conn,
                        from.as_ptr(),
                        to.as_ptr(),
                        subject.as_ptr(),
                        body.as_ptr(),
                    )
                };
                assert_eq!(rc, -1);
            });
            barrier.wait();

            let result = handle.join().expect("thread B panicked");
            assert!(
                result.contains("parse") || result.contains("address"),
                "run {run}: SMTP error recorded on thread A must be visible on thread B for the same actor, got {result:?}"
            );

            // SAFETY: `conn` came from `make_test_conn` and has not been freed yet.
            unsafe { hew_smtp_close(conn) };
            // SAFETY: test_actor was spawned above and not yet stopped/freed.
            unsafe { hew_runtime::actor::hew_actor_stop(test_actor) };
            // hew_actor_free reaps every parse_error_slot entry for this
            // actor via parse_error_slot::clear_all_for_actor — no manual
            // clear needed.
            // SAFETY: test_actor is stopped immediately above; free reclaims
            // it exactly once.
            assert_eq!(unsafe { hew_runtime::actor::hew_actor_free(test_actor) }, 0);
        }
    }
}
