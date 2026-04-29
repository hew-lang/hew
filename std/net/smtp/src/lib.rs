//! Hew runtime: SMTP email sending via lettre.
//!
//! Provides SMTP client functionality for compiled Hew programs.
//! All returned strings and connection handles are allocated with `libc::malloc`
//! / `Box` so callers can free them with the corresponding free function.

// Force-link hew-runtime so the linker can resolve hew_vec_* symbols
// referenced by hew-cabi's object code.
#[cfg(test)]
extern crate hew_runtime;

use hew_cabi::cabi::{cstr_to_str, str_to_malloc};
use std::cell::RefCell;
use std::os::raw::c_char;

use lettre::message::{header::ContentType, Mailbox};
use lettre::transport::smtp::authentication::Credentials;
use lettre::{Message, SmtpTransport, Transport};

std::thread_local! {
    static LAST_SMTP_ERROR: RefCell<Option<String>> = const { RefCell::new(None) };
}

fn set_smtp_last_error(msg: impl Into<String>) {
    LAST_SMTP_ERROR.with(|error| *error.borrow_mut() = Some(msg.into()));
}

fn clear_smtp_last_error() {
    LAST_SMTP_ERROR.with(|error| *error.borrow_mut() = None);
}

fn get_smtp_last_error() -> String {
    LAST_SMTP_ERROR.with(|error| error.borrow().clone().unwrap_or_default())
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
pub extern "C" fn hew_smtp_last_error() -> *mut c_char {
    str_to_malloc(&get_smtp_last_error())
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

/// Apply optional credentials and port to an SMTP transport builder.
fn configure_builder(
    builder: lettre::transport::smtp::SmtpTransportBuilder,
    port: u16,
    user: Option<&str>,
    pass: Option<&str>,
) -> SmtpTransport {
    let builder = builder.port(port);
    let builder = if let (Some(u), Some(p)) = (user, pass) {
        builder.credentials(Credentials::new(u.to_owned(), p.to_owned()))
    } else {
        builder
    };
    builder.build()
}

fn normalize_port(port: i32) -> Option<u16> {
    u16::try_from(port).ok()
}

/// Connect to an SMTP server using STARTTLS.
///
/// Returns a heap-allocated [`HewSmtpConn`] on success, or null on error.
/// `user` and `pass` may be null for unauthenticated connections.
/// The caller must close the connection with [`hew_smtp_close`].
///
/// # Safety
///
/// - `host` must be a valid NUL-terminated C string.
/// - If non-null, `user` and `pass` must be valid NUL-terminated C strings.
#[no_mangle]
pub unsafe extern "C" fn hew_smtp_connect(
    host: *const c_char,
    port: i32,
    user: *const c_char,
    pass: *const c_char,
) -> *mut HewSmtpConn {
    let Some(port) = normalize_port(port) else {
        return std::ptr::null_mut();
    };
    // SAFETY: host is a valid NUL-terminated C string per caller contract.
    let Some(host_str) = (unsafe { cstr_to_str(host) }) else {
        return std::ptr::null_mut();
    };
    // SAFETY: user is a valid NUL-terminated C string (or null) per caller contract.
    let user_str = unsafe { cstr_to_str(user) };
    // SAFETY: pass is a valid NUL-terminated C string (or null) per caller contract.
    let pass_str = unsafe { cstr_to_str(pass) };

    let Ok(builder) = SmtpTransport::starttls_relay(host_str) else {
        return std::ptr::null_mut();
    };

    let transport = configure_builder(builder, port, user_str, pass_str);
    Box::into_raw(Box::new(HewSmtpConn { transport }))
}

/// Connect to an SMTP server using implicit TLS (typically port 465).
///
/// Returns a heap-allocated [`HewSmtpConn`] on success, or null on error.
/// `user` and `pass` may be null for unauthenticated connections.
/// The caller must close the connection with [`hew_smtp_close`].
///
/// # Safety
///
/// - `host` must be a valid NUL-terminated C string.
/// - If non-null, `user` and `pass` must be valid NUL-terminated C strings.
#[no_mangle]
pub unsafe extern "C" fn hew_smtp_connect_tls(
    host: *const c_char,
    port: i32,
    user: *const c_char,
    pass: *const c_char,
) -> *mut HewSmtpConn {
    let Some(port) = normalize_port(port) else {
        return std::ptr::null_mut();
    };
    // SAFETY: host is a valid NUL-terminated C string per caller contract.
    let Some(host_str) = (unsafe { cstr_to_str(host) }) else {
        return std::ptr::null_mut();
    };
    // SAFETY: user is a valid NUL-terminated C string (or null) per caller contract.
    let user_str = unsafe { cstr_to_str(user) };
    // SAFETY: pass is a valid NUL-terminated C string (or null) per caller contract.
    let pass_str = unsafe { cstr_to_str(pass) };

    let Ok(builder) = SmtpTransport::relay(host_str) else {
        return std::ptr::null_mut();
    };

    let transport = configure_builder(builder, port, user_str, pass_str);
    Box::into_raw(Box::new(HewSmtpConn { transport }))
}

/// Build an email [`Message`] from C string arguments.
///
/// Returns an error if any pointer is null, contains invalid UTF-8, or the
/// addresses/message cannot be parsed.
///
/// # Safety
///
/// All non-null pointers must point to valid NUL-terminated C strings.
unsafe fn build_message(
    from: *const c_char,
    to: *const c_char,
    subject: *const c_char,
    body: *const c_char,
    html: bool,
) -> Result<Message, String> {
    // SAFETY: from is a valid NUL-terminated C string per caller contract.
    let from_str = unsafe { cstr_to_str(from) }.ok_or_else(|| {
        "SMTP message build failed: from address is null or invalid UTF-8".to_string()
    })?;
    // SAFETY: to is a valid NUL-terminated C string per caller contract.
    let to_str = unsafe { cstr_to_str(to) }.ok_or_else(|| {
        "SMTP message build failed: to address is null or invalid UTF-8".to_string()
    })?;
    // SAFETY: subject is a valid NUL-terminated C string per caller contract.
    let subject_str = unsafe { cstr_to_str(subject) }
        .ok_or_else(|| "SMTP message build failed: subject is null or invalid UTF-8".to_string())?;
    // SAFETY: body is a valid NUL-terminated C string per caller contract.
    let body_str = unsafe { cstr_to_str(body) }
        .ok_or_else(|| "SMTP message build failed: body is null or invalid UTF-8".to_string())?;

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
    from: *const c_char,
    to: *const c_char,
    subject: *const c_char,
    body: *const c_char,
    html: bool,
    send: impl FnOnce(&HewSmtpConn, &Message) -> Result<(), String>,
) -> i32 {
    if conn.is_null() {
        return smtp_error_result("SMTP send failed: connection pointer is null");
    }
    // SAFETY: from/to/subject/body are valid NUL-terminated C strings per caller contract.
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
/// - `from`, `to`, `subject`, and `body` must be valid NUL-terminated C strings.
#[no_mangle]
pub unsafe extern "C" fn hew_smtp_send(
    conn: *mut HewSmtpConn,
    from: *const c_char,
    to: *const c_char,
    subject: *const c_char,
    body: *const c_char,
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
/// - `from`, `to`, `subject`, and `html` must be valid NUL-terminated C strings.
#[no_mangle]
pub unsafe extern "C" fn hew_smtp_send_html(
    conn: *mut HewSmtpConn,
    from: *const c_char,
    to: *const c_char,
    subject: *const c_char,
    html: *const c_char,
) -> i32 {
    smtp_send_impl(conn, from, to, subject, html, true, |conn, message| {
        conn.transport
            .send(message)
            .map(|_| ())
            .map_err(|err| format!("hew_smtp_send_html: send failed: {err}"))
    })
}

unsafe fn connect_send_close(
    host: *const c_char,
    port: i32,
    user: *const c_char,
    pass: *const c_char,
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
/// Returns 0 on success, -1 on connection or send error.
///
/// # Safety
///
/// - `host` must be a valid NUL-terminated C string.
/// - If non-null, `user` and `pass` must be valid NUL-terminated C strings.
/// - `from`, `to`, `subject`, and `body` must be valid NUL-terminated C strings.
#[no_mangle]
pub unsafe extern "C" fn hew_smtp_send_once(
    host: *const c_char,
    port: i32,
    user: *const c_char,
    pass: *const c_char,
    from: *const c_char,
    to: *const c_char,
    subject: *const c_char,
    body: *const c_char,
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
/// Returns 0 on success, -1 on connection or send error.
///
/// # Safety
///
/// - `host` must be a valid NUL-terminated C string.
/// - If non-null, `user` and `pass` must be valid NUL-terminated C strings.
/// - `from`, `to`, `subject`, and `html` must be valid NUL-terminated C strings.
#[no_mangle]
pub unsafe extern "C" fn hew_smtp_send_html_once(
    host: *const c_char,
    port: i32,
    user: *const c_char,
    pass: *const c_char,
    from: *const c_char,
    to: *const c_char,
    subject: *const c_char,
    html: *const c_char,
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
    use std::ffi::{CStr, CString};
    use std::ptr;
    use std::rc::Rc;

    fn make_test_conn() -> *mut HewSmtpConn {
        Box::into_raw(Box::new(HewSmtpConn {
            transport: SmtpTransport::unencrypted_localhost(),
        }))
    }

    fn last_error_text() -> String {
        let err = hew_smtp_last_error();
        assert!(!err.is_null());
        // SAFETY: `err` was allocated by `hew_smtp_last_error`.
        let text = unsafe { CStr::from_ptr(err) }.to_str().unwrap().to_owned();
        // SAFETY: `err` was allocated via `malloc`.
        unsafe { libc::free(err.cast()) };
        text
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
        let from = CString::new("sender@example.com").unwrap();
        let to = CString::new("recipient@example.com").unwrap();
        let subject = CString::new("Test Subject").unwrap();
        let body = CString::new("Hello, world!").unwrap();

        // SAFETY: All CString pointers are valid NUL-terminated C strings.
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
        let from = CString::new("sender@example.com").unwrap();
        let to = CString::new("recipient@example.com").unwrap();
        let subject = CString::new("HTML Test").unwrap();
        let html = CString::new("<h1>Hello</h1>").unwrap();

        // SAFETY: All CString pointers are valid NUL-terminated C strings.
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
        let from = CString::new("a@b.com").unwrap();
        let to = CString::new("c@d.com").unwrap();
        let subj = CString::new("s").unwrap();
        let body = CString::new("b").unwrap();
        // SAFETY: conn is null (tested), other pointers are valid CStrings.
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
        // SAFETY: host is null (tested), remaining pointers are valid CStrings.
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
        // SAFETY: host is null (tested), remaining pointers are valid CStrings.
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
        // SAFETY: conn is null (tested), other pointers are valid CStrings.
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
        let valid = CString::new("a@b.com").unwrap();

        // SAFETY: Testing null-pointer handling in build_message.
        let msg = unsafe {
            build_message(
                ptr::null(),
                valid.as_ptr(),
                valid.as_ptr(),
                valid.as_ptr(),
                false,
            )
        };
        assert!(msg.is_err(), "null from should return an error");

        // SAFETY: Testing null-pointer handling in build_message.
        let msg = unsafe {
            build_message(
                valid.as_ptr(),
                ptr::null(),
                valid.as_ptr(),
                valid.as_ptr(),
                false,
            )
        };
        assert!(msg.is_err(), "null to should return an error");
    }

    #[test]
    fn bad_from_address_sets_last_error() {
        clear_smtp_last_error();
        let conn = make_test_conn();
        let from = CString::new("not-an-email").unwrap();
        let to = CString::new("recipient@example.com").unwrap();
        let subject = CString::new("Hello").unwrap();
        let body = CString::new("Body").unwrap();

        // SAFETY: `conn` is a valid test connection and the strings are valid C strings.
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
        let from = CString::new("sender@example.com").unwrap();
        let to = CString::new("recipient@example.com").unwrap();
        let empty = CString::new("").unwrap();

        // SAFETY: `conn` is a valid test connection and the strings are valid C strings.
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
        let bad_from = CString::new("not-an-email").unwrap();
        let from = CString::new("sender@example.com").unwrap();
        let to = CString::new("recipient@example.com").unwrap();
        let subject = CString::new("Hello").unwrap();
        let body = CString::new("Body").unwrap();

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
}
