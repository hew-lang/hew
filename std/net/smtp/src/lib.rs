//! Hew runtime: SMTP email sending via lettre.
//!
//! Provides SMTP client functionality for compiled Hew programs.
//! Opaque connection handles are freed via [`hew_smtp_close`].

use hew_cabi::cabi::cstr_to_str;
use std::os::raw::c_char;

use lettre::message::{header::ContentType, Mailbox};
use lettre::transport::smtp::authentication::Credentials;
use lettre::{Message, SmtpTransport, Transport};

/// Opaque SMTP connection handle.
///
/// Returned by [`hew_smtp_connect`] or [`hew_smtp_connect_tls`].
/// Must be closed with [`hew_smtp_close`].
pub struct HewSmtpConn {
    transport: SmtpTransport,
}

impl std::fmt::Debug for HewSmtpConn {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("HewSmtpConn").finish_non_exhaustive()
    }
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
    port: u16,
    user: *const c_char,
    pass: *const c_char,
) -> *mut HewSmtpConn {
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
    port: u16,
    user: *const c_char,
    pass: *const c_char,
) -> *mut HewSmtpConn {
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
/// Returns `None` if any pointer is null, contains invalid UTF-8, or the
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
) -> Option<Message> {
    // SAFETY: from is a valid NUL-terminated C string per caller contract.
    let from_str = unsafe { cstr_to_str(from) }?;
    // SAFETY: to is a valid NUL-terminated C string per caller contract.
    let to_str = unsafe { cstr_to_str(to) }?;
    // SAFETY: subject is a valid NUL-terminated C string per caller contract.
    let subject_str = unsafe { cstr_to_str(subject) }?;
    // SAFETY: body is a valid NUL-terminated C string per caller contract.
    let body_str = unsafe { cstr_to_str(body) }?;

    let from_mbox: Mailbox = from_str.parse().ok()?;
    let to_mbox: Mailbox = to_str.parse().ok()?;

    let builder = Message::builder()
        .from(from_mbox)
        .to(to_mbox)
        .subject(subject_str);

    if html {
        builder
            .header(ContentType::TEXT_HTML)
            .body(body_str.to_owned())
            .ok()
    } else {
        builder
            .header(ContentType::TEXT_PLAIN)
            .body(body_str.to_owned())
            .ok()
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
    if conn.is_null() {
        return -1;
    }
    // SAFETY: from/to/subject/body are valid NUL-terminated C strings per caller contract.
    let Some(message) = (unsafe { build_message(from, to, subject, body, false) }) else {
        return -1;
    };
    // SAFETY: conn is a valid HewSmtpConn pointer per caller contract.
    let conn = unsafe { &*conn };
    if conn.transport.send(&message).is_ok() {
        0
    } else {
        -1
    }
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
    if conn.is_null() {
        return -1;
    }
    // SAFETY: from/to/subject/html are valid NUL-terminated C strings per caller contract.
    let Some(message) = (unsafe { build_message(from, to, subject, html, true) }) else {
        return -1;
    };
    // SAFETY: conn is a valid HewSmtpConn pointer per caller contract.
    let conn = unsafe { &*conn };
    if conn.transport.send(&message).is_ok() {
        0
    } else {
        -1
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
extern crate hew_runtime; // Link hew_vec_* symbol implementations
#[cfg(test)]
mod tests {
    use super::*;
    use std::ffi::CString;
    use std::ptr;

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
        assert!(
            msg.is_some(),
            "plain-text message should build successfully"
        );
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
        assert!(msg.is_some(), "HTML message should build successfully");
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
        assert!(msg.is_none(), "null from should return None");

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
        assert!(msg.is_none(), "null to should return None");
    }
}
