//! Shared normalization for listener addresses.

use std::borrow::Cow;

/// Expand the documented `":port"` shorthand to an address the resolver accepts.
///
/// `":port"` is documented as "listen on all interfaces" but is not a socket
/// address any resolver parses, so it used to fail the bind and, before the
/// failure became observable, hand back a null-backed server.
pub(crate) fn normalize_bind_addr(addr: &str) -> Cow<'_, str> {
    match addr.strip_prefix(':') {
        Some(port) if !port.is_empty() && port.bytes().all(|b| b.is_ascii_digit()) => {
            Cow::Owned(format!("0.0.0.0:{port}"))
        }
        _ => Cow::Borrowed(addr),
    }
}

#[cfg(test)]
mod tests {
    use super::normalize_bind_addr;

    #[test]
    fn port_only_form_expands_to_all_interfaces() {
        assert_eq!(normalize_bind_addr(":8080"), "0.0.0.0:8080");
        assert_eq!(normalize_bind_addr(":0"), "0.0.0.0:0");
    }

    #[test]
    fn other_forms_are_left_exactly_as_written() {
        assert_eq!(normalize_bind_addr("127.0.0.1:0"), "127.0.0.1:0");
        assert_eq!(normalize_bind_addr(":"), ":");
        assert_eq!(normalize_bind_addr(":http"), ":http");
        assert_eq!(normalize_bind_addr("[::1]:80"), "[::1]:80");
    }
}
