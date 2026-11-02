//! `std.net.url`, `std.net.ipnet` and `std.misc.uuid` text parsers.
//!
//! Input: `first NUL second`. Properties: no parser or accessor panics; a
//! parsed URL prints to text that parses to the same URL, and joining any
//! reference never panics; the IP, CIDR and UUID answers agree with the
//! `std`, `ipnet` and `uuid` crates read directly.
#![no_main]

use std::net::IpAddr;

use hew_std::ipnet::{
    hew_cidr_broadcast, hew_cidr_contains, hew_cidr_hosts, hew_cidr_network, hew_ip_is_loopback,
    hew_ip_is_private, hew_ip_is_v4, hew_ip_is_v6, hew_ip_parse,
};
use hew_std::url::{
    hew_url_fragment, hew_url_free, hew_url_host, hew_url_join, hew_url_parse, hew_url_path,
    hew_url_port, hew_url_query, hew_url_scheme, hew_url_to_string, HewUrl,
};
use hew_std::uuid::hew_uuid_parse;
use libfuzzer_sys::fuzz_target;

mod support;
use support::{take, Managed};

unsafe fn print(url: *const HewUrl) -> String {
    // SAFETY: `url` is a live parsed URL; each accessor result is released.
    unsafe {
        for part in [
            hew_url_scheme(url),
            hew_url_host(url),
            hew_url_path(url),
            hew_url_query(url),
            hew_url_fragment(url),
        ] {
            drop(take(part));
        }
        hew_url_port(url);
        take(hew_url_to_string(url))
    }
}

fn url(first: &Managed, second: &Managed) {
    // SAFETY: inputs are live; each URL handle is released once.
    unsafe {
        let parsed = hew_url_parse(first.ptr());
        if parsed.is_null() {
            return;
        }
        let printed = Managed::new(&print(parsed));
        // The url crate does not print every `file:` URL to text it reads
        // back the same: `file://%2e%2e//c:/` prints as `file://../c:/` and
        // `file:/\re//p://` as `file://re/p://`, both of which then read with
        // an empty host. That gap is upstream, so `file:` skips this check.
        if printed.text().starts_with("file:") {
            hew_url_free(parsed);
            return;
        }
        let again = hew_url_parse(printed.ptr());
        assert!(
            !again.is_null(),
            "URL refuses its own text {:?}",
            printed.text()
        );
        assert_eq!(
            print(again),
            printed.text(),
            "URL printing is not a fixed point"
        );
        hew_url_free(again);
        let joined = hew_url_join(parsed, second.ptr());
        if !joined.is_null() {
            drop(print(joined));
            hew_url_free(joined);
        }
        hew_url_free(parsed);
    }
}

fn ip(text: &str, input: &Managed) {
    let reference = text.parse::<IpAddr>().ok();
    // SAFETY: `input` is live for each call.
    unsafe {
        assert_eq!(hew_ip_parse(input.ptr()) == 1, reference.is_some());
        assert_eq!(
            hew_ip_is_v4(input.ptr()) == 1,
            reference.is_some_and(|a| a.is_ipv4())
        );
        assert_eq!(
            hew_ip_is_v6(input.ptr()) == 1,
            reference.is_some_and(|a| a.is_ipv6())
        );
        assert_eq!(
            hew_ip_is_loopback(input.ptr()) == 1,
            reference.is_some_and(|a| a.is_loopback())
        );
        let private = matches!(reference, Some(IpAddr::V4(v4)) if v4.is_private());
        assert_eq!(hew_ip_is_private(input.ptr()) == 1, private);
    }
}

fn cidr(text: &str, input: &Managed, address: &Managed) {
    let reference = text.parse::<ipnet::IpNet>().ok();
    // SAFETY: inputs are live; string results are released.
    unsafe {
        let hosts = hew_cidr_hosts(input.ptr());
        let network = take(hew_cidr_network(input.ptr()));
        let broadcast = take(hew_cidr_broadcast(input.ptr()));
        let contains = hew_cidr_contains(input.ptr(), address.ptr());
        let Some(net) = reference else {
            assert_eq!(hosts, -1, "hosts for an invalid CIDR");
            assert!(network.is_empty() && broadcast.is_empty());
            assert_eq!(contains, -1);
            return;
        };
        assert_eq!(network, net.network().to_string());
        let expected_hosts = match net {
            ipnet::IpNet::V4(v4) => {
                assert_eq!(broadcast, v4.broadcast().to_string());
                let all = 1_i64 << (32 - v4.prefix_len());
                if v4.prefix_len() < 31 {
                    all - 2
                } else {
                    all
                }
            }
            ipnet::IpNet::V6(v6) => {
                assert!(broadcast.is_empty());
                let bits = 128 - u32::from(v6.prefix_len());
                if bits >= 63 {
                    -2
                } else {
                    1_i64 << bits
                }
            }
        };
        assert_eq!(hosts, expected_hosts, "host count for {net}");
        if let Ok(addr) = address.text().parse::<IpAddr>() {
            assert_eq!(contains, i32::from(net.contains(&addr)));
        } else {
            assert_eq!(contains, -1);
        }
    }
}

fuzz_target!(|data: &[u8]| {
    let Ok(text) = std::str::from_utf8(data) else {
        return;
    };
    let (first_text, second_text) = support::two_fields(text);
    let first = Managed::new(first_text);
    let second = Managed::new(second_text);
    url(&first, &second);
    ip(first_text, &first);
    cidr(first_text, &first, &second);
    // SAFETY: `first` is live for the call.
    let valid = unsafe { hew_uuid_parse(first.ptr()) } == 1;
    assert_eq!(valid, uuid::Uuid::parse_str(first_text).is_ok());
});
