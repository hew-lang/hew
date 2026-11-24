//! Differential oracle test comparing pure-Hew `std.net.ipnet` against Rust's
//! `std::net::IpAddr` and `ipnet::IpNet`.

use std::fmt::Write as _;
use std::fs;
use std::net::IpAddr;

use ipnet::IpNet;

mod support;

use support::{hew_binary, repo_root, require_codegen, run_bounded_command, tempdir};

const IP_CASES: &[&str] = &[
    // Empty, short, and malformed basics
    "",
    "a",
    ":",
    ":::",
    ".",
    "...",
    "1",
    "1.2.3",
    "1.2.3.4.5",
    "1.2.3.",
    ".1.2.3",
    "1..2.3",
    " 192.168.1.1",
    "192.168.1.1 ",
    // Valid IPv4
    "0.0.0.0",
    "10.0.0.1",
    "10.255.255.255",
    "11.0.0.1",
    "127.0.0.1",
    "127.255.255.254",
    "128.0.0.1",
    "172.15.255.255",
    "172.16.0.1",
    "172.31.255.254",
    "172.32.0.0",
    "192.168.0.1",
    "192.168.1.1",
    "192.169.1.1",
    "255.255.255.255",
    // IPv4 leading-zero and overflow rejections (RFC 6943 / core::net)
    "00.0.0.0",
    "01.0.0.0",
    "192.168.01.1",
    "192.168.1.001",
    "256.0.0.1",
    "1920.0.0.1",
    "192.168.1.-1",
    "192.168.1.+1",
    // IPv6 compression (`::`) edge cases
    "::",
    "::0",
    "::1",
    "1::",
    "0000::0001",
    "2001:db8::1",
    "2001:0db8::0001",
    "2001:db8:0:0:1:0:0:1",
    "fe80:0000:0000:0000:0202:b3ff:fe1e:8329",
    "ffff:ffff:ffff:ffff:ffff:ffff:ffff:ffff",
    "fc00::1",
    "fd00::1",
    "::2",
    "0:0:0:0:0:0:0:1",
    "1:2:3:4:5:6:7::",
    "::1:2:3:4:5:6:7",
    // IPv6 embedded IPv4 and `::ffff:...`
    "::ffff:192.168.1.1",
    "::ffff:127.0.0.1",
    "::ffff:0.0.0.0",
    "::ffff:255.255.255.255",
    "::ffff:c0a8:0101",
    "::192.0.2.128",
    "1:2:3:4:5:6:1.2.3.4",
    "1:2:3:4:5::1.2.3.4",
    "ffff:ffff:ffff:ffff:ffff:ffff:255.255.255.255",
    // Invalid IPv6 compression, trailing colons, zone IDs, overlong, UTF-8
    ":::1",
    "1:::",
    "1::2::3",
    "1:2:3:4:5:6:7:8::",
    "::1:2:3:4:5:6:7:8",
    "1:2:3:4:5:6:7::8",
    "1:2:3:4::5:6:7:8",
    "1:2:3:4:5:6::1.2.3.4",
    "::ffff:192.168.01.1",
    "::ffff:256.0.0.1",
    "192.168.1.1::1",
    "2001:db8::12345",
    "2001:db8::g",
    "1:2:3:4:5:6:7:8:9",
    "::1:",
    "1:2:3:4:5:6:7:",
    "fe80::1%eth0",
    "fe80::1%1",
    "ffff:ffff:ffff:ffff:ffff:ffff:255.255.255.2551",
    "192.168.1.1                                        ",
    "ééééééééééééééééééééééééééééééééééééééééééééé",
];

const CIDR_CASES: &[(&str, &str)] = &[
    ("10.0.0.0/8", "10.1.2.3"),
    ("10.0.0.0/8", "11.0.0.1"),
    ("10.0.0.0/8", "::1"),
    ("0.0.0.0/0", "8.8.8.8"),
    ("0.0.0.0/0", "0.0.0.0"),
    ("0.0.0.0/0", "255.255.255.255"),
    ("192.168.0.0/31", "192.168.0.1"),
    ("192.168.0.1/32", "192.168.0.1"),
    ("192.168.0.1/32", "192.168.0.2"),
    ("192.0.2.129/25", "192.0.2.254"),
    ("192.0.2.129/25", "192.0.2.0"),
    // `ipnet` crate quirks: leading zeros in octets and prefixes
    ("192.168.01.5/24", "192.168.1.99"),
    ("10.0.0.0/08", "10.9.9.9"),
    ("010.000.000.001/00", "1.2.3.4"),
    ("10.0.0.0/008", "10.1.1.1"),
    ("00010.0.0.0/8", "10.1.1.1"),
    // IPv6 CIDRs, RFC 5952 formatting, tie-breaking, single zeros, mapped v4
    ("::/0", "::1"),
    ("::/0", "10.0.0.1"),
    ("::/64", "::1"),
    ("::/65", "::1"),
    ("::/66", "::1"),
    ("::/128", "::"),
    ("2001:db8::/65", "2001:db8::1"),
    ("2001:db8:abcd:0012:ffff::1/64", "2001:db8:abcd:12::5"),
    ("2001:db8:1:0:2:0:0:1/112", "2001:db8:1:0:2::5"),
    ("0:0:1:2:0:0:3:4/128", "0:0:1:2::3:4"),
    ("1:0:2:3:4:5:6:7/128", "1:0:2:3:4:5:6:7"),
    ("::ffff:192.0.2.128/120", "::ffff:192.0.2.200"),
    ("::ffff:192.168.01.5/120", "::ffff:192.168.1.9"),
    ("::2/128", "::2"),
    (
        "2001:0db8:0000:0000:0000:ff00:0042:8329/128",
        "2001:db8::ff00:42:8329",
    ),
    ("2001:db8:8000::/33", "2001:db8:ffff::1"),
    ("2001:db8:8000::/33", "2001:db8:7fff::1"),
    ("2001:db8::/064", "2001:db8::1"),
    ("2001:db8::/008", "2000::1"),
    (
        "ffff:ffff:ffff:ffff:ffff:ffff:255.255.255.255/128",
        "ffff:ffff:ffff:ffff:ffff:ffff:255.255.255.255",
    ),
    // IPv6 CIDR `::` quirks vs non-CIDR
    ("1:2:3:4:5:6:7::8/64", "1:2:3:4::1"),
    ("1:2:3:4::5:6:7:8/64", "1:2:3:4::1"),
    ("::1:2:3:4:5:6:7:8/64", "0:1:2:3::1"),
    ("1:2:3:4:5:6::1.2.3.4/64", "1:2:3:4::1"),
    ("1:2:3:4:5::1.2.3.4/112", "1:2:3:4:5::1.2.0.9"),
    // Invalid CIDRs and invalid queries
    ("", "10.0.0.1"),
    ("::/", "::1"),
    ("10.0.0.0/", "10.0.0.1"),
    ("10.0.0.0", "10.0.0.1"),
    ("10.0.0.0/33", "10.0.0.1"),
    ("::/129", "::1"),
    ("10.0.0.0/24a", "10.0.0.1"),
    ("2001:db8::/64x", "2001:db8::1"),
    ("10.0.0.0/8", "fe80::1%eth0"),
    ("10.0.0.0/8", "192.168.01.1"),
    ("ffff:ffff:ffff:ffff:ffff:ffff:255.255.255.255/0128", "::1"),
    (
        "10.0.0.0/8                                        ",
        "10.0.0.1",
    ),
    (
        "ééééééééééééééééééééééééééééééééééééééééééééééééé/24",
        "10.0.0.1",
    ),
];

fn hew_string_literal(value: &str) -> String {
    let mut escaped = String::with_capacity(value.len() + 2);
    escaped.push('"');
    for ch in value.chars() {
        match ch {
            '\\' => escaped.push_str("\\\\"),
            '"' => escaped.push_str("\\\""),
            '\n' => escaped.push_str("\\n"),
            '\r' => escaped.push_str("\\r"),
            '\t' => escaped.push_str("\\t"),
            _ => escaped.push(ch),
        }
    }
    escaped.push('"');
    escaped
}

fn deterministic_ip_and_cidr_cases() -> (Vec<String>, Vec<(String, String)>) {
    let mut state: u64 = 0x5eed_cafe_beef_1234;
    let mut next_u64 = || {
        state ^= state << 13;
        state ^= state >> 7;
        state ^= state << 17;
        state
    };

    let mut ips: Vec<String> = IP_CASES.iter().map(|s| (*s).to_owned()).collect();
    let mut cidrs: Vec<(String, String)> = CIDR_CASES
        .iter()
        .map(|(c, a)| ((*c).to_owned(), (*a).to_owned()))
        .collect();

    for _ in 0..48 {
        let octets = [
            (next_u64() % 270) as u16,
            (next_u64() % 270) as u16,
            (next_u64() % 270) as u16,
            (next_u64() % 270) as u16,
        ];
        let v4 = format!("{}.{}.{}.{}", octets[0], octets[1], octets[2], octets[3]);
        let prefix = next_u64() % 36;
        ips.push(v4.clone());
        cidrs.push((format!("{v4}/{prefix}"), v4));

        let mut segs = Vec::with_capacity(8);
        for _ in 0..8 {
            segs.push(format!("{:x}", next_u64() % 0x10000));
        }
        let drop_start = (next_u64() % 8) as usize;
        let drop_len = ((next_u64() % 4) as usize).min(8 - drop_start);
        let v6 = if drop_len >= 2 {
            let head = segs[..drop_start].join(":");
            let tail = segs[drop_start + drop_len..].join(":");
            format!("{head}::{tail}")
        } else {
            segs.join(":")
        };
        let v6_prefix = next_u64() % 134;
        ips.push(v6.clone());
        cidrs.push((format!("{v6}/{v6_prefix}"), v6));
    }

    (ips, cidrs)
}

fn oracle_ip_line(input: &str) -> String {
    let parsed = input.parse::<IpAddr>().ok();
    let valid = parsed.is_some();
    let is_v4 = parsed.is_some_and(|addr| addr.is_ipv4());
    let is_v6 = parsed.is_some_and(|addr| addr.is_ipv6());
    let loopback = parsed.is_some_and(|addr| addr.is_loopback());
    let private = matches!(parsed, Some(IpAddr::V4(v4)) if v4.is_private());
    format!("IP|{valid}|{is_v4}|{is_v6}|{loopback}|{private}")
}

fn oracle_cidr_line(cidr: &str, ip: &str) -> String {
    let net = cidr.parse::<IpNet>().ok();
    let addr = ip.parse::<IpAddr>().ok();
    let network = match &net {
        Some(net) => format!("Ok({})", net.network()),
        None => "Err".to_owned(),
    };
    let broadcast = match &net {
        Some(IpNet::V4(v4)) => format!("Some({})", v4.broadcast()),
        Some(IpNet::V6(_)) => "None".to_owned(),
        None => "Err".to_owned(),
    };
    let hosts = match &net {
        Some(IpNet::V4(v4)) => {
            let all = 1_i64 << (32 - v4.prefix_len());
            let count = if v4.prefix_len() < 31 { all - 2 } else { all };
            format!("Ok({count})")
        }
        Some(IpNet::V6(v6)) => {
            let host_bits = 128 - u32::from(v6.prefix_len());
            if host_bits >= 63 {
                "Overflow".to_owned()
            } else {
                format!("Ok({})", 1_i64 << host_bits)
            }
        }
        None => "Err".to_owned(),
    };
    let contains = match (&net, addr) {
        (Some(net), Some(addr)) => format!("Ok({})", net.contains(&addr)),
        _ => "Err".to_owned(),
    };
    format!("CIDR|{network}|{broadcast}|{hosts}|{contains}")
}

#[test]
fn pure_hew_ipnet_matches_rust_ipaddr_and_ipnet_oracles() {
    require_codegen();

    let (ips, cidrs) = deterministic_ip_and_cidr_cases();
    let mut source = String::from(
        r#"import std.net.ipnet;

fn check_ip(addr: string) {
    let valid = ipnet.is_valid(addr);
    let v4 = ipnet.is_v4(addr);
    let v6 = ipnet.is_v6(addr);
    let lo = ipnet.is_loopback(addr);
    let priv_ = ipnet.is_private(addr);
    println(f"IP|{valid}|{v4}|{v6}|{lo}|{priv_}");
}

fn check_cidr(cidr: string, addr: string) {
    let network = match ipnet.cidr_network(cidr) {
        .Ok(value) => f"Ok({value})",
        .Err(_) => "Err",
    };
    let broadcast = match ipnet.cidr_broadcast(cidr) {
        .Ok(.Some(value)) => f"Some({value})",
        .Ok(.None) => "None",
        .Err(_) => "Err",
    };
    let hosts = match ipnet.cidr_hosts(cidr) {
        .Ok(count) => f"Ok({count})",
        .Err(.HostCountOverflow) => "Overflow",
        .Err(_) => "Err",
    };
    let contains = match ipnet.cidr_contains(cidr, addr) {
        .Ok(found) => f"Ok({found})",
        .Err(_) => "Err",
    };
    println(f"CIDR|{network}|{broadcast}|{hosts}|{contains}");
}

fn main() {
"#,
    );

    let mut expected_lines = Vec::with_capacity(ips.len() + cidrs.len());
    for ip in &ips {
        let lit = hew_string_literal(ip);
        let _ = writeln!(source, "    check_ip({lit});");
        expected_lines.push((format!("ip={ip:?}"), oracle_ip_line(ip)));
    }
    for (cidr, ip) in &cidrs {
        let cidr_lit = hew_string_literal(cidr);
        let ip_lit = hew_string_literal(ip);
        let _ = writeln!(source, "    check_cidr({cidr_lit}, {ip_lit});");
        expected_lines.push((
            format!("cidr={cidr:?} ip={ip:?}"),
            oracle_cidr_line(cidr, ip),
        ));
    }
    source.push_str("}\n");

    let dir = tempdir();
    let source_path = dir.path().join("ipnet_oracle.hew");
    let binary_path = dir.path().join(if cfg!(windows) {
        "ipnet_oracle.exe"
    } else {
        "ipnet_oracle"
    });
    fs::write(&source_path, source).expect("write ipnet oracle fixture");

    let mut build_cmd = std::process::Command::new(hew_binary());
    build_cmd
        .args(["build"])
        .arg(&source_path)
        .arg("-o")
        .arg(&binary_path)
        .current_dir(repo_root());
    let build = run_bounded_command(build_cmd, "build ipnet oracle fixture");
    assert!(
        build.status.success(),
        "hew build failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&build.stdout),
        String::from_utf8_lossy(&build.stderr)
    );

    let run = run_bounded_command(
        std::process::Command::new(&binary_path),
        "run ipnet oracle fixture",
    );
    assert!(
        run.status.success(),
        "oracle fixture failed:\nstdout:\n{}\nstderr:\n{}",
        String::from_utf8_lossy(&run.stdout),
        String::from_utf8_lossy(&run.stderr)
    );

    let stdout = String::from_utf8_lossy(&run.stdout);
    let actual_lines: Vec<&str> = stdout.lines().collect();
    assert_eq!(
        actual_lines.len(),
        expected_lines.len(),
        "oracle output line count mismatch"
    );

    for (index, ((label, expected), actual)) in
        expected_lines.iter().zip(actual_lines.iter()).enumerate()
    {
        assert_eq!(
            actual, expected,
            "oracle mismatch at case #{index} ({label})"
        );
    }
}
