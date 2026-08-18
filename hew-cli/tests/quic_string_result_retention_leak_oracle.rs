//! Darwin leak and poisoned-allocator proof for shipped QUIC observations.
//!
//! One real loopback endpoint/connection/stream lifecycle holds transport
//! resources constant while low/high probes vary only the number of calls to
//! the shipped `.hew` observation wrappers. The endpoint and stream errors are
//! induced before successful operations clear them; the connection error is
//! induced after the peer disconnects. Every iteration validates producer
//! state and prints one exact witness line.

#![cfg(unix)]

mod support;

use support::leak_slope::{
    assert_frame_slope_below_tolerance_exact_lines, compile_to_native, run_under_malloc_scribble,
    HIGH_FRAMES,
};
use support::{describe_output, require_codegen};

#[expect(
    clippy::too_many_lines,
    reason = "the source is one ordered cross-actor QUIC lifecycle with exact cleanup and witnesses"
)]
fn source(frames: usize) -> String {
    format!(
        r#"
import std.net;
import std.net.quic;

fn require_ok(result: Result<(), net.NetError>) {{
    match result {{
        Ok(_) => {{}},
        Err(_) => panic("QUIC retention operation failed"),
    }}
}}

actor Client {{
    let address: string;
    var complete: i64;

    receive fn run(unused: i64) {{
        let endpoint = quic.new_client();
        let connection = endpoint.connect(address, "localhost");
        let stream = connection.open_stream();
        require_ok(stream.send_string("client-probe"));
        let reply = stream.recv_string();
        if reply != "retention-probe" {{
            panic("QUIC retention reply mismatch");
        }}
        let eof = stream.recv();
        if eof.len() != 0 {{
            panic("QUIC retention expected EOF");
        }}
        stream.close();
        require_ok(connection.disconnect());
        endpoint.close();
        complete = 1;
    }}

    receive fn finished() -> i64 {{
        complete
    }}
}}

fn main() {{
    let endpoint = quic.new_server("127.0.0.1:0");

    // A failed connect deterministically populates endpoint.last_error without
    // consuming the live server endpoint.
    let _failed_connection = endpoint.connect("not-an-address", "localhost");
    for _ in 0..{frames} {{
        let observation = endpoint.observe();
        if !observation.local_addr.contains(":")
            || !observation.last_error.contains("parse failure") {{
            panic("QUIC endpoint observation lost state");
        }}
        println(101);
    }}

    let address = endpoint.observe().local_addr;
    let client = spawn Client(address: address, complete: 0);
    client.run(0);

    let connection = endpoint.accept();
    let endpoint_after_accept = endpoint.observe();
    if endpoint_after_accept.last_error.len() != 0 {{
        panic("successful QUIC accept did not clear endpoint error");
    }}
    let stream = connection.accept_stream();
    let opened = connection.on_event();
    if opened.kind() != 2 {{
        panic("QUIC stream-open event missing");
    }}
    opened.close();
    let client_probe = stream.recv_string();
    if client_probe != "client-probe" {{
        panic("QUIC retention client probe mismatch");
    }}

    match stream.stop(-7) {{
        Ok(_) => panic("invalid QUIC application code unexpectedly succeeded"),
        Err(_) => {{}},
    }}
    let errored = connection.on_event();
    if errored.kind() != -1 {{
        panic("QUIC stream-error event missing");
    }}
    errored.close();
    for _ in 0..{frames} {{
        let observation = stream.observe();
        if observation.last_error
            != "invalid QUIC application error code -7: expected 0..=2^62-1" {{
            panic("QUIC stream observation lost error state");
        }}
        println(202);
    }}

    require_ok(stream.send_string("retention-probe"));
    let stream_after_send = stream.observe();
    if stream_after_send.last_error.len() != 0 {{
        panic("successful QUIC send did not clear stream error");
    }}
    require_ok(stream.finish());
    stream.close();

    // The peer closes its stream then disconnects. Both events must be drained
    // before inducing the connection-scoped error.
    let event1 = connection.on_event();
    let kind1 = event1.kind();
    event1.close();
    let event2 = connection.on_event();
    let kind2 = event2.kind();
    event2.close();
    if !((kind1 == 3 || kind2 == 3) && (kind1 == 1 || kind2 == 1)) {{
        panic("QUIC close/disconnect events missing");
    }}

    let _failed_stream = connection.open_stream();
    for _ in 0..{frames} {{
        let observation = connection.observe();
        if !observation.local_addr.contains(":")
            || !observation.peer_addr.contains(":")
            || observation.last_error.len() == 0 {{
            panic("QUIC connection observation lost state");
        }}
        println(303);
    }}

    require_ok(connection.disconnect());
    endpoint.close();
    match await client.finished() {{
        Ok(done) => {{
            if done != 1 {{
                panic("QUIC retention client stopped early");
            }}
        }},
        Err(_) => panic("QUIC retention client failed"),
    }}
}}
"#
    )
}

fn expected_output(frames: usize) -> String {
    [
        "101\n".repeat(frames),
        "202\n".repeat(frames),
        "303\n".repeat(frames),
    ]
    .concat()
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "leak oracle needs macOS `leaks(1)` / the Darwin poisoned allocator; a host that cannot run it must record a SKIP, never a silent pass"
)]
#[test]
fn shipped_quic_observation_strings_have_no_per_call_leak() {
    assert_frame_slope_below_tolerance_exact_lines("quic_observation_strings", source, |frames| {
        frames * 3
    });
}

#[cfg_attr(
    not(target_os = "macos"),
    ignore = "the deterministic poisoned-allocator contract is macOS-only"
)]
#[test]
fn shipped_quic_observation_strings_are_exact_under_malloc_scribble() {
    require_codegen();
    let dir = tempfile::Builder::new()
        .prefix("quic-observation-string-scribble-")
        .tempdir()
        .expect("tempdir");
    let bin = compile_to_native(
        &source(HIGH_FRAMES),
        dir.path(),
        "quic_observation_string_scribble",
    );
    let output = run_under_malloc_scribble(&bin);
    assert!(
        output.status.success(),
        "QUIC observations must survive exact caller releases:\n{}",
        describe_output(&output)
    );
    assert_eq!(
        String::from_utf8_lossy(&output.stdout),
        expected_output(HIGH_FRAMES),
        "poisoned run must preserve every observation value:\n{}",
        describe_output(&output)
    );
}
