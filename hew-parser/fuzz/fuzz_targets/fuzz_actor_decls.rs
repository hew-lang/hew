#![no_main]

mod support;

use arbitrary::{Arbitrary, Unstructured};
use libfuzzer_sys::fuzz_target;

#[derive(Debug, Arbitrary)]
struct ActorInput {
    delta: i8,
    ask_shape: bool,
    call_syntax: bool,
}

impl ActorInput {
    fn to_source(&self) -> String {
        let lambda = if self.ask_shape {
            format!("actor |n: i64| -> i64 {{ n + {} }}", self.delta)
        } else {
            r#"actor |msg: string| {
        println(msg);
    }"#
            .to_string()
        };
        let dispatch = match (self.ask_shape, self.call_syntax) {
            (true, true) => "    let _reply = worker(5);\n",
            (true, false) => "    worker.send(5);\n",
            (false, true) => "    let _sent = worker(\"hello\");\n",
            (false, false) => "    worker.send(\"hello\");\n",
        };

        format!(
            r#"#[max_heap(4096)]
actor FuzzWorker {{
    let total: i64;

    init() {{
        total = 0;
    }}

    receive fn bump(n: i64) -> i64 {{
        total = total + n;
        total
    }}
}}

fn fuzz_actor_decls() {{
    let worker = {lambda};
{dispatch}}}

fn fuzz_channel_surfaces(
    stream: Stream<i64>,
    sink: Sink<i64>,
    duplex: Duplex<i64, string>,
    local: LocalPid<FuzzWorker>,
    remote: RemotePid<FuzzWorker>,
) {{
    sink.send(1);
    duplex.send(2);
    stream.recv();
    local;
    remote;
}}
"#
        )
    }
}

fuzz_target!(|data: &[u8]| {
    if let Ok(source) = std::str::from_utf8(data) {
        support::exercise_parse_check_lower(source);
    }
    if let Ok(input) = ActorInput::arbitrary(&mut Unstructured::new(data)) {
        support::exercise_parse_check_lower(&input.to_source());
    }
});
