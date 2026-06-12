#![no_main]

mod support;

use arbitrary::{Arbitrary, Unstructured};
use libfuzzer_sys::fuzz_target;

#[derive(Debug, Arbitrary)]
struct SupervisorInput {
    intensity: u8,
    window: u8,
    bind_child: bool,
}

impl SupervisorInput {
    fn to_source(&self) -> String {
        let fork = if self.bind_child {
            "        fork child_task = background_task();"
        } else {
            "        fork background_task();"
        };

        format!(
            r#"actor FuzzChild {{
    receive fn ping(n: i64) -> i64 {{
        n
    }}
}}

#[max_heap(8192)]
supervisor FuzzSupervisor {{
    strategy: one_for_one;
    intensity: {} within {}s;

    child worker: FuzzChild;
}}

fn background_task() {{}}

fn fuzz_scope_fork() {{
    scope {{
{fork}
    }};
}}

fn fuzz_supervisor_decls() -> i64 {{
    let app = spawn FuzzSupervisor;
    let worker = app.worker;
    worker.ping(7)
}}
"#,
            (self.intensity % 8) + 1,
            (self.window % 60) + 1
        )
    }
}

fuzz_target!(|data: &[u8]| {
    if let Ok(source) = std::str::from_utf8(data) {
        support::exercise_parse_check_lower(source);
    }
    if let Ok(input) = SupervisorInput::arbitrary(&mut Unstructured::new(data)) {
        support::exercise_parse_check_lower(&input.to_source());
    }
});
