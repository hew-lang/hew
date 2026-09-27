#![no_main]

mod support;

use arbitrary::{Arbitrary, Unstructured};
use libfuzzer_sys::fuzz_target;

#[derive(Debug, Arbitrary)]
struct MachineInput {
    active_value: u8,
    padded: bool,
    drive_calls: bool,
}

impl MachineInput {
    fn to_source(&self) -> String {
        let extra_state = if self.padded {
            "    state Active { tag_byte: i8; value: i64; }\n"
        } else {
            "    state Active { value: i64; }\n"
        };
        let active_ctor = if self.padded {
            format!("Active {{ tag_byte: {}, value: {} }}", self.active_value % 4, self.active_value)
        } else {
            format!("Active {{ value: {} }}", self.active_value)
        };
        let read_active = if self.padded {
            "{ tag_byte: state.tag_byte, value: state.value + 1 }"
        } else {
            "{ value: state.value + 1 }"
        };
        let calls = if self.drive_calls {
            r#"
fn drive_machine() {
    var m: FuzzMachine = .Idle;
    let _before = m.state_name();
    m.step(.Activate);
    m.step(.Bump);
    m.step(.Deactivate);
    let _after = m.state_name();
}
"#
        } else {
            ""
        };

        format!(
            r#"machine FuzzMachine {{
    events {{
        Activate;
        Bump;
        Deactivate;
    }}

    state Idle;
{extra_state}
    on Activate: Idle => {active_ctor}
    on Bump: Active => Active reenter {read_active}
    on Deactivate: Active => Idle;
    on Deactivate: Idle => Idle reenter;
    default {{ state }}
}}
{calls}
"#
        )
    }
}

fuzz_target!(|data: &[u8]| {
    if let Ok(source) = std::str::from_utf8(data) {
        support::exercise_parse_check_lower(source);
    }
    if let Ok(input) = MachineInput::arbitrary(&mut Unstructured::new(data)) {
        support::exercise_parse_check_lower(&input.to_source());
    }
});
