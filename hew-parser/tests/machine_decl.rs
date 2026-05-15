//! Tests for parsing `machine` declarations.

#[test]
fn parse_simple_machine() {
    let source = r"
machine Light {
    state Off;
    state On;

    event Toggle;

    on Toggle: Off -> On {
        On
    }

    on Toggle: On -> Off {
        Off
    }
}

fn main() {
}
";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    assert_eq!(result.program.items.len(), 2);

    if let hew_parser::ast::Item::Machine(m) = &result.program.items[0].0 {
        assert_eq!(m.name, "Light");
        assert_eq!(m.states.len(), 2);
        assert_eq!(m.states[0].name, "Off");
        assert_eq!(m.states[1].name, "On");
        assert_eq!(m.events.len(), 1);
        assert_eq!(m.events[0].name, "Toggle");
        assert_eq!(m.transitions.len(), 2);
        assert_eq!(m.transitions[0].event_name, "Toggle");
        assert_eq!(m.transitions[0].source_state, "Off");
        assert_eq!(m.transitions[0].target_state, "On");
        assert_eq!(m.transitions[1].source_state, "On");
        assert_eq!(m.transitions[1].target_state, "Off");
    } else {
        panic!("expected Machine item, got {:?}", result.program.items[0].0);
    }
}

#[test]
fn parse_machine_with_fields() {
    let source = r"
machine Counter {
    state Idle;
    state Running { count: Int; }

    event Increment;
    event Reset;

    on Increment: Idle -> Running {
        Running { count: 1 }
    }

    on Increment: Running -> Running {
        Running { count: self.count + 1 }
    }

    on Reset: _ -> Idle {
        Idle
    }
}
";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    if let hew_parser::ast::Item::Machine(m) = &result.program.items[0].0 {
        assert_eq!(m.name, "Counter");
        assert_eq!(m.states.len(), 2);
        assert_eq!(m.states[1].name, "Running");
        assert_eq!(m.states[1].fields.len(), 1);
        assert_eq!(m.states[1].fields[0].0, "count");
        assert_eq!(m.transitions.len(), 3);
        // Wildcard source
        assert_eq!(m.transitions[2].source_state, "_");
        assert_eq!(m.transitions[2].target_state, "Idle");
    } else {
        panic!("expected Machine item");
    }
}

#[test]
fn parse_machine_with_event_payload() {
    let source = r"
machine Tcp {
    state Closed;
    state Open { port: Int; }

    event Connect { port: Int; }
    event Disconnect;

    on Connect: Closed -> Open {
        Open { port: port }
    }

    on Disconnect: _ -> Closed {
        Closed
    }
}
";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    if let hew_parser::ast::Item::Machine(m) = &result.program.items[0].0 {
        assert_eq!(m.events[0].name, "Connect");
        assert_eq!(m.events[0].fields.len(), 1);
        assert_eq!(m.events[0].fields[0].0, "port");
    } else {
        panic!("expected Machine item");
    }
}

#[test]
fn parse_machine_wildcard_both() {
    let source = r"
machine Noop {
    state A;
    state B;

    event Ping;

    on Ping: _ -> _ {
        self
    }
}
";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    if let hew_parser::ast::Item::Machine(m) = &result.program.items[0].0 {
        assert_eq!(m.transitions[0].source_state, "_");
        assert_eq!(m.transitions[0].target_state, "_");
    } else {
        panic!("expected Machine item");
    }
}

#[test]
fn parse_state_with_entry_exit_blocks() {
    let source = r"
machine Traffic {
    state Red {
        entry { log(entering_red); }
        exit { log(leaving_red); }
    }

    state Green;

    event Tick;

    on Tick: Red -> Green;
    on Tick: Green -> Red;
}
";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    if let hew_parser::ast::Item::Machine(m) = &result.program.items[0].0 {
        assert_eq!(m.states.len(), 2);
        let red = &m.states[0];
        assert_eq!(red.name, "Red");
        assert!(red.entry.is_some(), "Red should have entry block");
        assert!(red.exit.is_some(), "Red should have exit block");
        let green = &m.states[1];
        assert!(green.entry.is_none(), "Green has no entry block");
        assert!(green.exit.is_none(), "Green has no exit block");
    } else {
        panic!("expected Machine item");
    }
}

#[test]
fn parse_emit_in_transition_body() {
    let source = r"
machine Counter {
    state Idle;
    state Running { count: Int; }

    event Start;
    event Tick;
    event Overflow;

    on Start: Idle -> Running {
        emit Overflow { code: 0 };
        Running { count: 0 }
    }

    on Tick: Running -> Running {
        Running { count: self.count + 1 }
    }
}
";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    if let hew_parser::ast::Item::Machine(m) = &result.program.items[0].0 {
        assert_eq!(m.name, "Counter");
        assert_eq!(m.transitions.len(), 2);
    } else {
        panic!("expected Machine item");
    }
}

#[test]
fn reject_nested_state_in_state_body() {
    let source = r"
machine Bad {
    state Outer {
        state Inner;
    }

    event Ping;
}
";
    let result = hew_parser::parse(source);
    // Must have at least one error about nested state
    assert!(
        !result.errors.is_empty(),
        "expected parse error for nested state, got none"
    );
    let has_nested_state_error = result
        .errors
        .iter()
        .any(|e| format!("{e:?}").contains("hierarchical"));
    assert!(
        has_nested_state_error,
        "expected hierarchical-states diagnostic, errors: {:?}",
        result.errors
    );
}
