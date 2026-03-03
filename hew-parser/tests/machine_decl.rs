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
