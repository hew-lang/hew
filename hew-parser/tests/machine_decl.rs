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
fn parse_machine_transition_with_reenter_annotation() {
    let source = r"
machine Counter {
    state Active { n: Int; }

    event Inc;

    on Inc: Active -> Active @reenter {
        Active { n: self.n + 1 }
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
        assert_eq!(m.transitions.len(), 1);
        assert!(
            m.transitions[0].reenter,
            "expected transition annotation to set reenter=true"
        );
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

#[test]
fn extern_fn_named_entry_exit_emit_parses() {
    // `entry`, `exit`, and `emit` are contextual keywords added for machine
    // declarations.  They must remain valid as function names in `extern` blocks
    // so existing FFI declarations are not broken.
    let source = r#"
extern "C" {
    fn entry(code: i32) -> i32;
    fn exit(code: i32) -> i32;
    fn emit(code: i32) -> i32;
}

fn main() {}
"#;
    let result = hew_parser::parse(source);
    let errors = &result.errors;
    assert!(
        result.errors.is_empty(),
        "contextual keywords entry/exit/emit must be usable as extern fn names, got: {errors:?}"
    );
    // Confirm the extern block has exactly 3 functions.
    let extern_block = result.program.items.iter().find_map(|item| {
        if let hew_parser::ast::Item::ExternBlock(b) = &item.0 {
            Some(b)
        } else {
            None
        }
    });
    let extern_block = extern_block.expect("expected ExternBlock item");
    assert_eq!(extern_block.functions.len(), 3);
    let names: Vec<&str> = extern_block
        .functions
        .iter()
        .map(|f| f.name.as_str())
        .collect();
    assert!(
        names.contains(&"entry") && names.contains(&"exit") && names.contains(&"emit"),
        "expected entry, exit, emit functions in extern block, got: {names:?}"
    );
}

#[test]
fn machine_generic_decl() {
    // `machine Name<T, U> { ... }` parses with the type-param names
    // threaded onto `MachineDecl::type_params`. No bounds, no defaults,
    // no machine-over-machine generics — those are not supported in v0.5.
    let source = r"
machine Light {
    state Off;
    state On;

    event Toggle;

    on Toggle: Off -> On { On }
    on Toggle: On -> Off { Off }
}

machine Lifecycle<T> {
    state Created;
    state Running;

    event Start;

    on Start: Created -> Running {
        Running
    }
}

machine Triple<T, U, V> {
    state Empty;
    state Filled;

    event Insert;

    on Insert: Empty -> Filled {
        Filled
    }
}

machine Collision<T> {
    state T;
}
";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );
    assert_eq!(result.program.items.len(), 4);

    let machines: Vec<&hew_parser::ast::MachineDecl> = result
        .program
        .items
        .iter()
        .map(|(item, _)| {
            let hew_parser::ast::Item::Machine(machine) = item else {
                panic!("expected Machine item, got {item:?}");
            };
            machine
        })
        .collect();

    assert_eq!(machines[0].name, "Light");
    assert!(machines[0].type_params.is_empty());

    assert_eq!(machines[1].name, "Lifecycle");
    assert_eq!(machines[1].type_params, vec!["T".to_string()]);
    assert_eq!(machines[1].states.len(), 2);
    assert_eq!(machines[1].events.len(), 1);
    assert_eq!(machines[1].transitions.len(), 1);

    assert_eq!(machines[2].name, "Triple");
    assert_eq!(
        machines[2].type_params,
        vec!["T".to_string(), "U".to_string(), "V".to_string()]
    );

    // Parser accepts a type-param/state-name collision because name binding is
    // not a parser concern; later semantic passes may reject it if needed.
    assert_eq!(machines[3].name, "Collision");
    assert_eq!(machines[3].type_params, vec!["T".to_string()]);
    assert_eq!(machines[3].states[0].name, "T");
}

#[test]
fn parse_machine_generic_decl_empty_angles_is_rejected() {
    // `machine Name<> { ... }` is meaningless — the author either forgot
    // to name the parameter or copied the wrong syntax. Fail closed.
    let source = r"
machine Empty<> {
    state Off;
}
";
    let result = hew_parser::parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected parse error for empty `<>`"
    );
}

#[test]
fn parse_machine_generic_decl_trait_bounds_are_rejected() {
    // Trait bounds on machine type parameters are not supported in v0.5;
    // they are a v0.6+ ratification. Reject with a clear diagnostic so
    // users do not silently lose the bound.
    let source = r"
machine Bounded<T: Display> {
    state Idle;
}
";
    let result = hew_parser::parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected parse error for `<T: Display>`"
    );
    let has_bound_msg = result
        .errors
        .iter()
        .any(|e| format!("{e:?}").contains("trait bounds on machine type parameters"));
    assert!(
        has_bound_msg,
        "expected trait-bound rejection diagnostic, got: {:?}",
        result.errors
    );
}
