//! Tests for parsing `machine` declarations.

#[test]
fn parse_simple_machine() {
    let source = r"
machine Light {
    events {
        Toggle;
    }

    state Off;
    state On;

    on Toggle: Off => On {
        On
    }

    on Toggle: On => Off {
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
    events {
        Increment;
        Reset;
    }

    state Idle;
    state Running { count: Int; }

    on Increment: Idle => Running {
        Running { count: 1 }
    }

    on Increment: Running => Running {
        Running { count: self.count + 1 }
    }

    on Reset: _ => Idle {
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
    events {
        Connect { port: Int; }
        Disconnect;
    }

    state Closed;
    state Open { port: Int; }

    on Connect: Closed => Open {
        Open { port: port }
    }

    on Disconnect: _ => Closed {
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
fn parse_machine_event_head_binding() {
    // `on E(field): …` head binding names the event payload at the rule site.
    // It lowers identically to `event.field`; the parser splices a `let`
    // prelude in front of the body, so the body just sees the bare name.
    let source = r"
machine Tcp {
    events {
        Connect { port: Int; }
    }

    state Closed;
    state Open { port: Int; }

    on Connect(port): Closed => Open {
        Open { port: port }
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
        // The binding names are recorded on the transition for the formatter.
        assert_eq!(m.transitions[0].event_bindings, vec!["port".to_string()]);
        // The head binding makes the body a block whose first statement binds
        // `port` from `event.port`.
        let hew_parser::ast::Expr::Block(block) = &m.transitions[0].body.0 else {
            panic!("expected head-binding body to be a block");
        };
        assert!(
            !block.stmts.is_empty(),
            "head binding should splice a `let port = event.port;` prelude"
        );
        let hew_parser::ast::Stmt::Let { pattern, value, .. } = &block.stmts[0].0 else {
            panic!("expected first prelude stmt to be a let binding");
        };
        assert!(
            matches!(&pattern.0, hew_parser::ast::Pattern::Identifier(n) if n == "port"),
            "expected `let port = …`"
        );
        let Some((hew_parser::ast::Expr::FieldAccess { field, .. }, _)) = value.as_ref() else {
            panic!("expected let value to be `event.port`");
        };
        assert_eq!(field, "port");
    } else {
        panic!("expected Machine item");
    }
}

#[test]
fn machine_event_head_binding_round_trips_through_formatter() {
    // The `on E(field): …` head form must survive parse → format → parse: the
    // formatter re-emits the head binding and strips the synthesized
    // `let field = event.field;` prelude, so the formatted source is the head
    // form again (not the desugar).
    let source = r"machine Tcp {
    events {
        Connect { port: Int; }
    }

    state Closed;
    state Open { port: Int; }

    on Connect(port): Closed => Open { Open { port: port } }
}
";
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );

    let formatted = hew_parser::fmt::format_program(&parsed.program);
    assert!(
        formatted.contains("on Connect(port): Closed => Open"),
        "formatter must re-emit the head binding; got:\n{formatted}"
    );
    assert!(
        !formatted.contains("let port = event.port"),
        "formatter must strip the synthesized head-binding prelude; got:\n{formatted}"
    );

    let reparsed = hew_parser::parse(&formatted);
    assert!(
        reparsed.errors.is_empty(),
        "reparse errors: {:?}\n---\n{}",
        reparsed.errors,
        formatted
    );
    assert!(
        hew_parser::ast_eq::program_eq_ignoring_spans(&parsed.program, &reparsed.program),
        "AST structural inequality after round-trip\n--- formatted ---\n{formatted}"
    );
}

#[test]
fn parse_machine_wildcard_both() {
    let source = r"
machine Noop {
    events {
        Ping;
    }

    state A;
    state B;

    on Ping: _ => _ {
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
    events {
        Tick;
    }

    state Red {
        entry { log(entering_red); }
        exit { log(leaving_red); }
    }

    state Green;

    on Tick: Red => Green;
    on Tick: Green => Red;
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
    events {
        Start;
        Tick;
        Overflow;
    }

    state Idle;
    state Running { count: Int; }

    on Start: Idle => Running {
        emit Overflow { code: 0 };
        Running { count: 0 }
    }

    on Tick: Running => Running {
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
fn parse_machine_emits_manifest() {
    // `emits { … }` manifest parses into `MachineDecl::emits`.
    let source = r"
machine Signal {
    events {
        Start;
        Ready;
    }

    emits {
        Ready;
    }

    state Idle;
    state Active;

    on Start: Idle => Active {
        emit Ready {};
        Active
    }
    on Ready: Idle => Idle reenter { Idle }
    on Ready: Active => Active reenter { Active }
    on Start: Active => Active reenter { Active }
}
";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    if let hew_parser::ast::Item::Machine(m) = &result.program.items[0].0 {
        assert_eq!(m.emits, vec!["Ready".to_string()]);
    } else {
        panic!("expected Machine item");
    }
}

#[test]
fn parse_machine_transition_with_reenter_keyword() {
    let source = r"
machine Counter {
    events {
        Inc;
    }

    state Active { n: Int; }

    on Inc: Active => Active reenter {
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
            "expected `reenter` keyword to set reenter=true"
        );
    } else {
        panic!("expected Machine item");
    }
}

#[test]
fn reject_interleaved_event_declaration() {
    // The legacy interleaved `event Name;` form is a hard cutover — events now
    // live in the `events { … }` header.
    let source = r"
machine Bad {
    state Off;
    event Toggle;
}
";
    let result = hew_parser::parse(source);
    assert!(
        !result.errors.is_empty(),
        "expected parse error for interleaved event decl"
    );
    let has_event_header_error = result
        .errors
        .iter()
        .any(|e| format!("{e:?}").contains("interleaved `event` declarations are no longer"));
    assert!(
        has_event_header_error,
        "expected events-header diagnostic, errors: {:?}",
        result.errors
    );
}

#[test]
fn composite_parent_rule_expands_to_concrete_source_transitions() {
    // The load-bearing constraint: a parent-level `on E: _ => T` inside a
    // composite expands to ONE transition per member with a CONCRETE source
    // state, never a literal `_` source (which the checker rejects for
    // `self.field` reads). The composite name is dropped from the flat lists.
    let source = r"
machine Conn {
    events {
        Disconnect;
        Connect;
    }

    state Disconnected;

    state Connected {
        initial state Authenticating;
        state Active;
        state Draining;

        on Disconnect: _ => Disconnected;
    }

    on Connect: Disconnected => Authenticating { Authenticating }
    on Connect: _ => _ { state }
    on Disconnect: _ => _ { state }
}
";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "parse errors: {:?}",
        result.errors
    );

    let hew_parser::ast::Item::Machine(m) = &result.program.items[0].0 else {
        panic!("expected Machine item");
    };

    // Composite name is NOT a flat state; the three substates + Disconnected are.
    let state_names: Vec<&str> = m.states.iter().map(|s| s.name.as_str()).collect();
    assert!(
        !state_names.contains(&"Connected"),
        "composite name must be dropped from flat states; got {state_names:?}"
    );
    for sub in ["Authenticating", "Active", "Draining", "Disconnected"] {
        assert!(state_names.contains(&sub), "missing flat state `{sub}`");
    }

    // The parent `Disconnect` rule expanded to three concrete-source
    // transitions — one per member — with NO literal `_` source.
    let disconnect_sources: Vec<&str> = m
        .transitions
        .iter()
        .filter(|t| t.event_name == "Disconnect" && t.target_state == "Disconnected")
        .map(|t| t.source_state.as_str())
        .collect();
    let mut sorted = disconnect_sources.clone();
    sorted.sort_unstable();
    assert_eq!(
        sorted,
        vec!["Active", "Authenticating", "Draining"],
        "parent rule must expand to concrete member sources, never `_`"
    );
    assert!(
        !disconnect_sources.contains(&"_"),
        "expanded parent rule must NOT carry a literal `_` source"
    );

    // The grouping side-table records the composite for the formatter/diagram.
    assert_eq!(m.composite_groups.len(), 1);
    let group = &m.composite_groups[0];
    assert_eq!(group.name, "Connected");
    assert_eq!(group.initial, "Authenticating");
    assert_eq!(group.members, vec!["Authenticating", "Active", "Draining"]);
}

#[test]
fn composite_block_round_trips_through_formatter() {
    // A depth-1 composite must survive parse → format → parse: the formatter
    // reconstructs the `state Composite { initial state …; on E: _ => T; }`
    // block from the grouping side-table.
    let source = r"machine Conn {
    events {
        Disconnect;
        Connect;
    }

    state Disconnected;

    state Connected {
        initial state Authenticating;
        state Active;

        on Disconnect: _ => Disconnected;
    }

    on Connect: Disconnected => Authenticating { Authenticating }
    on Connect: _ => _ { state }
    on Disconnect: _ => _ { state }
}
";
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "parse errors: {:?}",
        parsed.errors
    );

    let formatted = hew_parser::fmt::format_program(&parsed.program);
    assert!(
        formatted.contains("state Connected {"),
        "formatter must re-emit the composite block; got:\n{formatted}"
    );
    assert!(
        formatted.contains("initial state Authenticating"),
        "formatter must mark the initial substate; got:\n{formatted}"
    );

    let reparsed = hew_parser::parse(&formatted);
    assert!(
        reparsed.errors.is_empty(),
        "reparse errors: {:?}\n---\n{}",
        reparsed.errors,
        formatted
    );
    assert!(
        hew_parser::ast_eq::program_eq_ignoring_spans(&parsed.program, &reparsed.program),
        "AST structural inequality after composite round-trip\n--- formatted ---\n{formatted}"
    );
}

#[test]
fn reject_depth_two_composite_nesting() {
    // Depth-1 composites are accepted; a `state` inside a substate body
    // (depth > 1) is rejected with a v0.6 diagnostic.
    let source = r"
machine Deep {
    events {
        Go;
    }

    state Outer {
        initial state Inner {
            initial state TooDeep;
        }
    }

    on Go: _ => _ { state }
}
";
    let result = hew_parser::parse(source);
    assert!(
        result
            .errors
            .iter()
            .any(|e| format!("{e:?}").contains("depth > 1")),
        "expected depth>1 v0.6 diagnostic, errors: {:?}",
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
    // threaded onto `MachineDecl::type_params`. Trait bounds (`<T: Trait>`)
    // are accepted at the parser level (see the bounded-decl test below).
    let source = r"
machine Light {
    events {
        Toggle;
    }

    state Off;
    state On;

    on Toggle: Off => On { On }
    on Toggle: On => Off { Off }
}

machine Lifecycle<T> {
    events {
        Start;
    }

    state Created;
    state Running;

    on Start: Created => Running {
        Running
    }
}

machine Triple<T, U, V> {
    events {
        Insert;
    }

    state Empty;
    state Filled;

    on Insert: Empty => Filled {
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

    let names: Vec<Vec<String>> = machines
        .iter()
        .map(|m| m.type_params.iter().map(|p| p.name.clone()).collect())
        .collect();

    assert_eq!(machines[0].name, "Light");
    assert!(machines[0].type_params.is_empty());

    assert_eq!(machines[1].name, "Lifecycle");
    assert_eq!(names[1], vec!["T".to_string()]);
    assert!(machines[1].type_params[0].bounds.is_empty());
    assert_eq!(machines[1].states.len(), 2);
    assert_eq!(machines[1].events.len(), 1);
    assert_eq!(machines[1].transitions.len(), 1);

    assert_eq!(machines[2].name, "Triple");
    assert_eq!(
        names[2],
        vec!["T".to_string(), "U".to_string(), "V".to_string()]
    );

    // Parser accepts a type-param/state-name collision because name binding is
    // not a parser concern; later semantic passes may reject it if needed.
    assert_eq!(machines[3].name, "Collision");
    assert_eq!(names[3], vec!["T".to_string()]);
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
fn parse_machine_generic_decl_trait_bounds_are_accepted() {
    // Trait bounds on machine type parameters are accepted by the parser.
    // Bound enforcement is the type-checker's responsibility — the parser
    // simply threads the bounds through verbatim onto `TypeParam.bounds`.
    let source = r"
trait Resource {
    fn close(self);
}

trait Display {
    fn show(self) -> String;
}

machine Bounded<T: Resource, U: Resource + Display> {
    events {
        Start { handle: T; meta: U; }
        Stop;
    }

    state Idle;
    state Active { handle: T; meta: U; }

    on Start: Idle => Active { Active { handle: event.handle, meta: event.meta } }
    on Stop: Active => Idle { Idle }
    on Start: _ => _ { state }
    on Stop: _ => _ { state }
}
";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "expected clean parse, got: {:?}",
        result.errors
    );

    let machine = result
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            hew_parser::ast::Item::Machine(m) if m.name == "Bounded" => Some(m),
            _ => None,
        })
        .expect("Bounded machine should parse");

    assert_eq!(machine.type_params.len(), 2);
    assert_eq!(machine.type_params[0].name, "T");
    assert_eq!(machine.type_params[0].bounds.len(), 1);
    assert_eq!(machine.type_params[0].bounds[0].name, "Resource");
    assert_eq!(machine.type_params[1].name, "U");
    assert_eq!(machine.type_params[1].bounds.len(), 2);
    assert_eq!(machine.type_params[1].bounds[0].name, "Resource");
    assert_eq!(machine.type_params[1].bounds[1].name, "Display");
}

#[test]
fn machine_trait_bounds_round_trip_through_formatter() {
    // Parse → format → parse → assert structural equality on the AST.
    // Guards against the formatter silently dropping bounds when emitting
    // a machine declaration with `<T: Trait>` type parameters.
    let source = r"trait Resource {
    fn close(self);
}

machine Lifecycle<T: Resource> {
    events {
        Start { handle: T; }
        Stop;
    }

    state Idle;
    state Active { handle: T; }

    on Start: Idle => Active { Active { handle: event.handle } }
    on Stop: Active => Idle { Idle }
    on Start: _ => _ { state }
    on Stop: _ => _ { state }
}
";
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "initial parse errors: {:?}",
        parsed.errors
    );

    let formatted = hew_parser::fmt::format_program(&parsed.program);
    let reparsed = hew_parser::parse(&formatted);
    assert!(
        reparsed.errors.is_empty(),
        "reparse of formatted source failed: {:?}\n\n--- formatted ---\n{}",
        reparsed.errors,
        formatted
    );

    // Compare programs structurally, ignoring span byte offsets which
    // naturally differ between the original and reformatted source.
    assert!(
        hew_parser::ast_eq::program_eq_ignoring_spans(&parsed.program, &reparsed.program),
        "AST structural inequality after round-trip\n--- formatted ---\n{formatted}"
    );
}

#[test]
fn parse_machine_where_clause_simple() {
    // `machine M<T> where T: Trait` parses. The `where` clause is captured
    // separately from inline bounds on `TypeParam`.
    let source = r"
trait Resource {
    fn close(self);
}

machine Container<T> where T: Resource {
    events {
        Start { item: T; }
        Stop;
    }

    state Idle;
    state Active { item: T; }

    on Start: Idle => Active { Active { item: event.item } }
    on Stop: Active => Idle { Idle }
    on Start: _ => _ { state }
    on Stop: _ => _ { state }
}
";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "expected clean parse, got: {:?}",
        result.errors
    );

    let machine = result
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            hew_parser::ast::Item::Machine(m) if m.name == "Container" => Some(m),
            _ => None,
        })
        .expect("Container machine should parse");

    // Inline bounds remain empty.
    assert_eq!(machine.type_params.len(), 1);
    assert!(machine.type_params[0].bounds.is_empty());

    // Where clause carries the predicate.
    let where_clause = machine
        .where_clause
        .as_ref()
        .expect("expected where clause to be parsed");
    assert_eq!(where_clause.predicates.len(), 1);
    assert_eq!(where_clause.predicates[0].bounds.len(), 1);
    assert_eq!(where_clause.predicates[0].bounds[0].name, "Resource");
}

#[test]
fn parse_machine_where_clause_combines_with_inline_bound() {
    // `machine M<T: A> where T: B` parses with both the inline bound
    // and the where-clause predicate populated on the same param; the
    // type checker is responsible for folding them into a single
    // per-(machine, param) bound set.
    let source = r"
trait Resource {
    fn close(self);
}

trait Display {
    fn show(self) -> String;
}

machine Combined<T: Resource> where T: Display {
    events {
        Start { handle: T; }
        Stop;
    }

    state Idle;
    state Active { handle: T; }

    on Start: Idle => Active { Active { handle: event.handle } }
    on Stop: Active => Idle { Idle }
    on Start: _ => _ { state }
    on Stop: _ => _ { state }
}
";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "expected clean parse, got: {:?}",
        result.errors
    );

    let machine = result
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            hew_parser::ast::Item::Machine(m) if m.name == "Combined" => Some(m),
            _ => None,
        })
        .expect("Combined machine should parse");

    // Inline bound on T.
    assert_eq!(machine.type_params.len(), 1);
    assert_eq!(machine.type_params[0].bounds.len(), 1);
    assert_eq!(machine.type_params[0].bounds[0].name, "Resource");

    // Plus where-clause bound on T.
    let where_clause = machine
        .where_clause
        .as_ref()
        .expect("expected where clause to be parsed");
    assert_eq!(where_clause.predicates.len(), 1);
    assert_eq!(where_clause.predicates[0].bounds.len(), 1);
    assert_eq!(where_clause.predicates[0].bounds[0].name, "Display");
}

#[test]
fn parse_machine_where_clause_multi_predicate() {
    // Multi-predicate where clause: `where T: A, U: B + C`.
    let source = r"
trait Resource {
    fn close(self);
}

trait Display {
    fn show(self) -> String;
}

trait Send {
    fn send(self);
}

machine Multi<T, U> where T: Resource, U: Display + Send {
    events {
        Start { left: T; right: U; }
        Stop;
    }

    state Idle;
    state Active { left: T; right: U; }

    on Start: Idle => Active { Active { left: event.left, right: event.right } }
    on Stop: Active => Idle { Idle }
    on Start: _ => _ { state }
    on Stop: _ => _ { state }
}
";
    let result = hew_parser::parse(source);
    assert!(
        result.errors.is_empty(),
        "expected clean parse, got: {:?}",
        result.errors
    );

    let machine = result
        .program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            hew_parser::ast::Item::Machine(m) if m.name == "Multi" => Some(m),
            _ => None,
        })
        .expect("Multi machine should parse");

    let where_clause = machine
        .where_clause
        .as_ref()
        .expect("expected where clause to be parsed");
    assert_eq!(where_clause.predicates.len(), 2);
    assert_eq!(where_clause.predicates[0].bounds.len(), 1);
    assert_eq!(where_clause.predicates[0].bounds[0].name, "Resource");
    assert_eq!(where_clause.predicates[1].bounds.len(), 2);
    assert_eq!(where_clause.predicates[1].bounds[0].name, "Display");
    assert_eq!(where_clause.predicates[1].bounds[1].name, "Send");
}

#[test]
fn machine_where_clause_round_trips_through_formatter() {
    // Combined inline + where → format → reparse: where clause survives.
    let source = r"trait Resource {
    fn close(self);
}

trait Display {
    fn show(self) -> String;
}

machine Combined<T: Resource> where T: Display {
    events {
        Start { handle: T; }
        Stop;
    }

    state Idle;
    state Active { handle: T; }

    on Start: Idle => Active { Active { handle: event.handle } }
    on Stop: Active => Idle { Idle }
    on Start: _ => _ { state }
    on Stop: _ => _ { state }
}
";
    let parsed = hew_parser::parse(source);
    assert!(
        parsed.errors.is_empty(),
        "initial parse errors: {:?}",
        parsed.errors
    );

    let formatted = hew_parser::fmt::format_program(&parsed.program);
    assert!(
        formatted.contains("where T: Display"),
        "formatted output should preserve `where` clause; got:\n{formatted}"
    );

    let reparsed = hew_parser::parse(&formatted);
    assert!(
        reparsed.errors.is_empty(),
        "reparse of formatted source failed: {:?}\n\n--- formatted ---\n{}",
        reparsed.errors,
        formatted
    );

    assert!(
        hew_parser::ast_eq::program_eq_ignoring_spans(&parsed.program, &reparsed.program),
        "AST structural inequality after round-trip\n--- formatted ---\n{formatted}"
    );
}

// ── W3.039 Stage 1: const-generic machine parameters ───────────────────

fn first_machine_named<'a>(
    program: &'a hew_parser::ast::Program,
    name: &str,
) -> &'a hew_parser::ast::MachineDecl {
    program
        .items
        .iter()
        .find_map(|(item, _)| match item {
            hew_parser::ast::Item::Machine(m) if m.name == name => Some(m),
            _ => None,
        })
        .unwrap_or_else(|| panic!("machine `{name}` should parse"))
}

#[test]
fn machine_const_param_minimal_parses() {
    let src = r"machine M<const N: usize> {
    events {
        E;
    }

    state S;

    on E: S => S { S }
}
";
    let r = hew_parser::parse(src);
    assert!(r.errors.is_empty(), "parse errors: {:?}", r.errors);
    let m = first_machine_named(&r.program, "M");
    assert!(m.type_params.is_empty(), "expected no type params");
    assert_eq!(m.const_params.len(), 1);
    assert_eq!(m.const_params[0].name, "N");
    assert!(matches!(
        m.const_params[0].ty,
        hew_parser::ast::ConstParamTy::Usize
    ));
    assert_eq!(m.const_params[0].default, None);
}

#[test]
fn machine_const_param_with_default_parses() {
    let src = r"machine M<const N: usize = 16> {
    events {
        E;
    }

    state S;

    on E: S => S { S }
}
";
    let r = hew_parser::parse(src);
    assert!(r.errors.is_empty(), "parse errors: {:?}", r.errors);
    let m = first_machine_named(&r.program, "M");
    assert_eq!(m.const_params.len(), 1);
    assert_eq!(m.const_params[0].default, Some(16));
}

#[test]
fn machine_mixed_type_and_const_params_parses() {
    let src = r"machine M<T, const N: usize> {
    events {
        E;
    }

    state S { val: T; }

    on E: S => S { S { val: event.val } }
}
";
    let r = hew_parser::parse(src);
    assert!(r.errors.is_empty(), "parse errors: {:?}", r.errors);
    let m = first_machine_named(&r.program, "M");
    assert_eq!(m.type_params.len(), 1);
    assert_eq!(m.type_params[0].name, "T");
    assert_eq!(m.const_params.len(), 1);
    assert_eq!(m.const_params[0].name, "N");
}

#[test]
fn machine_const_param_rejects_non_usize_width() {
    let src = r"machine M<const N: i32> {
    events {
        E;
    }

    state S;

    on E: S => S { S }
}
";
    let r = hew_parser::parse(src);
    assert!(
        !r.errors.is_empty(),
        "expected diagnostic for non-usize const-param width"
    );
    let msg = format!("{:?}", r.errors);
    assert!(
        msg.contains("only `usize` is supported"),
        "expected usize-only diagnostic, got: {msg}"
    );
}

#[test]
fn machine_const_param_rejects_const_before_type_param() {
    let src = r"machine M<const N: usize, T> {
    events {
        E;
    }

    state S;

    on E: S => S { S }
}
";
    let r = hew_parser::parse(src);
    assert!(
        !r.errors.is_empty(),
        "expected diagnostic for type-param after const-param"
    );
}

#[test]
fn machine_const_param_round_trips_through_formatter() {
    let src = r"machine M<T, const N: usize = 16> {
    events {
        E;
    }

    state S { val: T; }

    on E: S => S { S { val: event.val } }
}
";
    let r = hew_parser::parse(src);
    assert!(r.errors.is_empty(), "parse errors: {:?}", r.errors);
    let formatted = hew_parser::fmt::format_program(&r.program);
    assert!(
        formatted.contains("const N: usize = 16"),
        "formatter must emit const param: {formatted}"
    );
    let r2 = hew_parser::parse(&formatted);
    assert!(
        r2.errors.is_empty(),
        "reparse errors: {:?}\n---\n{}",
        r2.errors,
        formatted
    );
    assert!(
        hew_parser::ast_eq::program_eq_ignoring_spans(&r.program, &r2.program),
        "round-trip AST mismatch:\n{formatted}"
    );
}
