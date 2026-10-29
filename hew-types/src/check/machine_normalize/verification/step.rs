//! Compose certified passes through the actual reaction loop. A decreasing
//! integer budget proves the loop bound for arbitrary histories, assuming the
//! user guards/actions themselves finish. No bounded loop unrolling is used.
use super::{
    and, not, or, save, solve, Expr, Ident, Literal, MachineDecl, MachineTrigger, Pattern, Span,
    Spanned, Stmt,
};
use crate::check::{walk_block, NodeVisitor};
use hew_parser::ast::{BinaryOp, Block, CallArg, UnaryOp};
use std::collections::BTreeMap;
use std::fmt::Write as _;
use std::sync::atomic::{AtomicUsize, Ordering};

pub(super) static STEPS: AtomicUsize = AtomicUsize::new(0);
pub(super) static CLAIMS: AtomicUsize = AtomicUsize::new(0);
pub(super) static REJECTED: AtomicUsize = AtomicUsize::new(0);
type Witness = (Spanned<Expr>, MachineTrigger);

fn ident(expr: &Spanned<Expr>, name: &str) -> bool {
    matches!(&expr.0,Expr::Ident(id) if *id == Ident::new(name))
}
fn variant(expr: &Spanned<Expr>, name: &str) -> bool {
    matches!(&expr.0,Expr::ContextVariant(v) if v.name == Ident::new(name))
}
fn integer(expr: &Spanned<Expr>) -> String {
    match &expr.0 {
        Expr::Ident(name) if *name == Ident::new("limit") => "limit".into(),
        Expr::Literal(Literal::Integer { value, .. }) => value.to_string(),
        _ => panic!("machine step: unsupported budget expression"),
    }
}
fn gate(expr: &Spanned<Expr>) -> String {
    match &expr.0 {
        Expr::Literal(Literal::Bool(value)) => value.to_string(),
        Expr::Unary {
            op: UnaryOp::Not,
            operand,
        } => not(&gate(operand)),
        Expr::Binary { op, left, right } => {
            let op = match op {
                BinaryOp::LessEqual => "<=",
                BinaryOp::Less => "<",
                BinaryOp::Greater => ">",
                BinaryOp::GreaterEqual => ">=",
                BinaryOp::Equal => "=",
                _ => panic!("machine step: unsupported limit predicate"),
            };
            format!("({op} {} {})", integer(left), integer(right))
        }
        _ => panic!("machine step: unsupported limit gate"),
    }
}

struct Iteration<'a> {
    witnesses: &'a [Witness],
    smt: String,
    fresh: usize,
    used: String,
    alive: String,
    continued: Vec<String>,
    stopped: Vec<String>,
    refused: Vec<String>,
    calls: Vec<(MachineTrigger, String)>,
}
impl Iteration<'_> {
    fn term(&mut self, value: impl AsRef<str>) -> String {
        let value = value.as_ref();
        let name = format!("b{}", self.fresh);
        self.fresh += 1;
        writeln!(self.smt, "(define-fun {name} () Bool {value})").unwrap();
        name
    }
    fn expression(&mut self, expr: &Spanned<Expr>, path: &str) -> String {
        if let Some((proved, trigger)) =
            self.witnesses.iter().find(|(proved, _)| proved.1 == expr.1)
        {
            assert_eq!(expr, proved, "machine step: pass changed after its proof");
            let trigger = *trigger;
            let available = match trigger {
                MachineTrigger::Completion => "completion_available",
                MachineTrigger::Always => "always_available",
                MachineTrigger::Event(_) => panic!("machine step: external input in internal loop"),
            };
            self.calls.push((trigger, path.into()));
            let refused = self.term(and([
                path.into(),
                available.into(),
                format!("(>= {} limit)", self.used),
            ]));
            self.refused.push(refused.clone());
            self.alive = self.term(and([self.alive.clone(), not(&refused)]));
            self.used = format!(
                "(ite {} (+ {} 1) {})",
                and([path.into(), available.into(), not(&refused)]),
                self.used,
                self.used
            );
            return available.into();
        }
        match &expr.0 {
            Expr::Literal(Literal::Bool(value)) => value.to_string(),
            Expr::Unary {
                op: UnaryOp::Not,
                operand,
            } => {
                let value = self.expression(operand, path);
                not(&value)
            }
            Expr::Binary {
                op: op @ (BinaryOp::And | BinaryOp::Or),
                left,
                right,
            } => {
                let left = self.expression(left, path);
                let right_path = self.term(and([
                    path.into(),
                    self.alive.clone(),
                    if *op == BinaryOp::And {
                        left.clone()
                    } else {
                        not(&left)
                    },
                ]));
                let right = self.expression(right, &right_path);
                if *op == BinaryOp::And {
                    and([left, right])
                } else {
                    or([left, right])
                }
            }
            _ => panic!(
                "machine step: internal loop condition is not composed from a certified pass"
            ),
        }
    }
    fn statement(&mut self, stmt: &Spanned<Stmt>, outer: &str) {
        let path = self.term(and([outer.into(), self.alive.clone()]));
        match &stmt.0 {
            Stmt::If {
                condition,
                then_block,
                else_block: None,
            } => {
                let condition = self.expression(condition, &path);
                let branch = self.term(and([path, condition, self.alive.clone()]));
                for stmt in &then_block.stmts {
                    self.statement(stmt, &branch);
                }
                assert!(then_block.trailing_expr.is_none());
            }
            Stmt::Continue { label: None } => {
                self.continued.push(path.clone());
                self.alive = self.term(and([self.alive.clone(), not(&path)]));
            }
            Stmt::Break {
                label: None,
                value: None,
            } => {
                self.stopped.push(path.clone());
                self.alive = self.term(and([self.alive.clone(), not(&path)]));
            }
            _ => panic!("machine step: unexpected internal loop operation"),
        }
    }
}

fn iteration(loop_body: &Block, witnesses: &[Witness]) -> (String, usize) {
    let mut model=Iteration {witnesses,smt:"(set-logic QF_LIA)\n(set-option :timeout 30000)\n(declare-const limit Int)\n(declare-const used_before Int)\n(declare-const completion_available Bool)\n(declare-const always_available Bool)\n(assert (and (<= 1 limit) (<= limit 9223372036854775807) (<= 0 used_before) (<= used_before 9223372036854775807)))\n".into(),fresh:0,used:"used_before".into(),alive:"true".into(),continued:Vec::new(),stopped:Vec::new(),refused:Vec::new(),calls:Vec::new()};
    let has_completion = witnesses
        .iter()
        .any(|(_, trigger)| *trigger == MachineTrigger::Completion);
    let has_always = witnesses
        .iter()
        .any(|(_, trigger)| *trigger == MachineTrigger::Always);
    if !has_completion {
        model.smt.push_str("(assert (not completion_available))\n");
    }
    if !has_always {
        model.smt.push_str("(assert (not always_available))\n");
    }
    for stmt in &loop_body.stmts {
        model.statement(stmt, "true");
    }
    assert!(loop_body.trailing_expr.is_none());
    let continued = or(model.continued);
    let stopped = or(model.stopped);
    let refused = or(model.refused);
    let mut claims=vec![
        ("no internal control path falls through the iteration",not(&model.alive)),
        ("continue strictly decreases the nonnegative reaction budget",format!("(=> {continued} (and (< used_before limit) (= {} (+ used_before 1)) (<= 0 (- limit {})) (= (- limit {}) (- (- limit used_before) 1))))",model.used,model.used,model.used)),
        ("quiescence is exactly the absence of internal transitions",format!("(= {stopped} (and (not completion_available) (not always_available)))")),
        ("an available reaction either progresses or reports budget exhaustion",format!("(= {refused} (and (or completion_available always_available) (>= used_before limit)))")),
        ("enabled internal progress is never suppressed",format!("(= {continued} (and (or completion_available always_available) (< used_before limit)))")),
    ];
    for trigger in [MachineTrigger::Completion, MachineTrigger::Always] {
        let expected = match trigger {
            MachineTrigger::Completion => {
                if has_completion {
                    "1"
                } else {
                    "0"
                }
            }
            _ => {
                if has_always {
                    "(ite completion_available 0 1)"
                } else {
                    "0"
                }
            }
        };
        let calls = model
            .calls
            .iter()
            .filter(|(kind, _)| *kind == trigger)
            .map(|(_, path)| format!("(ite {path} 1 0)"))
            .collect::<Vec<_>>();
        let actual = if calls.is_empty() {
            "0".into()
        } else {
            format!("(+ {})", calls.join(" "))
        };
        claims.push((
            "completion priority and one invocation per phase",
            format!("(= {actual} {expected})"),
        ));
    }
    let violation = or(claims.iter().map(|(_, claim)| not(claim)));
    for (label, _) in &claims {
        writeln!(model.smt, "; {label}").unwrap();
    }
    (
        format!(
            "{}(check-sat)\n(assert {violation})\n(check-sat)\n",
            model.smt
        ),
        claims.len(),
    )
}

const PROTECTED: [&str; 6] = [
    "_$machine_work",
    "_$machine_used",
    "_$machine_rule",
    "_$machine_outputs",
    "_$machine_disposition",
    "_$machine_departures",
];
fn root(expr: &Spanned<Expr>) -> Option<Ident> {
    match &expr.0 {
        Expr::Ident(name) => Some(*name),
        Expr::FieldAccess { object, .. } | Expr::Index { object, .. } => root(object),
        _ => None,
    }
}
#[derive(Default)]
struct Writes {
    receiver: Vec<Span>,
    protected: BTreeMap<(usize, usize, String), usize>,
}
impl Writes {
    fn record(&mut self, name: Ident, span: &Span) {
        if PROTECTED
            .iter()
            .any(|protected| name == Ident::new(protected))
        {
            *self
                .protected
                .entry((span.start, span.end, name.to_string()))
                .or_default() += 1;
        }
    }
}
impl NodeVisitor for Writes {
    fn visit_stmt(&mut self, stmt: &Stmt, span: &Span) {
        match stmt {
            Stmt::Assign { target, .. } => {
                if let Some(name) = root(target) {
                    if name == Ident::new("self") {
                        self.receiver.push(span.clone());
                    }
                    self.record(name, span);
                }
            }
            Stmt::Var { name, .. }
            | Stmt::Let {
                pattern: (Pattern::Identifier(name), _),
                ..
            } => self.record(*name, span),
            _ => {}
        }
    }
    fn visit_expr(&mut self, expr: &Expr, span: &Span) {
        if let Expr::MethodCall { receiver, .. } = expr {
            if let Some(name) = root(receiver) {
                self.record(name, span);
            }
        }
    }
}
fn record_fields(fields: &[(Ident, Spanned<Expr>)], expected: &[(&str, &str)]) {
    assert_eq!(
        fields.len(),
        expected.len(),
        "machine step: report shape changed"
    );
    for (field, value) in expected {
        assert_eq!(
            fields
                .iter()
                .filter(|(name, expression)| *name == Ident::new(field) && ident(expression, value))
                .count(),
            1,
            "machine step: report no longer publishes {field}"
        );
    }
}

#[expect(
    clippy::too_many_lines,
    reason = "the shell audit and loop composition enumerate each publication and control obligation"
)]
pub(in crate::check::machine_normalize) fn verify_step(
    machine: &MachineDecl,
    body: &Block,
    witnesses: &[Witness],
    observed: bool,
) {
    let mut invalid = None;
    let mut event_dispatch = None;
    let mut internal = None;
    let mut candidate = None;
    let mut report = None;
    let mut commit = None;
    let mut initialized = 0;
    let mut seeds = BTreeMap::new();
    for (position, (stmt, span)) in body.stmts.iter().enumerate() {
        if let Stmt::Var {
            name,
            value: Some(value),
            ..
        } = stmt
        {
            if [
                "_$machine_rule",
                "_$machine_outputs",
                "_$machine_departures",
            ]
            .iter()
            .any(|root| *name == Ident::new(root))
            {
                assert!(
                    matches!(&value.0,Expr::Array(elements) if elements.is_empty()),
                    "machine step: reports must start empty"
                );
            }
            if *name == Ident::new("_$machine_disposition") {
                assert!(
                    variant(value, "Ignored"),
                    "machine step: default inputs must begin ignored"
                );
            }
            if PROTECTED
                .iter()
                .any(|protected| *name == Ident::new(protected))
            {
                assert!(
                    seeds.insert(name.to_string(), (position, span)).is_none(),
                    "machine step: evaluation context was reset"
                );
            }
        }
        match stmt {
            Stmt::If {
                condition,
                then_block,
                ..
            } if matches!(then_block.stmts.as_slice(),[(Stmt::Return(Some((Expr::Call{function,args,..},_))),_)] if variant(function,"Err") && matches!(args.as_slice(),[CallArg::Positional(error)]if variant(error,"InvalidLimit"))) =>
            {
                assert!(invalid.is_none());
                invalid = Some((position, condition));
            }
            Stmt::Var {
                name,
                value: Some(value),
                ..
            } if *name == Ident::new("_$machine_used") => {
                assert_eq!(integer(value), "0");
                initialized += 1;
            }
            Stmt::Var {
                name,
                value: Some(value),
                ..
            } if *name == Ident::new("_$machine_work") => assert!(
                ident(value, "self"),
                "machine step: evaluation must begin from the receiver"
            ),
            Stmt::Expression((Expr::Match { scrutinee, arms }, _))
                if ident(scrutinee, "_$machine_event") =>
            {
                assert!(event_dispatch.replace(position).is_none());
                assert_eq!(arms.len(), machine.events.len());
                for event in &machine.events {
                    let matching:Vec<_>=arms.iter().filter(|arm|matches!(&arm.pattern.0,Pattern::ContextVariant(pattern)if pattern.name==event.name)).collect();
                    assert_eq!(matching.len(), 1);
                    let arm = matching[0];
                    assert!(arm.guard.is_none());
                    let Expr::Block(block) = &arm.body.0 else {
                        panic!("machine step: event arm is a value block")
                    };
                    let value = block.trailing_expr.as_deref().expect("event result");
                    assert!(
                        witnesses.iter().any(|(proved, trigger)| *trigger
                            == MachineTrigger::Event(event.name)
                            && proved == value),
                        "machine step: input dispatch changed its certified evaluator"
                    );
                }
            }
            Stmt::Loop { body, label: None } => {
                assert!(internal.replace((position, body)).is_none());
            }
            Stmt::Let {
                pattern: (Pattern::Identifier(name), _),
                value: Some(value),
                ..
            } if *name == Ident::new("_$machine_candidate") => {
                assert!(ident(value, "_$machine_work"));
                candidate = Some(position);
            }
            Stmt::Let {
                pattern: (Pattern::Identifier(name), _),
                value: Some((Expr::StructInit { fields, .. }, _)),
                ..
            } if *name == Ident::new("_$machine_report") => {
                assert!(report.replace(position).is_none());
                record_fields(
                    fields,
                    &[
                        ("outputs", "_$machine_outputs"),
                        ("disposition", "_$machine_disposition"),
                        ("input", "_$machine_input"),
                        ("rules", "_$machine_rule"),
                    ],
                );
            }
            Stmt::Assign {
                target,
                value,
                op: None,
            } if ident(target, "self") => {
                assert!(ident(value, "_$machine_candidate"));
                commit = Some((position, span));
            }
            _ => {}
        }
    }
    let (invalid_position, condition) = invalid.expect("machine step validates the limit");
    let event_position = event_dispatch.expect("machine step dispatches its input");
    assert!(initialized == 1 && invalid_position < event_position);
    for protected in PROTECTED
        .into_iter()
        .filter(|name| observed || *name != "_$machine_departures")
    {
        assert!(
            seeds[protected].0 < event_position,
            "machine step: context initialized after input dispatch"
        );
    }
    let (commit_position, commit_span) = commit.expect("machine step commits the candidate");
    assert!(
        event_position < candidate.unwrap()
            && candidate.unwrap() < report.unwrap()
            && report.unwrap() < commit_position
    );
    if let Some((position, _)) = internal {
        assert!(event_position < position && position < candidate.unwrap());
    }
    let mut allowed = Writes::default();
    for (proved, _) in witnesses {
        crate::check::walk_expr(&proved.0, &proved.1, &mut allowed);
    }
    for (name, (_, span)) in seeds {
        allowed.record(Ident::new(&name), span);
    }
    let mut writes = Writes::default();
    walk_block(body, &mut writes);
    assert_eq!(
        writes.protected, allowed.protected,
        "machine step: state, budget or reports changed outside certified passes"
    );
    assert_eq!(
        writes.receiver,
        vec![commit_span.clone()],
        "machine step: an error path can publish the receiver early"
    );
    assert!(
        matches!(body.trailing_expr.as_deref(),Some((Expr::Call {function,..},_))if variant(function,"Ok")),
        "machine step: only the success path publishes the staged receiver"
    );
    if let Some((Expr::Call { args, .. }, _)) = body.trailing_expr.as_deref() {
        if observed {
            let [CallArg::Positional((Expr::StructInit { fields, .. }, _))] = args.as_slice()
            else {
                panic!("machine step: success lost the observed report")
            };
            record_fields(
                fields,
                &[
                    ("report", "_$machine_report"),
                    ("departures", "_$machine_departures"),
                ],
            );
        } else {
            assert!(
                matches!(args.as_slice(), [CallArg::Positional(value)] if ident(value, "_$machine_report")),
                "machine step: success lost the plain report"
            );
        }
    }
    let gate = gate(condition);
    let query=format!("(set-logic QF_LIA)\n(declare-const limit Int)\n(assert (and (<= (- 9223372036854775808) limit) (<= limit 9223372036854775807)))\n(check-sat)\n(assert (not (= {gate} (<= limit 0))))\n(check-sat)\n");
    assert_eq!(
        solve(&query).trim(),
        "sat\nunsat",
        "machine step: invalid limits reach evaluation"
    );
    let sequence = STEPS.fetch_add(1, Ordering::Relaxed);
    save(
        &format!("step-{sequence:03}-{}-limit", machine.name),
        "smt2",
        &query,
    );
    CLAIMS.fetch_add(1, Ordering::Relaxed);
    if std::env::var_os("HEW_MACHINE_MUTATIONS").is_some()
        && REJECTED.load(Ordering::Relaxed) & 1 == 0
    {
        let query = query.replace(
            &format!("(= {gate} (<= limit 0))"),
            &format!("(= (not {gate}) (<= limit 0))"),
        );
        assert_eq!(
            solve(&query).trim(),
            "sat\nsat",
            "machine step: invalid-limit mutant escaped"
        );
        save("mutant-inverted-invalid-limit", "smt2", &query);
        save(
            "mutant-inverted-invalid-limit",
            "counterexample",
            &solve(&format!("{query}(get-model)\n")),
        );
        REJECTED.fetch_or(1, Ordering::Relaxed);
    }
    if let Some((_, loop_body)) = internal {
        let (query, count) = iteration(loop_body, witnesses);
        let result = solve(&query);
        save(
            &format!("step-{sequence:03}-{}-loop", machine.name),
            "smt2",
            &query,
        );
        save(
            &format!("step-{sequence:03}-{}-loop", machine.name),
            "result",
            &result,
        );
        if result.trim() == "sat\nsat" {
            save(
                &format!("step-{sequence:03}-{}-loop", machine.name),
                "failure",
                &solve(&format!("{query}(get-model)\n")),
            );
        }
        assert_eq!(
            result.trim(),
            "sat\nunsat",
            "machine step: reaction-loop contract failed"
        );
        CLAIMS.fetch_add(count, Ordering::Relaxed);
        if std::env::var_os("HEW_MACHINE_MUTATIONS").is_some() {
            for kind in 1..5 {
                if REJECTED.load(Ordering::Relaxed) & (1 << kind) != 0 {
                    continue;
                }
                let mut altered = loop_body.clone();
                match kind {
                    1 => altered.stmts.reverse(),
                    2 => {
                        for (stmt, _) in &mut altered.stmts {
                            if let Stmt::If { then_block, .. } = stmt {
                                for (stmt, _) in &mut then_block.stmts {
                                    if matches!(stmt, Stmt::Continue { .. }) {
                                        *stmt = Stmt::Break {
                                            label: None,
                                            value: None,
                                        };
                                    }
                                }
                            }
                        }
                    }
                    3 => {
                        altered.stmts.pop();
                    }
                    4 => {
                        for (stmt, _) in &mut altered.stmts {
                            if matches!(stmt, Stmt::Break { .. }) {
                                *stmt = Stmt::Continue { label: None };
                            }
                        }
                    }
                    _ => unreachable!(),
                }
                let (query, _) = iteration(&altered, witnesses);
                let result = solve(&query);
                assert_eq!(
                    result.trim(),
                    "sat\nsat",
                    "machine step: loop mutant {kind} escaped"
                );
                save(&format!("mutant-reaction-loop-{kind}"), "smt2", &query);
                save(
                    &format!("mutant-reaction-loop-{kind}"),
                    "counterexample",
                    &solve(&format!("{query}(get-model)\n")),
                );
                REJECTED.fetch_or(1 << kind, Ordering::Relaxed);
            }
        }
    } else {
        assert!(!witnesses.iter().any(|(_, trigger)| matches!(
            trigger,
            MachineTrigger::Completion | MachineTrigger::Always
        )));
    }
}
