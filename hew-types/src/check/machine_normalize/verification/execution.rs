//! Selection-to-dispatch composition over the emitted pass, including its
//! actual early return and integer budget arithmetic. User code inside a staged
//! update is opaque here; this theorem concerns which updates are invoked and
//! the order in which they are invoked, not the update's internal semantics.
use std::collections::HashSet;
use std::sync::atomic::{AtomicUsize, Ordering};

use super::{
    and, counterexample, not, or, query, save, solve, Expr, Ident, Literal, MachineTrigger, NodeId,
    Span, Spanned, Stmt, Validator, Value,
};
use hew_parser::ast::{BinaryOp, CallArg};

pub(super) static PASSES: AtomicUsize = AtomicUsize::new(0);
pub(super) static CLAIMS: AtomicUsize = AtomicUsize::new(0);
pub(super) static REJECTED: AtomicUsize = AtomicUsize::new(0);

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub(in crate::check::machine_normalize) enum Stage {
    Body(usize),
    Hook {
        node: NodeId,
        config: NodeId,
        entry: bool,
    },
    History {
        target: NodeId,
        owner: NodeId,
        config: NodeId,
    },
    Departure {
        scope: NodeId,
        config: NodeId,
    },
}
#[derive(Clone)]
pub(in crate::check::machine_normalize) struct Effect {
    pub span: Span,
    pub stage: Stage,
}

#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
enum Event {
    Dispatch(Stage),
    Rule(usize),
    Taken(usize),
}

struct Pass<'a, 'b> {
    checker: &'a mut Validator<'b>,
    effects: &'a [Effect],
    alive: String,
    refusals: Vec<String>,
    events: Vec<(Event, String)>,
    body: Option<usize>,
}

fn int(value: Value) -> String {
    let Value::Int(term) = value else {
        panic!("machine execution: expected integer")
    };
    term
}
fn ident(value: &Spanned<Expr>, name: &str) -> bool {
    matches!(&value.0, Expr::Ident(id) if *id == Ident::new(name))
}
fn variant(value: &Spanned<Expr>, name: &str) -> bool {
    matches!(&value.0, Expr::ContextVariant(v) if v.name == Ident::new(name))
}

impl Pass<'_, '_> {
    fn expression(&mut self, expression: &Spanned<Expr>, path: &str) -> Value {
        match &expression.0 {
            Expr::Literal(Literal::Integer { value, .. }) => Value::Int(value.to_string()),
            Expr::Binary {
                op: op @ (BinaryOp::GreaterEqual | BinaryOp::Add),
                left,
                right,
            } => {
                let a = int(self.expression(left, path));
                let b = int(self.expression(right, path));
                if *op == BinaryOp::GreaterEqual {
                    Value::Bool(self.checker.term(format!("(>= {a} {b})")))
                } else {
                    Value::Int(format!("(+ {a} {b})"))
                }
            }
            _ => self.checker.expr(expression, path),
        }
    }
    #[expect(
        clippy::too_many_lines,
        reason = "the closed emitted-statement interpreter makes early-return and side-effect sequencing explicit"
    )]
    fn statement(&mut self, statement: &Spanned<Stmt>, outer: &str) {
        let path = self.checker.term(and([outer.into(), self.alive.clone()]));
        if let Some(effect) = self
            .effects
            .iter()
            .find(|effect| effect.span == statement.1)
        {
            let (Stmt::Assign {
                value: expression, ..
            }
            | Stmt::Expression(expression)) = &statement.0
            else {
                panic!("staged effect is an expression")
            };
            super::frame::validate(expression)
                .unwrap_or_else(|reason| panic!("machine execution frame: {reason}"));
            match (&effect.stage, &statement.0) {
                (
                    Stage::Body(_) | Stage::Hook { .. },
                    Stmt::Assign {
                        target, op: None, ..
                    },
                ) => assert!(
                    ident(target, super::super::configuration::WORK),
                    "machine execution: update must write the staged receiver"
                ),
                (Stage::History { .. } | Stage::Departure { .. }, Stmt::Expression(_)) => {}
                _ => panic!("machine execution: effect marker no longer identifies its operation"),
            }
            self.events.push((Event::Dispatch(effect.stage), path));
            return;
        }
        match &statement.0 {
            Stmt::If {
                condition,
                then_block,
                else_block: None,
            } => {
                let condition = Validator::bool(self.expression(condition, &path));
                let branch = self.checker.term(and([path, condition]));
                let saved = self.body;
                self.body = then_block
                    .stmts
                    .iter()
                    .find_map(|stmt| {
                        self.effects
                            .iter()
                            .find_map(|effect| (effect.span == stmt.1).then_some(effect.stage))
                    })
                    .and_then(|stage| {
                        if let Stage::Body(index) = stage {
                            Some(index)
                        } else {
                            None
                        }
                    })
                    .or(saved);
                for stmt in &then_block.stmts {
                    self.statement(stmt, &branch);
                }
                assert!(
                    then_block.trailing_expr.is_none(),
                    "machine execution: unexpected branch value"
                );
                self.body = saved;
            }
            Stmt::Return(Some(value)) => {
                let Expr::Call { function, args, .. } = &value.0 else {
                    panic!("machine execution: unexpected pass return")
                };
                assert!(
                    variant(function, "Err")
                        && matches!(args.as_slice(), [CallArg::Positional(error)] if variant(error,"LimitExceeded")),
                    "machine execution: wrong step-limit error"
                );
                self.refusals.push(path.clone());
                self.alive = self.checker.term(and([self.alive.clone(), not(&path)]));
            }
            Stmt::Assign {
                target,
                op: None,
                value,
            } if ident(target, "_$machine_used") => {
                let value = int(self.expression(value, &path));
                self.checker.claim(
                    "step budget arithmetic cannot overflow".into(),
                    format!("(=> {path} (and (<= 0 {value}) (<= {value} 9223372036854775807)))"),
                );
                let prior = int(self.checker.vars[&Ident::new("_$machine_used")].clone());
                self.checker.vars.insert(
                    Ident::new("_$machine_used"),
                    Value::Int(format!("(ite {path} {value} {prior})")),
                );
            }
            Stmt::Assign {
                target,
                op: None,
                value,
            } if ident(target, "_$machine_disposition") => {
                assert!(
                    variant(value, "Taken"),
                    "machine execution: selected update must report Taken"
                );
                self.events.push((
                    Event::Taken(self.body.expect("Taken belongs to a transition dispatch")),
                    path,
                ));
            }
            Stmt::Expression((
                Expr::MethodCall {
                    receiver,
                    method,
                    args,
                },
                _,
            )) if ident(receiver, "_$machine_rule") && method.0 == Ident::new("push") => {
                let [CallArg::Positional((Expr::Literal(Literal::Integer { value, .. }), _))] =
                    args.as_slice()
                else {
                    panic!("machine execution: malformed rule record")
                };
                let body = self
                    .body
                    .expect("rule record belongs to a transition dispatch");
                assert_eq!(
                    usize::try_from(*value).ok(),
                    Some(self.checker.hierarchy.ordinal(body)),
                    "machine execution: reported rule is not the invoked rule"
                );
                self.events.push((Event::Rule(body), path));
            }
            _ => panic!(
                "machine execution: unsupported emitted pass statement: {:?}",
                statement.0
            ),
        }
    }
}

fn stages(checker: &Validator<'_>, trigger: MachineTrigger) -> HashSet<Stage> {
    let h = checker.hierarchy;
    let m = checker.machine;
    let rules: Vec<_> = m
        .transitions
        .iter()
        .enumerate()
        .filter_map(|(i, r)| (r.trigger == trigger).then_some(i))
        .collect();
    let mut expected: HashSet<_> = rules.iter().map(|i| Stage::Body(*i)).collect();
    for entry in [false, true] {
        for node in h.hook_nodes() {
            if h.hooks(m, node, entry).is_none() {
                continue;
            }
            let eligible = rules.iter().any(|index| {
                h.crosses(*index, node)
                    && h.configurations_compatible(
                        node,
                        if entry {
                            h.target(*index).map_or(node, |target| {
                                if m.transitions[*index].history {
                                    target
                                } else {
                                    h.leaf(target)
                                }
                            })
                        } else {
                            h.boundary(*index).0
                        },
                    )
            });
            if eligible {
                expected.extend(
                    h.configurations(h.container(node))
                        .into_iter()
                        .filter(|config| h.ancestors(*config).contains(&node))
                        .map(|config| Stage::Hook {
                            node,
                            config,
                            entry,
                        }),
                );
            }
        }
    }
    let targets: HashSet<_> = m
        .transitions
        .iter()
        .enumerate()
        .filter(|(_, r)| r.history)
        .map(|(i, _)| h.target(i).expect("history target"))
        .collect();
    for target in targets {
        let regions = h.group_regions(m, target);
        let owners = if regions.is_empty() {
            vec![target]
        } else {
            regions
        };
        for owner in owners {
            let container = if owner == target {
                h.container(owner)
            } else {
                Some(owner)
            };
            expected.extend(
                h.configurations(container)
                    .into_iter()
                    .filter(|config| h.ancestors(*config).contains(&owner))
                    .map(|config| Stage::History {
                        target,
                        owner,
                        config,
                    }),
            );
        }
    }
    for group in m.composite_groups.iter().filter(|_| checker.observed) {
        let scope = h
            .resolve(group.name, group.span.clone())
            .expect("source scope");
        expected.extend(
            h.configurations(h.container(scope))
                .into_iter()
                .filter(|config| h.ancestors(*config).contains(&scope))
                .map(|config| Stage::Departure { scope, config }),
        );
    }
    expected
}

fn expected_path(checker: &Validator<'_>, stage: Stage, trigger: MachineTrigger) -> String {
    let h = checker.hierarchy;
    let m = checker.machine;
    let rules = m
        .transitions
        .iter()
        .enumerate()
        .filter_map(|(i, r)| (r.trigger == trigger).then_some(i));
    let conditions = rules
        .filter(|i| match stage {
            Stage::Body(index) => *i == index,
            Stage::Hook { node, entry, .. } => {
                h.crosses(*i, node)
                    && h.configurations_compatible(
                        node,
                        if entry {
                            h.target(*i).map_or(node, |target| {
                                if m.transitions[*i].history {
                                    target
                                } else {
                                    h.leaf(target)
                                }
                            })
                        } else {
                            h.boundary(*i).0
                        },
                    )
            }
            Stage::History { target, .. } => h.crosses(*i, target),
            Stage::Departure { scope, .. } => h.crosses(*i, scope),
        })
        .map(|i| checker.read(&format!("_$machine_selected_{i}")));
    let enabled = or(conditions);
    let observe = if let Stage::Departure { scope, .. } = stage {
        checker.read(&format!("_$machine_observe_{}", h.name(m, scope)))
    } else {
        "true".into()
    };
    and([enabled, observe, "(< used_before limit)".into()])
}

fn key(checker: &Validator<'_>, event: Event) -> (u8, usize, usize, u8) {
    let h = checker.hierarchy;
    let m = checker.machine;
    match event {
        Event::Dispatch(Stage::Body(i)) | Event::Rule(i) | Event::Taken(i) => (
            3,
            m.transitions[i].span.start,
            0,
            match event {
                Event::Dispatch(_) => 0,
                Event::Rule(_) => 1,
                Event::Taken(_) => 2,
            },
        ),
        Event::Dispatch(Stage::Hook { node, entry, .. }) => {
            let (depth, span) = h.boundary_order_key(m, node, entry);
            (if entry { 4 } else { 0 }, depth, span, 0)
        }
        Event::Dispatch(Stage::History { .. }) => (1, 0, 0, 0),
        Event::Dispatch(Stage::Departure { scope, .. }) => {
            let (depth, span) = h.boundary_order_key(m, scope, false);
            (2, depth, span, 0)
        }
    }
}

fn obligations<'a>(
    mut checker: Validator<'a>,
    statements: &[Spanned<Stmt>],
    effects: &[Effect],
    result: &Spanned<Expr>,
    trigger: MachineTrigger,
) -> Validator<'a> {
    checker.smt.push_str("(declare-const used_before Int)\n(declare-const limit Int)\n(assert (and (<= 0 used_before) (<= used_before 9223372036854775807) (<= 1 limit) (<= limit 9223372036854775807)))\n");
    checker.vars.insert(
        Ident::new("_$machine_used"),
        Value::Int("used_before".into()),
    );
    checker
        .vars
        .insert(Ident::new("limit"), Value::Int("limit".into()));
    for (index, group) in checker.machine.composite_groups.iter().enumerate() {
        let name = format!("observe_{index}");
        writeln!(checker.smt, "(declare-const {name} Bool)").unwrap();
        checker.vars.insert(
            Ident::new(&format!("_$machine_observe_{}", group.name)),
            Value::Bool(name),
        );
    }
    let expected = stages(&checker, trigger);
    assert_eq!(
        effects
            .iter()
            .map(|effect| effect.stage)
            .collect::<HashSet<_>>(),
        expected,
        "machine execution: update dispatches do not cover the authored chart"
    );
    assert_eq!(
        effects.len(),
        expected.len(),
        "machine execution: duplicated update dispatch metadata"
    );
    let mut pass = Pass {
        checker: &mut checker,
        effects,
        alive: "true".into(),
        refusals: Vec::new(),
        events: Vec::new(),
        body: None,
    };
    for statement in statements {
        pass.statement(statement, "true");
    }
    let returned = Validator::bool(pass.expression(result, "true"));
    let alive = pass.alive.clone();
    let events = pass.events;
    let refusal = or(pass.refusals);
    for stage in expected {
        let path = expected_path(&checker, stage, trigger);
        let expected_events: Vec<_> = if let Stage::Body(i) = stage {
            vec![Event::Dispatch(stage), Event::Rule(i), Event::Taken(i)]
        } else {
            vec![Event::Dispatch(stage)]
        };
        for expected_event in expected_events {
            let count = events
                .iter()
                .filter(|(event, _)| *event == expected_event)
                .map(|(_, path)| format!("(ite {path} 1 0)"))
                .collect::<Vec<_>>()
                .join(" ");
            let count = if count.is_empty() {
                "0".into()
            } else {
                format!("(+ {count})")
            };
            checker.claim(
                format!("{expected_event:?}: exactly the enabled staged dispatch"),
                format!("(= {count} (ite {path} 1 0))"),
            );
        }
    }
    for (position, (event, path)) in events.iter().enumerate() {
        for (later, other) in &events[position + 1..] {
            if key(&checker, *event) > key(&checker, *later) {
                checker.claim(
                    format!("{event:?} precedes {later:?}: canonical phase and source order"),
                    not(&and([path.clone(), other.clone()])),
                );
            }
        }
    }
    let any = checker.read("_$machine_any");
    checker.claim(
        "pass result drives reaction-loop progress".into(),
        format!("(=> {alive} (= {returned} {any}))"),
    );
    checker.claim(
        "budget exhaustion refuses exactly a selected reaction".into(),
        format!("(= {refusal} (and {any} (>= used_before limit)))"),
    );
    let used = int(checker.vars[&Ident::new("_$machine_used")].clone());
    checker.claim(
        "each admitted reaction charges once, including parallel transitions".into(),
        format!("(= {used} (ite (and {any} (< used_before limit)) (+ used_before 1) used_before))"),
    );
    checker
}

fn mutate(statements: &mut [Spanned<Stmt>], effects: &[Effect], kind: usize) {
    if kind == 4 {
        statements[1..].reverse();
        return;
    }
    for (statement, _) in statements {
        if let Stmt::If {
            condition,
            then_block,
            ..
        } = statement
        {
            let stages: Vec<_> = then_block
                .stmts
                .iter()
                .filter_map(|stmt| {
                    effects
                        .iter()
                        .find(|effect| effect.span == stmt.1)
                        .map(|effect| effect.stage)
                })
                .collect();
            if (kind == 0 && stages.iter().any(|stage| matches!(stage, Stage::Body(_))))
                || (kind == 5
                    && stages
                        .iter()
                        .any(|stage| matches!(stage, Stage::Hook { .. })))
                || (kind == 6
                    && stages
                        .iter()
                        .any(|stage| matches!(stage, Stage::History { .. })))
            {
                condition.0 = Expr::Literal(Literal::Bool(kind == 0));
            }
            if kind == 7
                && stages
                    .iter()
                    .any(|stage| matches!(stage, Stage::Departure { .. }))
            {
                if let Expr::Binary { right, .. } = &mut condition.0 {
                    right.0 = Expr::Literal(Literal::Bool(true));
                }
            }
            if kind == 3 {
                then_block.stmts.retain(|(stmt,_)| !matches!(stmt,Stmt::Expression((Expr::MethodCall { receiver,.. },_)) if ident(receiver,"_$machine_rule")));
            }
            for (stmt, _) in &mut then_block.stmts {
                if kind == 1 {
                    if let Stmt::If { condition, .. } = stmt {
                        condition.0 = Expr::Literal(Literal::Bool(false));
                    }
                }
                if let Stmt::Assign { target, value, .. } = stmt {
                    if kind == 2 && ident(target, "_$machine_used") {
                        if let Expr::Binary { right, .. } = &mut value.0 {
                            right.0 = Expr::Literal(Literal::Integer {
                                value: 2,
                                radix: hew_parser::ast::IntRadix::Decimal,
                            });
                        }
                    }
                }
            }
        }
    }
}

fn reject_frame_reset(statements: &[Spanned<Stmt>]) {
    if REJECTED.load(Ordering::Relaxed) & (1 << 9) == 0 {
        if let Some(value) = statements.iter().find_map(|(stmt, _)| {
            if let Stmt::If { then_block, .. } = stmt {
                then_block.stmts.iter().find_map(|(stmt, _)| {
                    if let Stmt::Assign { target, value, .. } = stmt {
                        ident(target, "_$machine_work").then_some(value)
                    } else {
                        None
                    }
                })
            } else {
                None
            }
        }) {
            let write = (
                Stmt::Assign {
                    target: (Expr::Ident(Ident::new("_$machine_used")), 0..0),
                    op: None,
                    value: (
                        Expr::Literal(Literal::Integer {
                            value: 0,
                            radix: hew_parser::ast::IntRadix::Decimal,
                        }),
                        0..0,
                    ),
                },
                0..0,
            );
            let mutated = (
                Expr::Block(hew_parser::ast::Block {
                    stmts: vec![write],
                    trailing_expr: Some(Box::new(value.clone())),
                }),
                0..0,
            );
            let reason = super::frame::validate(&mutated)
                .expect_err("opaque update can reset the reaction budget");
            save("mutant-opaque-budget-reset", "counterexample", &reason);
            REJECTED.fetch_or(1 << 9, Ordering::Relaxed);
        }
    }
}

pub(super) fn verify(
    checker: Validator<'_>,
    statements: &[Spanned<Stmt>],
    returned_expr: &Spanned<Expr>,
    effects: &[Effect],
    trigger: MachineTrigger,
) {
    let baseline = checker.clone();
    let checker = obligations(checker, statements, effects, returned_expr, trigger);
    let query = query(&checker);
    let sequence = PASSES.fetch_add(1, Ordering::Relaxed);
    let name = format!(
        "execution-{sequence:03}-{}-{trigger:?}",
        checker.machine.name
    )
    .replace(['/', ' ', ':', '"', '{', '}'], "_");
    save(&name, "smt2", &query);
    let result = solve(&query);
    save(&name, "result", &result);
    if result.trim() != "sat\nunsat" {
        let model = if result.trim() == "sat\nsat" {
            counterexample(&checker, &query)
        } else {
            result.clone()
        };
        save(&name, "failure", &model);
        panic!(
            "machine {} {trigger:?}: execution validation failed: {model}",
            checker.machine.name
        );
    }
    CLAIMS.fetch_add(checker.claims.len(), Ordering::Relaxed);
    if std::env::var_os("HEW_MACHINE_MUTATIONS").is_some() {
        reject_frame_reset(statements);
        let names = [
            "unselected-update",
            "ignored-step-limit",
            "overcharged-reaction",
            "missing-rule-report",
            "wrong-phase-order",
            "suppressed-hook-dispatch",
            "missing-history-capture",
            "unrequested-departure",
            "suppressed-pass-result",
        ];
        for (kind, defect) in names.into_iter().enumerate() {
            if REJECTED.load(Ordering::Relaxed) & (1 << kind) != 0 {
                continue;
            }
            let mut mutated = statements.to_vec();
            mutate(&mut mutated, effects, kind);
            let mut returned = returned_expr.clone();
            if kind == 8 {
                returned.0 = Expr::Literal(Literal::Bool(false));
            }
            let mutant = obligations(baseline.clone(), &mutated, effects, &returned, trigger);
            let query = super::query(&mutant);
            let result = solve(&query);
            match result.trim() {
                "sat\nunsat" => {}
                "sat\nsat" => {
                    save(&format!("mutant-{defect}"), "smt2", &query);
                    save(
                        &format!("mutant-{defect}"),
                        "counterexample",
                        &counterexample(&mutant, &query),
                    );
                    REJECTED.fetch_or(1 << kind, Ordering::Relaxed);
                    eprintln!("Rejected execution defect {defect} with a counterexample");
                }
                _ => panic!("machine execution mutant {defect}: solver did not decide: {result}"),
            }
        }
    }
}

use std::fmt::Write as _;
