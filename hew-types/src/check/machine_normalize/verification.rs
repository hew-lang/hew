//! Translation validation of the actual emitted selection AST. Guard bodies
//! are uninterpreted pure Boolean inputs; focus, short-circuiting, assignments
//! and their evaluation paths are interpreted rather than replaced by a model.
use std::collections::HashMap;
use std::fmt::Write as _;
use std::io::Write;
use std::process::{Command, Stdio};
use std::sync::atomic::{AtomicUsize, Ordering};

static PASSES: AtomicUsize = AtomicUsize::new(0);
static CLAIMS: AtomicUsize = AtomicUsize::new(0);
static REJECTED: AtomicUsize = AtomicUsize::new(0);

#[derive(Debug)]
pub struct Statistics {
    pub selection_passes: usize,
    pub selection_claims: usize,
    pub selection_defects: usize,
    pub charts: usize,
    pub chart_defects: usize,
    pub execution_passes: usize,
    pub execution_claims: usize,
    pub execution_defects: usize,
    pub steps: usize,
    pub step_claims: usize,
    pub step_defects: usize,
}
pub fn statistics() -> Statistics {
    Statistics {
        selection_passes: PASSES.load(Ordering::Relaxed),
        selection_claims: CLAIMS.load(Ordering::Relaxed),
        selection_defects: REJECTED.load(Ordering::Relaxed).count_ones() as usize,
        charts: chart::CHARTS.load(Ordering::Relaxed),
        chart_defects: chart::REJECTED.load(Ordering::Relaxed).count_ones() as usize,
        execution_passes: execution::PASSES.load(Ordering::Relaxed),
        execution_claims: execution::CLAIMS.load(Ordering::Relaxed),
        execution_defects: execution::REJECTED.load(Ordering::Relaxed).count_ones() as usize,
        steps: step::STEPS.load(Ordering::Relaxed),
        step_claims: step::CLAIMS.load(Ordering::Relaxed),
        step_defects: step::REJECTED.load(Ordering::Relaxed).count_ones() as usize,
    }
}

use hew_parser::ast::{BinaryOp, NominalPatternPayload, UnaryOp};

use super::{
    Expr, Hierarchy, Ident, Literal, MachineDecl, MachineTrigger, Pattern, Span, Spanned, Stmt,
};
use crate::check::machine_hierarchy::NodeId;

mod chart;
pub(super) use chart::verify_chart;
mod execution;
mod frame;
pub(super) use execution::{Effect, Stage};
mod step;
pub(super) use step::verify_step;

#[derive(Clone)]
enum Value {
    Bool(String),
    Int(String),
    Configuration(Option<NodeId>),
    Data,
}

#[derive(Clone)]
struct Validator<'a> {
    machine: &'a MachineDecl,
    observed: bool,
    hierarchy: &'a Hierarchy,
    smt: String,
    fresh: usize,
    containers: Vec<Option<NodeId>>,
    vars: HashMap<Ident, Value>,
    atoms: &'a [(usize, Span)],
    evaluated: HashMap<usize, String>,
    claims: Vec<(String, String)>,
}

fn and(values: impl IntoIterator<Item = String>) -> String {
    let values: Vec<_> = values.into_iter().collect();
    if values.is_empty() {
        "true".into()
    } else {
        format!("(and {})", values.join(" "))
    }
}
fn or(values: impl IntoIterator<Item = String>) -> String {
    let values: Vec<_> = values.into_iter().collect();
    if values.is_empty() {
        "false".into()
    } else {
        format!("(or {})", values.join(" "))
    }
}
fn not(value: &str) -> String {
    format!("(not {value})")
}

impl Validator<'_> {
    fn term(&mut self, expression: impl AsRef<str>) -> String {
        let expression = expression.as_ref();
        let name = format!("b{}", self.fresh);
        self.fresh += 1;
        writeln!(self.smt, "(define-fun {name} () Bool {expression})").unwrap();
        name
    }

    fn claim(&mut self, label: String, proposition: String) {
        let term = self.term(proposition);
        self.claims.push((label, term));
    }

    fn bool(value: Value) -> String {
        let Value::Bool(value) = value else {
            panic!("machine verifier: expected Boolean")
        };
        value
    }

    fn read(&self, name: &str) -> String {
        Self::bool(
            self.vars
                .get(&Ident::new(name))
                .unwrap_or_else(|| panic!("machine verifier: unbound {name}"))
                .clone(),
        )
    }

    fn tag(&self, container: Option<NodeId>, node: NodeId) -> String {
        let container_index = self
            .containers
            .iter()
            .position(|item| *item == container)
            .expect("known configuration type");
        let configurations = self.hierarchy.configurations(container);
        let value = configurations
            .iter()
            .position(|item| *item == node)
            .expect("configuration belongs to scrutinee type");
        format!("(= tag_{container_index} {value})")
    }

    fn active(&self, node: NodeId) -> String {
        and(self
            .hierarchy
            .configuration_path(node)
            .into_iter()
            .map(|node| self.tag(self.hierarchy.container(node), node)))
    }

    fn complete(&self, node: NodeId) -> String {
        let children = self.hierarchy.children(node);
        if children.is_empty() {
            if self.hierarchy.is_final(self.machine, node) {
                self.active(node)
            } else {
                "false".into()
            }
        } else if self.hierarchy.is_parallel(node) {
            and(children.into_iter().map(|child| self.complete(child)))
        } else {
            or(children.into_iter().map(|child| self.complete(child)))
        }
    }

    fn assign(&mut self, name: Ident, value: Value, path: &str, declaration: bool) {
        let value = match value {
            Value::Bool(value) if !declaration => {
                let prior = Self::bool(
                    self.vars
                        .get(&name)
                        .expect("assignment to declared selection local")
                        .clone(),
                );
                Value::Bool(self.term(format!("(ite {path} {value} {prior})")))
            }
            value => value,
        };
        self.vars.insert(name, value);
    }

    fn stmt(&mut self, statement: &Spanned<Stmt>, path: &str) {
        match &statement.0 {
            Stmt::Var {
                name,
                value: Some(value),
                ..
            }
            | Stmt::Let {
                pattern: (Pattern::Identifier(name), _),
                value: Some(value),
                else_block: None,
                ..
            } => {
                let value = self.expr(value, path);
                self.assign(*name, value, path, true);
            }
            Stmt::Assign {
                target,
                value,
                op: None,
            } => {
                let Expr::Ident(name) = target.0 else {
                    panic!("machine verifier: nonlocal selection write")
                };
                assert!(
                    name.name.as_str().starts_with("_$machine_seen_")
                        || name.name.as_str().starts_with("_$machine_selected_"),
                    "machine verifier: selection mutated machine data"
                );
                let value = self.expr(value, path);
                self.assign(name, value, path, false);
            }
            Stmt::If {
                condition,
                then_block,
                else_block: None,
            } => {
                let condition = Self::bool(self.expr(condition, path));
                let path = self.term(and([path.into(), condition]));
                for stmt in &then_block.stmts {
                    self.stmt(stmt, &path);
                }
                assert!(
                    then_block.trailing_expr.is_none(),
                    "machine verifier: unexpected statement result"
                );
            }
            _ => panic!(
                "machine verifier: unsupported selection statement: {:?}",
                statement.0
            ),
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "closed AST interpreter keeps short-circuit effects explicit"
    )]
    fn expr(&mut self, expression: &Spanned<Expr>, path: &str) -> Value {
        if let Some((index, _)) = self.atoms.iter().find(|(_, span)| *span == expression.1) {
            frame::validate(expression)
                .unwrap_or_else(|reason| panic!("machine guard frame: {reason}"));
            let index = *index;
            let previous = self
                .evaluated
                .get(&index)
                .cloned()
                .unwrap_or_else(|| "false".into());
            let twice = self.term(and([previous.clone(), path.into()]));
            self.claim(format!("guard {index} evaluated twice"), not(&twice));
            let evaluated = self.term(or([previous, path.into()]));
            self.evaluated.insert(index, evaluated);
            return Value::Bool(format!("guard_{index}"));
        }
        match &expression.0 {
            Expr::Literal(Literal::Bool(value)) => Value::Bool(value.to_string()),
            Expr::Ident(name) => self
                .vars
                .get(name)
                .unwrap_or_else(|| panic!("machine verifier: unbound {name}"))
                .clone(),
            Expr::Unary {
                op: UnaryOp::Not,
                operand,
            } => {
                let value = Self::bool(self.expr(operand, path));
                Value::Bool(self.term(not(&value)))
            }
            Expr::Binary {
                op: op @ (BinaryOp::And | BinaryOp::Or),
                left,
                right,
            } => {
                let left = Self::bool(self.expr(left, path));
                let condition = if *op == BinaryOp::And {
                    left.clone()
                } else {
                    not(&left)
                };
                let right_path = self.term(and([path.into(), condition]));
                let right = Self::bool(self.expr(right, &right_path));
                Value::Bool(self.term(if *op == BinaryOp::And {
                    and([left, right])
                } else {
                    or([left, right])
                }))
            }
            Expr::Block(block) => {
                for stmt in &block.stmts {
                    self.stmt(stmt, path);
                }
                self.expr(
                    block
                        .trailing_expr
                        .as_deref()
                        .expect("selection block has result"),
                    path,
                )
            }
            Expr::Match { scrutinee, arms } => {
                let Value::Configuration(container) = self.expr(scrutinee, path) else {
                    panic!("machine verifier: focus scrutinee is not a configuration")
                };
                assert_eq!(arms.len(), 2, "machine verifier: unexpected focus arms");
                assert!(
                    arms.iter().all(|arm| arm.guard.is_none()),
                    "machine verifier: unexpected match guard"
                );
                let Pattern::ContextVariant(pattern) = &arms[0].pattern.0 else {
                    panic!("machine verifier: expected configuration pattern")
                };
                assert!(
                    matches!(arms[1].pattern.0, Pattern::Wildcard),
                    "machine verifier: missing focus fallback"
                );
                let node = self
                    .hierarchy
                    .configurations(container)
                    .into_iter()
                    .find(|node| self.hierarchy.name(self.machine, *node) == pattern.name)
                    .expect("pattern variant belongs to scrutinee");
                if let Some(payload) = &pattern.payload {
                    let NominalPatternPayload::Record { fields, rest: None } = payload else {
                        panic!("machine verifier: unexpected focus payload")
                    };
                    for field in fields {
                        let Some((Pattern::Identifier(binding), _)) = &field.pattern else {
                            panic!("machine verifier: unexpected focus binding")
                        };
                        let value = self
                            .hierarchy
                            .group_regions(self.machine, node)
                            .into_iter()
                            .find(|region| self.hierarchy.name(self.machine, *region) == field.name)
                            .map_or(Value::Data, |region| Value::Configuration(Some(region)));
                        self.vars.insert(*binding, value);
                    }
                }
                let condition = self.tag(container, node);
                let yes_path = self.term(and([path.into(), condition.clone()]));
                let yes = Self::bool(self.expr(&arms[0].body, &yes_path));
                let no_path = self.term(and([path.into(), not(&condition)]));
                let no = Self::bool(self.expr(&arms[1].body, &no_path));
                Value::Bool(self.term(format!("(ite {condition} {yes} {no})")))
            }
            _ => panic!(
                "machine verifier: unsupported selection expression: {:?}",
                expression.0
            ),
        }
    }
}

fn solve(query: &str) -> String {
    let executable = std::env::var_os("HEW_Z3").unwrap_or_else(|| "z3".into());
    let mut child = Command::new(executable)
        .args(["-in", "-smt2"])
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()
        .expect("machine verification needs Z3 (or HEW_Z3)");
    child
        .stdin
        .take()
        .expect("solver stdin")
        .write_all(query.as_bytes())
        .expect("send SMT query");
    let result = child.wait_with_output().expect("wait for Z3");
    assert!(
        result.status.success(),
        "machine verifier: solver failed: {} {}",
        String::from_utf8_lossy(&result.stdout),
        String::from_utf8_lossy(&result.stderr)
    );
    String::from_utf8(result.stdout).expect("solver output is UTF-8")
}

#[expect(
    clippy::too_many_lines,
    reason = "one reference selection pass states its complete postconditions"
)]
fn obligations<'a>(
    machine: &'a MachineDecl,
    hierarchy: &'a Hierarchy,
    trigger: MachineTrigger,
    statements: &[Spanned<Stmt>],
    atoms: &'a [(usize, Span)],
) -> Validator<'a> {
    let mut containers = vec![None];
    for group in hierarchy.parallel_groups(machine) {
        containers.extend(
            hierarchy
                .group_regions(machine, group)
                .into_iter()
                .map(Some),
        );
    }
    let mut checker = Validator {
        machine,
        observed: false,
        hierarchy,
        smt: "(set-logic QF_LIA)\n(set-option :timeout 30000)\n".into(),
        fresh: 0,
        containers,
        vars: HashMap::from([(
            Ident::new(super::configuration::WORK),
            Value::Configuration(None),
        )]),
        atoms,
        evaluated: HashMap::new(),
        claims: Vec::new(),
    };
    for (index, container) in checker.containers.iter().enumerate() {
        let count = hierarchy.configurations(*container).len();
        assert!(count > 0, "machine verifier: empty configuration type");
        writeln!(checker.smt, "(declare-const tag_{index} Int)\n(assert (and (<= 0 tag_{index}) (< tag_{index} {count})))").unwrap();
    }
    for (index, _) in atoms {
        writeln!(checker.smt, "(declare-const guard_{index} Bool)").unwrap();
    }
    for statement in statements {
        checker.stmt(statement, "true");
    }

    // The reference visits every matching declaration independently of the
    // emitter's candidate_order. Specific sources precede enclosing sources;
    // equal specificity is resolved by authored source order.
    let mut ordered: Vec<_> = machine
        .transitions
        .iter()
        .enumerate()
        .filter_map(|(index, rule)| (rule.trigger == trigger).then_some(index))
        .collect();
    ordered.sort_by_key(|index| {
        (
            std::cmp::Reverse(
                hierarchy
                    .selection_owner(*index)
                    .map_or(0, |owner| hierarchy.ancestors(owner).len()),
            ),
            machine.transitions[*index].span.start,
        )
    });
    let mut seen: HashMap<usize, String> = HashMap::new();
    let mut chosen: Vec<(usize, String)> = Vec::new();
    for index in &ordered {
        let ordinal = hierarchy.ordinal(*index);
        let previous = seen
            .get(&ordinal)
            .cloned()
            .unwrap_or_else(|| "false".into());
        let mut blockers = vec![not(&previous)];
        blockers.extend(
            chosen
                .iter()
                .filter(|(prior, _)| hierarchy.conflicts(*index, *prior))
                .map(|(_, selected)| not(selected)),
        );
        let (source, _) = hierarchy.boundary(*index);
        blockers.push(checker.active(source));
        if trigger == MachineTrigger::Completion {
            blockers.push(checker.complete(hierarchy.completion_owner(*index)));
        }
        let evaluated = checker.term(and(blockers));
        let guard = if machine.transitions[*index].guard.is_some() {
            let actual = checker
                .evaluated
                .get(index)
                .expect("authored guard occurs in emitted AST")
                .clone();
            checker.claim(
                format!("guard {index} eligibility"),
                format!("(= {actual} {evaluated})"),
            );
            format!("guard_{index}")
        } else {
            "true".into()
        };
        let expected = checker.term(and([evaluated.clone(), guard]));
        let actual = checker.read(&format!("_$machine_selected_{index}"));
        checker.claim(
            format!("transition {index} priority, enabling and progress"),
            format!("(= {actual} {expected})"),
        );
        for (prior, _) in &chosen {
            if hierarchy.conflicts(*index, *prior) {
                let other = checker.read(&format!("_$machine_selected_{prior}"));
                checker.claim(
                    format!("transitions {prior}/{index} conflict exclusion"),
                    not(&and([other, actual.clone()])),
                );
            }
        }
        chosen.push((*index, expected));
        seen.insert(ordinal, checker.term(or([previous, evaluated])));
    }
    for (ordinal, expected) in seen {
        let actual = checker.read(&format!("_$machine_seen_{ordinal}"));
        checker.claim(
            format!("authored rule {ordinal} evaluated once"),
            format!("(= {actual} {expected})"),
        );
    }
    let any = checker.read("_$machine_any");
    let expected = or(ordered
        .iter()
        .map(|index| checker.read(&format!("_$machine_selected_{index}"))));
    checker.claim(
        "reaction availability".into(),
        format!("(= {any} {expected})"),
    );
    if !machine.has_default && matches!(trigger, MachineTrigger::Event(_)) {
        checker.claim(
            "a machine without a default handles every configuration and guard result".into(),
            any,
        );
    }
    checker
}

fn query(checker: &Validator<'_>) -> String {
    let violation = or(checker.claims.iter().map(|(_, claim)| not(claim)));
    format!(
        "{}(check-sat)\n(assert {violation})\n(check-sat)\n",
        checker.smt
    )
}

fn save(name: &str, suffix: &str, contents: &str) {
    if let Some(directory) = std::env::var_os("HEW_MACHINE_PROOF_DIR") {
        let directory = std::path::PathBuf::from(directory);
        std::fs::create_dir_all(&directory).expect("create machine evidence directory");
        std::fs::write(directory.join(format!("{name}.{suffix}")), contents)
            .expect("write machine proof evidence");
    }
}

fn counterexample(checker: &Validator<'_>, query: &str) -> String {
    let values = checker
        .claims
        .iter()
        .map(|(_, term)| term.as_str())
        .collect::<Vec<_>>()
        .join(" ");
    let model = solve(&format!("{query}(get-value ({values}))\n(get-model)\n"));
    let configurations: Vec<_> = checker
        .containers
        .iter()
        .enumerate()
        .map(|(index, container)| {
            let variants: Vec<_> = checker
                .hierarchy
                .configurations(*container)
                .into_iter()
                .map(|node| checker.hierarchy.name(checker.machine, node).to_string())
                .collect();
            (format!("tag_{index}"), variants)
        })
        .collect();
    format!(
        "configuration tags (zero-based): {configurations:#?}\nclaims: {:#?}\n{model}",
        checker.claims
    )
}

// Mutate the emitted AST, never the reference obligations. A defect counts as
// rejected only for SAT with a concrete violating valuation, not for a crash,
// an unsupported AST node, a solver timeout or a malformed query.
fn mutate_expr(expression: &mut Spanned<Expr>) {
    match &mut expression.0 {
        Expr::Block(block) => {
            for (stmt, _) in &mut block.stmts {
                match stmt {
                    Stmt::Assign { target, value, .. } if matches!(&target.0, Expr::Ident(name) if name.name.as_str().starts_with("_$machine_seen_")) =>
                    {
                        value.0 = Expr::Literal(Literal::Bool(false));
                    }
                    _ => {}
                }
            }
            if let Some(value) = &mut block.trailing_expr {
                mutate_expr(value);
            }
        }
        Expr::Match { arms, .. } => {
            for arm in arms {
                mutate_expr(&mut arm.body);
            }
        }
        Expr::Binary { left, right, .. } => {
            mutate_expr(left);
            mutate_expr(right);
        }
        _ => {}
    }
}

fn mutate(statements: &mut [Spanned<Stmt>], kind: usize) {
    if kind == 5 {
        let positions: Vec<_> = statements
            .iter()
            .enumerate()
            .filter_map(|(index, (stmt, _))| matches!(stmt, Stmt::If { .. }).then_some(index))
            .collect();
        for pair in 0..positions.len() / 2 {
            statements.swap(positions[pair], positions[positions.len() - pair - 1]);
        }
        return;
    }
    for (statement, _) in statements {
        match statement {
            Stmt::Var {
                name,
                value: Some(value),
                ..
            } if kind == 0 && name.name.as_str().starts_with("_$machine_selected_") => {
                value.0 = Expr::Literal(Literal::Bool(true));
            }
            Stmt::If {
                condition,
                then_block,
                ..
            } => {
                if kind == 1 {
                    condition.0 = Expr::Literal(Literal::Bool(true));
                }
                for (stmt, _) in &mut then_block.stmts {
                    if let Stmt::Assign { value, .. } = stmt {
                        if let (
                            6,
                            Expr::Binary {
                                op: BinaryOp::And,
                                left,
                                ..
                            },
                        ) = (kind, &mut value.0)
                        {
                            // Completion is the left operand of the
                            // emitted complete && focused_guard expression.
                            left.0 = Expr::Literal(Literal::Bool(true));
                        }
                        if kind == 2 {
                            let right = (Expr::Literal(Literal::Bool(false)), 0..0);
                            value.0 = Expr::Binary {
                                op: BinaryOp::And,
                                left: Box::new(value.clone()),
                                right: Box::new(right),
                            };
                        }
                        if kind == 3 {
                            mutate_expr(value);
                        }
                    }
                }
            }
            Stmt::Let {
                pattern: (Pattern::Identifier(name), _),
                value: Some(value),
                ..
            } if kind == 4 && *name == Ident::new("_$machine_any") => {
                value.0 = Expr::Literal(Literal::Bool(false));
            }
            _ => {}
        }
    }
}

#[expect(
    clippy::too_many_arguments,
    reason = "validate the emitted pass with its source relation, guard and effect witnesses"
)]
pub(super) fn verify(
    machine: &MachineDecl,
    hierarchy: &Hierarchy,
    trigger: MachineTrigger,
    pass: &Spanned<Expr>,
    atoms: &[(usize, Span)],
    selection_end: usize,
    effects: &[Effect],
    observed: bool,
) {
    let Expr::Block(block) = &pass.0 else {
        panic!("machine pass is a value block")
    };
    let statements = &block.stmts;
    let result = block
        .trailing_expr
        .as_deref()
        .expect("machine pass has a result");
    let mut checker = obligations(
        machine,
        hierarchy,
        trigger,
        &statements[..selection_end],
        atoms,
    );
    checker.observed = observed;
    execution::verify(
        checker.clone(),
        &statements[selection_end..],
        result,
        effects,
        trigger,
    );
    PASSES.fetch_add(1, Ordering::Relaxed);
    CLAIMS.fetch_add(checker.claims.len(), Ordering::Relaxed);
    eprintln!(
        "Verified machine {} {trigger:?}: {} transitions, {} obligations",
        machine.name,
        hierarchy.candidate_order(machine, trigger).len(),
        checker.claims.len()
    );
    if std::env::var_os("HEW_MACHINE_MUTATIONS").is_some() {
        let names = [
            "preselected-transition",
            "ignored-conflict-priority",
            "suppressed-progress",
            "forgot-authored-rule",
            "false-quiescence",
            "reversed-priority",
            "premature-completion",
        ];
        for (kind, defect) in names.into_iter().enumerate() {
            if kind == 6 && trigger != MachineTrigger::Completion {
                continue;
            }
            if REJECTED.load(Ordering::Relaxed) & (1 << kind) != 0 {
                continue;
            }
            let mut mutated = statements[..selection_end].to_vec();
            mutate(&mut mutated, kind);
            let mutated = obligations(machine, hierarchy, trigger, &mutated, atoms);
            let query = self::query(&mutated);
            let result = solve(&query);
            match result.trim() {
                "sat\nunsat" => {} // This particular chart does not expose the defect.
                "sat\nsat" => {
                    let model = counterexample(&mutated, &query);
                    save(&format!("mutant-{defect}"), "smt2", &query);
                    save(&format!("mutant-{defect}"), "counterexample", &model);
                    REJECTED.fetch_or(1 << kind, Ordering::Relaxed);
                    eprintln!("Rejected machine defect {defect} with a counterexample");
                }
                _ => panic!("machine mutant {defect}: solver did not decide: {result}"),
            }
        }
    }
}
