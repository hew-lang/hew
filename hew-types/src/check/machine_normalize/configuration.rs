//! Emit selection and staged configuration updates as ordinary value operations.

use super::{
    Block, Builder, Expr, Hierarchy, Ident, Literal, MachineDecl, MachineEvent, MachineState,
    MachineTrigger, MatchArm, NodeId, Pattern, Spanned, Stmt, TypeError, DISPOSITION, INPUT,
};

pub(super) const EVENT: &str = "_$machine_event";
const USED: &str = "_$machine_used";

pub(super) const WORK: &str = "_$machine_work";

#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum Focus {
    Predicate,
    Configuration(Option<NodeId>),
}

impl Builder {
    pub(super) fn configuration(
        &mut self,
        machine: &MachineDecl,
        hierarchy: &Hierarchy,
        node: NodeId,
    ) -> Result<MachineState, TypeError> {
        let name = hierarchy.name(machine, node);
        let mut fields = if let Some(region) = hierarchy.container(node) {
            hierarchy.region_state_fields(name, region)
        } else {
            hierarchy.inherited_fields(machine, node)?
        };
        for region in hierarchy.group_regions(machine, node) {
            let region_name = hierarchy.name(machine, region);
            fields.push((
                region_name,
                self.machine_region_ty(machine, name, region_name),
            ));
        }
        Ok(MachineState {
            name,
            is_final: hierarchy.is_final(machine, node),
            fields,
            entry: None,
            exit: None,
            span: self.origin.clone(),
        })
    }

    pub(super) fn source_configuration(
        &mut self,
        machine: &MachineDecl,
        hierarchy: &Hierarchy,
        node: NodeId,
    ) -> Result<MachineState, TypeError> {
        let mut state = self.configuration(machine, hierarchy, node)?;
        let mut inherited = hierarchy.inherited_fields(machine, node)?;
        inherited.extend(
            state
                .fields
                .iter()
                .filter(|(name, _)| !inherited.iter().any(|(known, _)| known == name))
                .cloned()
                .collect::<Vec<_>>(),
        );
        state.fields = inherited;
        Ok(state)
    }

    pub(super) fn assign(
        &mut self,
        name: impl crate::env::LexicalName,
        value: Spanned<Expr>,
    ) -> Spanned<Stmt> {
        let target = self.ident(name);
        self.stmt(Stmt::Assign {
            target,
            op: None,
            value,
        })
    }

    pub(super) fn boolean(&mut self, value: bool) -> Spanned<Expr> {
        self.expr(Expr::Literal(Literal::Bool(value)))
    }

    pub(super) fn if_do(
        &mut self,
        condition: Spanned<Expr>,
        stmts: Vec<Spanned<Stmt>>,
    ) -> Spanned<Stmt> {
        self.stmt(Stmt::If {
            condition,
            then_block: Block {
                stmts,
                trailing_expr: None,
            },
            else_block: None,
        })
    }

    fn not(&mut self, value: Spanned<Expr>) -> Spanned<Expr> {
        self.expr(Expr::Unary {
            op: hew_parser::ast::UnaryOp::Not,
            operand: Box::new(value),
        })
    }

    fn conjunction(&mut self, values: Vec<Spanned<Expr>>) -> Spanned<Expr> {
        values
            .into_iter()
            .reduce(|left, right| {
                self.expr(Expr::Binary {
                    op: hew_parser::ast::BinaryOp::And,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            })
            .unwrap_or_else(|| self.boolean(true))
    }

    pub(super) fn disjunction(&mut self, values: Vec<Spanned<Expr>>) -> Spanned<Expr> {
        values
            .into_iter()
            .reduce(|left, right| {
                self.expr(Expr::Binary {
                    op: hew_parser::ast::BinaryOp::Or,
                    left: Box::new(left),
                    right: Box::new(right),
                })
            })
            .unwrap_or_else(|| self.boolean(false))
    }

    /// Match one configuration path. Sibling regions remain ordinary fields;
    /// no branch enumerates combinations of regional states.
    pub(super) fn focus(
        &mut self,
        machine: &MachineDecl,
        hierarchy: &Hierarchy,
        node: NodeId,
        mut body: Spanned<Expr>,
        mode: Focus,
        shared_read_only: bool,
    ) -> Result<Spanned<Expr>, TypeError> {
        let path = hierarchy.configuration_path(node);
        for (depth, current) in path.iter().enumerate().rev() {
            let state = self.configuration(machine, hierarchy, *current)?;
            let pattern = self.pattern(state.name, &state.fields, "_$machine_in_");
            let mut locals = Vec::new();
            for (field, _) in &state.fields {
                let value = self.ident(format!("_$machine_in_{field}"));
                let name = format!("_$machine_state_{field}");
                if shared_read_only
                    && depth + 1 != path.len()
                    && !hierarchy
                        .group_regions(machine, *current)
                        .iter()
                        .any(|region| hierarchy.name(machine, *region) == *field)
                {
                    locals.push(self.let_value(name, None, value));
                } else {
                    locals.push(self.var(name, None, value));
                }
            }
            let rebuild = match mode {
                Focus::Predicate => false,
                Focus::Configuration(region) => region.is_some_and(|region| {
                    depth + 1 < path.len()
                        && hierarchy.ancestors(region).contains(
                            &hierarchy
                                .container(path[depth + 1])
                                .expect("nested configuration region"),
                        )
                }),
            };
            if rebuild {
                let region = hierarchy
                    .container(path[depth + 1])
                    .expect("nested configuration region");
                let field = hierarchy.name(machine, region);
                locals.push(self.assign(format!("_$machine_state_{field}"), body));
                body = self.state_value(machine, &state);
            }
            body = self.block(locals, body);
            let input = if depth == 0 {
                self.ident(WORK)
            } else {
                let region = hierarchy
                    .container(*current)
                    .expect("nested configuration region");
                self.ident(format!(
                    "_$machine_state_{}",
                    hierarchy.name(machine, region)
                ))
            };
            let fallback = match mode {
                Focus::Predicate => self.boolean(false),
                Focus::Configuration(Some(region)) if depth > 0 && !rebuild => self.ident(format!(
                    "_$machine_state_{}",
                    hierarchy.name(machine, region)
                )),
                Focus::Configuration(Some(_)) => input.clone(),
                Focus::Configuration(None) => self.ident(WORK),
            };
            let wildcard = (Pattern::Wildcard, self.span());
            body = self.expr(Expr::Match {
                scrutinee: Box::new(input),
                arms: vec![
                    MatchArm {
                        append_separator: None,
                        pattern,
                        guard: None,
                        body,
                    },
                    MatchArm {
                        append_separator: None,
                        pattern: wildcard,
                        guard: None,
                        body: fallback,
                    },
                ],
            });
        }
        Ok(body)
    }

    pub(super) fn configuration_step(
        &mut self,
        machine: &MachineDecl,
        hierarchy: &Hierarchy,
    ) -> Result<Block, TypeError> {
        let mut shell = self.step_body_shell(machine);
        shell.stmts.extend(self.observation_prelude(machine));
        let limit = self.ident("limit");
        let zero = self.integer(0);
        let invalid = self.binary(hew_parser::ast::BinaryOp::LessEqual, limit, zero);
        let failure = self.limit_failure("InvalidLimit");
        shell.stmts.push(self.if_do(invalid, vec![failure]));
        shell.stmts.extend(self.history_prelude(machine, hierarchy));
        let current = self.ident("self");
        let ty = self.machine_ty(machine, "");
        shell.stmts.push(self.var(WORK, Some(ty), current));
        let zero = self.integer(0);
        let ty = self.ty("i64");
        shell.stmts.push(self.var(USED, Some(ty), zero));
        let mut arms = Vec::new();
        for event in &machine.events {
            let pattern = self.pattern(event.name, &event.fields, "_$machine_event_");
            let input = self.expr(Expr::Literal(Literal::String(event.name.to_string())));
            let set_input = self.assign(INPUT, input);
            let pass = self.configuration_pass(
                machine,
                hierarchy,
                MachineTrigger::Event(event.name),
                Some(event),
            )?;
            let body = self.block(vec![set_input], pass);
            arms.push(MatchArm {
                append_separator: None,
                pattern,
                guard: None,
                body,
            });
        }
        let event = self.ident(EVENT);
        let dispatch = self.expr(Expr::Match {
            scrutinee: Box::new(event),
            arms,
        });
        shell.stmts.push(self.stmt(Stmt::Expression(dispatch)));
        let mut reactions = Vec::new();
        for trigger in [MachineTrigger::Completion, MachineTrigger::Always] {
            if machine
                .transitions
                .iter()
                .any(|rule| rule.trigger == trigger)
            {
                let pass = self.configuration_pass(machine, hierarchy, trigger, None)?;
                let again = self.stmt(Stmt::Continue { label: None });
                reactions.push(self.if_do(pass, vec![again]));
            }
        }
        if !reactions.is_empty() {
            reactions.push(self.stmt(Stmt::Break {
                label: None,
                value: None,
            }));
            shell.stmts.push(self.stmt(Stmt::Loop {
                label: None,
                body: Block {
                    stmts: reactions,
                    trailing_expr: None,
                },
            }));
        }
        let candidate = self.ident(WORK);
        self.finish_step(machine, &mut shell, candidate);
        let report = *shell.trailing_expr.take().expect("step report");
        let observed = self.observed_report(report);
        shell.trailing_expr = Some(Box::new(self.result("Ok", observed)));
        Ok(shell)
    }

    fn integer(&mut self, value: i128) -> Spanned<Expr> {
        self.expr(Expr::Literal(Literal::Integer {
            value,
            radix: hew_parser::ast::IntRadix::Decimal,
        }))
    }

    fn binary(
        &mut self,
        op: hew_parser::ast::BinaryOp,
        left: Spanned<Expr>,
        right: Spanned<Expr>,
    ) -> Spanned<Expr> {
        self.expr(Expr::Binary {
            op,
            left: Box::new(left),
            right: Box::new(right),
        })
    }

    fn result(&mut self, name: &str, value: Spanned<Expr>) -> Spanned<Expr> {
        let callee = self.variant(Ident::new(name), Vec::new(), Vec::new());
        self.expr(Expr::Call {
            function: Box::new(callee),
            args: vec![hew_parser::ast::CallArg::Positional(value)],
            type_args: None,
            is_tail_call: false,
        })
    }

    fn limit_failure(&mut self, name: &str) -> Spanned<Stmt> {
        let error = self.variant(Ident::new(name), Vec::new(), Vec::new());
        let result = self.result("Err", error);
        self.stmt(Stmt::Return(Some(result)))
    }

    /// Completion is structural: an exclusive child's final descendant, or all
    /// active regions of a parallel state. Focus tests inspect staged values.
    fn configuration_complete(
        &mut self,
        machine: &MachineDecl,
        hierarchy: &Hierarchy,
        node: NodeId,
    ) -> Result<Spanned<Expr>, TypeError> {
        let children = hierarchy.children(node);
        if children.is_empty() {
            let value = self.boolean(hierarchy.is_final(machine, node));
            return self.focus(machine, hierarchy, node, value, Focus::Predicate, true);
        }
        let children = children
            .into_iter()
            .map(|child| self.configuration_complete(machine, hierarchy, child))
            .collect::<Result<Vec<_>, _>>()?;
        Ok(if hierarchy.is_parallel(node) {
            self.conjunction(children)
        } else {
            self.disjunction(children)
        })
    }

    #[expect(
        clippy::too_many_lines,
        reason = "one selection and staged update path serves external and internal transition sets"
    )]
    fn configuration_pass(
        &mut self,
        machine: &MachineDecl,
        hierarchy: &Hierarchy,
        trigger: MachineTrigger,
        event: Option<&MachineEvent>,
    ) -> Result<Spanned<Expr>, TypeError> {
        let mut stmts = Vec::new();
        let ordered = hierarchy.candidate_order(machine, trigger);
        let mut authored = Vec::new();
        for index in &ordered {
            let value = self.boolean(false);
            stmts.push(self.var(format!("_$machine_selected_{index}"), None, value));
            let ordinal = hierarchy.ordinal(*index);
            if !authored.contains(&ordinal) {
                authored.push(ordinal);
                let value = self.boolean(false);
                stmts.push(self.var(format!("_$machine_seen_{ordinal}"), None, value));
            }
        }
        for (position, index) in ordered.iter().enumerate() {
            let rule = &machine.transitions[*index];
            let (source, _) = hierarchy.boundary(*index);
            let state = self.source_configuration(machine, hierarchy, source)?;
            let guard = if let Some(guard) = &rule.guard {
                self.rewrite_rule_part(guard, *index, machine, &state, event)?
            } else {
                self.boolean(true)
            };
            let seen = self.boolean(true);
            let seen = self.assign(
                format!("_$machine_seen_{}", hierarchy.ordinal(*index)),
                seen,
            );
            let guard = self.block(vec![seen], guard);
            let guard = self.focus(machine, hierarchy, source, guard, Focus::Predicate, true)?;
            let guard = if trigger == MachineTrigger::Completion {
                let complete = self.configuration_complete(
                    machine,
                    hierarchy,
                    hierarchy.completion_owner(*index),
                )?;
                self.conjunction(vec![complete, guard])
            } else {
                guard
            };
            let mut conditions = Vec::new();
            let seen = self.ident(format!("_$machine_seen_{}", hierarchy.ordinal(*index)));
            conditions.push(self.not(seen));
            for prior in &ordered[..position] {
                if hierarchy.conflicts(*index, *prior) {
                    let selected = self.ident(format!("_$machine_selected_{prior}"));
                    conditions.push(self.not(selected));
                }
            }
            let eligible = self.conjunction(conditions);
            let select = self.assign(format!("_$machine_selected_{index}"), guard);
            stmts.push(self.if_do(eligible, vec![select]));
        }
        let selections = ordered
            .iter()
            .map(|index| self.ident(format!("_$machine_selected_{index}")))
            .collect();
        let selected = self.disjunction(selections);
        stmts.push(self.let_value("_$machine_any", None, selected));
        let used = self.ident(USED);
        let limit = self.ident("limit");
        let exhausted = self.binary(hew_parser::ast::BinaryOp::GreaterEqual, used, limit);
        let failure = self.limit_failure("LimitExceeded");
        let refuse = self.if_do(exhausted, vec![failure]);
        let used = self.ident(USED);
        let one = self.integer(1);
        let increment = self.binary(hew_parser::ast::BinaryOp::Add, used, one);
        let charge = self.assign(USED, increment);
        let selected = self.ident("_$machine_any");
        stmts.push(self.if_do(selected, vec![refuse, charge]));
        self.configuration_hooks(machine, hierarchy, event, &ordered, false, &mut stmts)?;
        self.capture_history(machine, hierarchy, &ordered, &mut stmts)?;
        self.capture_departures(machine, hierarchy, &ordered, &mut stmts)?;
        let mut bodies = ordered.clone();
        bodies.sort_by_key(|index| machine.transitions[*index].span.start);
        for index in bodies {
            let rule = &machine.transitions[index];
            let (source, _) = hierarchy.boundary(index);
            let state = self.source_configuration(machine, hierarchy, source)?;
            if hierarchy.target(index).is_none() {
                self.configuration_root = Some(self.current_root(machine, hierarchy, source)?);
            }
            self.history_target = rule
                .history
                .then(|| hierarchy.target(index).expect("checked history target"));
            self.history_sites = if rule.history {
                super::history::destination_sites(&rule.body)
            } else {
                Vec::new()
            };
            let body = self.rewrite_rule_part(&rule.body, index, machine, &state, event);
            self.history_target = None;
            self.history_sites.clear();
            self.configuration_root = None;
            let body = body?;
            let target = hierarchy.target(index).map(|node| hierarchy.leaf(node));
            let region = hierarchy.write_region(index);
            let body = if let Some(target) = target {
                self.wrap_destination(machine, hierarchy, target, region, body)?
            } else {
                body
            };
            let body = self.focus(
                machine,
                hierarchy,
                source,
                body,
                Focus::Configuration(region),
                hierarchy.regional_owner(index),
            )?;
            let body = self.retain_history(machine, hierarchy, body)?;
            let update = self.assign(WORK, body);
            let selected = self.ident(format!("_$machine_selected_{index}"));
            let record = self.record_rule(hierarchy.ordinal(index));
            let taken = self.variant(Ident::new("Taken"), Vec::new(), Vec::new());
            let disposition = self.assign(DISPOSITION, taken);
            stmts.push(self.if_do(selected, vec![update, record, disposition]));
        }
        self.configuration_hooks(machine, hierarchy, event, &ordered, true, &mut stmts)?;

        let selected = self.ident("_$machine_any");
        Ok(self.block(stmts, selected))
    }

    fn configuration_hooks(
        &mut self,
        machine: &MachineDecl,
        hierarchy: &Hierarchy,
        event: Option<&MachineEvent>,
        rules: &[usize],
        entry: bool,
        stmts: &mut Vec<Spanned<Stmt>>,
    ) -> Result<(), TypeError> {
        let mut nodes = hierarchy.hook_nodes();
        nodes.sort_by_key(|node| hierarchy.boundary_order_key(machine, *node, entry));
        for node in nodes {
            let Some(hook) = hierarchy.hooks(machine, node, entry) else {
                continue;
            };
            let eligible = rules
                .iter()
                .filter(|index| {
                    hierarchy.crosses(**index, node)
                        && hierarchy.configurations_compatible(
                            node,
                            if entry {
                                hierarchy.target(**index).map_or(node, |target| {
                                    if machine.transitions[**index].history {
                                        target
                                    } else {
                                        hierarchy.leaf(target)
                                    }
                                })
                            } else {
                                hierarchy.boundary(**index).0
                            },
                        )
                })
                .copied()
                .collect::<Vec<_>>();
            if eligible.is_empty() {
                continue;
            }
            let conditions = eligible
                .iter()
                .map(|index| self.ident(format!("_$machine_selected_{index}")))
                .collect();
            let condition = self.disjunction(conditions);
            let mut updates = Vec::new();
            for config in hierarchy.configurations(hierarchy.container(node)) {
                if !hierarchy.ancestors(config).contains(&node) {
                    continue;
                }
                let state = self.source_configuration(machine, hierarchy, config)?;
                let hook = self.rewrite_block(hook, machine, &state, event)?;
                let value = self.state_value(machine, &state);
                let hook_expr = self.expr(Expr::Block(hook));
                let hook_stmt = self.stmt(Stmt::Expression(hook_expr));
                let body = self.block(vec![hook_stmt], value);
                let update = self.focus(
                    machine,
                    hierarchy,
                    config,
                    body,
                    Focus::Configuration(hierarchy.container(config)),
                    hierarchy.container(node).is_some(),
                )?;
                updates.push(self.assign(WORK, update));
            }
            stmts.push(self.if_do(condition, updates));
        }
        Ok(())
    }

    fn wrap_destination(
        &mut self,
        machine: &MachineDecl,
        hierarchy: &Hierarchy,
        target: NodeId,
        region_limit: Option<NodeId>,
        mut value: Spanned<Expr>,
    ) -> Result<Spanned<Expr>, TypeError> {
        let path = hierarchy.configuration_path(target);
        for pair in path.windows(2).rev() {
            if hierarchy.container(pair[1]) == region_limit {
                break;
            }
            let parent = pair[0];
            let region = hierarchy
                .container(pair[1])
                .expect("nested configuration region");
            let state = self.configuration(machine, hierarchy, parent)?;
            let mut fields = Vec::new();
            for (field, _) in &state.fields {
                if *field == hierarchy.name(machine, region) {
                    fields.push((*field, value.clone()));
                } else if let Some(sibling) = hierarchy
                    .group_regions(machine, parent)
                    .into_iter()
                    .find(|sibling| hierarchy.name(machine, *sibling) == *field)
                {
                    fields.push((*field, self.initial_region(machine, hierarchy, sibling)?));
                } else {
                    fields.push((*field, self.ident(format!("_$machine_state_{field}"))));
                }
            }
            value = self.qualified_variant(machine, state.name, fields);
        }
        Ok(value)
    }

    fn current_root(
        &mut self,
        machine: &MachineDecl,
        hierarchy: &Hierarchy,
        node: NodeId,
    ) -> Result<Spanned<Expr>, TypeError> {
        let state = self.configuration(machine, hierarchy, node)?;
        let mut value = self.state_value(machine, &state);
        for pair in hierarchy.configuration_path(node).windows(2).rev() {
            let parent = self.configuration(machine, hierarchy, pair[0])?;
            let region = hierarchy
                .container(pair[1])
                .expect("regional configuration");
            let name = hierarchy.name(machine, region);
            let mut fields = parent
                .fields
                .iter()
                .map(|(field, _)| {
                    (
                        *field,
                        if *field == name {
                            value.clone()
                        } else {
                            self.ident(format!("_$machine_state_{field}"))
                        },
                    )
                })
                .collect();
            self.carry_history(machine, parent.name, &mut fields);
            value = self.qualified_variant(machine, parent.name, fields);
        }
        Ok(value)
    }

    pub(super) fn initial_region(
        &mut self,
        machine: &MachineDecl,
        hierarchy: &Hierarchy,
        region: NodeId,
    ) -> Result<Spanned<Expr>, TypeError> {
        if let Some(value) = hierarchy.initial_value(machine, region) {
            let parent = hierarchy
                .parent(region)
                .expect("region has a compound parent");
            let mut value = value.clone();
            let declared = match &value.0 {
                Expr::Ident(name) => Some(*name),
                Expr::ContextVariant(value) => Some(value.name),
                Expr::StructInit { path, .. } => path.segments.last().map(|(name, _)| *name),
                _ => None,
            };
            if let Some(declared) = declared.filter(|_| {
                !hierarchy.has_initial_value(
                    machine,
                    hierarchy
                        .initial_child(region)
                        .expect("region initial child"),
                )
            }) {
                let initial = hierarchy.name(machine, hierarchy.leaf(region));
                super::super::machine_hierarchy::rewrite_target(
                    &mut value.0,
                    declared,
                    initial,
                    machine.name,
                );
            }
            self.rewrite_initializer(machine, hierarchy, parent, &value)
        } else {
            let child = hierarchy
                .initial_child(region)
                .expect("region initial child");
            if hierarchy.has_initial_value(machine, child) {
                self.complete_compound_constructor(
                    machine,
                    hierarchy.name(machine, child),
                    Vec::new(),
                )
                .map(|value| value.expect("exclusive initial constructor"))
            } else {
                self.initial_configuration(machine, hierarchy, hierarchy.leaf(region))
            }
        }
    }

    pub(super) fn complete_compound_constructor(
        &mut self,
        machine: &MachineDecl,
        name: Ident,
        supplied: Vec<(Ident, Spanned<Expr>)>,
    ) -> Result<Option<Spanned<Expr>>, TypeError> {
        let Some(group) = machine
            .composite_groups
            .iter()
            .find(|group| group.name == name)
        else {
            return Ok(None);
        };
        let hierarchy = self
            .configuration_hierarchy
            .clone()
            .expect("machine hierarchy");
        let node = hierarchy.resolve(name, self.origin.clone())?;
        let restore =
            self.history_target == Some(node) && self.history_sites.contains(&self.origin);
        if group.regions.is_empty() && !restore && !hierarchy.has_initial_value(machine, node) {
            return Ok(None);
        }
        let config = self.configuration(machine, &hierarchy, node)?;
        let mut stmts = Vec::new();
        let mut fields = Vec::new();
        // Bind explicit payload once before initializers read that staged payload.
        for (field, value) in supplied {
            let local = format!("_$machine_state_{field}");
            let ty = config
                .fields
                .iter()
                .find(|(name, _)| *name == field)
                .map(|(_, ty)| ty.clone());
            stmts.push(self.let_value(local.clone(), ty, value));
            fields.push((field, self.ident(local)));
        }
        if group.regions.is_empty() {
            for (field, _) in &config.fields {
                if !fields.iter().any(|(supplied, _)| supplied == field) {
                    return Err(self.error(format!(
                        "initial state `{name}` needs an explicit value for `{field}`"
                    )));
                }
            }
            let value = if restore {
                self.restore_history(machine, &hierarchy, node, fields)?
            } else {
                self.exclusive_initial(machine, &hierarchy, node, fields)?
            };
            return Ok(Some(self.block(stmts, value)));
        }
        for region in hierarchy.group_regions(machine, node) {
            let name = hierarchy.name(machine, region);
            if !fields.iter().any(|(field, _)| *field == name) {
                let value = if restore {
                    self.restore_history(machine, &hierarchy, region, Vec::new())?
                } else {
                    self.initial_region(machine, &hierarchy, region)?
                };
                fields.push((name, value));
            }
        }
        // Ordinary record checking diagnoses any missing explicit parent payload.
        let value = self.qualified_variant(machine, group.name, fields);
        Ok(Some(self.block(stmts, value)))
    }

    pub(super) fn exclusive_initial(
        &mut self,
        machine: &MachineDecl,
        hierarchy: &Hierarchy,
        node: NodeId,
        fields: Vec<(Ident, Spanned<Expr>)>,
    ) -> Result<Spanned<Expr>, TypeError> {
        let child = hierarchy
            .initial_child(node)
            .expect("exclusive initial child");
        let child_name = hierarchy.name(machine, child);
        let mut value = hierarchy
            .initial_value(machine, node)
            .cloned()
            .unwrap_or_else(|| self.variant(child_name, fields.clone(), Vec::new()));
        let child_fields = self
            .configuration(machine, hierarchy, hierarchy.leaf(child))?
            .fields;
        let inherited: Vec<_> = fields
            .into_iter()
            .filter(|(field, _)| child_fields.iter().any(|(name, _)| name == field))
            .collect();
        let add_inherited = |target: &mut Vec<(Ident, Spanned<Expr>)>| {
            for (field, value) in &inherited {
                if !target.iter().any(|(name, _)| name == field) {
                    target.push((*field, value.clone()));
                }
            }
        };
        match &mut value.0 {
            Expr::Ident(_) => value = self.variant(child_name, inherited, Vec::new()),
            Expr::ContextVariant(context) => {
                if let Some(record) = &mut context.record {
                    add_inherited(&mut record.fields);
                } else {
                    value = self.variant(child_name, inherited, Vec::new());
                }
            }
            Expr::StructInit { fields, .. } => add_inherited(fields),
            _ => unreachable!("parser requires an initial constructor"),
        }
        if !hierarchy.has_initial_value(machine, child) {
            super::super::machine_hierarchy::rewrite_target(
                &mut value.0,
                child_name,
                hierarchy.name(machine, hierarchy.leaf(child)),
                machine.name,
            );
        }
        self.rewrite_initializer(machine, hierarchy, node, &value)
    }

    fn rewrite_initializer(
        &mut self,
        machine: &MachineDecl,
        hierarchy: &Hierarchy,
        parent: NodeId,
        value: &Spanned<Expr>,
    ) -> Result<Spanned<Expr>, TypeError> {
        let mut scope = self.source_configuration(machine, hierarchy, parent)?;
        scope.fields = hierarchy.inherited_fields(machine, parent)?;
        let was_initializing = self.initializing;
        self.initializing = true;
        let result = self.rewrite(value, machine, &scope, None);
        self.initializing = was_initializing;
        result
    }

    pub(super) fn initial_configuration(
        &mut self,
        machine: &MachineDecl,
        hierarchy: &Hierarchy,
        node: NodeId,
    ) -> Result<Spanned<Expr>, TypeError> {
        let state = self.configuration(machine, hierarchy, node)?;
        let mut fields = Vec::new();
        for (field, _) in &state.fields {
            let region = hierarchy
                .group_regions(machine, node)
                .into_iter()
                .find(|region| hierarchy.name(machine, *region) == *field)
                .ok_or_else(|| {
                    self.error(format!(
                        "initial state `{}` needs an explicit value for `{field}`",
                        state.name
                    ))
                })?;
            fields.push((*field, self.initial_region(machine, hierarchy, region)?));
        }
        Ok(self.qualified_variant(machine, state.name, fields))
    }
}
