//! Emit selection and staged configuration updates as ordinary value operations.

use super::{
    Block, Builder, Expr, Hierarchy, Ident, Literal, MachineDecl, MachineEvent, MachineState,
    MatchArm, NodeId, Pattern, Spanned, Stmt, TypeError, DISPOSITION, INPUT,
};

const WORK: &str = "_$machine_work";

#[derive(Clone, Copy, PartialEq, Eq)]
enum Focus {
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
            fields,
            entry: None,
            exit: None,
            span: self.origin.clone(),
        })
    }

    fn source_configuration(
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

    fn boolean(&mut self, value: bool) -> Spanned<Expr> {
        self.expr(Expr::Literal(Literal::Bool(value)))
    }

    fn if_do(&mut self, condition: Spanned<Expr>, stmts: Vec<Spanned<Stmt>>) -> Spanned<Stmt> {
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

    fn disjunction(&mut self, values: Vec<Spanned<Expr>>) -> Spanned<Expr> {
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
    fn focus(
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

    #[expect(
        clippy::too_many_lines,
        reason = "emit one event transaction with selection, exits, bodies and entries in their shared staging scope"
    )]
    pub(super) fn configuration_step(
        &mut self,
        machine: &MachineDecl,
        hierarchy: &Hierarchy,
    ) -> Result<Block, TypeError> {
        // Reuse the ordinary report/commit shell. Only its dispatch expression
        // differs: all selected configuration writes share this staging value.
        let mut shell = self.step_body_shell(machine);
        let mut arms = Vec::new();
        for event in &machine.events {
            let pattern = self.pattern(event.name, &event.fields, "_$machine_event_");
            let mut stmts = Vec::new();
            let input = self.expr(Expr::Literal(Literal::String(event.name.to_string())));
            stmts.push(self.assign(INPUT, input));
            let current = self.ident("self");
            let ty = self.machine_ty(machine, "");
            stmts.push(self.var(WORK, Some(ty), current));
            let ordered = hierarchy.candidate_order(machine, event.name);
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
                let guard =
                    self.focus(machine, hierarchy, source, guard, Focus::Predicate, true)?;
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
            self.configuration_hooks(machine, hierarchy, event, &ordered, false, &mut stmts)?;
            let mut bodies = ordered.clone();
            bodies.sort_by_key(|index| machine.transitions[*index].span.start);
            for index in bodies {
                let rule = &machine.transitions[index];
                let (source, _) = hierarchy.boundary(index);
                let state = self.source_configuration(machine, hierarchy, source)?;
                if hierarchy.target(index).is_none() {
                    self.configuration_root = Some(self.current_root(machine, hierarchy, source)?);
                }
                let body = self.rewrite_rule_part(&rule.body, index, machine, &state, event);
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
                let update = self.assign(WORK, body);
                let selected = self.ident(format!("_$machine_selected_{index}"));
                let record = self.record_rule(hierarchy.ordinal(index));
                let taken = self.variant(Ident::new("Taken"), Vec::new(), Vec::new());
                let disposition = self.assign(DISPOSITION, taken);
                stmts.push(self.if_do(selected, vec![update, record, disposition]));
            }
            self.configuration_hooks(machine, hierarchy, event, &ordered, true, &mut stmts)?;
            let value = self.ident(WORK);
            let body = self.block(stmts, value);
            arms.push(MatchArm {
                append_separator: None,
                pattern,
                guard: None,
                body,
            });
        }
        let event = self.ident("event");
        let dispatch = self.expr(Expr::Match {
            scrutinee: Box::new(event),
            arms,
        });
        self.finish_step(machine, &mut shell, dispatch);
        Ok(shell)
    }

    fn configuration_hooks(
        &mut self,
        machine: &MachineDecl,
        hierarchy: &Hierarchy,
        event: &MachineEvent,
        rules: &[usize],
        entry: bool,
        stmts: &mut Vec<Spanned<Stmt>>,
    ) -> Result<(), TypeError> {
        let mut nodes = hierarchy.hook_nodes();
        nodes.sort_by_key(|node| {
            let depth = hierarchy.ancestors(*node).len();
            (
                if entry { depth } else { usize::MAX - depth },
                hierarchy.span(machine, *node).start,
            )
        });
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
                                hierarchy
                                    .target(**index)
                                    .map_or(node, |target| hierarchy.leaf(target))
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
            let fields = parent
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
            value = self.qualified_variant(machine, parent.name, fields);
        }
        Ok(value)
    }

    fn initial_region(
        &mut self,
        machine: &MachineDecl,
        hierarchy: &Hierarchy,
        region: NodeId,
    ) -> Result<Spanned<Expr>, TypeError> {
        if let Some(value) = hierarchy.initial_value(machine, region) {
            let parent = hierarchy
                .parent(region)
                .expect("region has a compound parent");
            let mut state = self.source_configuration(machine, hierarchy, parent)?;
            state.fields = hierarchy.inherited_fields(machine, parent)?;
            let event = MachineEvent {
                name: Ident::new("initial"),
                fields: Vec::new(),
                span: self.origin.clone(),
            };
            let mut value = value.clone();
            let declared = match &value.0 {
                Expr::Ident(name) => Some(*name),
                Expr::ContextVariant(value) => Some(value.name),
                Expr::StructInit { path, .. } => path.segments.last().map(|(name, _)| *name),
                _ => None,
            };
            if let Some(declared) = declared {
                let initial = hierarchy.name(machine, hierarchy.leaf(region));
                super::super::machine_hierarchy::rewrite_target(
                    &mut value.0,
                    declared,
                    initial,
                    machine.name,
                );
            }
            let was_initializing = self.initializing;
            self.initializing = true;
            let result = self.rewrite(&value, machine, &state, &event);
            self.initializing = was_initializing;
            result
        } else {
            self.initial_configuration(machine, hierarchy, hierarchy.leaf(region))
        }
    }

    pub(super) fn complete_parallel_constructor(
        &mut self,
        machine: &MachineDecl,
        name: Ident,
        supplied: Vec<(Ident, Spanned<Expr>)>,
    ) -> Result<Option<Spanned<Expr>>, TypeError> {
        let Some(group) = machine
            .composite_groups
            .iter()
            .find(|group| group.name == name && !group.regions.is_empty())
        else {
            return Ok(None);
        };
        let hierarchy = self
            .configuration_hierarchy
            .clone()
            .expect("machine hierarchy");
        let node = hierarchy.resolve(name, self.origin.clone())?;
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
        for region in hierarchy.group_regions(machine, node) {
            let name = hierarchy.name(machine, region);
            if !fields.iter().any(|(field, _)| *field == name) {
                fields.push((name, self.initial_region(machine, &hierarchy, region)?));
            }
        }
        // Ordinary record checking diagnoses any missing explicit parent payload.
        let value = self.qualified_variant(machine, group.name, fields);
        Ok(Some(self.block(stmts, value)))
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
