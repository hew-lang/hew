//! Shallow history as finite, ordinary enum payloads owned by the machine.

use super::configuration::Focus;
use super::{
    machine_companion, Builder, CallArg, ContextVariantPattern, Expr, Hierarchy, Ident,
    MachineDecl, MatchArm, NodeId, NominalPatternPayload, Pattern, Span, Spanned, Stmt,
    TypeBodyItem, TypeDecl, TypeError, TypeExpr, VariantKind,
};

#[derive(Clone)]
pub(super) struct HistorySlot {
    /// The compound whose exit saves this slot.
    target: NodeId,
    /// The exclusive compound or parallel region choosing an immediate child.
    owner: NodeId,
    pub(super) field: Ident,
    suffix: String,
}

pub(super) fn slots(machine: &MachineDecl, hierarchy: &Hierarchy) -> Vec<HistorySlot> {
    let mut targets = Vec::new();
    for (index, rule) in machine.transitions.iter().enumerate() {
        if rule.history {
            let target = hierarchy.target(index).expect("checked history target");
            if !targets.contains(&target) {
                targets.push(target);
            }
        }
    }
    let mut slots = Vec::new();
    for target in targets {
        let regions = hierarchy.group_regions(machine, target);
        let owners = if regions.is_empty() {
            vec![target]
        } else {
            regions
        };
        for owner in owners {
            let index = slots.len();
            slots.push(HistorySlot {
                target,
                owner,
                field: Ident::new(&format!("_$machine_history_{index}")),
                suffix: format!("History{index}"),
            });
        }
    }
    slots
}

/// A target modifier applies to the resulting destination, not to temporary
/// values constructed in preceding statements or field expressions.
pub(super) fn destination_sites(value: &Spanned<Expr>) -> Vec<Span> {
    match &value.0 {
        Expr::Block(block) => block
            .trailing_expr
            .as_deref()
            .map_or_else(Vec::new, destination_sites),
        Expr::If {
            then_block,
            else_block,
            ..
        } => {
            let mut sites = destination_sites(then_block);
            if let Some(other) = else_block {
                sites.extend(destination_sites(other));
            }
            sites
        }
        Expr::Match { arms, .. } => arms
            .iter()
            .flat_map(|arm| destination_sites(&arm.body))
            .collect(),
        _ => vec![value.1.clone()],
    }
}

impl Builder {
    /// Receiver history follows the transaction's destination. An ordinary
    /// temporary machine constructor still initializes fresh, empty history.
    pub(super) fn carry_history(
        &mut self,
        machine: &MachineDecl,
        name: Ident,
        fields: &mut Vec<(Ident, Spanned<Expr>)>,
    ) {
        if self
            .configuration_types
            .get(&name)
            .is_some_and(|(owner, _)| *owner == machine.name)
        {
            for slot in self.history_slots.clone() {
                fields.push((slot.field, self.ident(slot.field)));
            }
        }
    }

    pub(super) fn retain_history(
        &mut self,
        machine: &MachineDecl,
        hierarchy: &Hierarchy,
        value: Spanned<Expr>,
    ) -> Result<Spanned<Expr>, TypeError> {
        if self.history_slots.is_empty() {
            return Ok(value);
        }
        let mut arms = Vec::new();
        for node in hierarchy.configurations(None) {
            let state = self.configuration(machine, hierarchy, node)?;
            let pattern = self.pattern(state.name, &state.fields, "_$machine_state_");
            let body = self.state_value(machine, &state);
            arms.push(MatchArm {
                append_separator: None,
                pattern,
                guard: None,
                body,
            });
        }
        Ok(self.expr(Expr::Match {
            scrutinee: Box::new(value),
            arms,
        }))
    }

    pub(super) fn history_fields(
        &mut self,
        machine: &MachineDecl,
    ) -> Vec<(Ident, Spanned<TypeExpr>)> {
        self.history_slots
            .clone()
            .into_iter()
            .map(|slot| {
                let child = self.machine_ty(machine, &slot.suffix);
                let ty = (TypeExpr::Option(Box::new(child)), self.span());
                (slot.field, ty)
            })
            .collect()
    }

    pub(super) fn mark_history_fields(&self, declaration: &mut TypeDecl) {
        use hew_parser::ast::{ImplicitVariantField, ImplicitVariantFieldInitialization};
        for member in &mut declaration.body {
            if let TypeBodyItem::Variant(variant) = member {
                if let VariantKind::Struct(fields) = &variant.kind {
                    let start = fields.len() - self.history_slots.len();
                    variant.implicit_fields = (start..fields.len())
                        .map(|field_index| ImplicitVariantField {
                            field_index,
                            initialization: ImplicitVariantFieldInitialization::EmptyOption,
                        })
                        .collect();
                }
            }
        }
    }

    pub(super) fn history_declarations(
        &mut self,
        machine: &MachineDecl,
        hierarchy: &Hierarchy,
    ) -> Vec<TypeDecl> {
        self.history_slots
            .clone()
            .into_iter()
            .map(|slot| {
                let variants = hierarchy
                    .children(slot.owner)
                    .into_iter()
                    .map(|child| {
                        (
                            hierarchy.name(machine, child),
                            hierarchy.own_fields(child).to_vec(),
                        )
                    })
                    .collect::<Vec<_>>();
                let mut declaration = self.enum_decl(
                    machine_companion(machine, &slot.suffix),
                    &variants,
                    machine.visibility,
                );
                declaration.type_params =
                    (!machine.type_params.is_empty()).then(|| machine.type_params.clone());
                declaration.where_clause.clone_from(&machine.where_clause);
                declaration
            })
            .collect()
    }

    pub(super) fn history_prelude(
        &mut self,
        machine: &MachineDecl,
        hierarchy: &Hierarchy,
    ) -> Vec<Spanned<Stmt>> {
        if self.history_slots.is_empty() {
            return Vec::new();
        }
        let fields = self.history_fields(machine);
        let mut stmts = Vec::new();
        for (field, ty) in &fields {
            let none = self.variant(Ident::new("None"), Vec::new(), Vec::new());
            stmts.push(self.var(*field, Some(ty.clone()), none));
        }
        let mut arms = Vec::new();
        for root in hierarchy.configurations(None) {
            let name = hierarchy.name(machine, root);
            let pattern = self.pattern(name, &fields, "_$machine_saved_");
            let mut restore = Vec::new();
            for (field, _) in &fields {
                let value = self.ident(format!("_$machine_saved_{field}"));
                restore.push(self.assign(*field, value));
            }
            let unit = self.expr(Expr::Tuple(Vec::new()));
            let body = self.block(restore, unit);
            arms.push(MatchArm {
                append_separator: None,
                pattern,
                guard: None,
                body,
            });
        }
        let receiver = self.ident("self");
        let restore = self.expr(Expr::Match {
            scrutinee: Box::new(receiver),
            arms,
        });
        stmts.push(self.stmt(Stmt::Expression(restore)));
        stmts
    }

    fn some_value(&mut self, value: Spanned<Expr>) -> Spanned<Expr> {
        let function = self.variant(Ident::new("Some"), Vec::new(), Vec::new());
        self.expr(Expr::Call {
            function: Box::new(function),
            type_args: None,
            args: vec![CallArg::Positional(value)],
            is_tail_call: false,
        })
    }

    pub(super) fn capture_history(
        &mut self,
        machine: &MachineDecl,
        hierarchy: &Hierarchy,
        rules: &[usize],
        stmts: &mut Vec<Spanned<Stmt>>,
        #[cfg(feature = "verify-machines")] effects: &mut Vec<super::verification::Effect>,
    ) -> Result<(), TypeError> {
        for slot in self.history_slots.clone() {
            let selected = rules
                .iter()
                .filter(|index| hierarchy.crosses(**index, slot.target))
                .map(|index| self.ident(format!("_$machine_selected_{index}")))
                .collect();
            let condition = self.disjunction(selected);
            let mut capture = Vec::new();
            let container = if hierarchy.group_regions(machine, slot.target).is_empty() {
                hierarchy.container(slot.owner)
            } else {
                Some(slot.owner)
            };
            for config in hierarchy.configurations(container) {
                let ancestors = hierarchy.ancestors(config);
                let Some(child) = ancestors
                    .windows(2)
                    .find_map(|pair| (pair[1] == slot.owner).then_some(pair[0]))
                else {
                    continue;
                };
                let fields = hierarchy
                    .own_fields(child)
                    .iter()
                    .map(|(field, _)| (*field, self.ident(format!("_$machine_state_{field}"))))
                    .collect();
                let value = self.enum_value(
                    machine_companion(machine, &slot.suffix),
                    hierarchy.name(machine, child),
                    fields,
                );
                let value = self.some_value(value);
                let assign = self.assign(slot.field, value);
                let truth = self.boolean(true);
                let body = self.block(vec![assign], truth);
                let body = self.focus(machine, hierarchy, config, body, Focus::Predicate, true)?;
                let capture_stmt = self.stmt(Stmt::Expression(body));
                #[cfg(feature = "verify-machines")]
                effects.push(super::verification::Effect {
                    span: capture_stmt.1.clone(),
                    stage: super::verification::Stage::History {
                        target: slot.target,
                        owner: slot.owner,
                        config,
                    },
                });
                capture.push(capture_stmt);
            }
            stmts.push(self.if_do(condition, capture));
        }
        Ok(())
    }

    pub(super) fn restore_history(
        &mut self,
        machine: &MachineDecl,
        hierarchy: &Hierarchy,
        owner: NodeId,
        parent_fields: Vec<(Ident, Spanned<Expr>)>,
    ) -> Result<Spanned<Expr>, TypeError> {
        let slot = self
            .history_slots
            .iter()
            .find(|slot| slot.owner == owner)
            .expect("history slot")
            .clone();
        let mut arms = Vec::new();
        for child in hierarchy.children(owner) {
            let name = hierarchy.name(machine, child);
            let own_fields = hierarchy.own_fields(child);
            let pattern = self.pattern(name, own_fields, "_$machine_history_payload_");
            let mut fields = parent_fields.clone();
            fields.extend(own_fields.iter().map(|(field, _)| {
                (
                    *field,
                    self.ident(format!("_$machine_history_payload_{field}")),
                )
            }));
            let value = if let Some(value) =
                self.complete_compound_constructor(machine, name, fields.clone())?
            {
                value
            } else {
                self.qualified_variant(
                    machine,
                    hierarchy.name(machine, hierarchy.leaf(child)),
                    fields,
                )
            };
            arms.push(MatchArm {
                append_separator: None,
                pattern,
                guard: None,
                body: value,
            });
        }
        let saved = self.ident("_$machine_history_child");
        let restored = self.expr(Expr::Match {
            scrutinee: Box::new(saved),
            arms,
        });
        let fallback = if owner == slot.target {
            self.exclusive_initial(machine, hierarchy, owner, parent_fields)?
        } else {
            self.initial_region(machine, hierarchy, owner)?
        };
        let binding = (
            Pattern::Identifier(Ident::new("_$machine_history_child")),
            self.span(),
        );
        let some = (
            Pattern::ContextVariant(ContextVariantPattern {
                name: Ident::new("Some"),
                payload: Some(NominalPatternPayload::Tuple(vec![binding])),
            }),
            self.span(),
        );
        let none = self.pattern(Ident::new("None"), &[], "");
        let saved = self.ident(slot.field);
        Ok(self.expr(Expr::Match {
            scrutinee: Box::new(saved),
            arms: vec![
                MatchArm {
                    append_separator: None,
                    pattern: some,
                    guard: None,
                    body: restored,
                },
                MatchArm {
                    append_separator: None,
                    pattern: none,
                    guard: None,
                    body: fallback,
                },
            ],
        }))
    }
}
