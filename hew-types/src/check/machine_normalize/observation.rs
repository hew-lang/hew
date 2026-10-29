//! Opt-in typed departure snapshots; history retention remains ordinary Hew code.

use super::configuration::Focus;
use super::FreshSpans;
use super::{
    machine_companion, Block, Builder, CallArg, DeclarationOrigin, Expr, FnDecl, Hierarchy, Ident,
    Item, MachineDecl, MatchArm, NodeId, Path, Pattern, Spanned, Stmt, TypeError, TypeExpr,
};
use hew_parser::ast::{ImplDecl, Param};

const DEPARTURES: &str = "_$machine_departures";

type Fields = Vec<(Ident, Spanned<TypeExpr>)>;

struct Snapshot {
    scope: NodeId,
    suffix: String,
    variants: Vec<(NodeId, Fields)>,
    parameters: Vec<RestoreParameter>,
}

struct RestoreParameter {
    owner: NodeId,
    field: Ident,
    binding: Ident,
    ty: Spanned<TypeExpr>,
}

impl Builder {
    fn snapshots(
        &mut self,
        machine: &MachineDecl,
        hierarchy: &Hierarchy,
    ) -> Result<Vec<Snapshot>, TypeError> {
        let mut result = Vec::new();
        for group in &machine.composite_groups {
            let scope = hierarchy.resolve(group.name, group.span.clone())?;
            let enclosing = hierarchy.inherited_fields(machine, scope)?;
            let mut variants = Vec::new();
            for node in hierarchy.configurations(hierarchy.container(scope)) {
                if hierarchy.ancestors(node).contains(&scope) {
                    let config = self.configuration(machine, hierarchy, node)?;
                    let regions = hierarchy.group_regions(machine, node);
                    let fields: Fields = config
                        .fields
                        .into_iter()
                        .filter(|(name, _)| {
                            regions
                                .iter()
                                .any(|region| hierarchy.name(machine, *region) == *name)
                                || !enclosing.iter().any(|(parent, _)| parent == name)
                        })
                        .collect();
                    variants.push((node, fields));
                }
            }
            let mut parameters = Vec::new();
            if let Some((node, saved)) = variants.first() {
                let path = hierarchy.configuration_path(*node);
                for (depth, config) in path.iter().enumerate() {
                    let next_region = path
                        .get(depth + 1)
                        .and_then(|next| hierarchy.container(*next));
                    for (field, ty) in self.configuration(machine, hierarchy, *config)?.fields {
                        let region = hierarchy
                            .group_regions(machine, *config)
                            .into_iter()
                            .find(|region| hierarchy.name(machine, *region) == field);
                        if region.is_some() && region == next_region {
                            continue;
                        }
                        if depth + 1 == path.len() && saved.iter().any(|(name, _)| *name == field) {
                            continue;
                        }
                        let hint =
                            restore_parameter(machine, hierarchy, *config, field, region.is_some());
                        let mut binding = hint;
                        let mut suffix = 2;
                        while parameters
                            .iter()
                            .any(|parameter: &RestoreParameter| parameter.binding == binding)
                        {
                            binding = Ident::new(&format!("{hint}_{suffix}"));
                            suffix += 1;
                        }
                        parameters.push(RestoreParameter {
                            owner: restore_field_owner(hierarchy, *config, field, region.is_some()),
                            field,
                            binding,
                            ty,
                        });
                    }
                }
            }
            result.push(Snapshot {
                scope,
                suffix: format!("{}Snapshot", group.name),
                variants,
                parameters,
            });
        }
        // Match the canonical exit ordering; caller request order is irrelevant.
        result.sort_by_key(|snapshot| hierarchy.boundary_order_key(machine, snapshot.scope, false));
        Ok(result)
    }

    pub(super) fn observation_declarations(
        &mut self,
        machine: &MachineDecl,
        hierarchy: &Hierarchy,
    ) -> Result<Vec<Spanned<Item>>, TypeError> {
        let snapshots = self.snapshots(machine, hierarchy)?;
        let scopes = snapshots
            .iter()
            .map(|snapshot| (hierarchy.name(machine, snapshot.scope), Vec::new()))
            .collect::<Vec<_>>();
        let scope = self.enum_decl(
            machine_companion(machine, "Scope"),
            &scopes,
            machine.visibility,
        );
        let subtrees = snapshots
            .iter()
            .map(|snapshot| {
                let ty = self.machine_ty(machine, &snapshot.suffix);
                (
                    hierarchy.name(machine, snapshot.scope),
                    vec![(Ident::new("snapshot"), ty)],
                )
            })
            .collect::<Vec<_>>();
        let mut subtree = self.enum_decl(
            machine_companion(machine, "Subtree"),
            &subtrees,
            machine.visibility,
        );
        let params = (!machine.type_params.is_empty()).then(|| machine.type_params.clone());
        subtree.type_params.clone_from(&params);
        subtree.where_clause.clone_from(&machine.where_clause);
        let mut declarations = vec![Item::TypeDecl(scope), Item::TypeDecl(subtree)];
        for snapshot in &snapshots {
            let variants = snapshot
                .variants
                .iter()
                .map(|(node, fields)| (hierarchy.name(machine, *node), fields.clone()))
                .collect::<Vec<_>>();
            let mut declaration = self.enum_decl(
                machine_companion(machine, &snapshot.suffix),
                &variants,
                machine.visibility,
            );
            declaration.type_params.clone_from(&params);
            declaration.where_clause.clone_from(&machine.where_clause);
            declarations.push(Item::TypeDecl(declaration));
            declarations.push(Item::Impl(
                self.snapshot_restore(machine, hierarchy, snapshot)?,
            ));
        }
        Ok(declarations
            .into_iter()
            .map(|mut item| {
                let start = self.next_span;
                match &mut item {
                    Item::TypeDecl(decl) => {
                        self.refresh_generics(
                            decl.type_params.as_mut(),
                            decl.where_clause.as_mut(),
                        );
                        self.refresh_type_decl(decl);
                    }
                    Item::Impl(decl) => {
                        self.refresh_generics(
                            decl.type_params.as_mut(),
                            decl.where_clause.as_mut(),
                        );
                        self.refresh_type(&mut decl.target_type);
                        for method in &mut decl.methods {
                            self.refresh_signature(method);
                        }
                    }
                    _ => unreachable!(),
                }
                self.span();
                (item, start..self.next_span)
            })
            .collect())
    }

    fn snapshot_restore(
        &mut self,
        machine: &MachineDecl,
        hierarchy: &Hierarchy,
        snapshot: &Snapshot,
    ) -> Result<ImplDecl, TypeError> {
        let mut arguments = vec![Param {
            default: None,
            default_provider: None,
            name: Ident::new("self"),
            name_span: self.span(),
            ty: self.ty("Self"),
            is_mutable: false,
            is_consume: false,
            is_receiver: true,
        }];
        arguments.extend(snapshot.parameters.iter().map(|parameter| Param {
            default: None,
            default_provider: None,
            name: parameter.binding,
            name_span: self.span(),
            ty: parameter.ty.clone(),
            is_mutable: false,
            is_consume: false,
            is_receiver: false,
        }));
        let mut arms = Vec::new();
        for (node, saved) in &snapshot.variants {
            let pattern = self.pattern(hierarchy.name(machine, *node), saved, "_$machine_saved_");
            let body = self.restore_configuration(machine, hierarchy, snapshot, *node, saved)?;
            arms.push(MatchArm {
                append_separator: None,
                pattern,
                guard: None,
                body,
            });
        }
        let receiver = self.ident("self");
        let body = self.expr(Expr::Match {
            scrutinee: Box::new(receiver),
            arms,
        });
        Ok(ImplDecl {
            type_params: (!machine.type_params.is_empty()).then(|| machine.type_params.clone()),
            trait_bound: None,
            target_type: self.machine_ty(machine, &snapshot.suffix),
            where_clause: machine.where_clause.clone(),
            type_aliases: Vec::new(),
            methods: vec![FnDecl {
                origin: DeclarationOrigin::MachineCompanion,
                attributes: Vec::new(),
                is_generator: false,
                visibility: machine.visibility,
                name: Ident::new("restore"),
                type_params: None,
                params: arguments,
                return_type: Some(self.machine_ty(machine, "")),
                where_clause: None,
                body: Block {
                    stmts: Vec::new(),
                    trailing_expr: Some(Box::new(body)),
                },
                doc_comment: None,
                decl_span: self.span(),
                fn_span: self.span(),
                intrinsic: None,
                consumes_self: false,
            }],
            doc_comment: None,
        })
    }

    fn restore_configuration(
        &mut self,
        machine: &MachineDecl,
        hierarchy: &Hierarchy,
        snapshot: &Snapshot,
        node: NodeId,
        saved: &Fields,
    ) -> Result<Spanned<Expr>, TypeError> {
        let path = hierarchy.configuration_path(node);
        let mut value = None;
        for (depth, config) in path.iter().enumerate().rev() {
            let next_region = path
                .get(depth + 1)
                .and_then(|next| hierarchy.container(*next));
            let mut fields = Vec::new();
            for (field, _) in self.configuration(machine, hierarchy, *config)?.fields {
                let region = hierarchy
                    .group_regions(machine, *config)
                    .into_iter()
                    .find(|region| hierarchy.name(machine, *region) == field);
                let field_value = if region.is_some() && region == next_region {
                    value.take().expect("nested restored configuration")
                } else if depth + 1 == path.len() && saved.iter().any(|(name, _)| *name == field) {
                    self.ident(format!("_$machine_saved_{field}"))
                } else {
                    let owner = restore_field_owner(hierarchy, *config, field, region.is_some());
                    let parameter = snapshot
                        .parameters
                        .iter()
                        .find(|parameter| parameter.owner == owner && parameter.field == field)
                        .expect("enclosing configuration has a restore parameter");
                    self.ident(parameter.binding)
                };
                fields.push((field, field_value));
            }
            value = Some(self.qualified_variant(machine, hierarchy.name(machine, *config), fields));
        }
        Ok(value.expect("configuration path"))
    }

    fn applied_type(&mut self, name: &str, args: Vec<Spanned<TypeExpr>>) -> Spanned<TypeExpr> {
        let path = Path::single(Ident::new(name), self.span());
        (
            TypeExpr::Named {
                path,
                type_args: Some(args),
            },
            self.span(),
        )
    }

    pub(super) fn observation_prelude(&mut self, machine: &MachineDecl) -> Vec<Spanned<Stmt>> {
        let scope = self.ty(machine_companion(machine, "Scope"));
        let subtree = self.machine_ty(machine, "Subtree");
        let departure = self.applied_type("MachineDeparture", vec![scope, subtree]);
        let vector = self.applied_type("Vec", vec![departure]);
        let empty = self.expr(Expr::Array(Vec::new()));
        let mut stmts = vec![self.var(DEPARTURES, Some(vector), empty)];
        let mut arms = Vec::new();
        for group in &machine.composite_groups {
            let flag = format!("_$machine_observe_{}", group.name);
            let no = self.boolean(false);
            stmts.push(self.var(&flag, None, no));
            let yes = self.boolean(true);
            let assign = self.assign(&flag, yes);
            let unit = self.expr(Expr::Tuple(Vec::new()));
            let body = self.block(vec![assign], unit);
            let pattern = self.pattern(group.name, &[], "");
            arms.push(MatchArm {
                append_separator: None,
                pattern,
                guard: None,
                body,
            });
        }
        if !arms.is_empty() {
            let request = self.ident("_$machine_request");
            let select = self.expr(Expr::Match {
                scrutinee: Box::new(request),
                arms,
            });
            let select = self.stmt(Stmt::Expression(select));
            let pattern = (
                Pattern::Identifier(Ident::new("_$machine_request")),
                self.span(),
            );
            let iterable = self.ident("requested");
            stmts.push(self.stmt(Stmt::For {
                label: None,
                pattern,
                iterable,
                body: Block {
                    stmts: vec![select],
                    trailing_expr: None,
                },
            }));
        }
        stmts
    }

    pub(super) fn observed_report(&mut self, report: Spanned<Expr>) -> Spanned<Expr> {
        let departures = self.ident(DEPARTURES);
        self.record_value(
            "MachineObservedStep",
            vec![
                (Ident::new("report"), report),
                (Ident::new("departures"), departures),
            ],
        )
    }

    fn record_value(&mut self, name: &str, fields: Vec<(Ident, Spanned<Expr>)>) -> Spanned<Expr> {
        let path = Path::single(Ident::new(name), self.span());
        self.expr(Expr::StructInit {
            path,
            fields,
            field_labels: Vec::new(),
            type_args: None,
            base: None,
        })
    }

    fn method_call(
        &mut self,
        receiver: &str,
        name: &str,
        args: Vec<Spanned<Expr>>,
    ) -> Spanned<Expr> {
        let receiver = self.ident(receiver);
        let method = (Ident::new(name), self.span());
        self.expr(Expr::MethodCall {
            receiver: Box::new(receiver),
            method,
            args: args.into_iter().map(CallArg::Positional).collect(),
        })
    }

    pub(super) fn capture_departures(
        &mut self,
        machine: &MachineDecl,
        hierarchy: &Hierarchy,
        rules: &[usize],
        stmts: &mut Vec<Spanned<Stmt>>,
        #[cfg(feature = "verify-machines")] effects: &mut Vec<super::verification::Effect>,
    ) -> Result<(), TypeError> {
        for snapshot in self.snapshots(machine, hierarchy)? {
            let selected = rules
                .iter()
                .filter(|index| hierarchy.crosses(**index, snapshot.scope))
                .map(|index| self.ident(format!("_$machine_selected_{index}")))
                .collect();
            let selected = self.disjunction(selected);
            let scope = self.enum_value(
                machine_companion(machine, "Scope"),
                hierarchy.name(machine, snapshot.scope),
                Vec::new(),
            );
            let requested = self.ident(format!(
                "_$machine_observe_{}",
                hierarchy.name(machine, snapshot.scope)
            ));
            let condition = self.expr(Expr::Binary {
                op: hew_parser::ast::BinaryOp::And,
                left: Box::new(selected),
                right: Box::new(requested),
            });
            let mut captures = Vec::new();
            for (node, fields) in snapshot.variants {
                let fields = fields
                    .into_iter()
                    .map(|(field, _)| (field, self.ident(format!("_$machine_state_{field}"))))
                    .collect();
                let saved = self.enum_value(
                    machine_companion(machine, &snapshot.suffix),
                    hierarchy.name(machine, node),
                    fields,
                );
                let subtree = self.enum_value(
                    machine_companion(machine, "Subtree"),
                    hierarchy.name(machine, snapshot.scope),
                    vec![(Ident::new("snapshot"), saved)],
                );
                let departure = self.record_value(
                    "MachineDeparture",
                    vec![
                        (Ident::new("target"), scope.clone()),
                        (Ident::new("subtree"), subtree),
                    ],
                );
                let push = self.method_call(DEPARTURES, "push", vec![departure]);
                let push = self.stmt(Stmt::Expression(push));
                let truth = self.boolean(true);
                let capture = self.block(vec![push], truth);
                let capture =
                    self.focus(machine, hierarchy, node, capture, Focus::Predicate, true)?;
                let capture_stmt = self.stmt(Stmt::Expression(capture));
                #[cfg(feature = "verify-machines")]
                effects.push(super::verification::Effect {
                    span: capture_stmt.1.clone(),
                    stage: super::verification::Stage::Departure {
                        scope: snapshot.scope,
                        config: node,
                    },
                });
                captures.push(capture_stmt);
            }
            stmts.push(self.if_do(condition, captures));
        }
        Ok(())
    }

    pub(super) fn observed_forwarding(&mut self) -> Block {
        let args = ["event", "limit", "requested"]
            .into_iter()
            .map(|name| self.ident(name))
            .collect();
        let call = self.method_call("self", "step_observed_with_limit", args);
        Block {
            stmts: Vec::new(),
            trailing_expr: Some(Box::new(call)),
        }
    }
}

fn restore_parameter(
    machine: &MachineDecl,
    hierarchy: &Hierarchy,
    config: NodeId,
    field: Ident,
    is_region: bool,
) -> Ident {
    if is_region {
        Ident::new(&format!("{}_{}", hierarchy.name(machine, config), field))
    } else {
        field
    }
}

/// An inherited scalar belongs to its declaration, not the active leaf carrying it.
/// A regional configuration belongs to the parallel state declaring that region.
fn restore_field_owner(
    hierarchy: &Hierarchy,
    config: NodeId,
    field: Ident,
    is_region: bool,
) -> NodeId {
    if is_region {
        config
    } else {
        hierarchy
            .ancestors(config)
            .into_iter()
            .find(|owner| {
                hierarchy
                    .own_fields(*owner)
                    .iter()
                    .any(|(name, _)| *name == field)
            })
            .expect("configuration scalar has a checked declaration")
    }
}
