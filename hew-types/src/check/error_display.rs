//! `impl Error for X {}` supplies `X`'s `Display` (D577).
//!
//! An `Error` type must render, and the specification fixes how: the variant
//! name, then `: ` and the payloads, each payload through its own `Display`
//! when it has one and structurally otherwise. An `impl Error` that no
//! `Display` impl of its type overlaps receives that rendering as an ordinary
//! impl, written beside it and checked like source, so interpolation, bounds,
//! `dyn Error`, `expect` and a failing `main` all see one impl.
//!
//! Impl admission selects the trait and resolved receiver, including aliases.
//! The generated body keeps the source header's binders and bounds, with its
//! resolved target; `Display` and `string` use an expansion context. Payload
//! rendering uses the checked structural-formatting contract.

use std::collections::{HashMap, HashSet};
use std::fmt::Write as _;

use hew_parser::ast::{
    Ident, ImplDecl, Item, Path, Program, Span, Spanned, SyntaxContext, TraitBound, TypeDecl,
    TypeDeclKind, TypeExpr,
};
use hew_parser::module::ModulePath;

use super::machine_normalize::FreshSpans;
use super::scope::{Namespace, Resolution, ScopeSite};
use super::types::TraitImplArgs;
use super::Checker;
use crate::ty::{Ty, TypeHead};

/// The program with every implied `Display` appended beside its `impl
/// Error`, the span range each synthesized impl occupies with the span a
/// diagnostic inside it is reported at, and the expansion context its
/// compiler-written names carry.
pub(super) struct ErrorDisplays {
    pub program: Program,
    pub origins: Vec<(Span, Span)>,
    pub context: SyntaxContext,
}

/// One item of the program and the file module its names resolve in.
struct ItemSite<'p> {
    /// The graph module holding the item, or `None` for a graph-less root.
    module: Option<&'p ModulePath>,
    index: usize,
    item: &'p Item,
    span: &'p Span,
    file: crate::ModuleId,
}

/// Fresh spans handed out from a reserved range of the synthesized source.
struct SpanCursor {
    next: usize,
}

impl FreshSpans for SpanCursor {
    fn fresh_span(&mut self) -> Span {
        let span = self.next..self.next + 1;
        self.next += 2;
        span
    }
}

/// Whether two impl heads can name one type: a binder admits anything, and
/// otherwise the heads and their arguments agree.
fn heads_overlap(left: &TraitImplArgs, right: &TraitImplArgs) -> bool {
    types_overlap(&left.target, &right.target)
}

fn types_overlap(left: &Ty, right: &Ty) -> bool {
    match (left, right) {
        (
            Ty::Named {
                head: TypeHead::Param(_),
                ..
            },
            _,
        )
        | (
            _,
            Ty::Named {
                head: TypeHead::Param(_),
                ..
            },
        ) => true,
        (
            Ty::Named {
                head: left_head,
                args: left_args,
            },
            Ty::Named {
                head: right_head,
                args: right_args,
            },
        ) => {
            left_head == right_head
                && left_args.len() == right_args.len()
                && left_args
                    .iter()
                    .zip(right_args)
                    .all(|(left, right)| types_overlap(left, right))
        }
        _ => left == right,
    }
}

impl Checker {
    /// The expansion context of compiler-written names in implied impls:
    /// they resolve in the module that declares `Display`.
    pub(super) fn mint_implied_display_context(&mut self) -> Option<SyntaxContext> {
        let display_trait = self
            .lang_items
            .get(crate::LangItem::Display.key())?
            .trait_id;
        let module = self
            .defs
            .module(display_trait)
            .or_else(|| self.defs.root_module())?;
        Some(self.scopes.contexts_mut().mint(SyntaxContext::ROOT, module))
    }

    /// Append the implied `Display` of every `impl Error` no `Display` impl
    /// of its type overlaps. Runs once impls are admitted; returns `None`
    /// when no impl needs one.
    pub(super) fn synthesize_error_displays(&mut self, program: &Program) -> Option<ErrorDisplays> {
        let needed = self.impls_needing_display()?;
        if needed.is_empty() {
            return None;
        }
        let sites = self.item_sites(program);

        let wanted = needed.values().map(|(nominal, _)| *nominal).collect();
        let declarations = self.type_declarations(&sites, &wanted);

        // Each needed impl's header, in program order so the output is
        // deterministic.
        let mut headers = Vec::new();
        for site in &sites {
            let Item::Impl(implementation) = site.item else {
                continue;
            };
            let Some((nominal, target)) = self
                .impl_declaration_at(site)
                .and_then(|id| needed.get(&id))
            else {
                continue;
            };
            let Some(&(declaration, declaration_span, declaration_file)) =
                declarations.get(nominal)
            else {
                continue;
            };
            // A diagnostic inside the rendering is reported at the error
            // type's declaration when the impl shares its file.
            let origin = if declaration_file == site.file {
                declaration_span.clone()
            } else {
                site.span.clone()
            };
            let Some(implementation) =
                self.implied_display_header(implementation, target, site.file)
            else {
                continue;
            };
            headers.push((site, implementation, declaration, origin));
        }
        if headers.is_empty() {
            return None;
        }
        let context = self.mint_implied_display_context()?;

        // One parse for every rendering: the source is padded past the
        // program's last span, and each impl reserves room after its text
        // for the fresh spans of its copied header.
        let mut source = " ".repeat(next_free_span(program));
        let mut layouts = Vec::with_capacity(headers.len());
        for (_, implementation, declaration, _) in &headers {
            let start = source.len();
            source.push_str(&display_impl_source(declaration));
            let header_start = source.len() + 1;
            let mut counter = SpanCursor { next: 0 };
            copy_header(implementation, &mut counter);
            source.push_str(&" ".repeat(counter.next + 2));
            layouts.push((start, header_start, source.len()));
            source.push('\n');
        }
        let parsed = hew_parser::parse(&source);
        debug_assert!(
            parsed.errors.is_empty(),
            "implied Display source must parse: {:?}",
            parsed.errors
        );
        if !parsed.errors.is_empty() || parsed.program.items.len() != headers.len() {
            return None;
        }

        let mut augmented = program.clone();
        let mut origins = Vec::with_capacity(headers.len());
        let mut appended: HashMap<Option<ModulePath>, Vec<(Spanned<Item>, usize)>> = HashMap::new();
        for ((site, implementation, _, origin), ((generated, _), (start, header_start, end))) in
            headers
                .into_iter()
                .zip(parsed.program.items.into_iter().zip(layouts))
        {
            let Item::Impl(mut rendering) = generated else {
                return None;
            };
            let mut cursor = SpanCursor { next: header_start };
            let header = copy_header(&implementation, &mut cursor);
            debug_assert!(cursor.next <= end, "the reserved header spans fit");
            rendering.type_params = header.type_params;
            rendering.where_clause = header.where_clause;
            rendering.target_type = header.target_type;
            name_in_context(&mut rendering, context);
            origins.push((start..end, origin));
            appended
                .entry(site.module.cloned())
                .or_default()
                .push(((Item::Impl(rendering), start..end), site.index));
        }
        for (module, items) in appended {
            append_items(&mut augmented, module.as_ref(), items);
        }
        // An import carries its target module's items; it must carry the
        // impls written into that module too.
        super::machine_normalize::project_normalized_imports(&mut augmented);
        Some(ErrorDisplays {
            program: augmented,
            origins,
            context,
        })
    }

    fn implied_display_header(
        &mut self,
        implementation: &ImplDecl,
        target: &Ty,
        file: crate::ModuleId,
    ) -> Option<ImplDecl> {
        let mut implementation = implementation.clone();
        let alias = match &implementation.target_type.0 {
                TypeExpr::Named { path, .. } => self.resolve_at(file, &path.segments)
                    .is_some_and(|resolved| matches!(resolved,
                        Resolution::Nominal(nominal) if self.defs.kind(nominal.declaration()) == crate::DeclarationKind::TypeAlias)),
                _ => false,
            };
        if alias {
            let target_source = format!("fn target(value: {}) {{}}", target.user_facing());
            let parsed_target = hew_parser::parse(&target_source);
            let Some((Item::Function(function), _)) =
                parsed_target.program.items.into_iter().next()
            else {
                return None;
            };
            let parameter = function.params.into_iter().next()?;
            implementation.target_type = parameter.ty;
            self.contextualize_alias_target(&mut implementation.target_type, target);
        }
        Some(implementation)
    }

    fn contextualize_alias_target(&mut self, syntax: &mut Spanned<TypeExpr>, ty: &Ty) {
        match (&mut syntax.0, ty) {
            (TypeExpr::Named { path, type_args }, Ty::Named { head, args }) => {
                if let Some(nominal) = head.nominal() {
                    if let Some(module) = self.defs.module(nominal.declaration()) {
                        let context = self.scopes.contexts_mut().mint(SyntaxContext::ROOT, module);
                        *path = Path::single(
                            Ident {
                                name: self.defs.name(nominal.declaration()),
                                ctx: context,
                            },
                            syntax.1.clone(),
                        );
                    }
                }
                for (syntax, ty) in type_args.iter_mut().flatten().zip(args) {
                    self.contextualize_alias_target(syntax, ty);
                }
            }
            (TypeExpr::Named { path, .. }, ty) if ty.canonical_lowering_name().is_some() => {
                if let Some(context) = self.mint_implied_display_context() {
                    for (name, _) in &mut path.segments {
                        name.ctx = context;
                    }
                }
            }
            (TypeExpr::Tuple(syntax), Ty::Tuple(types)) => {
                for (syntax, ty) in syntax.iter_mut().zip(types) {
                    self.contextualize_alias_target(syntax, ty);
                }
            }
            (TypeExpr::Array { element, .. }, Ty::Array(ty, _))
            | (TypeExpr::Borrow(element), Ty::Borrow { pointee: ty })
            | (
                TypeExpr::Pointer {
                    pointee: element, ..
                },
                Ty::Pointer { pointee: ty, .. },
            ) => {
                self.contextualize_alias_target(element, ty);
            }
            (TypeExpr::Option(syntax), ty) => {
                if let Some(ty) = ty.as_option() {
                    self.contextualize_alias_target(syntax, ty);
                }
            }
            (TypeExpr::Result { ok, err }, ty) => {
                if let Some((success, error)) = ty.as_result() {
                    self.contextualize_alias_target(ok, success);
                    self.contextualize_alias_target(err, error);
                }
            }
            (
                TypeExpr::Function {
                    params,
                    return_type,
                    ..
                },
                Ty::Function {
                    params: types, ret, ..
                },
            ) => {
                for (syntax, ty) in params.iter_mut().zip(types) {
                    self.contextualize_alias_target(syntax, ty);
                }
                self.contextualize_alias_target(return_type, ret);
            }
            _ => {}
        }
    }

    /// Each source `impl Error` no `Display` impl of its type overlaps, by
    /// its declaration, with the nominal it renders.
    fn impls_needing_display(&self) -> Option<HashMap<crate::DefId, (crate::NominalId, Ty)>> {
        let error_trait = self.lang_items.get(crate::LangItem::Error.key())?.trait_id;
        let display_trait = self
            .lang_items
            .get(crate::LangItem::Display.key())?
            .trait_id;
        let rows = |owner| {
            self.source_impl_declarations
                .iter()
                .filter(move |((_, trait_id), _)| *trait_id == Some(owner))
                .flat_map(|(_, rows)| rows.iter())
        };
        let displays: Vec<&TraitImplArgs> = rows(display_trait).map(|row| &row.head).collect();
        let needed = rows(error_trait)
            .filter(|row| row.origin == super::types::SourceImplOrigin::Source)
            .filter(|row| {
                !displays
                    .iter()
                    .any(|display| heads_overlap(&row.head, display))
            })
            .filter_map(|row| match &row.head.target {
                Ty::Named { head, .. } => {
                    Some((row.declaration, (head.nominal()?, row.head.target.clone())))
                }
                _ => None,
            })
            .collect();
        Some(needed)
    }

    /// The declaration of each wanted nominal, with its span and file.
    fn type_declarations<'p>(
        &mut self,
        sites: &[ItemSite<'p>],
        wanted: &HashSet<crate::NominalId>,
    ) -> HashMap<crate::NominalId, (&'p TypeDecl, &'p Span, crate::ModuleId)> {
        let mut declarations = HashMap::new();
        for site in sites {
            let Item::TypeDecl(declaration) = site.item else {
                continue;
            };
            let name = [(declaration.name, site.span.clone())];
            if let Some(Resolution::Nominal(nominal)) = self.resolve_at(site.file, &name) {
                if wanted.contains(&nominal) {
                    declarations
                        .entry(nominal)
                        .or_insert((declaration, site.span, site.file));
                }
            }
        }
        declarations
    }

    /// The impl declaration an impl item mints.
    fn impl_declaration_at(&self, site: &ItemSite<'_>) -> Option<crate::DefId> {
        self.source_impl_declarations
            .values()
            .flatten()
            .map(|row| row.declaration)
            .find(|&declaration| {
                self.defs.site(declaration).is_some_and(|occurrence| {
                    occurrence.span() == *site.span && occurrence.module() == Some(site.file)
                })
            })
    }

    /// Every item of the program with the file module it resolves in.
    fn item_sites<'p>(&self, program: &'p Program) -> Vec<ItemSite<'p>> {
        let mut sites = Vec::new();
        let Some(graph) = &program.module_graph else {
            if let Some(root) = self.defs.root_module() {
                for (index, (item, span)) in program.items.iter().enumerate() {
                    sites.push(ItemSite {
                        module: None,
                        index,
                        item,
                        span,
                        file: root,
                    });
                }
            }
            return sites;
        };
        for module_id in &graph.topo_order {
            let Some(module) = graph.modules.get(module_id) else {
                continue;
            };
            let assembler = module
                .source_paths
                .first()
                .and_then(|source| self.defs.module_for_source(source))
                .or_else(|| self.defs.module_for_path(&module_id.dotted()))
                .or_else(|| {
                    (*module_id == graph.root)
                        .then(|| self.defs.root_module())
                        .flatten()
                });
            for (index, (item, span)) in module.items.iter().enumerate() {
                let file = graph
                    .item_source(module_id, index)
                    .or_else(|| module.source_paths.first())
                    .and_then(|source| self.defs.module_for_source(source))
                    .or(assembler);
                if let Some(file) = file {
                    sites.push(ItemSite {
                        module: Some(module_id),
                        index,
                        item,
                        span,
                        file,
                    });
                }
            }
        }
        sites
    }

    /// What a type-namespace path names in `file`, without publishing the
    /// resolution: the ordinary passes resolve and publish every path again.
    fn resolve_at(
        &mut self,
        file: crate::ModuleId,
        path: &[Spanned<hew_parser::ast::Ident>],
    ) -> Option<Resolution> {
        let site = ScopeSite {
            file,
            span_file: 0,
            publish: false,
        };
        self.scopes
            .resolve(&self.env, site, Namespace::Type, path)
            .ok()
    }
}

/// The first span past every item of every module.
fn next_free_span(program: &Program) -> usize {
    let graph_items = program
        .module_graph
        .iter()
        .flat_map(|graph| graph.modules.values())
        .flat_map(|module| module.items.iter());
    program
        .items
        .iter()
        .chain(graph_items)
        .map(|(_, span)| span.end)
        .max()
        .unwrap_or(0)
        + 1
}

/// Append the synthesized items of one module, each recording the source
/// file of the declaration it renders. A graph root's items are also the
/// program's root surface, so both lists grow together.
fn append_items(
    program: &mut Program,
    module: Option<&ModulePath>,
    items: Vec<(Spanned<Item>, usize)>,
) {
    let Some(module) = module else {
        program
            .items
            .extend(items.into_iter().map(|(item, _)| item));
        return;
    };
    let graph = program
        .module_graph
        .as_mut()
        .expect("a module key names a graph module");
    let is_root = *module == graph.root;
    let Some(entry) = graph.modules.get_mut(module) else {
        return;
    };
    let mirrors_root =
        is_root && super::machine_normalize::same_item_list(&entry.items, &program.items);
    let fallback_source = entry.source_paths.first().cloned();
    for (item, declaration_index) in items {
        if let Some(sources) = graph.item_sources.get_mut(&module.dotted()) {
            let source = sources
                .get(declaration_index)
                .cloned()
                .or_else(|| fallback_source.clone());
            if let Some(source) = source {
                sources.push(source);
            }
        }
        if mirrors_root {
            program.items.push(item.clone());
        }
        entry.items.push(item);
    }
}

/// The Hew source of `declaration`'s implied Display. Its header is
/// replaced by a copy of the `impl Error` header.
fn display_impl_source(declaration: &TypeDecl) -> String {
    let name = declaration.name;
    let body = match declaration.kind {
        TypeDeclKind::Enum => enum_rendering(declaration),
        TypeDeclKind::Struct => record_rendering(declaration),
    };
    format!("impl Display for {name} {{ fn fmt(self) -> string {{ {body} }} }}")
}

/// The `impl Error` header's generics, bounds and target, with fresh spans.
fn copy_header(implementation: &ImplDecl, spans: &mut impl FreshSpans) -> ImplDecl {
    let mut header = ImplDecl {
        type_params: implementation.type_params.clone(),
        trait_bound: None,
        target_type: implementation.target_type.clone(),
        where_clause: implementation.where_clause.clone(),
        type_aliases: Vec::new(),
        methods: Vec::new(),
        doc_comment: None,
    };
    spans.refresh_generics(header.type_params.as_mut(), header.where_clause.as_mut());
    spans.refresh_type(&mut header.target_type);
    header
}

/// Give the compiler-written trait and return type names the expansion
/// context, so a binder spelled `Display` or `string` never captures them.
fn name_in_context(rendering: &mut ImplDecl, context: SyntaxContext) {
    let in_context = |path: &mut Path| {
        for (ident, _) in &mut path.segments {
            *ident = Ident {
                name: ident.name,
                ctx: context,
            };
        }
    };
    if let Some(TraitBound { path, .. }) = &mut rendering.trait_bound {
        in_context(path);
    }
    for method in &mut rendering.methods {
        if let Some((TypeExpr::Named { path, .. }, _)) = &mut method.return_type {
            in_context(path);
        }
    }
}

/// `match self` with one arm per variant: `Name`, `Name: a, b` or
/// `Name: field: a, other: b`.
fn enum_rendering(declaration: &TypeDecl) -> String {
    use hew_parser::ast::{TypeBodyItem, VariantKind};
    let mut arms = String::new();
    for item in &declaration.body {
        let TypeBodyItem::Variant(variant) = item else {
            continue;
        };
        let name = variant.name;
        match &variant.kind {
            VariantKind::Unit => {
                let _ = write!(arms, ".{name} => \"{name}\", ");
            }
            VariantKind::Tuple(payloads) => {
                let bindings: Vec<_> = (0..payloads.len()).map(|i| format!("value{i}")).collect();
                let rendered: Vec<_> = bindings.iter().map(|b| format!("{{{b}:?}}")).collect();
                let _ = write!(
                    arms,
                    ".{name}({}) => f\"{name}: {}\", ",
                    bindings.join(", "),
                    rendered.join(", ")
                );
            }
            VariantKind::Struct(fields) => {
                let names: Vec<_> = fields.iter().map(|(field, _)| field.to_string()).collect();
                let rendered: Vec<_> = names
                    .iter()
                    .map(|field| format!("{field}: {{{field}:?}}"))
                    .collect();
                let _ = write!(
                    arms,
                    ".{name} {{ {} }} => f\"{name}: {}\", ",
                    names.join(", "),
                    rendered.join(", ")
                );
            }
        }
    }
    if arms.is_empty() {
        format!("\"{}\"", declaration.name)
    } else {
        format!("match self {{ {arms}}}")
    }
}

/// `Name: field: a, other: b`, or `Name` for a record without fields.
fn record_rendering(declaration: &TypeDecl) -> String {
    use hew_parser::ast::TypeBodyItem;
    let rendered: Vec<_> = declaration
        .body
        .iter()
        .filter_map(|item| match item {
            TypeBodyItem::Field { name, .. } => Some(format!("{name}: {{self.{name}:?}}")),
            _ => None,
        })
        .collect();
    if rendered.is_empty() {
        format!("\"{}\"", declaration.name)
    } else {
        format!("f\"{}: {}\"", declaration.name, rendered.join(", "))
    }
}
