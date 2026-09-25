//! The spelling boundary (identity plan §3.2): the one place a source
//! identifier becomes an identity.
//!
//! A head identifier `Ident { name, ctx }` binds to the innermost lexical
//! binder with the same `(name, ctx)`; failing that, to an item of the module
//! the context was written in (the caller's module for source identifiers,
//! the defining module of a macro template for an expansion context), then
//! that module's imports, then the prelude. Every further segment walks the
//! binding found: a module's items, a nominal's or trait's members, an enum's
//! variants. Every segment resolved is recorded in `resolutions` by span so
//! tooling and lowering read the answer instead of resolving again.

use std::collections::HashMap;

use hew_parser::ast::{Ident, Span, Spanned, Symbol, SyntaxContext};

use super::types::SpanKey;
use crate::builtin_type::BuiltinType;
use crate::def_table::{DeclarationKind, Predicate, TypeParamId};
use crate::env::{TypeBindingId, TypeEnv};
use crate::{DefId, DefTable, ModuleId, NominalId};

/// What a name is bound to in a module or the prelude.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Binding {
    Type(NominalId),
    Actor(NominalId),
    Trait(DefId),
    Fn(DefId),
    Const(DefId),
    Module(ModuleId),
    Builtin(BuiltinType),
    /// A compiler predicate (`Send`, `Clone`, ...): a sourceless trait row.
    Predicate(DefId),
}

impl Binding {
    /// The binding a top-level declaration row introduces, or `None` for a
    /// member row (methods, handlers, states), which binds under its owner.
    #[must_use]
    pub fn of_item(defs: &DefTable, id: DefId) -> Option<Self> {
        let nominal = || NominalId::from_minted_declaration(id);
        Some(match defs.kind(id) {
            DeclarationKind::Type
            | DeclarationKind::TypeAlias
            | DeclarationKind::Record
            | DeclarationKind::Supervisor
            | DeclarationKind::Machine => Self::Type(nominal()),
            DeclarationKind::Actor => Self::Actor(nominal()),
            DeclarationKind::Trait if DefTable::as_predicate(id).is_some() => Self::Predicate(id),
            DeclarationKind::Trait => Self::Trait(id),
            DeclarationKind::Function | DeclarationKind::ExternFunction => Self::Fn(id),
            DeclarationKind::Const => Self::Const(id),
            _ => return None,
        })
    }

    fn resolution(self) -> Resolution {
        match self {
            Self::Type(id) | Self::Actor(id) => Resolution::Nominal(id),
            Self::Trait(id) | Self::Fn(id) | Self::Const(id) | Self::Predicate(id) => {
                Resolution::Def(id)
            }
            Self::Module(id) => Resolution::Module(id),
            Self::Builtin(builtin) => Resolution::Builtin(builtin),
        }
    }
}

/// The identity one resolved path segment names.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Resolution {
    /// A function, constant, trait or predicate.
    Def(DefId),
    Nominal(NominalId),
    Param(TypeParamId),
    Local(TypeBindingId),
    Module(ModuleId),
    Builtin(BuiltinType),
    /// A variant of an enum, by declaration index.
    Variant(NominalId, u32),
    /// A member row under a nominal, trait or actor: a method, handler,
    /// machine state or companion.
    Member(DefId),
}

/// Which namespace a head identifier is looked up in. A type position never
/// sees a value local, and a value position never sees a generic binder.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Namespace {
    Type,
    Value,
}

/// A path whose segment `segment` names nothing reachable from the segments
/// before it.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Unresolved {
    pub segment: usize,
    pub name: Ident,
    pub span: Span,
}

/// Where a path is being resolved: the file it was written in (imports are
/// file-local) and that file's span-key index.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ScopeSite {
    pub file: ModuleId,
    pub span_file: u32,
}

/// One hygiene context: which context it was minted under and, for an
/// expansion, the module its template was written in.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ContextRow {
    pub parent: SyntaxContext,
    /// `None` for [`SyntaxContext::ROOT`]: a source identifier resolves in the
    /// module it was written in.
    pub def_module: Option<ModuleId>,
}

/// The syntax contexts of one compilation. Until macro expansion mints
/// contexts, the table holds only the source context.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxContexts {
    rows: Vec<ContextRow>,
}

impl Default for SyntaxContexts {
    fn default() -> Self {
        Self::new()
    }
}

impl SyntaxContexts {
    #[must_use]
    pub fn new() -> Self {
        Self {
            rows: vec![ContextRow {
                parent: SyntaxContext::ROOT,
                def_module: None,
            }],
        }
    }

    /// Mint an expansion context whose template was written in `def_module`.
    ///
    /// # Panics
    ///
    /// Panics if a compilation mints more than `u32::MAX` contexts.
    pub fn mint(&mut self, parent: SyntaxContext, def_module: ModuleId) -> SyntaxContext {
        let ctx = SyntaxContext::from_row(
            u32::try_from(self.rows.len()).expect("more than u32::MAX syntax contexts"),
        );
        self.rows.push(ContextRow {
            parent,
            def_module: Some(def_module),
        });
        ctx
    }

    /// The module whose items an identifier of `ctx` resolves against.
    #[must_use]
    pub fn def_module(&self, ctx: SyntaxContext, site: ModuleId) -> ModuleId {
        self.rows
            .get(ctx.row() as usize)
            .and_then(|row| row.def_module)
            .unwrap_or(site)
    }

    #[must_use]
    pub fn len(&self) -> usize {
        self.rows.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.rows.is_empty()
    }
}

/// One source file's imports and the namespace its declarations join.
#[derive(Debug, Clone, Default)]
struct FileScope {
    /// The module whose items this file sees unqualified: its own for a
    /// single-file module, the assembler for a directory module's peer file.
    namespace: Option<ModuleId>,
    imports: HashMap<Symbol, Binding>,
}

/// Every scope of one compilation, and the resolutions made through them.
#[derive(Debug, Clone, Default)]
pub struct Scopes {
    items: HashMap<ModuleId, HashMap<Symbol, Binding>>,
    files: HashMap<ModuleId, FileScope>,
    prelude: HashMap<Symbol, Binding>,
    members: HashMap<(DefId, Symbol), DefId>,
    variants: HashMap<(NominalId, Symbol), u32>,
    /// Generic binders in scope, innermost last.
    type_params: Vec<HashMap<Ident, TypeParamId>>,
    contexts: SyntaxContexts,
    resolutions: HashMap<SpanKey, Resolution>,
}

impl Scopes {
    /// Scopes holding the prelude: the compiler predicates and the builtin
    /// types written without an import.
    #[must_use]
    pub fn new() -> Self {
        let mut scopes = Self::default();
        for predicate in Predicate::ALL {
            scopes.prelude.insert(
                Symbol::intern(predicate.spelling()),
                Binding::Predicate(DefTable::predicate(predicate)),
            );
        }
        for info in crate::builtin_type::builtin_types() {
            let builtin = info.kind;
            if builtin.is_encoding_value()
                || builtin.requires_source_import()
                || info.canonical_name.contains('.')
            {
                continue;
            }
            scopes
                .prelude
                .entry(Symbol::intern(info.canonical_name))
                .or_insert(Binding::Builtin(builtin));
        }
        scopes
    }

    /// Record that `file` joins `namespace`'s items.
    pub fn join_namespace(&mut self, file: ModuleId, namespace: ModuleId) {
        self.files.entry(file).or_default().namespace = Some(namespace);
    }

    /// Bind a top-level declaration in its module's namespace. Returns the
    /// binding it displaced, which the caller reports as a duplicate.
    pub fn declare_item(
        &mut self,
        namespace: ModuleId,
        name: Symbol,
        binding: Binding,
    ) -> Option<Binding> {
        let previous = self
            .items
            .entry(namespace)
            .or_default()
            .insert(name, binding);
        previous.filter(|previous| *previous != binding)
    }

    /// The item `name` binds in `namespace`.
    #[must_use]
    pub fn item(&self, namespace: ModuleId, name: Symbol) -> Option<Binding> {
        self.items.get(&namespace)?.get(&name).copied()
    }

    /// Every item of `namespace`.
    #[must_use]
    pub fn items(&self, namespace: ModuleId) -> Vec<(Symbol, Binding)> {
        self.items.get(&namespace).map_or_else(Vec::new, |items| {
            items
                .iter()
                .map(|(name, binding)| (*name, *binding))
                .collect()
        })
    }

    /// Bind a member row under its owner.
    pub fn declare_member(&mut self, owner: DefId, name: Symbol, member: DefId) {
        self.members.entry((owner, name)).or_insert(member);
    }

    /// Bind an enum's variants in declaration order.
    ///
    /// # Panics
    ///
    /// Panics when an enum declares more than `u32::MAX` variants.
    pub fn declare_variants(&mut self, nominal: NominalId, variants: &[Symbol]) {
        for (index, name) in variants.iter().enumerate() {
            self.variants.insert(
                (nominal, *name),
                u32::try_from(index).expect("more than u32::MAX variants"),
            );
        }
    }

    /// Bind an import in one file's scope.
    pub fn bind_import(&mut self, file: ModuleId, name: Symbol, binding: Binding) {
        self.files
            .entry(file)
            .or_default()
            .imports
            .insert(name, binding);
    }

    /// Bind a name in the prelude.
    pub fn bind_prelude(&mut self, name: Symbol, binding: Binding) {
        self.prelude.insert(name, binding);
    }

    /// Open a generic binder scope.
    pub fn push_type_params(&mut self, params: impl IntoIterator<Item = (Ident, TypeParamId)>) {
        self.type_params.push(params.into_iter().collect());
    }

    /// Close the innermost generic binder scope.
    pub fn pop_type_params(&mut self) {
        self.type_params.pop();
    }

    #[must_use]
    pub fn contexts(&self) -> &SyntaxContexts {
        &self.contexts
    }

    pub fn contexts_mut(&mut self) -> &mut SyntaxContexts {
        &mut self.contexts
    }

    /// Every segment resolved so far, by span.
    #[must_use]
    pub fn resolutions(&self) -> &HashMap<SpanKey, Resolution> {
        &self.resolutions
    }

    /// Take the resolution table for publication.
    pub fn take_resolutions(&mut self) -> HashMap<SpanKey, Resolution> {
        std::mem::take(&mut self.resolutions)
    }

    /// Resolve a whole path. Every segment must name something.
    ///
    /// # Errors
    ///
    /// Returns the first segment that names nothing reachable.
    pub fn resolve(
        &mut self,
        env: &TypeEnv,
        site: ScopeSite,
        namespace: Namespace,
        path: &[Spanned<Ident>],
    ) -> Result<Resolution, Unresolved> {
        let (resolution, rest) = self.resolve_prefix(env, site, namespace, path)?;
        match rest.first() {
            None => Ok(resolution),
            Some((name, span)) => Err(Unresolved {
                segment: path.len() - rest.len(),
                name: *name,
                span: span.clone(),
            }),
        }
    }

    /// Resolve the longest item prefix of an `Ident`/`FieldAccess` chain and
    /// return the member segments left over (fields, methods).
    ///
    /// # Errors
    ///
    /// Returns an error only when the head names nothing.
    pub fn resolve_prefix<'p>(
        &mut self,
        env: &TypeEnv,
        site: ScopeSite,
        namespace: Namespace,
        path: &'p [Spanned<Ident>],
    ) -> Result<(Resolution, &'p [Spanned<Ident>]), Unresolved> {
        let Some((head, head_span)) = path.first() else {
            return Err(Unresolved {
                segment: 0,
                name: Ident::from_symbol(hew_parser::ast::sym::UNDERSCORE),
                span: 0..0,
            });
        };
        let Some(mut current) = self.resolve_head(env, site, namespace, *head) else {
            return Err(Unresolved {
                segment: 0,
                name: *head,
                span: head_span.clone(),
            });
        };
        self.record(site, head_span, current);
        let mut index = 1;
        while let Some((segment, span)) = path.get(index) {
            let Some(next) = self.resolve_member(current, segment.name) else {
                break;
            };
            self.record(site, span, next);
            current = next;
            index += 1;
        }
        Ok((current, &path[index..]))
    }

    fn record(&mut self, site: ScopeSite, span: &Span, resolution: Resolution) {
        if span.is_empty() {
            return;
        }
        self.resolutions
            .insert(SpanKey::in_module(span, site.span_file), resolution);
    }

    fn resolve_head(
        &self,
        env: &TypeEnv,
        site: ScopeSite,
        namespace: Namespace,
        head: Ident,
    ) -> Option<Resolution> {
        match namespace {
            Namespace::Value => {
                if let Some(binding) = env.lookup_ref(head) {
                    return Some(Resolution::Local(binding.id));
                }
            }
            Namespace::Type => {
                if let Some(param) = self
                    .type_params
                    .iter()
                    .rev()
                    .find_map(|scope| scope.get(&head))
                {
                    return Some(Resolution::Param(*param));
                }
            }
        }
        let module = self.contexts.def_module(head.ctx, site.file);
        let file = self.files.get(&module);
        let own = file.and_then(|file| file.namespace).unwrap_or(module);
        self.items
            .get(&own)
            .and_then(|items| items.get(&head.name))
            .or_else(|| file.and_then(|file| file.imports.get(&head.name)))
            .or_else(|| self.prelude.get(&head.name))
            .map(|binding| binding.resolution())
    }

    fn resolve_member(&self, current: Resolution, name: Symbol) -> Option<Resolution> {
        match current {
            Resolution::Module(module) => self
                .items
                .get(&module)
                .and_then(|items| items.get(&name))
                .map(|binding| binding.resolution()),
            Resolution::Nominal(nominal) => self
                .variants
                .get(&(nominal, name))
                .map(|index| Resolution::Variant(nominal, *index))
                .or_else(|| self.member(nominal.declaration(), name)),
            Resolution::Def(owner) | Resolution::Member(owner) => self.member(owner, name),
            Resolution::Param(_)
            | Resolution::Local(_)
            | Resolution::Builtin(_)
            | Resolution::Variant(..) => None,
        }
    }

    fn member(&self, owner: DefId, name: Symbol) -> Option<Resolution> {
        self.members
            .get(&(owner, name))
            .map(|member| Resolution::Member(*member))
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::def_table::DeclarationOccurrence;
    use crate::ty::Ty;

    fn declare(
        defs: &mut DefTable,
        scopes: &mut Scopes,
        module: ModuleId,
        span: std::ops::Range<usize>,
        kind: DeclarationKind,
        name: &str,
        path: &str,
    ) -> DefId {
        let id = defs
            .declare(
                DeclarationOccurrence::new(Some(module), &span, kind, 0),
                Symbol::intern(name),
                None,
                path,
            )
            .expect("fresh declaration");
        let binding = Binding::of_item(defs, id).expect("an item row");
        scopes.declare_item(module, Symbol::intern(name), binding);
        id
    }

    fn at(name: &str, start: usize) -> Spanned<Ident> {
        (Ident::new(name), start..start + name.len())
    }

    /// Hygiene readiness (identity plan §3.8): an identifier written by an
    /// expansion context resolves in the template's defining module and never
    /// sees the caller's locals; the same spelling in the source context
    /// resolves in the caller's module and sees them.
    #[test]
    fn expansion_context_resolves_in_its_defining_module() {
        let mut defs = DefTable::new();
        let mut scopes = Scopes::new();
        let ma = defs.mint_module("ma", &[std::path::PathBuf::from("/nonexistent/ma.hew")]);
        let root = defs
            .mint_root_module(&[std::path::PathBuf::from("/nonexistent/main.hew")])
            .expect("root");
        let ma_helper = declare(
            &mut defs,
            &mut scopes,
            ma,
            0..10,
            DeclarationKind::Function,
            "helper",
            "ma.helper",
        );
        let root_helper = declare(
            &mut defs,
            &mut scopes,
            root,
            0..10,
            DeclarationKind::Function,
            "helper",
            "helper",
        );
        let ctx1 = scopes.contexts_mut().mint(SyntaxContext::ROOT, ma);
        assert_eq!(scopes.contexts().len(), 2);

        let mut env = TypeEnv::new();
        env.define_with_span("v", Ty::I64, false, 40..41);
        let local = env.lookup_ref("v").expect("local").id;
        let site = ScopeSite {
            file: root,
            span_file: 0,
        };

        let caller_v = [(Ident::new("v"), 50..51)];
        assert_eq!(
            scopes.resolve(&env, site, Namespace::Value, &caller_v),
            Ok(Resolution::Local(local))
        );
        let template_v = [(
            Ident {
                name: Symbol::intern("v"),
                ctx: ctx1,
            },
            60..61,
        )];
        assert!(
            scopes
                .resolve(&env, site, Namespace::Value, &template_v)
                .is_err(),
            "a template identifier must not capture a caller local"
        );

        let caller_helper = [at("helper", 70)];
        assert_eq!(
            scopes.resolve(&env, site, Namespace::Value, &caller_helper),
            Ok(Resolution::Def(root_helper))
        );
        let template_helper = [(
            Ident {
                name: Symbol::intern("helper"),
                ctx: ctx1,
            },
            80..86,
        )];
        assert_eq!(
            scopes.resolve(&env, site, Namespace::Value, &template_helper),
            Ok(Resolution::Def(ma_helper))
        );
    }

    /// A module is reached through its import binding and its items through
    /// the module; a module is never visible transitively, and a segment
    /// past an item that has no such member is reported by position.
    #[test]
    fn qualified_paths_walk_module_items_and_members() {
        let mut defs = DefTable::new();
        let mut scopes = Scopes::new();
        let ma = defs.mint_module("ma", &[std::path::PathBuf::from("/nonexistent/ma.hew")]);
        let mb = defs.mint_module("mb", &[std::path::PathBuf::from("/nonexistent/mb.hew")]);
        let root = defs
            .mint_root_module(&[std::path::PathBuf::from("/nonexistent/main.hew")])
            .expect("root");
        let shape = declare(
            &mut defs,
            &mut scopes,
            ma,
            0..10,
            DeclarationKind::Type,
            "Shape",
            "ma.Shape",
        );
        declare(
            &mut defs,
            &mut scopes,
            mb,
            0..10,
            DeclarationKind::Type,
            "X",
            "mb.X",
        );
        let shape_nominal = NominalId::from_minted_declaration(shape);
        scopes.declare_variants(shape_nominal, &[Symbol::intern("Circle")]);
        scopes.bind_import(root, Symbol::intern("ma"), Binding::Module(ma));
        scopes.bind_import(ma, Symbol::intern("mb"), Binding::Module(mb));
        let env = TypeEnv::new();
        let site = ScopeSite {
            file: root,
            span_file: 0,
        };

        let path = [at("ma", 0), at("Shape", 3)];
        assert_eq!(
            scopes.resolve(&env, site, Namespace::Type, &path),
            Ok(Resolution::Nominal(shape_nominal))
        );
        assert_eq!(
            scopes.resolutions().get(&SpanKey::in_module(&(0..2), 0)),
            Some(&Resolution::Module(ma))
        );
        let variant = [at("ma", 20), at("Shape", 23), at("Circle", 29)];
        assert_eq!(
            scopes.resolve(&env, site, Namespace::Value, &variant),
            Ok(Resolution::Variant(shape_nominal, 0))
        );
        let transitive = [at("mb", 40), at("X", 43)];
        assert_eq!(
            scopes.resolve(&env, site, Namespace::Type, &transitive),
            Err(Unresolved {
                segment: 0,
                name: Ident::new("mb"),
                span: 40..42,
            })
        );
        let chain = [at("ma", 50), at("Shape", 53), at("area", 59)];
        let (resolution, rest) = scopes
            .resolve_prefix(&env, site, Namespace::Value, &chain)
            .expect("prefix");
        assert_eq!(resolution, Resolution::Nominal(shape_nominal));
        assert_eq!(rest.len(), 1);
    }

    /// A user trait spelled like a predicate takes the name in its module
    /// scope; the predicate stays reachable from modules that declare none.
    #[test]
    fn module_items_shadow_prelude_predicates() {
        let mut defs = DefTable::new();
        let mut scopes = Scopes::new();
        let root = defs
            .mint_root_module(&[std::path::PathBuf::from("/nonexistent/main.hew")])
            .expect("root");
        let other = defs.mint_module("other", &[std::path::PathBuf::from("/nonexistent/o.hew")]);
        let user_send = declare(
            &mut defs,
            &mut scopes,
            root,
            0..10,
            DeclarationKind::Trait,
            "Send",
            "Send#user",
        );
        let env = TypeEnv::new();
        let send = [at("Send", 0)];
        assert_eq!(
            scopes.resolve(
                &env,
                ScopeSite {
                    file: root,
                    span_file: 0
                },
                Namespace::Type,
                &send
            ),
            Ok(Resolution::Def(user_send))
        );
        assert_eq!(
            scopes.resolve(
                &env,
                ScopeSite {
                    file: other,
                    span_file: 1
                },
                Namespace::Type,
                &send
            ),
            Ok(Resolution::Def(DefTable::predicate(Predicate::Send)))
        );
    }

    /// A generic binder shadows a nominal of the same spelling in type
    /// position and is invisible in value position (rule R6).
    #[test]
    fn type_params_shadow_nominals_in_type_position_only() {
        let mut defs = DefTable::new();
        let mut scopes = Scopes::new();
        let root = defs
            .mint_root_module(&[std::path::PathBuf::from("/nonexistent/main.hew")])
            .expect("root");
        let nominal = declare(
            &mut defs,
            &mut scopes,
            root,
            0..10,
            DeclarationKind::Type,
            "T",
            "T",
        );
        let function = declare(
            &mut defs,
            &mut scopes,
            root,
            20..40,
            DeclarationKind::Function,
            "g",
            "g",
        );
        let param = TypeParamId::new(function, 0);
        scopes.push_type_params([(Ident::new("T"), param)]);
        let env = TypeEnv::new();
        let site = ScopeSite {
            file: root,
            span_file: 0,
        };
        let t = [at("T", 25)];
        assert_eq!(
            scopes.resolve(&env, site, Namespace::Type, &t),
            Ok(Resolution::Param(param))
        );
        assert_eq!(
            scopes.resolve(&env, site, Namespace::Value, &t),
            Ok(Resolution::Nominal(NominalId::from_minted_declaration(
                nominal
            )))
        );
        scopes.pop_type_params();
        assert_eq!(
            scopes.resolve(&env, site, Namespace::Type, &t),
            Ok(Resolution::Nominal(NominalId::from_minted_declaration(
                nominal
            )))
        );
    }
}
