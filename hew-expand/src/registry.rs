use std::collections::{BTreeMap, BTreeSet};
use std::path::{Path, PathBuf};

use hew_parser::ast::{Ident, ImportSpec, Item, Span, SyntaxContext, Visibility};
use hew_parser::macro_syntax::{
    Expansion, ExpansionCategory, ExpansionId, ExpansionInput, ExpansionMap, ExpansionProvider,
    MacroCall, MacroDecl, MacroTemplate, Origin, TemplateBody,
};
use hew_parser::module::{ModuleGraph, ModulePath};

use crate::{transcribe, OriginKind, Pattern};

#[derive(Debug)]
struct Definition {
    declaration: MacroDecl,
    patterns: Vec<Pattern>,
    source: PathBuf,
}

#[derive(Debug)]
struct MacroImport {
    target: ModulePath,
    spec: Option<ImportSpec>,
    qualifier: String,
    span: Span,
    file_import: bool,
}

#[derive(Debug)]
pub(crate) struct Registry {
    definitions: BTreeMap<(ModulePath, String), Definition>,
    modules: BTreeMap<PathBuf, ModulePath>,
    imports: BTreeMap<PathBuf, Vec<MacroImport>>,
    ordinary_names: BTreeSet<(ModulePath, String)>,
    pub(crate) map: ExpansionMap,
    active: Vec<ExpansionId>,
    next_offset: usize,
    remaining_nodes: usize,
}

impl Registry {
    pub(crate) fn new(graph: &ModuleGraph, sources: BTreeMap<PathBuf, String>) -> Self {
        let next_offset = sources.values().map(String::len).max().unwrap_or(0) + 1;
        let modules: BTreeMap<_, _> = graph
            .modules
            .values()
            .flat_map(|module| {
                module
                    .source_paths
                    .iter()
                    .map(|path| (path.clone(), module.id.clone()))
            })
            .collect();
        let mut imports = BTreeMap::<PathBuf, Vec<MacroImport>>::new();
        let mut ordinary_names = BTreeSet::new();
        for module in graph.modules.values() {
            for (index, (item, span)) in module.items.iter().enumerate() {
                let name = match item {
                    Item::Function(value) => Some(value.name),
                    Item::Const(value) => Some(value.name),
                    Item::TypeDecl(value) => Some(value.name),
                    Item::TypeAlias(value) => Some(value.name),
                    Item::Record(value) => Some(value.name),
                    Item::Trait(value) => Some(value.name),
                    Item::Actor(value) => Some(value.name),
                    Item::Supervisor(value) => Some(value.name),
                    Item::Machine(value) => Some(value.name),
                    _ => None,
                };
                if let Some(name) = name {
                    ordinary_names.insert((module.id.clone(), name.to_string()));
                }
                let Item::Import(import) = item else { continue };
                let Some(source) = graph
                    .item_source(&module.id, index)
                    .or_else(|| module.source_paths.first())
                else {
                    continue;
                };
                let target = import
                    .resolved_source_paths
                    .iter()
                    .find_map(|path| modules.get(path))
                    .cloned()
                    .unwrap_or_else(|| ModulePath {
                        segments: import
                            .path
                            .segments
                            .iter()
                            .map(|(ident, _)| ident.name)
                            .collect(),
                    });
                let qualifier = import
                    .module_alias
                    .or_else(|| import.path.segments.last().map(|(ident, _)| *ident))
                    .map_or_else(String::new, |name| name.to_string());
                imports
                    .entry(source.clone())
                    .or_default()
                    .push(MacroImport {
                        span: span.clone(),
                        target,
                        qualifier,
                        spec: import.spec.clone(),
                        file_import: import.file_path.is_some(),
                    });
            }
        }
        Self {
            definitions: BTreeMap::new(),
            modules,
            imports,
            ordinary_names,
            map: ExpansionMap {
                sources,
                ..ExpansionMap::default()
            },
            active: Vec::new(),
            next_offset,
            remaining_nodes: 0,
        }
    }

    pub(crate) fn collect(&mut self, graph: &ModuleGraph) -> Result<(), (PathBuf, Span, String)> {
        for module in graph.modules.values() {
            for (index, (item, span)) in module.items.iter().enumerate() {
                let Item::Macro(declaration) = item else {
                    continue;
                };
                let Some(source) = graph
                    .item_source(&module.id, index)
                    .or_else(|| module.source_paths.first())
                else {
                    continue;
                };
                let key = (module.id.clone(), declaration.name.to_string());
                if self.definitions.contains_key(&key) {
                    return Err((
                        source.clone(),
                        span.clone(),
                        format!("duplicate macro `{}`", declaration.name),
                    ));
                }
                if hew_parser::macro_syntax::is_builtin_macro(declaration.name.name.as_str()) {
                    return Err((
                        source.clone(),
                        span.clone(),
                        "this macro name is reserved for a built-in".into(),
                    ));
                }
                let patterns = declaration
                    .rules
                    .iter()
                    .map(|rule| Pattern::compile(&rule.pattern))
                    .collect::<Result<Vec<_>, _>>()
                    .map_err(|error| (source.clone(), error.span, error.message))?;
                for (pattern, rule) in patterns.iter().zip(&declaration.rules) {
                    let template = validate_template(
                        pattern,
                        rule,
                        source,
                        self.next_offset,
                        declaration.visibility,
                    )
                    .map_err(|error| (source.clone(), error.span, error.message))?;
                    self.map.templates.push(template);
                }
                self.definitions.insert(
                    key,
                    Definition {
                        declaration: declaration.clone(),
                        patterns,
                        source: source.clone(),
                    },
                );
            }
        }
        for definition in self.definitions.values() {
            for rule in &definition.declaration.rules {
                self.validate_macro_references(&rule.template, definition)?;
            }
        }
        Ok(())
    }

    fn validate_macro_references(
        &self,
        tree: &hew_parser::macro_syntax::TokenTree,
        definition: &Definition,
    ) -> Result<(), (PathBuf, Span, String)> {
        use hew_parser::macro_syntax::TokenTree;
        let children = match tree {
            TokenTree::Group { children, .. }
            | TokenTree::Fragment {
                tokens: children, ..
            } => children,
            TokenTree::Interpolated { expressions, .. } => {
                for (_, children) in expressions {
                    self.validate_macro_reference_tokens(children, definition)?;
                }
                return Ok(());
            }
            TokenTree::Token { .. } => return Ok(()),
        };
        self.validate_macro_reference_tokens(children, definition)
    }

    fn validate_macro_reference_tokens(
        &self,
        children: &[hew_parser::macro_syntax::TokenTree],
        definition: &Definition,
    ) -> Result<(), (PathBuf, Span, String)> {
        use crate::tokens::spelling;
        use hew_parser::macro_syntax::TokenTree;
        let identifier = |tree: &TokenTree| {
            let TokenTree::Token {
                spelling,
                span,
                context,
                ..
            } = tree
            else {
                return None;
            };
            let mut tokens = hew_lexer::Lexer::new(spelling);
            matches!(tokens.next(), Some((hew_lexer::Token::Identifier(_), _))).then(|| {
                (
                    Ident {
                        name: hew_parser::ast::Symbol::intern(spelling),
                        ctx: *context,
                    },
                    span.clone(),
                )
            })
        };
        let mut index = 0;
        while index < children.len() {
            if let Some(head) = identifier(&children[index]) {
                let mut path = vec![head];
                let mut cursor = index + 1;
                while children.get(cursor).and_then(spelling) == Some(".") {
                    let Some(segment) = children.get(cursor + 1).and_then(identifier) else {
                        break;
                    };
                    path.push(segment);
                    cursor += 2;
                }
                if children.get(cursor).and_then(spelling) == Some("!") {
                    if let Some(input @ TokenTree::Group { .. }) = children.get(cursor + 1) {
                        let captured =
                            index > 0 && matches!(spelling(&children[index - 1]), Some("$" | "."));
                        let builtin = path.len() == 1
                            && hew_parser::macro_syntax::is_builtin_macro(path[0].0.name.as_str());
                        if !captured && !builtin {
                            let span = path[0].1.clone();
                            let call = MacroCall {
                                path: hew_parser::ast::Path { segments: path },
                                input: input.clone(),
                            };
                            let (target, _) =
                                self.definition(&call, &definition.source)
                                    .map_err(|error| {
                                        (definition.source.clone(), span.clone(), error)
                                    })?;
                            if definition.declaration.visibility == Visibility::Pub
                                && target.declaration.visibility != Visibility::Pub
                            {
                                return Err((definition.source.clone(), span, "E_MACRO_PRIVATE_REFERENCE: a public macro cannot invoke a private macro".into()));
                            }
                        }
                        // Invocation input remains syntax data until a rule selects its fragment kinds.
                        index = cursor + 2;
                        continue;
                    }
                }
            }
            self.validate_macro_references(&children[index], definition)?;
            index += 1;
        }
        Ok(())
    }

    fn definition(
        &self,
        call: &MacroCall,
        source: &Path,
    ) -> Result<(&Definition, Option<(PathBuf, Span)>), String> {
        let Some((first, _)) = call.path.segments.first() else {
            return Err("empty macro path".into());
        };
        let anchor = self.anchor(*first, source);
        let module = self
            .modules
            .get(anchor)
            .ok_or("macro source has no module")?;
        let name = call
            .path
            .segments
            .last()
            .expect("nonempty path")
            .0
            .to_string();
        if call.path.segments.len() == 1 {
            if let Some(definition) = self.definitions.get(&(module.clone(), name.clone())) {
                return Ok((definition, None));
            }
        }
        let prefix = call.path.segments[..call.path.segments.len() - 1]
            .iter()
            .map(|(ident, _)| ident.name.as_str())
            .collect::<Vec<_>>()
            .join(".");
        let mut found = None;
        for import in self.imports.get(anchor).into_iter().flatten() {
            let imported_name = match &import.spec {
                Some(ImportSpec::Names(names)) if prefix.is_empty() => {
                    names.iter().find_map(|entry| {
                        (entry.alias.as_ref().unwrap_or(&entry.name).name.as_str() == name)
                            .then(|| entry.name.to_string())
                    })
                }
                None if import.file_import && prefix.is_empty()
                    || import.qualifier == prefix
                    || import.target.dotted() == prefix =>
                {
                    Some(name.clone())
                }
                _ => None,
            };
            let Some(imported_name) = imported_name else {
                continue;
            };
            if let Some(definition) = self
                .definitions
                .get(&(import.target.clone(), imported_name))
            {
                if !import.file_import && definition.declaration.visibility == Visibility::Private {
                    return Err(format!("macro `{name}` is private"));
                }
                if found
                    .replace((
                        definition,
                        Some((anchor.to_path_buf(), import.span.clone())),
                    ))
                    .is_some()
                {
                    return Err(format!("macro `{name}` is ambiguous"));
                }
            }
        }
        found.ok_or_else(|| {
            format!(
                "unknown macro `{}`",
                call.path
                    .segments
                    .iter()
                    .map(|(ident, _)| ident.to_string())
                    .collect::<Vec<_>>()
                    .join(".")
            )
        })
    }

    pub(crate) fn strip_macro_names(&self, target: &ModulePath, spec: &mut Option<ImportSpec>) {
        if let Some(ImportSpec::Names(names)) = spec {
            names.retain(|name| {
                let key = (target.clone(), name.name.to_string());
                !self.definitions.contains_key(&key) || self.ordinary_names.contains(&key)
            });
        }
    }

    pub(crate) fn strip_import(
        &self,
        import: &mut hew_parser::ast::ImportDecl,
        source: &Path,
        span: &Span,
    ) {
        if let Some(resolved) = self
            .imports
            .get(source)
            .into_iter()
            .flatten()
            .find(|import| import.span == *span)
        {
            self.strip_macro_names(&resolved.target, &mut import.spec);
        }
    }

    fn anchor<'a>(&'a self, ident: Ident, source: &'a Path) -> &'a Path {
        if ident.ctx == SyntaxContext::ROOT {
            source
        } else {
            self.map
                .contexts
                .rows
                .get(ident.ctx.row() as usize - 1)
                .map_or(source, |row| row.definition_source.as_path())
        }
    }
}

impl ExpansionProvider for Registry {
    fn begin(
        &mut self,
        call: &MacroCall,
        category: ExpansionCategory,
        source: &Path,
    ) -> Result<ExpansionInput, String> {
        if self.active.len() >= 64 {
            let mut chain: Vec<_> = self
                .active
                .iter()
                .map(|id| self.map.expansions[id.0 as usize].name.as_str())
                .collect();
            chain.push(
                call.path
                    .segments
                    .last()
                    .map_or("", |(ident, _)| ident.name.as_str()),
            );
            return Err(format!(
                "macro expansion exceeded depth 64: {}",
                chain.join(" -> ")
            ));
        }
        if self.active.is_empty() {
            self.remaining_nodes = 1_000_000;
        }
        let (definition, imported) = self.definition(call, source)?;
        let matched = definition
            .patterns
            .iter()
            .zip(&definition.declaration.rules)
            .find_map(|(pattern, rule)| {
                pattern
                    .matches(&call.input)
                    .map(|captures| (pattern, rule, captures))
            })
            .ok_or_else(|| {
                let shapes = definition
                    .declaration
                    .rules
                    .iter()
                    .filter_map(|rule| {
                        self.map
                            .sources
                            .get(&definition.source)
                            .and_then(|source| source.get(rule.pattern.span().clone()))
                    })
                    .collect::<Vec<_>>()
                    .join(" or ");
                format!("no macro rule matches this input; expected {shapes}")
            })?;
        let (pattern, rule, captures) = matched;
        if rule.category != category {
            return Err(format!(
                "the first matching macro rule produces {:?}, but this position requires {:?}",
                rule.category, category
            ));
        }
        let macro_name = definition.declaration.name.to_string();
        let definition_source = definition.source.clone();
        let definition_span = rule.span.clone();
        let id = ExpansionId(
            self.map
                .expansions
                .len()
                .try_into()
                .map_err(|_| "too many macro expansions")?,
        );
        let context = SyntaxContext::from_row(
            (self.map.contexts.rows.len() + 1)
                .try_into()
                .map_err(|_| "too many macro contexts")?,
        );
        let mut remaining_nodes = self.remaining_nodes;
        let generated = transcribe(
            pattern,
            &rule.template,
            &captures,
            context,
            self.next_offset,
            &mut remaining_nodes,
        )
        .map_err(|error| error.message)?;
        if let Some(imported) = imported {
            if !self.map.used_imports.contains(&imported) {
                self.map.used_imports.push(imported);
            }
        }
        self.map
            .contexts
            .mint(call.path.segments[0].0.ctx, id, definition_source.clone());
        self.remaining_nodes = remaining_nodes;
        self.next_offset += generated.source.len() + 1;
        self.map.expansions.push(Expansion {
            name: macro_name,
            file: source.to_path_buf(),
            synthetic: generated.offset..generated.offset + generated.source.len(),
            invocation: Origin::Caller {
                file: source.to_path_buf(),
                span: call.path.segments[0].1.start..call.input.span().end,
            },
            callee: Origin::Caller {
                file: source.to_path_buf(),
                span: call.path.segments[0].1.start..call.path.segments.last().unwrap().1.end,
            },
            definition: Origin::Template {
                file: definition_source.clone(),
                span: definition_span,
            },
            parent: self.active.last().copied(),
            origins: generated
                .origins
                .into_iter()
                .map(|origin| {
                    let original = match origin.kind {
                        OriginKind::Template => Origin::Template {
                            file: definition_source.clone(),
                            span: origin.original,
                        },
                        OriginKind::Capture => Origin::Caller {
                            file: source.to_path_buf(),
                            span: origin.original,
                        },
                    };
                    (origin.generated, original)
                })
                .collect(),
        });
        self.active.push(id);
        Ok(ExpansionInput {
            source: generated.source,
            offset: generated.offset,
            contexts: generated.contexts,
            fragments: generated.fragments,
        })
    }

    fn complete(&mut self) {
        self.active.pop();
    }

    fn declare(&mut self, _: &MacroDecl, _: Span, _: &Path) -> Result<(), String> {
        if self.active.is_empty() {
            Ok(())
        } else {
            Err("a macro expansion cannot introduce a macro declaration".into())
        }
    }
}

fn validate_template(
    pattern: &Pattern,
    rule: &hew_parser::macro_syntax::MacroRule,
    source: &Path,
    offset: usize,
    visibility: Visibility,
) -> Result<MacroTemplate, crate::ExpansionError> {
    use hew_parser::macro_syntax::{Fragment, FragmentKind};
    let generated = transcribe(
        pattern,
        &rule.template,
        &pattern.placeholders(),
        SyntaxContext::ROOT,
        offset,
        &mut 1_000_000,
    )?;
    let original = |span: &Span| {
        generated
            .origins
            .iter()
            .find(|origin| origin.generated.contains(&span.start))
            .map_or_else(|| rule.span.clone(), |origin| origin.original.clone())
    };
    let map = ExpansionMap {
        expansions: vec![Expansion {
            name: String::new(),
            file: source.to_path_buf(),
            synthetic: offset..offset + generated.source.len(),
            invocation: Origin::Caller {
                file: source.to_path_buf(),
                span: rule.span.clone(),
            },
            callee: Origin::Caller {
                file: source.to_path_buf(),
                span: rule.span.clone(),
            },
            definition: Origin::Template {
                file: source.to_path_buf(),
                span: rule.span.clone(),
            },
            parent: None,
            origins: generated
                .origins
                .iter()
                .map(|origin| {
                    (
                        origin.generated.clone(),
                        match origin.kind {
                            OriginKind::Template => Origin::Template {
                                file: source.to_path_buf(),
                                span: origin.original.clone(),
                            },
                            OriginKind::Capture => Origin::Caller {
                                file: source.to_path_buf(),
                                span: origin.original.clone(),
                            },
                        },
                    )
                })
                .collect(),
        }],
        ..ExpansionMap::default()
    };
    let mut control = crate::control::Control::new(&map);
    let result = match rule.category {
        ExpansionCategory::Expr => {
            let parsed = hew_parser::parse_generated_fragment(
                &generated.source,
                offset,
                generated.contexts,
                generated.fragments,
                FragmentKind::Expr,
            );
            if let Some(error) = parsed.errors.first() {
                return Err(crate::ExpansionError {
                    message: error.message.clone(),
                    span: original(&error.span),
                });
            }
            if !generated.source[parsed.consumed..].trim().is_empty() {
                return Err(crate::ExpansionError {
                    message: "an expression macro must produce exactly one expression".into(),
                    span: rule.span.clone(),
                });
            }
            let Some(Fragment::Expr(mut expression)) = parsed.fragment else {
                return Err(crate::ExpansionError {
                    message: "expected an expression macro template".into(),
                    span: rule.span.clone(),
                });
            };
            control
                .expr(&mut expression)
                .map(|()| TemplateBody::Expr(expression))
        }
        ExpansionCategory::Items => {
            let parsed = hew_parser::parse_generated(
                &generated.source,
                offset,
                generated.contexts,
                generated.fragments,
            );
            if let Some(error) = parsed
                .errors
                .iter()
                .find(|error| error.severity == hew_parser::Severity::Error)
            {
                return Err(crate::ExpansionError {
                    message: error.message.clone(),
                    span: original(&error.span),
                });
            }
            if parsed.program.items.is_empty() {
                return Err(crate::ExpansionError {
                    message: "an items macro must produce at least one declaration".into(),
                    span: rule.span.clone(),
                });
            }
            let mut items = parsed.program.items;
            for (item, span) in &mut items {
                if matches!(item, Item::Import(_) | Item::Macro(_)) {
                    return Err(crate::ExpansionError {
                        message: "an items macro cannot introduce imports or macro declarations"
                            .into(),
                        span: original(&span),
                    });
                }
                control
                    .item(item, span)
                    .map_err(|error| crate::ExpansionError {
                        message: error.message,
                        span: original(&error.span),
                    })?;
            }
            Ok(TemplateBody::Items(items))
        }
    };
    result
        .map(|body| MacroTemplate {
            source: source.to_path_buf(),
            visibility,
            body,
            origins: map.expansions[0].origins.clone(),
        })
        .map_err(|error| crate::ExpansionError {
            message: error.message,
            span: original(&error.span),
        })
}
