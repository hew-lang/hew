use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use hew_parser::ast::{Ident, ImportSpec, Item, Span, SyntaxContext, Visibility};
use hew_parser::macro_syntax::{
    Expansion, ExpansionCategory, ExpansionId, ExpansionInput, ExpansionMap, ExpansionProvider,
    MacroCall, MacroDecl, Origin,
};
use hew_parser::module::{ModuleGraph, ModulePath};

use crate::{transcribe, OriginKind, Pattern};

#[derive(Debug)]
struct Definition {
    declaration: MacroDecl,
    patterns: Vec<Pattern>,
    source: PathBuf,
    span: Span,
}

#[derive(Debug)]
pub(crate) struct Registry {
    definitions: BTreeMap<(ModulePath, String), Definition>,
    modules: BTreeMap<PathBuf, ModulePath>,
    imports: BTreeMap<ModulePath, Vec<hew_parser::module::ModuleImport>>,
    pub(crate) map: ExpansionMap,
    active: Vec<ExpansionId>,
    next_offset: usize,
    remaining_nodes: usize,
}

impl Registry {
    pub(crate) fn new(graph: &ModuleGraph, sources: BTreeMap<PathBuf, String>) -> Self {
        let next_offset = sources.values().map(String::len).max().unwrap_or(0) + 1;
        Self {
            definitions: BTreeMap::new(),
            modules: graph
                .modules
                .values()
                .flat_map(|module| {
                    module
                        .source_paths
                        .iter()
                        .map(|path| (path.clone(), module.id.clone()))
                })
                .collect(),
            imports: graph
                .modules
                .values()
                .map(|module| (module.id.clone(), module.imports.clone()))
                .collect(),
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
                if matches!(
                    declaration.name.name.as_str(),
                    "stringify" | "location" | "include_str" | "include_bytes" | "embed"
                ) {
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
                    validate_template(pattern, rule, source, self.next_offset)
                        .map_err(|error| (source.clone(), error.span, error.message))?;
                }
                self.definitions.insert(
                    key,
                    Definition {
                        declaration: declaration.clone(),
                        patterns,
                        source: source.clone(),
                        span: span.clone(),
                    },
                );
            }
        }
        Ok(())
    }

    fn definition(&self, call: &MacroCall, source: &Path) -> Result<&Definition, String> {
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
                return Ok(definition);
            }
        }
        let prefix = call.path.segments[..call.path.segments.len() - 1]
            .iter()
            .map(|(ident, _)| ident.name.as_str())
            .collect::<Vec<_>>()
            .join(".");
        let mut found = None;
        for import in self.imports.get(module).into_iter().flatten() {
            let imported_name = match &import.spec {
                Some(ImportSpec::Names(names)) if prefix.is_empty() => {
                    names.iter().find_map(|entry| {
                        (entry.alias.as_ref().unwrap_or(&entry.name).name.as_str() == name)
                            .then(|| entry.name.to_string())
                    })
                }
                None if import
                    .target
                    .segments
                    .last()
                    .is_some_and(|segment| segment.as_str() == prefix)
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
                if definition.declaration.visibility == Visibility::Private {
                    return Err(format!("macro `{name}` is private"));
                }
                if found.replace(definition).is_some() {
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
            return Err("macro expansion exceeded depth 64".into());
        }
        if self.active.is_empty() {
            self.remaining_nodes = 1_000_000;
        }
        let definition = self.definition(call, source)?;
        let matched = definition
            .patterns
            .iter()
            .zip(&definition.declaration.rules)
            .find_map(|(pattern, rule)| {
                (rule.category == category)
                    .then(|| pattern.matches(&call.input))
                    .flatten()
                    .map(|captures| (pattern, rule, captures))
            })
            .ok_or("no macro rule matches this input and expansion position")?;
        let (pattern, rule, captures) = matched;
        let definition_source = definition.source.clone();
        let definition_span = definition.span.clone();
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
        self.map
            .contexts
            .mint(call.path.segments[0].0.ctx, id, definition_source.clone());
        self.remaining_nodes = remaining_nodes;
        self.next_offset += generated.source.len() + 1;
        self.map.expansions.push(Expansion {
            file: source.to_path_buf(),
            synthetic: generated.offset..generated.offset + generated.source.len(),
            invocation: Origin::Caller {
                file: source.to_path_buf(),
                span: call.path.segments[0].1.start..call.input.span().end,
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
) -> Result<(), crate::ExpansionError> {
    use hew_parser::macro_syntax::{Fragment, FragmentKind};
    let generated = transcribe(
        pattern,
        &rule.template,
        &pattern.placeholders(),
        SyntaxContext::from_row(1),
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
            file: source.to_path_buf(),
            synthetic: offset..offset + generated.source.len(),
            invocation: Origin::Caller {
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
            control.expr(&mut expression)
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
            for (mut item, span) in parsed.program.items {
                if matches!(item, Item::Import(_) | Item::Macro(_)) {
                    return Err(crate::ExpansionError {
                        message: "an items macro cannot introduce imports or macro declarations"
                            .into(),
                        span: original(&span),
                    });
                }
                control
                    .item(&mut item, &span)
                    .map_err(|error| crate::ExpansionError {
                        message: error.message,
                        span: original(&error.span),
                    })?;
            }
            Ok(())
        }
    };
    result.map_err(|error| crate::ExpansionError {
        message: error.message,
        span: original(&error.span),
    })
}
