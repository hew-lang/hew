use crate::{tokens::spelling, ExpansionError};
use hew_parser::ast::{Expr, Ident, Item, Span, Stmt, SyntaxContext};
use hew_parser::macro_syntax::{AttributePattern, Delimiter, FunctionInput, TokenTree};

pub(crate) fn projection(
    input: &FunctionInput,
    spec: &AttributePattern,
    name: Option<&str>,
    at: &TokenTree,
) -> Result<Vec<TokenTree>, ExpansionError> {
    let trees = &input.tokens;
    let fail = |message: &str| ExpansionError {
        message: message.into(),
        span: at.span().clone(),
    };
    let Some(name) = name else {
        return Ok(trees.clone());
    };
    let fn_index = trees
        .iter()
        .position(|tree| spelling(tree) == Some("fn"))
        .ok_or_else(|| fail("expected a function capture"))?;
    let name_index = fn_index + 1;
    let mut angle_depth: usize = 0;
    let params_index = trees
        .iter()
        .enumerate()
        .skip(name_index + 1)
        .find_map(|(index, tree)| {
            match spelling(tree) {
                Some("<") => angle_depth += 1,
                Some(">") => angle_depth = angle_depth.saturating_sub(1),
                Some(">>") => angle_depth = angle_depth.saturating_sub(2),
                _ => {}
            }
            (angle_depth == 0
                && matches!(
                    tree,
                    TokenTree::Group {
                        delimiter: Delimiter::Parenthesis,
                        ..
                    }
                ))
            .then_some(index)
        })
        .ok_or_else(|| fail("expected function parameters"))?;
    let body_index = trees.len() - 1;
    let where_index = trees[params_index + 1..body_index]
        .iter()
        .position(|tree| spelling(tree) == Some("where"))
        .map_or(body_index, |index| params_index + 1 + index);
    let TokenTree::Group {
        children: params, ..
    } = &trees[params_index]
    else {
        unreachable!()
    };
    Ok(match name {
        "vis" => trees
            .iter()
            .take(usize::from(
                input.function.visibility != hew_parser::ast::Visibility::Private,
            ))
            .cloned()
            .collect(),
        "name" => vec![trees[name_index].clone()],
        "generics" => trees[name_index + 1..params_index].to_vec(),
        "params" => params.clone(),
        "ret" => trees[params_index + 1..where_index].to_vec(),
        "where" => trees[where_index..body_index].to_vec(),
        "body" => vec![trees[body_index].clone()],
        "receiver" if spec.method => params
            .iter()
            .take_while(|tree| spelling(tree) != Some(","))
            .cloned()
            .collect(),
        "receiver" => {
            return Err(fail(
                "E_MACRO_BAD_PROJECTION: `.receiver` requires a method fragment",
            ))
        }
        "args" => {
            let mut result = Vec::new();
            for param in input
                .function
                .params
                .iter()
                .filter(|param| !param.is_receiver)
            {
                if !result.is_empty() {
                    result.push(token(",", param.name_span.clone(), param.name.ctx));
                }
                result.push(token(
                    param.name.name.as_str(),
                    param.name_span.clone(),
                    param.name.ctx,
                ));
            }
            result
        }
        _ => return Err(fail("E_MACRO_BAD_PROJECTION: unknown function projection")),
    })
}

fn token(spelling: &str, span: Span, context: SyntaxContext) -> TokenTree {
    TokenTree::Token {
        spelling: spelling.into(),
        span,
        context,
        leading: String::new(),
    }
}

pub(crate) fn placeholder(spec: &AttributePattern) -> FunctionInput {
    let source = if spec.method {
        "fn __attribute_carrier(self) {}"
    } else {
        "fn __attribute_carrier() {}"
    };
    let mut parser = hew_parser::parser::Parser::new(source);
    parser.allow_macro_receivers();
    let program = parser.parse_program();
    let Item::Function(function) = program.items[0].0.clone() else {
        unreachable!()
    };
    parser
        .function_input(function)
        .expect("valid placeholder function")
}

pub(crate) fn validate_carrier(
    items: &[(Item, Span)],
    origins: &[crate::TokenOrigin],
    span: &Span,
) -> Result<(), ExpansionError> {
    let count = items.iter().filter(|(item, _)| matches!(item, Item::Function(function) if function.name == Ident::new("__attribute_carrier") && origins.iter().any(|origin| origin.generated.contains(&function.decl_span.start) && matches!(origin.kind, crate::OriginKind::Capture)))).count();
    let named = items
        .iter()
        .filter(|(item, _)| {
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
            name == Some(Ident::new("__attribute_carrier"))
        })
        .count();
    if count == 1 && named == 1 {
        Ok(())
    } else {
        Err(ExpansionError { message: "E_MACRO_NAME_CARRIER: an attribute template must produce exactly one function named `$f.name`".into(), span: span.clone() })
    }
}

/// A projected body must be a direct statement or tail in one template function.
pub(crate) fn validate_bodies(
    items: &[(Item, Span)],
    bodies: &[Span],
    source: &str,
    offset: usize,
) -> Result<(), ExpansionError> {
    let mut accepted = Vec::new();
    for (item, _) in items {
        let Item::Function(function) = item else {
            continue;
        };
        let mut count = 0;
        for span in bodies {
            if function.fn_span.start < span.start
                && span.end <= function.fn_span.end
                && source
                    .get(span.end - offset..function.fn_span.end - offset)
                    .is_some_and(|suffix| suffix.trim().is_empty())
            {
                accepted.push(span.start);
                count += 1;
            }
        }
        for expression in function
            .body
            .stmts
            .iter()
            .filter_map(|(stmt, _)| {
                if let Stmt::Expression(expr) = stmt {
                    Some(expr)
                } else {
                    None
                }
            })
            .chain(function.body.trailing_expr.iter().map(Box::as_ref))
        {
            if matches!(expression.0, Expr::Block(_))
                && bodies.iter().any(|span| span.start == expression.1.start)
            {
                count += 1;
                accepted.push(expression.1.start);
            }
        }
        if count > 1 {
            return Err(ExpansionError {
                message:
                    "E_MACRO_BODY_PLACEMENT: a captured body may occur at most once per function"
                        .into(),
                span: function.fn_span.clone(),
            });
        }
    }
    if let Some(span) = bodies.iter().find(|span| !accepted.contains(&span.start)) {
        return Err(ExpansionError {
            message: "E_MACRO_BODY_PLACEMENT: place `$f.body` directly in a template function body"
                .into(),
            span: span.clone(),
        });
    }
    Ok(())
}
