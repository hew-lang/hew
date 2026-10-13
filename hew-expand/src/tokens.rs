use hew_parser::ast::Span;
use hew_parser::macro_syntax::{Delimiter, TokenContexts, TokenTree};

pub(crate) fn delimiters(delimiter: Delimiter) -> (&'static str, &'static str) {
    match delimiter {
        Delimiter::Parenthesis => ("(", ")"),
        Delimiter::Bracket => ("[", "]"),
        Delimiter::Brace => ("{", "}"),
        Delimiter::Attribute => ("#[", "]"),
    }
}

pub(crate) fn spelling(tree: &TokenTree) -> Option<&str> {
    match tree {
        TokenTree::Token { spelling, .. } => Some(spelling),
        TokenTree::Group { .. } => None,
    }
}

pub(crate) fn source(trees: &[TokenTree]) -> (String, usize, TokenContexts) {
    let start = trees.first().map_or(0, |tree| tree.span().start);
    let end = trees.last().map_or(start, |tree| tree.span().end);
    let mut bytes = vec![b' '; end - start];
    let mut contexts = TokenContexts::default();
    for tree in trees {
        write_source(tree, start, &mut bytes, &mut contexts);
    }
    (
        String::from_utf8(bytes).expect("token spellings preserve UTF-8"),
        start,
        contexts,
    )
}

fn write_source(tree: &TokenTree, offset: usize, bytes: &mut [u8], contexts: &mut TokenContexts) {
    let leading = tree.leading().as_bytes();
    if let Some(start) = tree.span().start.checked_sub(leading.len() + offset) {
        bytes[start..tree.span().start - offset].copy_from_slice(leading);
    }
    match tree {
        TokenTree::Token {
            spelling,
            span,
            context,
            ..
        } => {
            bytes[span.start - offset..span.end - offset].copy_from_slice(spelling.as_bytes());
            contexts.insert(span.clone(), *context);
        }
        TokenTree::Group {
            delimiter,
            span,
            children,
            closing,
            ..
        } => {
            let (open, close) = delimiters(*delimiter);
            bytes[span.start - offset..span.start - offset + open.len()]
                .copy_from_slice(open.as_bytes());
            bytes[span.end - offset - 1] = close.as_bytes()[0];
            let end = span.end - offset - 1;
            bytes[end - closing.len()..end].copy_from_slice(closing.as_bytes());
            for child in children {
                write_source(child, offset, bytes, contexts);
            }
        }
    }
}

pub(crate) fn span(trees: &[TokenTree]) -> Span {
    trees.first().map_or(0, |tree| tree.span().start)
        ..trees.last().map_or(0, |tree| tree.span().end)
}
