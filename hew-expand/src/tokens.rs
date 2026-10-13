use hew_parser::ast::Span;
use hew_parser::macro_syntax::{Delimiter, FragmentInputs, TokenContexts, TokenTree};

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
        TokenTree::Token { spelling, .. } | TokenTree::Interpolated { spelling, .. } => {
            Some(spelling)
        }
        TokenTree::Group { .. } | TokenTree::Fragment { .. } => None,
    }
}

pub(crate) fn source(trees: &[TokenTree]) -> (String, usize, TokenContexts, FragmentInputs) {
    let start = trees.first().map_or(0, |tree| tree.span().start);
    let end = trees.last().map_or(start, |tree| tree.span().end);
    let mut bytes = vec![b' '; end - start];
    let mut contexts = TokenContexts::default();
    let mut fragments = FragmentInputs::default();
    for tree in trees {
        write_source(tree, start, &mut bytes, &mut contexts, &mut fragments);
    }
    (
        String::from_utf8(bytes).expect("token spellings preserve UTF-8"),
        start,
        contexts,
        fragments,
    )
}

fn write_source(
    tree: &TokenTree,
    offset: usize,
    bytes: &mut [u8],
    contexts: &mut TokenContexts,
    fragments: &mut FragmentInputs,
) {
    let leading = tree.leading().as_bytes();
    if let Some(start) = tree.span().start.checked_sub(leading.len() + offset) {
        bytes[start..tree.span().start - offset].copy_from_slice(leading);
    }
    match tree {
        TokenTree::Interpolated {
            spelling,
            span,
            expressions,
            ..
        } => {
            bytes[span.start - offset..span.end - offset].copy_from_slice(spelling.as_bytes());
            for (_, children) in expressions {
                for child in children {
                    write_source(child, offset, bytes, contexts, fragments);
                }
            }
        }
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
                write_source(child, offset, bytes, contexts, fragments);
            }
        }
        TokenTree::Fragment { syntax, tokens } => {
            for token in tokens {
                write_source(token, offset, bytes, contexts, fragments);
            }
            fragments.insert((**syntax).clone());
        }
    }
}

pub(crate) fn span(trees: &[TokenTree]) -> Span {
    trees.first().map_or(0, |tree| tree.span().start)
        ..trees.last().map_or(0, |tree| tree.span().end)
}
