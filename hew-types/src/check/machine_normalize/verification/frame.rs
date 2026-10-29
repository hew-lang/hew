//! Frame condition for opaque staged expressions: they may update payloads and
//! outputs, but cannot reset selection, budget, rule reporting or the receiver.
use crate::check::{walk_expr, NodeVisitor};
use hew_parser::ast::{Expr, Ident, Pattern, Span, Spanned, Stmt};

fn protected(name: Ident) -> bool {
    let text = name.name.as_str();
    text.starts_with("_$machine_selected_")
        || text.starts_with("_$machine_seen_")
        || [
            "self",
            "_$machine_work",
            "_$machine_used",
            "_$machine_any",
            "_$machine_rule",
            "_$machine_disposition",
        ]
        .contains(&text)
}
fn root(expr: &Spanned<Expr>) -> Option<Ident> {
    match &expr.0 {
        Expr::Ident(name) => Some(*name),
        Expr::FieldAccess { object, .. } | Expr::Index { object, .. } => root(object),
        _ => None,
    }
}
#[derive(Default)]
struct Frame {
    error: Option<String>,
}
impl Frame {
    fn write(&mut self, name: Ident, span: &Span) {
        if protected(name) {
            self.error = Some(format!(
                "staged expression writes control variable {name} at {span:?}"
            ));
        }
    }
}
impl NodeVisitor for Frame {
    fn visit_stmt(&mut self, stmt: &Stmt, span: &Span) {
        match stmt {
            Stmt::Assign { target, .. } => {
                if let Some(name) = root(target) {
                    self.write(name, span);
                }
            }
            Stmt::Var { name, .. }
            | Stmt::Let {
                pattern: (Pattern::Identifier(name), _),
                ..
            } if *name != Ident::new("self") => self.write(*name, span),
            _ => {}
        }
    }
    fn visit_expr(&mut self, expr: &Expr, span: &Span) {
        if let Expr::MethodCall { receiver, .. } = expr {
            if let Some(name) = root(receiver) {
                self.write(name, span);
            }
        }
    }
}
pub(super) fn validate(expr: &Spanned<Expr>) -> Result<(), String> {
    crate::check::machine_control::validate_expr(expr).map_err(|error| {
        format!("staged expression escapes its lexical control boundary: {error:?}")
    })?;
    let mut frame = Frame::default();
    walk_expr(&expr.0, &expr.1, &mut frame);
    frame.error.map_or(Ok(()), Err)
}
