use std::collections::BTreeSet;

use hew_parser::ast::{CallArg, Expr, ImportDecl, Item, Pattern, Program, Spanned, Stmt, TypeExpr};
use hew_types::{check::SpanKey, Ty};

use crate::Diagnostic;

pub const DEFAULT_PROFILE_ALIAS: &str = "sandbox-vm-export";
pub const DEFAULT_PROFILE_CANONICAL: &str = "sandbox.sandbox-vm-export.v0";

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[expect(
    non_camel_case_types,
    reason = "diagnostic enum variant names intentionally match the documented sandbox diagnostic vocabulary"
)]
enum Unsupported {
    NATIVE_ONLY,
}

impl Unsupported {
    fn diagnostic_kind(self) -> &'static str {
        match self {
            Self::NATIVE_ONLY => "Unsupported::NATIVE_ONLY",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum NativeOnlySurface {
    FileIo,
    NetworkSockets,
    NativeFfi,
    OsEnvironment,
    OsProcesses,
    RealTime,
}

impl NativeOnlySurface {
    fn reason(self) -> &'static str {
        match self {
            Self::FileIo => "host filesystem and file-backed I/O are unavailable in the browser sandbox",
            Self::NetworkSockets => "host networking is unavailable in the browser sandbox",
            Self::NativeFfi => "native FFI declarations are unavailable in the browser sandbox",
            Self::OsEnvironment => "host OS interfaces and environment variables are unavailable in the browser sandbox",
            Self::OsProcesses => "host process execution and process inspection are unavailable in the browser sandbox",
            Self::RealTime => "real-clock APIs are unavailable; use virtual time fixtures instead",
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ProfileReport {
    pub diagnostics: Vec<Diagnostic>,
}

/// Canonicalize the caller-facing sandbox profile alias.
///
/// # Errors
///
/// Returns a profile diagnostic when the requested profile is not admitted by
/// this crate's explicit browser bytecode-export allowlist.
#[expect(
    clippy::result_large_err,
    reason = "profile selection failures are surfaced as the same diagnostic shape as compile gates"
)]
pub fn canonical_profile(profile: Option<&str>) -> Result<String, Diagnostic> {
    match profile.unwrap_or(DEFAULT_PROFILE_ALIAS).trim() {
        "" | DEFAULT_PROFILE_ALIAS | DEFAULT_PROFILE_CANONICAL => {
            Ok(DEFAULT_PROFILE_CANONICAL.to_string())
        }
        unknown => Err(Diagnostic::profile_error(
            0..0,
            "unknown_sandbox_profile",
            format!(
                "unknown sandbox profile `{unknown}`; use `{DEFAULT_PROFILE_ALIAS}` for browser bytecode export"
            ),
        )),
    }
}

pub fn check_program(
    program: &Program,
    type_output: &hew_types::TypeCheckOutput,
    _profile: &str,
) -> ProfileReport {
    let mut checker = ProfileChecker::new(program, type_output);
    checker.check_program(program);
    ProfileReport {
        diagnostics: checker.diagnostics,
    }
}

#[derive(Debug)]
struct ProfileChecker<'a> {
    diagnostics: Vec<Diagnostic>,
    imports: BTreeSet<String>,
    user_functions: BTreeSet<String>,
    user_types: BTreeSet<String>,
    enum_variants: BTreeSet<String>,
    type_output: &'a hew_types::TypeCheckOutput,
}

impl<'a> ProfileChecker<'a> {
    fn new(program: &Program, type_output: &'a hew_types::TypeCheckOutput) -> Self {
        let mut checker = Self {
            diagnostics: Vec::new(),
            imports: BTreeSet::new(),
            user_functions: BTreeSet::new(),
            user_types: BTreeSet::new(),
            enum_variants: BTreeSet::new(),
            type_output,
        };
        checker.collect_declarations(program);
        checker
    }

    fn collect_declarations(&mut self, program: &Program) {
        for (item, _) in &program.items {
            match item {
                Item::Function(function) => {
                    self.user_functions.insert(function.name.clone());
                }
                Item::TypeDecl(type_decl) => {
                    self.user_types.insert(type_decl.name.clone());
                    for body_item in &type_decl.body {
                        if let hew_parser::ast::TypeBodyItem::Variant(variant) = body_item {
                            self.enum_variants.insert(variant.name.clone());
                        }
                    }
                }
                Item::Record(record) => {
                    self.user_types.insert(record.name.clone());
                }
                _ => {}
            }
        }
    }

    fn check_program(&mut self, program: &Program) {
        for (item, span) in &program.items {
            match item {
                Item::Import(import) => self.check_import(import, span),
                Item::ExternBlock(_) => self.reject_native_only(
                    span.clone(),
                    NativeOnlySurface::NativeFfi,
                    NativeOnlySurface::NativeFfi.reason(),
                ),
                Item::Function(function) => self.check_block(&function.body),
                Item::Actor(_)
                | Item::Supervisor(_)
                | Item::Machine(_)
                | Item::Wire(_)
                | Item::Trait(_)
                | Item::Impl(_) => self.reject(
                    span.clone(),
                    "reserved_runtime_feature",
                    "this declaration requires runtime features that are reserved for a later sandbox VM milestone",
                ),
                Item::Const(const_decl) => self.check_expr(&const_decl.value),
                Item::TypeDecl(type_decl) => {
                    // W3.030 V15: `#[resource]` types carry an implicit drop
                    // contract that dispatches `<T>::close` through the
                    // unified `ScopeExitPlan` stream. The sandbox-WASM
                    // bytecode export does not yet model that drop
                    // scheduling (the W3.021 `defer_rejected` follow-up
                    // tracks the same gap). Fail closed at the surface
                    // boundary with a named diagnostic so the sandbox can
                    // never silently miss a resource close — tracked in
                    // `.tmp/deferred-v05-followups.md`.
                    if !type_decl.resource_marker.is_none() {
                        self.reject(
                            span.clone(),
                            "user_resource_close_not_yet_admitted_sandbox",
                            "`#[resource]` and `#[linear]` types carry an \
                             implicit drop contract that is not yet admitted \
                             to sandbox bytecode export (W3.030 follow-up)",
                        );
                    }
                }
                Item::TypeAlias(_) | Item::Record(_) => {}
            }
        }
    }

    fn check_import(&mut self, import: &ImportDecl, span: &std::ops::Range<usize>) {
        let path = import.path.join("::");
        self.imports.insert(path.clone());
        if path == "std::text::regex" {
            return;
        }

        let rejected_prefixes = [
            ("std::fs", NativeOnlySurface::FileIo),
            ("std::io", NativeOnlySurface::FileIo),
            ("std::net", NativeOnlySurface::NetworkSockets),
            ("std::process", NativeOnlySurface::OsProcesses),
            ("std::os", NativeOnlySurface::OsEnvironment),
            ("std::env", NativeOnlySurface::OsEnvironment),
            ("std::time", NativeOnlySurface::RealTime),
        ];
        if let Some((_, surface)) = rejected_prefixes
            .iter()
            .find(|(prefix, _)| path == *prefix || path.starts_with(&format!("{prefix}::")))
        {
            self.reject_native_only(
                span.clone(),
                *surface,
                format!("`{path}` is rejected: {}", surface.reason()),
            );
            return;
        }

        if path.starts_with("std::") {
            self.reject(
                span.clone(),
                "unknown_stdlib_symbol",
                format!("`{path}` is not in the sandbox profile allowlist"),
            );
        } else {
            self.reject(
                span.clone(),
                "unknown_module_import",
                "browser sandbox bytecode export is single-source today; module imports are rejected by default",
            );
        }
    }

    fn check_block(&mut self, block: &hew_parser::ast::Block) {
        for (stmt, span) in &block.stmts {
            self.check_stmt(stmt, span);
        }
        if let Some(expr) = &block.trailing_expr {
            self.check_expr(expr);
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "check_stmt walks every admitted statement variant fail-closed; splitting would obscure the admission contract"
    )]
    fn check_stmt(&mut self, stmt: &Stmt, span: &std::ops::Range<usize>) {
        match stmt {
            Stmt::Let { value, ty, .. } | Stmt::Var { value, ty, .. } => {
                if let Some((ty, _)) = ty {
                    self.check_type_expr(ty, span);
                }
                if let Some(expr) = value {
                    self.check_expr(expr);
                }
            }
            Stmt::Assign { target, value, .. } => {
                self.check_expr(target);
                self.check_expr(value);
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                self.check_expr(condition);
                self.check_block(then_block);
                if let Some(else_block) = else_block {
                    if let Some(block) = &else_block.block {
                        self.check_block(block);
                    }
                    if let Some(if_stmt) = &else_block.if_stmt {
                        self.check_stmt(&if_stmt.0, &if_stmt.1);
                    }
                }
            }
            Stmt::Match { scrutinee, arms } => {
                self.check_expr(scrutinee);
                for arm in arms {
                    self.check_pattern(&arm.pattern);
                    if let Some(guard) = &arm.guard {
                        self.check_expr(guard);
                    }
                    self.check_expr(&arm.body);
                }
            }
            Stmt::Return(expr) => {
                if let Some(expr) = expr.as_ref() {
                    self.check_expr(expr);
                }
            }
            Stmt::Expression(expr) => self.check_expr(expr),
            Stmt::Loop { body, label } => {
                if label.is_some() {
                    self.reject(
                        span.clone(),
                        "reserved_control_flow",
                        "labeled loops are not yet admitted in the sandbox profile",
                    );
                }
                self.check_block(body);
            }
            Stmt::For { is_await, iterable, body, label, .. } => {
                if *is_await {
                    self.reject(
                        span.clone(),
                        "reserved_runtime_feature",
                        "for-await requires async runtime support reserved for a later sandbox VM milestone",
                    );
                    return;
                }
                if label.is_some() {
                    self.reject(
                        span.clone(),
                        "reserved_control_flow",
                        "labeled for loops are not yet admitted in the sandbox profile",
                    );
                }
                self.check_expr(iterable);
                self.check_block(body);
            }
            Stmt::While { condition, body, label } => {
                if label.is_some() {
                    self.reject(
                        span.clone(),
                        "reserved_control_flow",
                        "labeled while loops are not yet admitted in the sandbox profile",
                    );
                }
                self.check_expr(condition);
                self.check_block(body);
            }
            Stmt::WhileLet { expr, body, label, .. } => {
                if label.is_some() {
                    self.reject(
                        span.clone(),
                        "reserved_control_flow",
                        "labeled while-let loops are not yet admitted in the sandbox profile",
                    );
                }
                self.check_expr(expr);
                self.check_block(body);
            }
            Stmt::Break { value, .. } => {
                if value.is_some() {
                    self.reject(
                        span.clone(),
                        "reserved_control_flow",
                        "break-with-value is not yet admitted in the sandbox profile",
                    );
                }
            }
            Stmt::Continue { .. } => {}
            Stmt::IfLet { expr, body, else_body, .. } => {
                self.check_expr(expr);
                self.check_block(body);
                if let Some(block) = else_body {
                    self.check_block(block);
                }
            }
            Stmt::Defer(_) => self.reject(
                span.clone(),
                "defer_rejected",
                "defer needs runtime drop scheduling and is not admitted to sandbox bytecode export yet",
            ),
        }
    }

    #[expect(
        clippy::too_many_lines,
        reason = "profile admission walks every parser expression variant fail-closed"
    )]
    fn check_expr(&mut self, expr: &Spanned<Expr>) {
        let (expr, span) = expr;
        match expr {
            Expr::Literal(_) | Expr::Identifier(_) | Expr::This | Expr::RegexLiteral(_) => {}
            Expr::Binary { left, right, .. } => {
                self.check_expr(left);
                self.check_expr(right);
            }
            Expr::Is { lhs, rhs } => {
                self.reject(
                    span.clone(),
                    "reserved_runtime_feature",
                    "identity comparison needs heap identity semantics reserved for a later sandbox VM milestone",
                );
                self.check_expr(lhs);
                self.check_expr(rhs);
            }
            Expr::Unary { operand, .. }
            | Expr::Cast { expr: operand, .. }
            | Expr::PostfixTry(operand)
            | Expr::Await(operand) => {
                if matches!(expr, Expr::Await(_)) {
                    self.reject(
                        span.clone(),
                        "reserved_runtime_feature",
                        "await requires task scheduling and is reserved for a later sandbox VM milestone",
                    );
                }
                self.check_expr(operand);
            }
            Expr::Tuple(items) | Expr::Array(items) | Expr::Join(items) => {
                for item in items {
                    self.check_expr(item);
                }
            }
            Expr::ArrayRepeat { value, count } => {
                self.check_expr(value);
                self.check_expr(count);
            }
            Expr::MapLiteral { entries } => {
                for (key, value) in entries {
                    self.check_expr(key);
                    self.check_expr(value);
                }
            }
            Expr::Block(block) | Expr::Scope { body: block } | Expr::GenBlock { body: block } => {
                if matches!(expr, Expr::Scope { .. } | Expr::GenBlock { .. }) {
                    self.reject(
                        span.clone(),
                        "reserved_runtime_feature",
                        "structured concurrency and generator blocks are reserved for a later sandbox VM milestone",
                    );
                }
                self.check_block(block);
            }
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                self.check_expr(condition);
                self.check_expr(then_block);
                if let Some(expr) = else_block {
                    self.check_expr(expr);
                }
            }
            Expr::IfLet {
                pattern,
                expr,
                body,
                else_body,
            } => {
                self.check_pattern(pattern);
                self.check_expr(expr);
                self.check_block(body);
                if let Some(block) = else_body {
                    self.check_block(block);
                }
            }
            Expr::Match { scrutinee, arms } => {
                self.check_expr(scrutinee);
                for arm in arms {
                    self.check_pattern(&arm.pattern);
                    if let Some(guard) = &arm.guard {
                        self.check_expr(guard);
                    }
                    self.check_expr(&arm.body);
                }
            }
            Expr::Call { function, args, .. } => {
                self.check_call(function, args, span);
                for arg in args {
                    self.check_expr(arg.expr());
                }
            }
            Expr::MethodCall { receiver, method, args } => {
                self.check_expr(receiver);
                for arg in args {
                    self.check_expr(arg.expr());
                }
                if !self.method_is_admitted(receiver, method) {
                    self.reject(
                        span.clone(),
                        "unknown_method_symbol",
                        format!("method `{method}` is not in the sandbox profile allowlist"),
                    );
                }
            }
            Expr::StructInit { fields, base, .. } => {
                for (_, value) in fields {
                    self.check_expr(value);
                }
                if let Some(base) = base {
                    self.check_expr(base);
                }
            }
            Expr::Select { .. }
            | Expr::Spawn { .. }
            | Expr::SpawnLambdaActor { .. }
            | Expr::ForkChild { .. }
            | Expr::ForkBlock { .. }
            | Expr::ScopeDeadline { .. }
            | Expr::Timeout { .. }
            | Expr::Yield(_)
            | Expr::Cooperate
            | Expr::MachineEmit { .. } => self.reject(
                span.clone(),
                "reserved_runtime_feature",
                "this expression requires scheduler or machine runtime support reserved for a later sandbox VM milestone",
            ),
            Expr::InterpolatedString(parts) => {
                for part in parts {
                    if let hew_parser::ast::StringPart::Expr(expr) = part {
                        self.check_expr(expr);
                    }
                }
            }
            Expr::FieldAccess { object, .. } => self.check_expr(object),
            Expr::Index { object, index } => {
                self.check_expr(object);
                self.check_expr(index);
            }
            Expr::Range { start, end, .. } => {
                if let Some(start) = start {
                    self.check_expr(start);
                }
                if let Some(end) = end {
                    self.check_expr(end);
                }
            }
            Expr::UnsafeBlock(block) => {
                self.reject(
                    span.clone(),
                    "unsafe_rejected",
                    "unsafe blocks are rejected by the browser sandbox profile",
                );
                self.check_block(block);
            }
            Expr::ByteStringLiteral(_) | Expr::ByteArrayLiteral(_) | Expr::Lambda { .. } => self.reject(
                span.clone(),
                "reserved_runtime_feature",
                "this value form is not admitted to sandbox bytecode export yet",
            ),
        }
    }

    fn check_call(
        &mut self,
        function: &Spanned<Expr>,
        args: &[CallArg],
        span: &std::ops::Range<usize>,
    ) {
        match &function.0 {
            Expr::Identifier(name) => {
                if self.user_functions.contains(name)
                    || self.enum_variants.contains(name)
                    || matches!(
                        name.as_str(),
                        "print" | "println" | "panic" | "Some" | "None" | "Ok" | "Err" | "Vec::new"
                    )
                {
                    return;
                }
                self.reject(
                    span.clone(),
                    "unknown_symbol",
                    format!("`{name}` is not in the sandbox profile allowlist"),
                );
            }
            Expr::FieldAccess { object, field } => {
                if let Expr::Identifier(module) = &object.0 {
                    let symbol = format!("{module}.{field}");
                    if symbol == "regex.new"
                        || symbol == "Vec.new"
                        || matches!(
                            symbol.as_str(),
                            "regex.is_match" | "regex.find" | "regex.replace"
                        )
                    {
                        return;
                    }
                }
                self.reject(
                    span.clone(),
                    "unknown_stdlib_symbol",
                    "module-qualified call is not in the sandbox profile allowlist",
                );
            }
            _ => {
                if args.is_empty() {
                    self.reject(
                        span.clone(),
                        "unknown_indirect_call",
                        "indirect calls are not admitted to sandbox bytecode export yet",
                    );
                }
            }
        }
    }

    fn check_pattern(&mut self, pattern: &Spanned<Pattern>) {
        match &pattern.0 {
            Pattern::Wildcard | Pattern::Literal(_) | Pattern::Identifier(_) => {}
            Pattern::Regex { .. } => self.reject(
                pattern.1.clone(),
                "reserved_regex_pattern",
                "regex match-arm patterns need dedicated predicate bytecode lowering and are reserved for a later sandbox VM milestone",
            ),
            Pattern::Constructor { patterns, .. } | Pattern::Tuple(patterns) => {
                for pattern in patterns {
                    self.check_pattern(pattern);
                }
            }
            Pattern::Struct { fields, .. } => {
                for field in fields {
                    if let Some(pattern) = &field.pattern {
                        self.check_pattern(pattern);
                    }
                }
            }
            Pattern::Or(left, right) => {
                self.check_pattern(left);
                self.check_pattern(right);
            }
        }
    }

    fn method_is_admitted(&self, receiver: &Spanned<Expr>, method: &str) -> bool {
        if matches!((&receiver.0, method), (Expr::Identifier(module), "new") if module == "regex" || module == "Vec")
        {
            return true;
        }

        let Some(receiver_ty) = self.ty_for_expr(receiver) else {
            return false;
        };
        match receiver_ty.materialize_literal_defaults() {
            Ty::String => matches!(method, "len" | "slice"),
            Ty::Array(_, _) | Ty::Slice(_) => matches!(method, "len" | "get"),
            Ty::Named { name, .. } if name == "Vec" => matches!(method, "len" | "push" | "get"),
            Ty::Named { name, .. }
                if name.eq_ignore_ascii_case("Regex")
                    || name.ends_with(".Regex")
                    || name == "regex.Pattern" =>
            {
                matches!(method, "find" | "is_match" | "replace" | "free")
            }
            _ => false,
        }
    }

    fn ty_for_expr(&self, expr: &Spanned<Expr>) -> Option<Ty> {
        self.type_output
            .expr_types
            .get(&SpanKey::from(&expr.1))
            .cloned()
    }

    fn check_type_expr(&mut self, ty: &TypeExpr, span: &std::ops::Range<usize>) {
        match ty {
            TypeExpr::Pointer { .. } => self.reject_native_only(
                span.clone(),
                NativeOnlySurface::NativeFfi,
                "raw pointer types are native FFI and are rejected by the browser sandbox profile",
            ),
            TypeExpr::Function {
                params,
                return_type,
            } => {
                for (param, param_span) in params {
                    self.check_type_expr(param, param_span);
                }
                self.check_type_expr(&return_type.0, &return_type.1);
            }
            TypeExpr::Result { ok, err } => {
                self.check_type_expr(&ok.0, &ok.1);
                self.check_type_expr(&err.0, &err.1);
            }
            TypeExpr::Option(inner)
            | TypeExpr::Array { element: inner, .. }
            | TypeExpr::Slice(inner)
            | TypeExpr::Borrow(inner) => self.check_type_expr(&inner.0, &inner.1),
            TypeExpr::Tuple(items) => {
                for (item, item_span) in items {
                    self.check_type_expr(item, item_span);
                }
            }
            TypeExpr::Named { .. } | TypeExpr::TraitObject(_) | TypeExpr::Infer => {}
        }
    }

    fn reject(
        &mut self,
        span: std::ops::Range<usize>,
        kind: &'static str,
        message: impl Into<String>,
    ) {
        self.diagnostics
            .push(Diagnostic::profile_error(span, kind, message));
    }

    fn reject_native_only(
        &mut self,
        span: std::ops::Range<usize>,
        _surface: NativeOnlySurface,
        message: impl Into<String>,
    ) {
        let message = message.into();
        self.reject(
            span.clone(),
            Unsupported::NATIVE_ONLY.diagnostic_kind(),
            message.clone(),
        );
        self.reject(span, "sandbox_profile_rejected", message);
    }
}
