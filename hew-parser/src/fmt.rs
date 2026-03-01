//! Pretty-printer that converts an AST back to canonical Hew source text.

use std::ops::Range;

use crate::ast::{
    ActorDecl, ActorInit, BinaryOp, Block, CallArg, ChildSpec, CompoundAssignOp, ConstDecl,
    ElseBlock, Expr, ExternBlock, ExternFnDecl, FieldDecl, FnDecl, ImplDecl, ImportDecl,
    ImportSpec, IntRadix, Item, LambdaParam, Literal, MatchArm, OverflowPolicy, Param, Pattern,
    PatternField, Program, ReceiveFnDecl, RestartPolicy, SelectArm, Spanned, Stmt, StringPart,
    SupervisorDecl, SupervisorStrategy, TimeoutClause, TraitBound, TraitDecl, TraitItem,
    TraitMethod, TypeAliasDecl, TypeBodyItem, TypeDecl, TypeDeclKind, TypeExpr, TypeParam, UnaryOp,
    VariantDecl, VariantKind, Visibility, WhereClause, WireDecl, WireDeclKind, WireFieldDecl,
};

/// Format an AST [`Program`] as canonical Hew source text (comments are not preserved).
#[must_use]
pub fn format_program(program: &Program) -> String {
    let mut f = Formatter::new("", Vec::new());
    f.format_program(program);
    f.output
}

/// Format an AST [`Program`] as canonical Hew source text, preserving comments from `source`.
#[must_use]
pub fn format_source(source: &str, program: &Program) -> String {
    let comments = extract_comments(source);
    let mut f = Formatter::new(source, comments);
    f.format_program(program);
    f.flush_comments_before(usize::MAX);
    f.output
}

struct Formatter<'a> {
    output: String,
    indent: usize,
    source: &'a str,
    comments: Vec<Comment>,
    next_comment: usize,
    prev_source_pos: usize,
}

impl<'a> Formatter<'a> {
    fn new(source: &'a str, comments: Vec<Comment>) -> Self {
        Self {
            output: String::new(),
            indent: 0,
            source,
            comments,
            next_comment: 0,
            prev_source_pos: 0,
        }
    }

    fn has_comments(&self) -> bool {
        !self.comments.is_empty()
    }

    // ------------------------------------------------------------------
    // Helpers
    // ------------------------------------------------------------------

    fn write(&mut self, s: &str) {
        self.output.push_str(s);
    }

    fn writeln(&mut self, s: &str) {
        self.write_indent();
        self.output.push_str(s);
        self.output.push('\n');
    }

    fn newline(&mut self) {
        self.output.push('\n');
    }

    fn write_indent(&mut self) {
        for _ in 0..self.indent {
            self.output.push_str("    ");
        }
    }

    /// Write a comma-separated list using a per-item formatting closure.
    fn comma_sep<T>(&mut self, items: &[T], mut fmt_item: impl FnMut(&mut Self, &T)) {
        for (i, item) in items.iter().enumerate() {
            if i > 0 {
                self.write(", ");
            }
            fmt_item(self, item);
        }
    }

    fn write_visibility(&mut self, vis: Visibility) {
        match vis {
            Visibility::Private => {}
            Visibility::Pub => self.write("pub "),
            Visibility::PubPackage => self.write("pub(package) "),
            Visibility::PubSuper => self.write("pub(super) "),
        }
    }

    // ------------------------------------------------------------------
    // Comment flushing
    // ------------------------------------------------------------------

    fn flush_comments_before(&mut self, pos: usize) {
        while self.next_comment < self.comments.len()
            && self.comments[self.next_comment].span.start < pos
        {
            let idx = self.next_comment;
            if is_trailing_comment(self.source, self.comments[idx].span.start) {
                if self.output.ends_with('\n') {
                    self.output.pop();
                }
                self.write(" ");
            } else {
                let gap_start = self.prev_source_pos.min(self.source.len());
                let gap_end = self.comments[idx].span.start.min(self.source.len());
                if gap_start < gap_end {
                    let newlines = self.source[gap_start..gap_end]
                        .chars()
                        .filter(|&c| c == '\n')
                        .count();
                    if self.prev_source_pos > 0 && newlines > 1 && !self.output.ends_with("\n\n") {
                        self.newline();
                    }
                }
                self.write_indent();
            }

            let text = self.comments[idx].text.clone();
            self.write(&text);
            self.newline();
            self.prev_source_pos = self.comments[idx].span.end;
            self.next_comment += 1;
        }
    }

    fn enter_block_scope(&mut self, scope_end: usize) {
        if self.has_comments() {
            let from = self.prev_source_pos.min(self.source.len());
            let to = scope_end.min(self.source.len());
            if from < to {
                if let Some(off) = self.source[from..to].find('{') {
                    self.prev_source_pos = from + off + 1;
                }
            }
        }
    }

    fn flush_block_end_comments(&mut self, scope_end: usize) {
        if self.has_comments() {
            let brace = find_block_close(
                self.source,
                self.prev_source_pos,
                scope_end.min(self.source.len()),
            );
            self.flush_comments_before(brace);
            if brace < self.source.len() {
                self.prev_source_pos = brace + 1;
            }
        }
    }

    fn find_keyword_after(&self, keyword: &str, after: usize) -> usize {
        let from = after.min(self.source.len());
        self.source[from..]
            .find(keyword)
            .map_or(self.source.len(), |off| from + off)
    }

    // ------------------------------------------------------------------
    // Program
    // ------------------------------------------------------------------

    fn format_program(&mut self, program: &Program) {
        for (i, item) in program.items.iter().enumerate() {
            let had_comments = self.next_comment;
            self.flush_comments_before(item.1.start);
            let flushed_comments = self.next_comment > had_comments;
            if i > 0 && !flushed_comments && !self.output.ends_with("\n\n") {
                self.newline();
            }
            self.prev_source_pos = item.1.start;
            self.format_item(&item.0, item.1.end);
            // Only advance if format_item didn't already advance past the item
            // (block-containing items advance via flush_block_end_comments).
            if self.prev_source_pos < item.1.start {
                self.prev_source_pos = item.1.end;
            }
        }
    }

    // ------------------------------------------------------------------
    // Items
    // ------------------------------------------------------------------

    fn format_item(&mut self, item: &Item, span_end: usize) {
        match item {
            Item::Import(decl) => self.format_import(decl),
            Item::Const(decl) => self.format_const(decl),
            Item::TypeDecl(decl) => self.format_type_decl(decl, span_end),
            Item::TypeAlias(decl) => self.format_type_alias(decl),
            Item::Trait(decl) => self.format_trait(decl, span_end),
            Item::Impl(decl) => self.format_impl(decl, span_end),
            Item::Wire(decl) => self.format_wire(decl),
            Item::Function(decl) => self.format_fn(decl, span_end),
            Item::ExternBlock(decl) => self.format_extern_block(decl, span_end),
            Item::Actor(decl) => self.format_actor(decl, span_end),
            Item::Supervisor(decl) => self.format_supervisor(decl),
        }
    }

    fn format_import(&mut self, decl: &ImportDecl) {
        self.write_indent();
        self.write("import ");
        if let Some(file_path) = &decl.file_path {
            self.write("\"");
            self.write(file_path);
            self.write("\"");
        } else {
            self.write(&decl.path.join("::"));
            if let Some(spec) = &decl.spec {
                match spec {
                    ImportSpec::Glob => self.write("::*"),
                    ImportSpec::Names(names) => {
                        self.write("::{");
                        self.comma_sep(names, |f, n| {
                            f.write(&n.name);
                            if let Some(alias) = &n.alias {
                                f.write(" as ");
                                f.write(alias);
                            }
                        });
                        self.write("}");
                    }
                }
            }
        }
        self.write(";\n");
    }

    fn format_const(&mut self, decl: &ConstDecl) {
        self.write_indent();
        self.write_visibility(decl.visibility);
        self.write("const ");
        self.write(&decl.name);
        self.write(": ");
        self.format_type_expr(&decl.ty.0);
        self.write(" = ");
        self.format_expr(&decl.value.0);
        self.write(";\n");
    }

    fn format_type_alias(&mut self, decl: &TypeAliasDecl) {
        self.write_indent();
        self.write("type ");
        self.write(&decl.name);
        self.write(" = ");
        self.format_type_expr(&decl.ty.0);
        self.write(";\n");
    }

    fn format_type_decl(&mut self, decl: &TypeDecl, _span_end: usize) {
        self.write_indent();
        match decl.kind {
            TypeDeclKind::Struct => self.write("type "),
            TypeDeclKind::Enum => self.write("enum "),
        }
        self.write(&decl.name);
        self.format_opt_type_params(decl.type_params.as_ref());
        self.format_opt_where_clause(decl.where_clause.as_ref());
        self.write(" {\n");
        self.indent += 1;
        for item in &decl.body {
            match item {
                TypeBodyItem::Field { name, ty } => {
                    self.write_indent();
                    self.write(name);
                    self.write(": ");
                    self.format_type_expr(&ty.0);
                    self.write(";");
                    self.newline();
                }
                TypeBodyItem::Variant(v) => {
                    self.format_variant(v, true);
                }
                TypeBodyItem::Method(f) => {
                    self.format_fn(f, self.source.len());
                }
            }
        }
        self.indent -= 1;
        self.writeln("}");
    }

    fn format_variant(&mut self, v: &VariantDecl, trailing_semicolon: bool) {
        self.write_indent();
        self.write(&v.name);
        match &v.kind {
            VariantKind::Unit => {}
            VariantKind::Tuple(fields) => {
                if !fields.is_empty() {
                    self.write("(");
                    self.comma_sep(fields, |f, ty| f.format_type_expr(&ty.0));
                    self.write(")");
                }
            }
            VariantKind::Struct(fields) => {
                self.write(" { ");
                self.comma_sep(fields, |f, (name, ty)| {
                    f.write(name);
                    f.write(": ");
                    f.format_type_expr(&ty.0);
                });
                self.write(" }");
            }
        }
        if trailing_semicolon {
            self.write(";");
        }
        self.newline();
    }

    fn format_trait(&mut self, decl: &TraitDecl, _span_end: usize) {
        self.write_indent();
        self.write("trait ");
        self.write(&decl.name);
        self.format_opt_type_params(decl.type_params.as_ref());
        if let Some(supers) = &decl.super_traits {
            self.write(": ");
            self.format_trait_bound_list(supers);
        }
        self.write(" {\n");
        self.indent += 1;
        for item in &decl.items {
            match item {
                TraitItem::Method(m) => self.format_trait_method(m),
                TraitItem::AssociatedType {
                    name,
                    bounds,
                    default,
                } => {
                    self.write_indent();
                    self.write("type ");
                    self.write(name);
                    if !bounds.is_empty() {
                        self.write(": ");
                        self.format_trait_bound_list(bounds);
                    }
                    if let Some(def) = default {
                        self.write(" = ");
                        self.format_type_expr(&def.0);
                    }
                    self.write(";\n");
                }
            }
        }
        self.indent -= 1;
        self.writeln("}");
    }

    fn format_trait_method(&mut self, m: &TraitMethod) {
        self.write_indent();
        if m.is_pure {
            self.write("pure ");
        }
        self.write("fn ");
        self.write(&m.name);
        self.format_fn_signature(
            m.type_params.as_ref(),
            &m.params,
            m.return_type.as_ref(),
            m.where_clause.as_ref(),
        );
        if let Some(body) = &m.body {
            self.write(" ");
            self.format_block(body, self.source.len());
            self.newline();
        } else {
            self.write(";\n");
        }
    }

    fn format_impl(&mut self, decl: &ImplDecl, span_end: usize) {
        self.write_indent();
        self.write("impl");
        self.format_opt_type_params(decl.type_params.as_ref());
        self.write(" ");
        if let Some(bound) = &decl.trait_bound {
            self.format_trait_bound(bound);
            self.write(" for ");
        }
        self.format_type_expr(&decl.target_type.0);
        self.format_opt_where_clause(decl.where_clause.as_ref());
        self.write(" {\n");
        self.indent += 1;
        if !decl.type_aliases.is_empty() {
            for (i, alias) in decl.type_aliases.iter().enumerate() {
                if self.has_comments() {
                    let pos = self
                        .find_keyword_after(&format!("type {}", alias.name), self.prev_source_pos);
                    self.flush_comments_before(pos);
                } else if i > 0 {
                    self.newline();
                }
                self.write_indent();
                self.write("type ");
                self.write(&alias.name);
                self.write(" = ");
                self.format_type_expr(&alias.ty.0);
                self.write(";\n");
            }
            if !self.has_comments() && !decl.methods.is_empty() {
                self.newline();
            }
        }
        for (i, method) in decl.methods.iter().enumerate() {
            if self.has_comments() {
                let pos =
                    self.find_keyword_after(&format!("fn {}", method.name), self.prev_source_pos);
                self.flush_comments_before(pos);
            } else if i > 0 {
                self.newline();
            }
            self.format_fn(method, span_end);
        }
        if self.has_comments() {
            self.flush_block_end_comments(span_end);
        }
        self.indent -= 1;
        self.writeln("}");
    }

    fn format_wire(&mut self, decl: &WireDecl) {
        // Emit struct-level naming attributes before the declaration.
        if let Some(case) = decl.json_case {
            self.write_indent();
            let s = case.as_str();
            let needs_quotes = s.contains('-');
            self.write("#[json(");
            if needs_quotes {
                self.write("\"");
            }
            self.write(s);
            if needs_quotes {
                self.write("\"");
            }
            self.write(")]\n");
        }
        if let Some(case) = decl.yaml_case {
            self.write_indent();
            let s = case.as_str();
            let needs_quotes = s.contains('-');
            self.write("#[yaml(");
            if needs_quotes {
                self.write("\"");
            }
            self.write(s);
            if needs_quotes {
                self.write("\"");
            }
            self.write(")]\n");
        }
        self.write_indent();
        match decl.kind {
            WireDeclKind::Struct => self.write("wire type "),
            WireDeclKind::Enum => self.write("wire enum "),
        }
        self.write(&decl.name);
        self.write(" {\n");
        self.indent += 1;
        for field in &decl.fields {
            self.format_wire_field(field);
        }
        for v in &decl.variants {
            self.format_variant(v, true);
        }
        self.indent -= 1;
        self.writeln("}");
    }

    fn format_wire_field(&mut self, f: &WireFieldDecl) {
        self.write_indent();
        if f.is_reserved {
            self.write("reserved ");
        }
        self.write(&f.name);
        self.write(": ");
        self.write(&f.ty);
        self.write(" @");
        self.write(&f.field_number.to_string());
        // Modifiers go after the tag (parser expects: name: type @N optional;)
        if f.is_optional {
            self.write(" optional");
        }
        if f.is_deprecated {
            self.write(" deprecated");
        }
        if f.is_repeated {
            self.write(" repeated");
        }
        if let Some(name) = &f.json_name {
            self.write(" json(\"");
            self.write(name);
            self.write("\")");
        }
        if let Some(name) = &f.yaml_name {
            self.write(" yaml(\"");
            self.write(name);
            self.write("\")");
        }
        self.write(";\n");
    }

    fn format_extern_block(&mut self, decl: &ExternBlock, span_end: usize) {
        self.write_indent();
        self.write("extern \"");
        self.write(&decl.abi);
        self.write("\" {\n");
        self.indent += 1;
        for f in &decl.functions {
            if self.has_comments() {
                let pos = self.find_keyword_after(&format!("fn {}", f.name), self.prev_source_pos);
                self.flush_comments_before(pos);
            }
            self.format_extern_fn(f);
        }
        if self.has_comments() {
            self.flush_block_end_comments(span_end);
        }
        self.indent -= 1;
        self.writeln("}");
    }

    fn format_extern_fn(&mut self, f: &ExternFnDecl) {
        self.write_indent();
        self.write("fn ");
        self.write(&f.name);
        self.write("(");
        self.format_params(&f.params);
        if f.is_variadic {
            if !f.params.is_empty() {
                self.write(", ");
            }
            self.write("...");
        }
        self.write(")");
        if let Some(ret) = &f.return_type {
            self.write(" -> ");
            self.format_type_expr(&ret.0);
        }
        self.write(";\n");
    }

    fn format_actor(&mut self, decl: &ActorDecl, span_end: usize) {
        self.write_indent();
        self.write("actor ");
        self.write(&decl.name);
        if let Some(supers) = &decl.super_traits {
            self.write(": ");
            self.format_trait_bound_list(supers);
        }
        self.write(" {\n");
        self.indent += 1;

        for field in &decl.fields {
            if self.has_comments() {
                let pos =
                    self.find_keyword_after(&format!("let {}", field.name), self.prev_source_pos);
                self.flush_comments_before(pos);
            }
            self.format_field_decl(field);
        }

        if let Some(cap) = &decl.mailbox_capacity {
            self.newline();
            self.write_indent();
            self.write("mailbox ");
            self.write(&cap.to_string());
            if let Some(policy) = &decl.overflow_policy {
                self.write(" overflow ");
                match policy {
                    OverflowPolicy::DropNew => self.write("drop_new"),
                    OverflowPolicy::DropOld => self.write("drop_old"),
                    OverflowPolicy::Block => self.write("block"),
                    OverflowPolicy::Fail => self.write("fail"),
                    OverflowPolicy::Coalesce {
                        key_field,
                        fallback,
                    } => {
                        self.write("coalesce(");
                        self.write(key_field);
                        if let Some(fb) = fallback {
                            self.write(", ");
                            match fb {
                                crate::ast::OverflowFallback::DropNew => self.write("drop_new"),
                                crate::ast::OverflowFallback::DropOld => self.write("drop_old"),
                                crate::ast::OverflowFallback::Block => self.write("block"),
                                crate::ast::OverflowFallback::Fail => self.write("fail"),
                            }
                        }
                        self.write(")");
                    }
                }
            }
            self.write(";\n");
        }

        if let Some(init) = &decl.init {
            if self.has_comments() {
                let pos = self.find_keyword_after("init(", self.prev_source_pos);
                self.flush_comments_before(pos);
            } else if !decl.fields.is_empty() {
                self.newline();
            }
            self.format_actor_init(init, span_end);
        }

        for recv in &decl.receive_fns {
            if self.has_comments() {
                let kw = if recv.is_generator {
                    format!("receive gen fn {}", recv.name)
                } else {
                    format!("receive fn {}", recv.name)
                };
                let pos = self.find_keyword_after(&kw, self.prev_source_pos);
                self.flush_comments_before(pos);
            } else {
                self.newline();
            }
            self.format_receive_fn(recv, span_end);
        }

        for method in &decl.methods {
            if self.has_comments() {
                let pos =
                    self.find_keyword_after(&format!("fn {}", method.name), self.prev_source_pos);
                self.flush_comments_before(pos);
            } else {
                self.newline();
            }
            self.format_fn(method, span_end);
        }

        if self.has_comments() {
            self.flush_block_end_comments(span_end);
        }
        self.indent -= 1;
        self.writeln("}");
    }

    fn format_field_decl(&mut self, f: &FieldDecl) {
        self.write_indent();
        self.write("let ");
        self.write(&f.name);
        self.write(": ");
        self.format_type_expr(&f.ty.0);
        self.write(";\n");
    }

    fn format_actor_init(&mut self, init: &ActorInit, scope_end: usize) {
        self.write_indent();
        self.write("init(");
        self.format_params(&init.params);
        self.write(") ");
        self.format_block(&init.body, scope_end);
        self.newline();
    }

    fn format_receive_fn(&mut self, recv: &ReceiveFnDecl, scope_end: usize) {
        self.write_indent();
        if recv.is_pure {
            self.write("pure ");
        }
        if recv.is_generator {
            self.write("receive gen fn ");
        } else {
            self.write("receive fn ");
        }
        self.write(&recv.name);
        self.format_fn_signature(
            recv.type_params.as_ref(),
            &recv.params,
            recv.return_type.as_ref(),
            recv.where_clause.as_ref(),
        );
        self.write(" ");
        self.format_block(&recv.body, scope_end);
        self.newline();
    }

    fn format_supervisor(&mut self, decl: &SupervisorDecl) {
        self.write_indent();
        self.write("supervisor ");
        self.write(&decl.name);
        self.write(" {\n");
        self.indent += 1;

        if let Some(strategy) = &decl.strategy {
            self.write_indent();
            self.write("strategy: ");
            match strategy {
                SupervisorStrategy::OneForOne => self.write("one_for_one"),
                SupervisorStrategy::OneForAll => self.write("one_for_all"),
                SupervisorStrategy::RestForOne => self.write("rest_for_one"),
            }
            self.write(";\n");
        }
        if let Some(max) = decl.max_restarts {
            self.write_indent();
            self.write("max_restarts: ");
            self.write(&max.to_string());
            self.write(";\n");
        }
        if let Some(window) = &decl.window {
            self.write_indent();
            self.write("window: ");
            self.write(window);
            self.write(";\n");
        }

        if !decl.children.is_empty() {
            self.newline();
            for child in &decl.children {
                self.format_child_spec(child);
            }
        }

        self.indent -= 1;
        self.writeln("}");
    }

    fn format_child_spec(&mut self, spec: &ChildSpec) {
        self.write_indent();
        self.write("child ");
        self.write(&spec.name);
        self.write(": ");
        self.write(&spec.actor_type);
        if !spec.args.is_empty() {
            self.write("(");
            self.comma_sep(&spec.args, |f, arg| f.format_expr(&arg.0));
            self.write(")");
        }
        if let Some(restart) = &spec.restart {
            self.write(" restart: ");
            match restart {
                RestartPolicy::Permanent => self.write("permanent"),
                RestartPolicy::Transient => self.write("transient"),
                RestartPolicy::Temporary => self.write("temporary"),
            }
        }
        self.write(";\n");
    }

    fn format_fn(&mut self, decl: &FnDecl, span_end: usize) {
        for attr in &decl.attributes {
            self.write_indent();
            self.write("#[");
            self.write(&attr.name);
            if !attr.args.is_empty() {
                self.write("(");
                self.write(&attr.args.join(", "));
                self.write(")");
            }
            self.write("]\n");
        }
        self.write_indent();
        self.write_visibility(decl.visibility);
        if decl.is_async {
            self.write("async ");
        }
        if decl.is_generator {
            self.write("gen ");
        }
        self.write("fn ");
        self.write(&decl.name);
        self.format_fn_signature(
            decl.type_params.as_ref(),
            &decl.params,
            decl.return_type.as_ref(),
            decl.where_clause.as_ref(),
        );
        self.write(" ");
        self.format_block(&decl.body, span_end);
        self.newline();
    }

    // ------------------------------------------------------------------
    // Types
    // ------------------------------------------------------------------

    fn format_type_expr(&mut self, ty: &TypeExpr) {
        match ty {
            TypeExpr::Named { name, type_args } => {
                self.write(name);
                if let Some(args) = type_args {
                    self.write("<");
                    self.comma_sep(args, |f, arg| f.format_type_expr(&arg.0));
                    self.write(">");
                }
            }
            TypeExpr::Result { ok, err } => {
                self.write("Result<");
                self.format_type_expr(&ok.0);
                self.write(", ");
                self.format_type_expr(&err.0);
                self.write(">");
            }
            TypeExpr::Option(inner) => {
                self.write("Option<");
                self.format_type_expr(&inner.0);
                self.write(">");
            }
            TypeExpr::Tuple(elems) => {
                self.write("(");
                self.comma_sep(elems, |f, elem| f.format_type_expr(&elem.0));
                self.write(")");
            }
            TypeExpr::Array { element, size } => {
                self.write("[");
                self.format_type_expr(&element.0);
                self.write("; ");
                self.write(&size.to_string());
                self.write("]");
            }
            TypeExpr::Slice(inner) => {
                self.write("[");
                self.format_type_expr(&inner.0);
                self.write("]");
            }
            TypeExpr::Function {
                params,
                return_type,
            } => {
                self.write("fn(");
                self.comma_sep(params, |f, p| f.format_type_expr(&p.0));
                self.write(") -> ");
                self.format_type_expr(&return_type.0);
            }
            TypeExpr::Pointer {
                is_mutable,
                pointee,
            } => {
                if *is_mutable {
                    self.write("*mut ");
                } else {
                    self.write("*const ");
                }
                self.format_type_expr(&pointee.0);
            }
            TypeExpr::TraitObject(bounds) => {
                self.write("dyn ");
                if bounds.len() == 1 {
                    self.format_trait_bound(&bounds[0]);
                } else {
                    self.write("(");
                    for (i, bound) in bounds.iter().enumerate() {
                        if i > 0 { self.write(" + "); }
                        self.format_trait_bound(bound);
                    }
                    self.write(")");
                }
            }
            TypeExpr::Infer => {
                self.write("_");
            }
        }
    }

    /// Format `<type_params>(params) -> return_type where clause`.
    fn format_fn_signature(
        &mut self,
        type_params: Option<&Vec<TypeParam>>,
        params: &[Param],
        return_type: Option<&Spanned<TypeExpr>>,
        where_clause: Option<&WhereClause>,
    ) {
        self.format_opt_type_params(type_params);
        self.write("(");
        self.format_params(params);
        self.write(")");
        if let Some(ret) = return_type {
            self.write(" -> ");
            self.format_type_expr(&ret.0);
        }
        self.format_opt_where_clause(where_clause);
    }

    fn format_opt_type_params(&mut self, params: Option<&Vec<TypeParam>>) {
        if let Some(params) = params {
            self.write("<");
            self.comma_sep(params, |f, p| {
                f.write(&p.name);
                if !p.bounds.is_empty() {
                    f.write(": ");
                    f.format_trait_bound_list(&p.bounds);
                }
            });
            self.write(">");
        }
    }

    fn format_trait_bound(&mut self, bound: &TraitBound) {
        self.write(&bound.name);
        if let Some(args) = &bound.type_args {
            self.write("<");
            self.comma_sep(args, |f, arg| f.format_type_expr(&arg.0));
            self.write(">");
        }
    }

    fn format_trait_bound_list(&mut self, bounds: &[TraitBound]) {
        for (i, b) in bounds.iter().enumerate() {
            if i > 0 {
                self.write(" + ");
            }
            self.format_trait_bound(b);
        }
    }

    fn format_opt_where_clause(&mut self, clause: Option<&WhereClause>) {
        if let Some(clause) = clause {
            self.write(" where ");
            self.comma_sep(&clause.predicates, |f, pred| {
                f.format_type_expr(&pred.ty.0);
                f.write(": ");
                f.format_trait_bound_list(&pred.bounds);
            });
        }
    }

    fn format_params(&mut self, params: &[Param]) {
        self.comma_sep(params, |f, p| {
            if p.is_mutable {
                f.write("var ");
            }
            f.write(&p.name);
            f.write(": ");
            f.format_type_expr(&p.ty.0);
        });
    }

    // ------------------------------------------------------------------
    // Blocks & statements
    // ------------------------------------------------------------------

    fn format_block(&mut self, block: &Block, scope_end: usize) {
        self.write("{\n");
        self.indent += 1;
        self.enter_block_scope(scope_end);
        for stmt in &block.stmts {
            self.flush_comments_before(stmt.1.start);
            self.format_stmt(&stmt.0);
            self.prev_source_pos = stmt.1.end;
        }
        if let Some(trailing) = &block.trailing_expr {
            self.flush_comments_before(trailing.1.start);
            self.write_indent();
            self.format_expr(&trailing.0);
            self.newline();
            self.prev_source_pos = trailing.1.end;
        }
        self.flush_block_end_comments(scope_end);
        self.indent -= 1;
        self.write_indent();
        self.write("}");
    }

    #[expect(clippy::too_many_lines, reason = "match on all Stmt variants")]
    fn format_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let { pattern, ty, value } => {
                self.write_indent();
                self.write("let ");
                self.format_pattern(&pattern.0);
                if let Some(ty) = ty {
                    self.write(": ");
                    self.format_type_expr(&ty.0);
                }
                if let Some(val) = value {
                    self.write(" = ");
                    self.format_expr(&val.0);
                }
                self.write(";\n");
            }
            Stmt::Var { name, ty, value } => {
                self.write_indent();
                self.write("var ");
                self.write(name);
                if let Some(ty) = ty {
                    self.write(": ");
                    self.format_type_expr(&ty.0);
                }
                if let Some(val) = value {
                    self.write(" = ");
                    self.format_expr(&val.0);
                }
                self.write(";\n");
            }
            Stmt::Assign { target, op, value } => {
                self.write_indent();
                self.format_expr(&target.0);
                if let Some(op) = op {
                    self.write(" ");
                    self.write(compound_assign_op_str(*op));
                    self.write(" ");
                } else {
                    self.write(" = ");
                }
                self.format_expr(&value.0);
                self.write(";\n");
            }
            Stmt::If {
                condition,
                then_block,
                else_block,
            } => {
                self.write_indent();
                self.write("if ");
                self.format_expr(&condition.0);
                self.write(" ");
                self.format_block(then_block, self.source.len());
                if let Some(eb) = else_block {
                    self.format_else_block(eb);
                }
                self.newline();
            }
            Stmt::IfLet {
                pattern,
                expr,
                body,
                else_body,
            } => {
                self.write_indent();
                self.write("if let ");
                self.format_pattern(&pattern.0);
                self.write(" = ");
                self.format_expr(&expr.0);
                self.write(" ");
                self.format_block(body, self.source.len());
                if let Some(else_block) = else_body {
                    self.write(" else ");
                    self.format_block(else_block, self.source.len());
                }
                self.newline();
            }
            Stmt::Match { scrutinee, arms } => {
                self.write_indent();
                self.write("match ");
                self.format_expr(&scrutinee.0);
                self.write(" {\n");
                self.indent += 1;
                for arm in arms {
                    self.format_match_arm(arm);
                }
                self.indent -= 1;
                self.write_indent();
                self.write("}\n");
            }
            Stmt::Loop { label, body } => {
                self.write_indent();
                if let Some(label) = label {
                    self.write("@");
                    self.write(label);
                    self.write(": ");
                }
                self.write("loop ");
                self.format_block(body, self.source.len());
                self.newline();
            }
            Stmt::For {
                label,
                is_await,
                pattern,
                iterable,
                body,
            } => {
                self.write_indent();
                if let Some(label) = label {
                    self.write("@");
                    self.write(label);
                    self.write(": ");
                }
                self.write("for ");
                if *is_await {
                    self.write("await ");
                }
                self.format_pattern(&pattern.0);
                self.write(" in ");
                self.format_expr(&iterable.0);
                self.write(" ");
                self.format_block(body, self.source.len());
                self.newline();
            }
            Stmt::While {
                label,
                condition,
                body,
            } => {
                self.write_indent();
                if let Some(label) = label {
                    self.write("@");
                    self.write(label);
                    self.write(": ");
                }
                self.write("while ");
                self.format_expr(&condition.0);
                self.write(" ");
                self.format_block(body, self.source.len());
                self.newline();
            }
            Stmt::Break { label, value } => {
                self.write_indent();
                self.write("break");
                if let Some(label) = label {
                    self.write(" @");
                    self.write(label);
                }
                if let Some(val) = value {
                    self.write(" ");
                    self.format_expr(&val.0);
                }
                self.write(";\n");
            }
            Stmt::Continue { label } => {
                self.write_indent();
                self.write("continue");
                if let Some(label) = label {
                    self.write(" @");
                    self.write(label);
                }
                self.write(";\n");
            }
            Stmt::Return(val) => {
                self.write_indent();
                self.write("return");
                if let Some(val) = val {
                    self.write(" ");
                    self.format_expr(&val.0);
                }
                self.write(";\n");
            }
            Stmt::Expression(expr) => {
                self.write_indent();
                self.format_expr(&expr.0);
                self.write(";\n");
            }
            Stmt::Defer(expr) => {
                self.write_indent();
                self.write("defer ");
                self.format_expr(&expr.0);
                self.write(";\n");
            }
        }
    }

    fn format_else_block(&mut self, eb: &ElseBlock) {
        if eb.is_if {
            if let Some(if_stmt) = &eb.if_stmt {
                self.write(" else ");
                // Print the inner `if` without leading indent (it's on the same line).
                match &if_stmt.0 {
                    Stmt::If {
                        condition,
                        then_block,
                        else_block,
                    } => {
                        self.write("if ");
                        self.format_expr(&condition.0);
                        self.write(" ");
                        self.format_block(then_block, self.source.len());
                        if let Some(eb) = else_block {
                            self.format_else_block(eb);
                        }
                    }
                    _ => {
                        // Shouldn't happen for well-formed ASTs, but handle gracefully.
                        self.format_stmt(&if_stmt.0);
                    }
                }
            }
        } else if let Some(block) = &eb.block {
            self.write(" else ");
            self.format_block(block, self.source.len());
        }
    }

    fn format_match_arm(&mut self, arm: &MatchArm) {
        self.write_indent();
        self.format_pattern(&arm.pattern.0);
        if let Some(guard) = &arm.guard {
            self.write(" if ");
            self.format_expr(&guard.0);
        }
        self.write(" => ");
        self.format_expr(&arm.body.0);
        self.write(",");
        self.newline();
    }

    // ------------------------------------------------------------------
    // Expressions
    // ------------------------------------------------------------------

    /// Format an expression with precedence tracking for correct parenthesization.
    ///
    /// `parent_prec` is the precedence of the enclosing binary operator (0 at top level).
    /// `is_right` indicates this expression is the right operand of its parent binary op.
    fn format_expr_prec(&mut self, expr: &Expr, parent_prec: u8, is_right: bool) {
        if let Expr::Binary { left, op, right } = expr {
            let prec = binop_precedence(*op);
            // Need parens when:
            // 1. Our precedence is lower than the parent (tighter parent binds first)
            // 2. Same precedence AND we're on the right side (handles non-associative like a-b-c)
            let needs_parens = prec < parent_prec || (prec == parent_prec && is_right);
            if needs_parens {
                self.write("(");
            }
            self.format_expr_prec(&left.0, prec, false);
            self.write(" ");
            self.write(binary_op_str(*op));
            self.write(" ");
            self.format_expr_prec(&right.0, prec, true);
            if needs_parens {
                self.write(")");
            }
        } else {
            self.format_expr(expr);
        }
    }

    #[expect(clippy::too_many_lines, reason = "match on all Expr variants")]
    fn format_expr(&mut self, expr: &Expr) {
        match expr {
            Expr::Binary { left, op, right } => {
                let prec = binop_precedence(*op);
                self.format_expr_prec(&left.0, prec, false);
                self.write(" ");
                self.write(binary_op_str(*op));
                self.write(" ");
                self.format_expr_prec(&right.0, prec, true);
            }
            Expr::Unary { op, operand } => {
                match op {
                    UnaryOp::Not => self.write("!"),
                    UnaryOp::Negate => self.write("-"),
                    UnaryOp::BitNot => self.write("~"),
                }
                self.format_expr(&operand.0);
            }
            Expr::Literal(lit) => self.format_literal(lit),
            Expr::Identifier(name) => self.write(name),
            Expr::Tuple(elems) => {
                self.write("(");
                self.comma_sep(elems, |f, elem| f.format_expr(&elem.0));
                self.write(")");
            }
            Expr::Array(elems) => {
                self.write("[");
                self.comma_sep(elems, |f, elem| f.format_expr(&elem.0));
                self.write("]");
            }
            Expr::ArrayRepeat { value, count } => {
                self.write("[");
                self.format_expr(&value.0);
                self.write("; ");
                self.format_expr(&count.0);
                self.write("]");
            }
            Expr::Block(block) => {
                self.format_block(block, self.source.len());
            }
            Expr::If {
                condition,
                then_block,
                else_block,
            } => {
                self.write("if ");
                self.format_expr(&condition.0);
                self.write(" ");
                self.format_expr(&then_block.0);
                if let Some(eb) = else_block {
                    self.write(" else ");
                    self.format_expr(&eb.0);
                }
            }
            Expr::IfLet {
                pattern,
                expr,
                body,
                else_body,
            } => {
                self.write("if let ");
                self.format_pattern(&pattern.0);
                self.write(" = ");
                self.format_expr(&expr.0);
                self.write(" ");
                self.format_block(body, self.source.len());
                if let Some(else_block) = else_body {
                    self.write(" else ");
                    self.format_block(else_block, self.source.len());
                }
            }
            Expr::Match { scrutinee, arms } => {
                self.write("match ");
                self.format_expr(&scrutinee.0);
                self.write(" {\n");
                self.indent += 1;
                for arm in arms {
                    self.format_match_arm(arm);
                }
                self.indent -= 1;
                self.write_indent();
                self.write("}");
            }
            Expr::Lambda {
                is_move,
                params,
                return_type,
                body,
                ..
            } => {
                if *is_move {
                    self.write("move ");
                }
                self.write("(");
                self.format_lambda_params(params);
                self.write(")");
                if let Some(ret) = return_type {
                    self.write(" -> ");
                    self.format_type_expr(&ret.0);
                }
                self.write(" => ");
                self.format_expr(&body.0);
            }
            Expr::Spawn { target, args } => {
                self.write("spawn ");
                self.format_expr(&target.0);
                if !args.is_empty() {
                    self.write("(");
                    self.comma_sep(args, |f, (name, value)| {
                        f.write(name);
                        f.write(": ");
                        f.format_expr(&value.0);
                    });
                    self.write(")");
                }
            }
            Expr::SpawnLambdaActor {
                is_move,
                params,
                return_type,
                body,
            } => {
                self.write("spawn ");
                if *is_move {
                    self.write("move ");
                }
                self.write("(");
                self.format_lambda_params(params);
                self.write(")");
                if let Some(ret) = return_type {
                    self.write(" -> ");
                    self.format_type_expr(&ret.0);
                }
                self.write(" => ");
                self.format_expr(&body.0);
            }
            Expr::Scope { binding, body } => {
                self.write("scope ");
                if let Some(name) = binding {
                    self.write("|");
                    self.write(name);
                    self.write("| ");
                }
                self.format_block(body, self.source.len());
            }
            Expr::InterpolatedString(parts) => {
                self.write("f\"");
                for part in parts {
                    match part {
                        StringPart::Literal(s) => self.write(s),
                        StringPart::Expr((expr, _)) => {
                            self.write("{");
                            self.format_expr(expr);
                            self.write("}");
                        }
                    }
                }
                self.write("\"");
            }
            Expr::Call {
                function,
                type_args,
                args,
                ..
            } => {
                self.format_expr(&function.0);
                if let Some(type_args) = type_args {
                    self.write("::<");
                    self.comma_sep(type_args, |f, ta| f.format_type_expr(&ta.0));
                    self.write(">");
                }
                self.write("(");
                self.format_call_args(args);
                self.write(")");
            }
            Expr::MethodCall {
                receiver,
                method,
                args,
            } => {
                self.format_expr(&receiver.0);
                self.write(".");
                self.write(method);
                self.write("(");
                self.format_call_args(args);
                self.write(")");
            }
            Expr::StructInit { name, fields } => {
                self.write(name);
                self.write(" { ");
                self.comma_sep(fields, |f, (fname, fval)| {
                    f.write(fname);
                    f.write(": ");
                    f.format_expr(&fval.0);
                });
                self.write(" }");
            }
            Expr::Send { target, message } => {
                self.format_expr(&target.0);
                self.write(" <- ");
                self.format_expr(&message.0);
            }
            Expr::Select { arms, timeout } => {
                self.write("select {\n");
                self.indent += 1;
                for arm in arms {
                    self.format_select_arm(arm);
                }
                if let Some(t) = timeout {
                    self.format_timeout(t);
                }
                self.indent -= 1;
                self.write_indent();
                self.write("}");
            }
            Expr::Join(exprs) => {
                self.write("join {\n");
                self.indent += 1;
                for e in exprs {
                    self.write_indent();
                    self.format_expr(&e.0);
                    self.write(",\n");
                }
                self.indent -= 1;
                self.write_indent();
                self.write("}");
            }
            Expr::Timeout { expr, duration } => {
                self.format_expr(&expr.0);
                self.write(" | after ");
                self.format_expr(&duration.0);
            }
            Expr::Unsafe(block) => {
                self.write("unsafe ");
                self.format_block(block, self.source.len());
            }
            Expr::Yield(val) => {
                self.write("yield");
                if let Some(val) = val {
                    self.write(" ");
                    self.format_expr(&val.0);
                }
            }
            Expr::Cooperate => self.write("cooperate"),
            Expr::FieldAccess { object, field } => {
                self.format_expr(&object.0);
                self.write(".");
                self.write(field);
            }
            Expr::Index { object, index } => {
                self.format_expr(&object.0);
                self.write("[");
                self.format_expr(&index.0);
                self.write("]");
            }
            Expr::Cast { expr, ty } => {
                self.format_expr(&expr.0);
                self.write(" as ");
                self.format_type_expr(&ty.0);
            }
            Expr::PostfixTry(expr) => {
                self.format_expr(&expr.0);
                self.write("?");
            }
            Expr::Range {
                start,
                end,
                inclusive,
            } => {
                if let Some(s) = start {
                    self.format_expr(&s.0);
                }
                if *inclusive {
                    self.write("..=");
                } else {
                    self.write("..");
                }
                if let Some(e) = end {
                    self.format_expr(&e.0);
                }
            }
            Expr::Await(inner) => {
                self.write("await ");
                self.format_expr(&inner.0);
            }
            Expr::ScopeLaunch(block) => {
                self.write("s.launch ");
                self.format_block(block, self.source.len());
            }
            Expr::ScopeSpawn(block) => {
                self.write("s.spawn ");
                self.format_block(block, self.source.len());
            }
            Expr::ScopeCancel => self.write("s.cancel()"),

            Expr::RegexLiteral(pattern) => {
                self.write("re\"");
                self.write(pattern);
                self.write("\"");
            }
        }
    }

    fn format_select_arm(&mut self, arm: &SelectArm) {
        self.write_indent();
        self.format_pattern(&arm.binding.0);
        self.write(" from ");
        self.format_expr(&arm.source.0);
        self.write(" => ");
        self.format_expr(&arm.body.0);
        self.write(",");
        self.newline();
    }

    fn format_timeout(&mut self, tc: &TimeoutClause) {
        self.write_indent();
        self.write("after ");
        self.format_expr(&tc.duration.0);
        self.write(" => ");
        self.format_expr(&tc.body.0);
        self.write(",");
        self.newline();
    }

    fn format_call_args(&mut self, args: &[CallArg]) {
        self.comma_sep(args, |f, arg| match arg {
            CallArg::Named { name, value } => {
                f.write(name);
                f.write(": ");
                f.format_expr(&value.0);
            }
            CallArg::Positional(e) => f.format_expr(&e.0),
        });
    }

    fn format_lambda_params(&mut self, params: &[LambdaParam]) {
        self.comma_sep(params, |f, p| {
            f.write(&p.name);
            if let Some(ty) = &p.ty {
                f.write(": ");
                f.format_type_expr(&ty.0);
            }
        });
    }

    fn format_literal(&mut self, lit: &Literal) {
        use std::fmt::Write;
        match lit {
            Literal::Integer { value, radix } => match radix {
                IntRadix::Hex => { let _ = write!(self.output, "0x{value:X}"); }
                IntRadix::Octal => { let _ = write!(self.output, "0o{value:o}"); }
                IntRadix::Binary => { let _ = write!(self.output, "0b{value:b}"); }
                IntRadix::Decimal => { let _ = write!(self.output, "{value}"); }
            },
            Literal::Float(f) => {
                let s = f.to_string();
                self.write(&s);
                // Ensure there's always a decimal point for clarity.
                if !s.contains('.') {
                    self.write(".0");
                }
            }
            Literal::String(s) => {
                self.write("\"");
                self.write(&escape_string(s));
                self.write("\"");
            }
            Literal::Bool(b) => self.write(if *b { "true" } else { "false" }),
            Literal::Char(c) => {
                self.write("'");
                if let Some(esc) = escape_char(*c) {
                    self.write(esc);
                } else {
                    self.output.push(*c);
                }
                self.write("'");
            }
            Literal::Duration(ns) => {
                if *ns >= 3_600_000_000_000 && *ns % 3_600_000_000_000 == 0 {
                    let _ = write!(self.output, "{}h", *ns / 3_600_000_000_000);
                } else if *ns >= 60_000_000_000 && *ns % 60_000_000_000 == 0 {
                    let _ = write!(self.output, "{}m", *ns / 60_000_000_000);
                } else if *ns >= 1_000_000_000 && *ns % 1_000_000_000 == 0 {
                    let _ = write!(self.output, "{}s", *ns / 1_000_000_000);
                } else if *ns >= 1_000_000 && *ns % 1_000_000 == 0 {
                    let _ = write!(self.output, "{}ms", *ns / 1_000_000);
                } else if *ns >= 1_000 && *ns % 1_000 == 0 {
                    let _ = write!(self.output, "{}us", *ns / 1_000);
                } else {
                    let _ = write!(self.output, "{ns}ns");
                }
            }
        }
    }

    // ------------------------------------------------------------------
    // Patterns
    // ------------------------------------------------------------------

    fn format_pattern(&mut self, pat: &Pattern) {
        match pat {
            Pattern::Wildcard => self.write("_"),
            Pattern::Literal(lit) => self.format_literal(lit),
            Pattern::Identifier(name) => self.write(name),
            Pattern::Constructor { name, patterns } => {
                self.write(name);
                if !patterns.is_empty() {
                    self.write("(");
                    self.comma_sep(patterns, |f, p| f.format_pattern(&p.0));
                    self.write(")");
                }
            }
            Pattern::Struct { name, fields } => {
                self.write(name);
                self.write(" { ");
                self.comma_sep(fields, Self::format_pattern_field);
                self.write(" }");
            }
            Pattern::Tuple(patterns) => {
                self.write("(");
                self.comma_sep(patterns, |f, p| f.format_pattern(&p.0));
                self.write(")");
            }
            Pattern::Or(left, right) => {
                self.format_pattern(&left.0);
                self.write(" | ");
                self.format_pattern(&right.0);
            }
        }
    }

    fn format_pattern_field(&mut self, f: &PatternField) {
        self.write(&f.name);
        if let Some(pat) = &f.pattern {
            self.write(": ");
            self.format_pattern(&pat.0);
        }
    }
}

// ---------------------------------------------------------------------------
// String helpers
// ---------------------------------------------------------------------------

fn binary_op_str(op: BinaryOp) -> &'static str {
    match op {
        BinaryOp::Add => "+",
        BinaryOp::Subtract => "-",
        BinaryOp::Multiply => "*",
        BinaryOp::Divide => "/",
        BinaryOp::Modulo => "%",
        BinaryOp::Equal => "==",
        BinaryOp::NotEqual => "!=",
        BinaryOp::Less => "<",
        BinaryOp::LessEqual => "<=",
        BinaryOp::Greater => ">",
        BinaryOp::GreaterEqual => ">=",
        BinaryOp::And => "&&",
        BinaryOp::Or => "||",
        BinaryOp::BitAnd => "&",
        BinaryOp::BitOr => "|",
        BinaryOp::BitXor => "^",
        BinaryOp::Shl => "<<",
        BinaryOp::Shr => ">>",
        BinaryOp::Range => "..",
        BinaryOp::RangeInclusive => "..=",
        BinaryOp::Send => "<-",
        BinaryOp::RegexMatch => "=~",
        BinaryOp::RegexNotMatch => "!~",
    }
}

/// Map binary operators to their precedence level (higher = tighter binding).
/// Values match the Pratt parser's binding powers in parser.rs.
fn binop_precedence(op: BinaryOp) -> u8 {
    match op {
        BinaryOp::Send => 1,
        BinaryOp::Or => 3,
        BinaryOp::BitOr => 5,
        BinaryOp::BitXor => 7,
        BinaryOp::BitAnd => 9,
        BinaryOp::And => 11,
        BinaryOp::Equal | BinaryOp::NotEqual | BinaryOp::RegexMatch | BinaryOp::RegexNotMatch => 13,
        BinaryOp::Less | BinaryOp::LessEqual | BinaryOp::Greater | BinaryOp::GreaterEqual => 15,
        BinaryOp::Range | BinaryOp::RangeInclusive => 17,
        BinaryOp::Shl | BinaryOp::Shr => 19,
        BinaryOp::Add | BinaryOp::Subtract => 21,
        BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Modulo => 23,
    }
}

fn compound_assign_op_str(op: CompoundAssignOp) -> &'static str {
    match op {
        CompoundAssignOp::Add => "+=",
        CompoundAssignOp::Subtract => "-=",
        CompoundAssignOp::Multiply => "*=",
        CompoundAssignOp::Divide => "/=",
        CompoundAssignOp::Modulo => "%=",
        CompoundAssignOp::BitAnd => "&=",
        CompoundAssignOp::BitOr => "|=",
        CompoundAssignOp::BitXor => "^=",
        CompoundAssignOp::Shl => "<<=",
        CompoundAssignOp::Shr => ">>=",
    }
}

fn escape_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len());
    for c in s.chars() {
        match c {
            '\\' => out.push_str("\\\\"),
            '"' => out.push_str("\\\""),
            '\n' => out.push_str("\\n"),
            '\t' => out.push_str("\\t"),
            '\r' => out.push_str("\\r"),
            '\0' => out.push_str("\\0"),
            other => out.push(other),
        }
    }
    out
}

/// Return the escape sequence for a char, or `None` if no escaping is needed.
fn escape_char(c: char) -> Option<&'static str> {
    match c {
        '\\' => Some("\\\\"),
        '\'' => Some("\\'"),
        '\n' => Some("\\n"),
        '\t' => Some("\\t"),
        '\r' => Some("\\r"),
        '\0' => Some("\\0"),
        _ => None,
    }
}

// ---------------------------------------------------------------------------
// Comment extraction
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
struct Comment {
    text: String,
    span: Range<usize>,
}

/// Scan source text and extract all comments with their byte positions.
fn extract_comments(source: &str) -> Vec<Comment> {
    let mut comments = Vec::new();
    let bytes = source.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            b'/' if i + 1 < bytes.len() => match bytes[i + 1] {
                b'/' => {
                    let start = i;
                    i += 2;
                    while i < bytes.len() && bytes[i] != b'\n' {
                        i += 1;
                    }
                    comments.push(Comment {
                        text: source[start..i].to_string(),
                        span: start..i,
                    });
                }
                b'*' => {
                    let start = i;
                    i += 2;
                    let mut depth = 1u32;
                    while i + 1 < bytes.len() && depth > 0 {
                        if bytes[i] == b'/' && bytes[i + 1] == b'*' {
                            depth += 1;
                            i += 2;
                        } else if bytes[i] == b'*' && bytes[i + 1] == b'/' {
                            depth -= 1;
                            i += 2;
                        } else {
                            i += 1;
                        }
                    }
                    comments.push(Comment {
                        text: source[start..i].to_string(),
                        span: start..i,
                    });
                }
                _ => i += 1,
            },
            b'"' => {
                i += 1;
                while i < bytes.len() && bytes[i] != b'"' {
                    if bytes[i] == b'\\' {
                        i += 1;
                    }
                    i += 1;
                }
                if i < bytes.len() {
                    i += 1;
                }
            }
            b'\'' => {
                i += 1;
                while i < bytes.len() && bytes[i] != b'\'' {
                    if bytes[i] == b'\\' {
                        i += 1;
                    }
                    i += 1;
                }
                if i < bytes.len() {
                    i += 1;
                }
            }
            _ => i += 1,
        }
    }
    comments
}

fn is_trailing_comment(source: &str, comment_start: usize) -> bool {
    let bytes = source.as_bytes();
    let mut i = comment_start;
    while i > 0 && bytes[i - 1] != b'\n' {
        i -= 1;
    }
    source[i..comment_start].chars().any(|c| !c.is_whitespace())
}

fn find_block_close(source: &str, from: usize, before: usize) -> usize {
    let bytes = source.as_bytes();
    let end = before.min(bytes.len());
    let mut i = from.min(end);
    while i < end {
        match bytes[i] {
            b'}' => return i,
            b'/' if i + 1 < end => match bytes[i + 1] {
                b'/' => {
                    i += 2;
                    while i < end && bytes[i] != b'\n' {
                        i += 1;
                    }
                }
                b'*' => {
                    i += 2;
                    while i + 1 < end {
                        if bytes[i] == b'*' && bytes[i + 1] == b'/' {
                            i += 2;
                            break;
                        }
                        i += 1;
                    }
                }
                _ => i += 1,
            },
            _ => i += 1,
        }
    }
    end
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::parse;

    fn roundtrip(src: &str) -> String {
        let result = parse(src);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        format_program(&result.program)
    }

    #[test]
    fn simple_function() {
        let src = "fn main() -> i32 {\n    0\n}\n";
        let formatted = roundtrip(src);
        assert_eq!(formatted, src);
    }

    #[test]
    fn fibonacci() {
        let src = "\
fn fibonacci(n: i32) -> i32 {
    if n <= 1 {
        n
    } else {
        fibonacci(n - 1) + fibonacci(n - 2)
    }
}

fn main() -> i32 {
    let result = fibonacci(10);
    println(result);
    0
}
";
        let formatted = roundtrip(src);
        assert_eq!(formatted, src);
    }

    #[test]
    fn actor_declaration() {
        let src = "\
actor Counter {
    let count: i32;

    receive fn increment() {
        self.count = self.count + 1;
    }

    receive fn get_count() -> i32 {
        self.count
    }
}
";
        let formatted = roundtrip(src);
        assert_eq!(formatted, src);
    }

    #[test]
    fn enum_declaration() {
        let src = "\
enum Color {
    Red;
    Green;
    Blue;
}
";
        let formatted = roundtrip(src);
        assert_eq!(formatted, src);
    }

    #[test]
    fn extern_block() {
        let src = "\
extern \"C\" {
    fn puts(s: string) -> i32;
    fn exit(code: i32);
}
";
        let formatted = roundtrip(src);
        assert_eq!(formatted, src);
    }

    fn roundtrip_source(src: &str) -> String {
        let result = parse(src);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        format_source(src, &result.program)
    }

    #[test]
    fn preserves_leading_comment() {
        let src = "\
// A greeting function
fn greet() {
    println(\"hello\");
}
";
        assert_eq!(roundtrip_source(src), src);
    }

    #[test]
    fn preserves_trailing_comment() {
        let src = "\
fn main() -> i32 {
    let x = 5; // init value
    println(x);
    0
}
";
        assert_eq!(roundtrip_source(src), src);
    }

    #[test]
    fn preserves_comment_between_items() {
        let src = "\
fn foo() -> i32 {
    1
}

// Bar does something else
fn bar() -> i32 {
    2
}
";
        assert_eq!(roundtrip_source(src), src);
    }

    #[test]
    fn preserves_comment_inside_block() {
        let src = "\
fn main() -> i32 {
    // compute result
    let x = 5;
    0
}
";
        assert_eq!(roundtrip_source(src), src);
    }

    #[test]
    fn hex_literal_preserved() {
        let src = "fn main() {\n    let x = 0xFF;\n}\n";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn binary_literal_preserved() {
        let src = "fn main() {\n    let x = 0b1010;\n}\n";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn octal_literal_preserved() {
        let src = "fn main() {\n    let x = 0o77;\n}\n";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn await_prefix_syntax() {
        let src = "async fn main() {\n    let x = await foo();\n}\n";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn pub_const_preserved() {
        let src = "pub const MAX: i32 = 100;\n";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn named_call_args_preserved() {
        let src = "\
fn main() {
    foo(name: 42, value: 10);
}
";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn operator_precedence_parens() {
        // Lower-precedence operand on the right of higher-precedence op needs parens
        let src = "fn main() {\n    let x = a * (b + c);\n}\n";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn operator_same_precedence_right_assoc() {
        // a - (b - c) must keep parens (same precedence, right operand)
        let src = "fn main() {\n    let x = a - (b - c);\n}\n";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn enum_all_variants_semicolon() {
        let src = "\
enum Color {
    Red;
    Green;
    Blue;
}
";
        assert_eq!(roundtrip(src), src);
    }
}
