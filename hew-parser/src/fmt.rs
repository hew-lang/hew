//! Pretty-printer that converts an AST back to canonical Hew source text.

use std::fmt::Write as _;
use std::ops::Range;

use finl_unicode::categories::CharacterCategories;

use crate::ast::{
    ActorDecl, ActorInit, Attribute, AttributeArg, BinaryOp, Block, CallArg, ChildSpec,
    CompoundAssignOp, ConstDecl, ElseBlock, Expr, ExternBlock, ExternFnDecl, FieldDecl, FnDecl,
    ImplDecl, ImportDecl, ImportSpec, IntRadix, Item, LambdaParam, Literal, MachineDecl,
    MachineState, MachineTransition, MatchArm, NamingCase, OverflowPolicy, Param, Pattern,
    PatternField, Program, ReceiveFnDecl, RecordDecl, RecordKind, RestartPolicy, SelectArm,
    ShutdownDirective, Spanned, Stmt, StringPart, SupervisorDecl, SupervisorStrategy,
    TimeoutClause, TraitBound, TraitDecl, TraitItem, TraitMethod, TypeAliasDecl, TypeBodyItem,
    TypeDecl, TypeDeclKind, TypeExpr, TypeParam, UnaryOp, VariantDecl, VariantKind, Visibility,
    WhereClause, WireMetadata,
};

/// Format a duration in nanoseconds to the most natural unit suffix.
fn format_duration_ns(ns: i64) -> String {
    if ns == 0 {
        return "0ns".to_string();
    }
    if ns % 3_600_000_000_000 == 0 {
        format!("{}h", ns / 3_600_000_000_000)
    } else if ns % 60_000_000_000 == 0 {
        format!("{}m", ns / 60_000_000_000)
    } else if ns % 1_000_000_000 == 0 {
        format!("{}s", ns / 1_000_000_000)
    } else if ns % 1_000_000 == 0 {
        format!("{}ms", ns / 1_000_000)
    } else if ns % 1_000 == 0 {
        format!("{}us", ns / 1_000)
    } else {
        format!("{ns}ns")
    }
}

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
    let comments = extract_comments(source, false);
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
            Visibility::Package => self.write("package "),
        }
    }

    /// Emit an outer (`///`) or inner (`//!`) doc-comment block, one line per
    /// `\n` in the stored content. The parser strips the prefix and one
    /// leading space; we add them back. Empty lines emit the prefix alone,
    /// matching parser input.
    fn write_doc_comment(&mut self, doc: &str, prefix: &str) {
        for line in doc.split('\n') {
            self.write_indent();
            if line.is_empty() {
                self.write(prefix);
                self.write("\n");
            } else {
                self.write(prefix);
                self.write(" ");
                self.write(line);
                self.write("\n");
            }
        }
    }

    fn write_outer_doc(&mut self, doc: Option<&String>) {
        if let Some(d) = doc {
            self.write_doc_comment(d, "///");
        }
    }

    // ------------------------------------------------------------------
    // Comment flushing
    // ------------------------------------------------------------------

    fn flush_comments_before(&mut self, pos: usize) {
        while self.next_comment < self.comments.len()
            && self.comments[self.next_comment].span.start < pos
        {
            self.flush_one_comment();
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
            // Comments in `[prev_source_pos, brace)` whose source column is at
            // or before the closing `}`'s column are typed at outer indent and
            // logically document the next branch in an `if/else if/else` chain
            // (or a similar trailing construct), e.g.
            //     if cond {
            //         body
            //     // documents next branch
            //     } else if other {
            // Emit those at the outer indent so they round-trip stably; emit
            // anything at deeper column with the inner indent (the default).
            let brace_col = source_column(self.source, brace);
            while self.next_comment < self.comments.len()
                && self.comments[self.next_comment].span.start < brace
            {
                let c_start = self.comments[self.next_comment].span.start;
                let outer = !is_trailing_comment(self.source, c_start)
                    && source_column(self.source, c_start) <= brace_col
                    && self.indent > 0;
                if outer {
                    self.indent -= 1;
                    self.flush_one_comment();
                    self.indent += 1;
                } else {
                    self.flush_one_comment();
                }
            }
            if brace < self.source.len() {
                self.prev_source_pos = brace + 1;
            }
        }
    }

    /// Emit exactly one pending comment (the next one) using the comment-flush
    /// rules that `flush_comments_before` applies. Caller is responsible for
    /// any indent adjustment around the call.
    fn flush_one_comment(&mut self) {
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

    fn find_keyword_after(&self, keyword: &str, after: usize) -> usize {
        let from = after.min(self.source.len());
        self.source[from..]
            .find(keyword)
            .map_or(self.source.len(), |off| from + off)
    }

    fn flush_comments_and_separate(&mut self, pos: usize, needs_blank_line: bool) {
        let had_comments = self.next_comment;
        self.flush_comments_before(pos);
        let flushed_comments = self.next_comment > had_comments;
        if needs_blank_line && !flushed_comments && !self.output.ends_with("\n\n") {
            self.newline();
        }
    }

    // ------------------------------------------------------------------
    // Program
    // ------------------------------------------------------------------

    fn format_program(&mut self, program: &Program) {
        if let Some(doc) = &program.module_doc {
            for line in doc.split('\n') {
                if line.is_empty() {
                    self.write("//!\n");
                } else {
                    self.write("//! ");
                    self.write(line);
                    self.write("\n");
                }
            }
            if !program.items.is_empty() {
                self.write("\n");
            }
        }
        for (i, item) in program.items.iter().enumerate() {
            self.flush_comments_and_separate(item.1.start, i > 0);
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
            Item::Function(decl) => self.format_fn(decl, span_end),
            Item::ExternBlock(decl) => self.format_extern_block(decl, span_end),
            Item::Actor(decl) => self.format_actor(decl, span_end),
            Item::Supervisor(decl) => self.format_supervisor(decl),
            Item::Machine(decl) => self.format_machine(decl, span_end),
            Item::Record(decl) => self.format_record(decl),
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
            // Whole-module alias (`import path as alias;`). The parser only
            // accepts this for the whole-module form, so it never coexists
            // with a `::{ }` / `::*` spec.
            if let Some(alias) = &decl.module_alias {
                self.write(" as ");
                self.write(alias);
            }
        }
        self.write(";\n");
    }

    fn format_const(&mut self, decl: &ConstDecl) {
        self.write_outer_doc(decl.doc_comment.as_ref());
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
        self.write_outer_doc(decl.doc_comment.as_ref());
        self.write_indent();
        self.write("type ");
        self.write(&decl.name);
        self.write(" = ");
        self.format_type_expr(&decl.ty.0);
        self.write(";\n");
    }

    fn format_record(&mut self, decl: &RecordDecl) {
        self.write_outer_doc(decl.doc_comment.as_ref());
        self.write_indent();
        self.write_visibility(decl.visibility);
        self.write("record ");
        self.write(&decl.name);
        self.format_opt_type_params(decl.type_params.as_ref());
        self.format_opt_where_clause(decl.where_clause.as_ref());
        match &decl.kind {
            RecordKind::Named(fields) => {
                self.write(" {\n");
                self.indent += 1;
                for (i, field) in fields.iter().enumerate() {
                    self.write_outer_doc(field.doc_comment.as_ref());
                    self.write_indent();
                    self.write(&field.name);
                    self.write(": ");
                    self.format_type_expr(&field.ty.0);
                    if i + 1 < fields.len() {
                        self.write(",");
                    }
                    self.write("\n");
                }
                self.indent -= 1;
                self.write_indent();
                self.write("}\n");
            }
            RecordKind::Tuple(field_types) => {
                self.write("(");
                for (i, (ty, _)) in field_types.iter().enumerate() {
                    self.format_type_expr(ty);
                    if i + 1 < field_types.len() {
                        self.write(", ");
                    }
                }
                self.write(");\n");
            }
        }
    }

    fn format_type_decl(&mut self, decl: &TypeDecl, _span_end: usize) {
        if let Some(wire) = &decl.wire {
            self.format_wire_type_decl(decl, wire);
            return;
        }
        self.write_outer_doc(decl.doc_comment.as_ref());
        match decl.resource_marker {
            crate::ast::ResourceMarker::None => {}
            crate::ast::ResourceMarker::Resource => {
                self.write_indent();
                self.write("#[resource]\n");
            }
            crate::ast::ResourceMarker::Linear => {
                self.write_indent();
                self.write("#[linear]\n");
            }
        }
        if decl.is_opaque {
            self.write_indent();
            self.write("#[opaque]\n");
        }
        self.write_indent();
        self.write_visibility(decl.visibility);
        if decl.is_indirect {
            self.write("indirect ");
        }
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
                TypeBodyItem::Field {
                    name,
                    ty,
                    doc_comment,
                    span,
                    ..
                } => {
                    // flush inline comments that appear before this field
                    self.flush_comments_before(span.start);
                    // position at item start so the blank-line heuristic in
                    // the trailing-comment flush below counts only newlines
                    // between the field name and any trailing comment
                    self.prev_source_pos = span.start;
                    self.write_outer_doc(doc_comment.as_ref());
                    self.write_indent();
                    self.write(name);
                    self.write(": ");
                    self.format_type_expr(&ty.0);
                    self.write(";");
                    self.newline();
                    // flush any trailing comment on this line; span.end is the
                    // first token of the next item (or closing brace), so any
                    // comment between content and span.end is captured here
                    self.flush_comments_before(span.end);
                    self.prev_source_pos = span.end;
                }
                TypeBodyItem::Variant(v) => {
                    // flush inline comments that appear before this variant
                    self.flush_comments_before(v.span.start);
                    // position at item start so the blank-line heuristic in
                    // the trailing-comment flush below counts only newlines
                    // between the variant name and any trailing comment
                    self.prev_source_pos = v.span.start;
                    self.format_variant(v, true);
                    // flush any trailing comment on this line; v.span.end is
                    // the first token of the next item (or closing brace), so
                    // any comment between content and v.span.end is captured
                    self.flush_comments_before(v.span.end);
                    self.prev_source_pos = v.span.end;
                }
                TypeBodyItem::Method(f) => {
                    let pos = if self.has_comments() {
                        self.find_keyword_after(&format!("fn {}", f.name), self.prev_source_pos)
                    } else {
                        usize::MAX
                    };
                    self.flush_comments_before(pos);
                    let has_consuming_self =
                        decl.consuming_methods.iter().any(|name| name == &f.name);
                    self.format_type_body_method(f, self.source.len(), has_consuming_self);
                }
            }
        }
        self.indent -= 1;
        self.writeln("}");
    }

    fn format_type_body_method(
        &mut self,
        decl: &FnDecl,
        span_end: usize,
        has_consuming_self: bool,
    ) {
        self.write_outer_doc(decl.doc_comment.as_ref());
        self.format_attributes(&decl.attributes);
        self.write_indent();
        if decl.is_async {
            self.write("async ");
        }
        if decl.is_generator {
            self.write("gen ");
        }
        self.write("fn ");
        self.write(&decl.name);
        self.format_opt_type_params(decl.type_params.as_ref());
        self.write("(");
        if has_consuming_self {
            self.write("consuming self");
            if !decl.params.is_empty() {
                self.write(", ");
            }
        }
        self.format_params(&decl.params);
        self.write(")");
        if let Some(ret) = &decl.return_type {
            self.write(" -> ");
            self.format_type_expr(&ret.0);
        }
        self.format_opt_where_clause(decl.where_clause.as_ref());
        self.write(" ");
        self.format_block(&decl.body, span_end);
        self.newline();
    }

    fn format_wire_type_decl(&mut self, decl: &TypeDecl, wire: &WireMetadata) {
        // Emit type-level naming attributes
        self.format_naming_attr("json", wire.json_case);
        self.format_naming_attr("yaml", wire.yaml_case);
        self.write_indent();
        if wire.version.is_some() || wire.min_version.is_some() {
            self.write("#[wire(");
            let mut first = true;
            if let Some(v) = wire.version {
                write!(self.output, "version = {v}").unwrap();
                first = false;
            }
            if let Some(v) = wire.min_version {
                if !first {
                    self.write(", ");
                }
                write!(self.output, "min_version = {v}").unwrap();
            }
            self.write(")]\n");
        } else {
            self.write("#[wire]\n");
        }
        self.write_indent();
        self.write_visibility(decl.visibility);
        match decl.kind {
            TypeDeclKind::Struct => self.write("type "),
            TypeDeclKind::Enum => self.write("enum "),
        }
        self.write(&decl.name);
        self.write(" {\n");
        self.indent += 1;
        match decl.kind {
            TypeDeclKind::Struct => {
                for (i, item) in decl.body.iter().enumerate() {
                    if let TypeBodyItem::Field { name, ty, .. } = item {
                        self.write_indent();
                        self.write(name);
                        self.write(": ");
                        self.format_type_expr(&ty.0);
                        // Emit wire field metadata
                        if let Some(meta) = wire.field_meta.get(i) {
                            self.write(" @");
                            self.write(&meta.field_number.to_string());
                            self.format_wire_field_modifiers(
                                meta.is_optional,
                                meta.is_deprecated,
                                meta.is_repeated,
                                meta.since,
                                meta.json_name.as_deref(),
                                meta.yaml_name.as_deref(),
                            );
                        }
                        self.write(",");
                        self.newline();
                    }
                }
                // Emit reserved field numbers
                if !wire.reserved_numbers.is_empty() {
                    self.write_indent();
                    self.write("reserved ");
                    for (i, n) in wire.reserved_numbers.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.write("@");
                        self.write(&n.to_string());
                    }
                    self.write(";\n");
                }
            }
            TypeDeclKind::Enum => {
                // Variant bodies are tagged by variant index, not by per-field
                // `@N`; reserved tags do not apply.  Delegate to the regular
                // variant formatter to handle unit / tuple / struct payloads.
                for item in &decl.body {
                    if let TypeBodyItem::Variant(v) = item {
                        self.format_variant(v, true);
                    }
                }
            }
        }
        self.indent -= 1;
        self.writeln("}");
    }

    fn format_naming_attr(&mut self, attr_name: &str, case: Option<NamingCase>) {
        if let Some(case) = case {
            self.write_indent();
            let s = case.as_str();
            let needs_quotes = s.contains('-');
            self.write("#[");
            self.write(attr_name);
            self.write("(");
            if needs_quotes {
                self.write("\"");
            }
            self.write(s);
            if needs_quotes {
                self.write("\"");
            }
            self.write(")]\n");
        }
    }

    fn format_wire_field_modifiers(
        &mut self,
        is_optional: bool,
        is_deprecated: bool,
        is_repeated: bool,
        since: Option<u32>,
        json_name: Option<&str>,
        yaml_name: Option<&str>,
    ) {
        if is_optional {
            self.write(" optional");
        }
        if is_deprecated {
            self.write(" deprecated");
        }
        if is_repeated {
            self.write(" repeated");
        }
        if let Some(version) = since {
            self.write(" since ");
            self.write(&version.to_string());
        }
        if let Some(name) = json_name {
            self.write(" json(\"");
            self.write(name);
            self.write("\")");
        }
        if let Some(name) = yaml_name {
            self.write(" yaml(\"");
            self.write(name);
            self.write("\")");
        }
    }

    fn format_variant(&mut self, v: &VariantDecl, trailing_semicolon: bool) {
        self.write_outer_doc(v.doc_comment.as_ref());
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
        self.write_outer_doc(decl.doc_comment.as_ref());
        if let Some(key) = &decl.lang_item {
            self.write_indent();
            self.write(&format!("#[lang_item(\"{key}\")]\n"));
        }
        self.write_indent();
        self.write_visibility(decl.visibility);
        self.write("trait ");
        self.write(&decl.name);
        self.format_opt_type_params(decl.type_params.as_ref());
        if let Some(supers) = &decl.super_traits {
            self.write(": ");
            self.format_trait_bound_list(supers);
        }
        self.write(" {\n");
        self.indent += 1;
        for (i, item) in decl.items.iter().enumerate() {
            match item {
                TraitItem::Method(m) => {
                    let pos = if self.has_comments() {
                        self.find_keyword_after(&format!("fn {}", m.name), self.prev_source_pos)
                    } else {
                        usize::MAX
                    };
                    self.flush_comments_and_separate(pos, i > 0);
                    self.format_trait_method(m);
                }
                TraitItem::AssociatedType {
                    name,
                    bounds,
                    default,
                    ..
                } => {
                    let pos = if self.has_comments() {
                        self.find_keyword_after(&format!("type {name}"), self.prev_source_pos)
                    } else {
                        usize::MAX
                    };
                    self.flush_comments_and_separate(pos, i > 0);
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
        self.write_outer_doc(m.doc_comment.as_ref());
        if let Some(key) = &m.lang_item {
            self.write_indent();
            self.write(&format!("#[lang_item(\"{key}\")]\n"));
        }
        self.write_indent();
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

    fn format_extern_block(&mut self, decl: &ExternBlock, span_end: usize) {
        self.write_indent();
        self.write("extern \"");
        self.write(&decl.abi);
        self.write("\" {\n");
        self.indent += 1;
        for f in &decl.functions {
            // flush inline comments that appear before this fn
            self.flush_comments_before(f.span.start);
            // position at fn start so the blank-line heuristic in the
            // trailing-comment flush below counts only newlines between
            // the fn declaration and any trailing comment on its line
            self.prev_source_pos = f.span.start;
            self.format_extern_fn(f);
            // flush any trailing comment on this fn's line; f.span.end
            // is the first byte after the trailing `;`, so any same-line
            // comment falls in the range [f.span.start, f.span.end)
            self.flush_comments_before(f.span.end);
            self.prev_source_pos = f.span.end;
        }
        if self.has_comments() {
            self.flush_block_end_comments(span_end);
        }
        self.indent -= 1;
        self.writeln("}");
    }

    fn format_extern_fn(&mut self, f: &ExternFnDecl) {
        self.format_attributes(&f.attributes);
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

    #[expect(clippy::too_many_lines, reason = "actor formatting has many sections")]
    fn format_actor(&mut self, decl: &ActorDecl, span_end: usize) {
        self.write_outer_doc(decl.doc_comment.as_ref());
        if let Some(bytes) = decl.max_heap_bytes {
            self.write_indent();
            self.write(&format!("#[max_heap({bytes})]\n"));
        }
        self.write_indent();
        self.write_visibility(decl.visibility);
        self.write("actor ");
        self.write(&decl.name);
        if !decl.type_params.is_empty() {
            self.write("<");
            let mut first = true;
            for param in &decl.type_params {
                if !first {
                    self.write(", ");
                }
                first = false;
                self.write(&param.name);
                if !param.bounds.is_empty() {
                    self.write(": ");
                    self.format_trait_bound_list(&param.bounds);
                }
            }
            self.write(">");
        }
        if let Some(supers) = &decl.super_traits {
            self.write(": ");
            self.format_trait_bound_list(supers);
        }
        self.write(" {\n");
        self.indent += 1;
        let mut has_body_item = false;

        for field in &decl.fields {
            if self.has_comments() {
                let kw = if field.is_mutable { "var" } else { "let" };
                let pos =
                    self.find_keyword_after(&format!("{kw} {}", field.name), self.prev_source_pos);
                self.flush_comments_before(pos);
            }
            self.format_field_decl(field);
            has_body_item = true;
        }

        if let Some(cap) = &decl.mailbox_capacity {
            if has_body_item {
                self.newline();
            }
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
            has_body_item = true;
        }

        if let Some(init) = &decl.init {
            if self.has_comments() {
                let pos = self.find_keyword_after("init(", self.prev_source_pos);
                self.flush_comments_before(pos);
            } else if has_body_item {
                self.newline();
            }
            self.format_actor_init(init, span_end);
            has_body_item = true;
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
            } else if has_body_item {
                self.newline();
            }
            self.format_receive_fn(recv, span_end);
            has_body_item = true;
        }

        for method in &decl.methods {
            if self.has_comments() {
                let pos =
                    self.find_keyword_after(&format!("fn {}", method.name), self.prev_source_pos);
                self.flush_comments_before(pos);
            } else if has_body_item {
                self.newline();
            }
            self.format_fn(method, span_end);
            has_body_item = true;
        }

        if self.has_comments() {
            self.flush_block_end_comments(span_end);
        }
        self.indent -= 1;
        self.writeln("}");
    }

    #[expect(clippy::too_many_lines, reason = "machine formatting has many clauses")]
    fn format_machine(&mut self, decl: &MachineDecl, span_end: usize) {
        self.write_indent();
        self.write_visibility(decl.visibility);
        self.write("machine ");
        self.write(&decl.name);
        if !decl.type_params.is_empty() || !decl.const_params.is_empty() {
            self.write("<");
            let mut first = true;
            for param in &decl.type_params {
                if !first {
                    self.write(", ");
                }
                first = false;
                self.write(&param.name);
                if !param.bounds.is_empty() {
                    self.write(": ");
                    self.format_trait_bound_list(&param.bounds);
                }
            }
            for param in &decl.const_params {
                if !first {
                    self.write(", ");
                }
                first = false;
                self.write("const ");
                self.write(&param.name);
                self.write(": ");
                match param.ty {
                    crate::ast::ConstParamTy::Usize => self.write("usize"),
                }
                if let Some(default) = param.default {
                    self.write(" = ");
                    self.write(&default.to_string());
                }
            }
            self.write(">");
        }
        self.format_opt_where_clause(decl.where_clause.as_ref());
        self.write(" {\n");
        self.indent += 1;

        // `events { … }` header — the input-event vocabulary.
        let mut emitted_section = false;
        if !decl.events.is_empty() {
            self.write_indent();
            self.write("events {\n");
            self.indent += 1;
            for event in &decl.events {
                self.write_indent();
                self.write(&event.name);
                self.format_machine_field_list(&event.fields);
                self.write("\n");
            }
            self.indent -= 1;
            self.write_indent();
            self.write("}\n");
            emitted_section = true;
        }

        // `emits { … }` Mealy-output manifest (optional).
        if !decl.emits.is_empty() {
            self.newline();
            self.write_indent();
            self.write("emits {\n");
            self.indent += 1;
            for name in &decl.emits {
                self.write_indent();
                self.write(name);
                self.write(";\n");
            }
            self.indent -= 1;
            self.write_indent();
            self.write("}\n");
            emitted_section = true;
        }

        // Composite-group membership: substates owned by a composite are
        // re-emitted inside their `state Composite { … }` block (driven by the
        // side-table), not as flat top-level states.
        let composite_members: std::collections::HashSet<&str> = decl
            .composite_groups
            .iter()
            .flat_map(|g| g.members.iter().map(String::as_str))
            .collect();

        if !decl.states.is_empty() {
            if emitted_section {
                self.newline();
            }
            for state in &decl.states {
                if composite_members.contains(state.name.as_str()) {
                    continue;
                }
                self.format_machine_leaf_state(state);
            }
            emitted_section = true;
        }

        // Composite blocks (depth-1) reconstructed from the grouping side-table.
        for group in &decl.composite_groups {
            self.newline();
            self.format_machine_composite(decl, group, span_end);
            emitted_section = true;
        }

        // Top-level transitions, excluding those that belong to a composite's
        // parent-rule block (those are re-emitted inside the composite).
        let parent_rule_keys: std::collections::HashSet<(String, String, String)> = decl
            .composite_groups
            .iter()
            .flat_map(|g| {
                g.members.iter().flat_map(move |m| {
                    g.parent_transitions
                        .iter()
                        .map(move |pt| (m.clone(), pt.event_name.clone(), pt.target_state.clone()))
                })
            })
            .collect();

        let top_transitions: Vec<&MachineTransition> = decl
            .transitions
            .iter()
            .filter(|t| {
                !parent_rule_keys.contains(&(
                    t.source_state.clone(),
                    t.event_name.clone(),
                    t.target_state.clone(),
                ))
            })
            .collect();

        if !top_transitions.is_empty() {
            if emitted_section {
                self.newline();
            }
            for transition in &top_transitions {
                self.write_indent();
                self.format_machine_transition(transition, span_end);
                self.newline();
            }
            emitted_section = true;
        }

        if decl.has_default {
            if emitted_section {
                self.newline();
            }
            self.write_indent();
            self.write("default { state }\n");
        }

        self.indent -= 1;
        self.writeln("}");
    }

    /// Emit `{ name: Type; … }` after an event/state name, or `;` when empty.
    fn format_machine_field_list(&mut self, fields: &[(String, Spanned<TypeExpr>)]) {
        if fields.is_empty() {
            self.write(";");
        } else {
            self.write(" { ");
            for (i, (name, ty)) in fields.iter().enumerate() {
                if i > 0 {
                    self.write(" ");
                }
                self.write(name);
                self.write(": ");
                self.format_type_expr(&ty.0);
                self.write(";");
            }
            self.write(" }");
        }
    }

    /// Emit one leaf `state` declaration (fields + entry/exit).
    fn format_machine_leaf_state(&mut self, state: &MachineState) {
        self.write_indent();
        self.write("state ");
        self.write(&state.name);
        let has_entry_exit = state.entry.is_some() || state.exit.is_some();
        if has_entry_exit {
            self.write(" {\n");
            self.indent += 1;
            for (name, ty) in &state.fields {
                self.write_indent();
                self.write(name);
                self.write(": ");
                self.format_type_expr(&ty.0);
                self.write(";\n");
            }
            if let Some(entry) = &state.entry {
                self.write_indent();
                self.write("entry ");
                self.format_block(entry, self.source.len());
                self.newline();
            }
            if let Some(exit) = &state.exit {
                self.write_indent();
                self.write("exit ");
                self.format_block(exit, self.source.len());
                self.newline();
            }
            self.indent -= 1;
            self.write_indent();
            self.write("}\n");
        } else if !state.fields.is_empty() {
            self.write(" {");
            for (name, ty) in &state.fields {
                self.write(" ");
                self.write(name);
                self.write(": ");
                self.format_type_expr(&ty.0);
                self.write(";");
            }
            self.write(" }\n");
        } else {
            self.write(";\n");
        }
    }

    /// Emit one machine transition head + body in the `=>` / `reenter` surface.
    /// Caller writes the leading indent.
    fn format_machine_transition(&mut self, transition: &MachineTransition, span_end: usize) {
        self.write("on ");
        self.write(&transition.event_name);
        // Re-emit the `on E(a, b):` head binding from the side-list. The
        // parser splices a `let a = event.a;` prelude into the body for
        // lowering; we strip that prelude below so it does not double up.
        if !transition.event_bindings.is_empty() {
            self.write("(");
            for (i, name) in transition.event_bindings.iter().enumerate() {
                if i > 0 {
                    self.write(", ");
                }
                self.write(name);
            }
            self.write(")");
        }
        self.write(": ");
        self.write(&transition.source_state);
        self.write(" => ");
        self.write(&transition.target_state);
        if transition.reenter {
            self.write(" reenter");
        }
        if let Some(guard) = &transition.guard {
            self.write(" when ");
            self.format_expr(&guard.0);
        }
        // Re-emit the AUTHORED body: strip the composite entry/exit hook
        // prelude (D2/D3 splices, counted by `composite_prelude_len`) first,
        // then the head-binding `let a = event.a;` prelude. Both are parser
        // desugar artifacts the formatter must not echo, or re-parsing would
        // double-apply them.
        let hook_stripped;
        let after_hooks = if transition.composite_prelude_len == 0 {
            &transition.body.0
        } else {
            hook_stripped = Self::strip_leading_block_stmts(
                &transition.body.0,
                transition.composite_prelude_len,
            );
            &hook_stripped
        };
        let stripped_body;
        let body_expr = if transition.event_bindings.is_empty() {
            after_hooks
        } else {
            stripped_body =
                Self::strip_event_binding_prelude(after_hooks, &transition.event_bindings);
            &stripped_body
        };
        let is_implicit_body = matches!(body_expr,
            Expr::Identifier(name) if name == &transition.target_state);
        if is_implicit_body {
            self.write(";");
        } else if let Expr::StructInit { name, fields, .. } = body_expr {
            if name == &transition.target_state {
                self.write(" { ");
                for (i, (fname, fval)) in fields.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.write(fname);
                    self.write(": ");
                    self.format_expr(&fval.0);
                }
                self.write(" }");
            } else {
                self.write(" { ");
                self.format_expr(body_expr);
                self.write(" }");
            }
        } else if let Expr::Block(block) = body_expr {
            self.write(" ");
            self.format_block(block, span_end);
        } else {
            self.write(" { ");
            self.format_expr(body_expr);
            self.write(" }");
        }
    }

    /// Drop the first `count` statements of a block body (the composite
    /// entry/exit hook splice prelude). If only a tail remains, surface it
    /// directly so the body collapses back to its authored shorthand.
    fn strip_leading_block_stmts(body: &Expr, count: usize) -> Expr {
        let Expr::Block(block) = body else {
            return body.clone();
        };
        if count == 0 || count > block.stmts.len() {
            return body.clone();
        }
        let remaining: Vec<Spanned<Stmt>> = block.stmts[count..].to_vec();
        if remaining.is_empty() {
            if let Some(tail) = &block.trailing_expr {
                return tail.0.clone();
            }
        }
        Expr::Block(Block {
            stmts: remaining,
            trailing_expr: block.trailing_expr.clone(),
        })
    }

    /// Strip the leading `let <binding> = event.<binding>;` prelude statements
    /// the parser splices in for an `on E(bindings): …` head binding, so the
    /// formatter can re-emit the head form without the desugar. If, after
    /// stripping, only a tail expression remains, that tail becomes the body
    /// (collapsing a one-line `on E(x): S => T { Body }` back to its head form).
    fn strip_event_binding_prelude(body: &Expr, bindings: &[String]) -> Expr {
        let Expr::Block(block) = body else {
            return body.clone();
        };
        // Count how many leading statements are the synthesized prelude lets.
        let mut skip = 0;
        for (stmt, _) in &block.stmts {
            if skip >= bindings.len() {
                break;
            }
            let Stmt::Let {
                pattern: (Pattern::Identifier(name), _),
                value: Some((Expr::FieldAccess { object, field }, _)),
                ..
            } = stmt
            else {
                break;
            };
            let is_event_field = matches!(&object.0, Expr::Identifier(o) if o == "event");
            if is_event_field && name == field && bindings.contains(name) {
                skip += 1;
            } else {
                break;
            }
        }
        if skip == 0 {
            return body.clone();
        }
        let remaining: Vec<Spanned<Stmt>> = block.stmts[skip..].to_vec();
        // If only a tail expression remains, surface it directly so the body
        // collapses to the canonical struct-init / identifier shorthand.
        if remaining.is_empty() {
            if let Some(tail) = &block.trailing_expr {
                return tail.0.clone();
            }
        }
        Expr::Block(Block {
            stmts: remaining,
            trailing_expr: block.trailing_expr.clone(),
        })
    }

    /// Re-emit a composite `state Composite { … }` block from the grouping
    /// side-table: composite fields, entry/exit, `initial`-marked substates,
    /// then the parent-level transitions authored inside the block.
    fn format_machine_composite(
        &mut self,
        decl: &MachineDecl,
        group: &crate::ast::CompositeGroup,
        span_end: usize,
    ) {
        self.write_indent();
        self.write("state ");
        self.write(&group.name);
        self.write(" {\n");
        self.indent += 1;

        for (name, ty) in &group.fields {
            self.write_indent();
            self.write(name);
            self.write(": ");
            self.format_type_expr(&ty.0);
            self.write(";\n");
        }
        if let Some(entry) = &group.entry {
            self.write_indent();
            self.write("entry ");
            self.format_block(entry, self.source.len());
            self.newline();
        }
        if let Some(exit) = &group.exit {
            self.write_indent();
            self.write("exit ");
            self.format_block(exit, self.source.len());
            self.newline();
        }

        for member_name in &group.members {
            let Some(state) = decl.states.iter().find(|s| &s.name == member_name) else {
                continue;
            };
            self.write_indent();
            if &group.initial == member_name {
                self.write("initial ");
            }
            // Substate fields exclude the composite-owned shared fields (which
            // are emitted on the composite, not stamped here).
            let own_fields: Vec<&(String, Spanned<TypeExpr>)> = state
                .fields
                .iter()
                .filter(|(fname, _)| !group.fields.iter().any(|(gn, _)| gn == fname))
                .collect();
            self.format_machine_substate(&state.name, &own_fields, state, span_end);
        }

        for pt in &group.parent_transitions {
            self.write_indent();
            self.format_machine_transition(pt, span_end);
            self.newline();
        }

        self.indent -= 1;
        self.write_indent();
        self.write("}\n");
    }

    /// Emit a substate declaration inside a composite block. The `initial`
    /// modifier (when present) is already written by the caller.
    fn format_machine_substate(
        &mut self,
        name: &str,
        own_fields: &[&(String, Spanned<TypeExpr>)],
        state: &MachineState,
        _span_end: usize,
    ) {
        self.write("state ");
        self.write(name);
        let has_entry_exit = state.entry.is_some() || state.exit.is_some();
        if has_entry_exit {
            self.write(" {\n");
            self.indent += 1;
            for (fname, ty) in own_fields {
                self.write_indent();
                self.write(fname);
                self.write(": ");
                self.format_type_expr(&ty.0);
                self.write(";\n");
            }
            if let Some(entry) = &state.entry {
                self.write_indent();
                self.write("entry ");
                self.format_block(entry, self.source.len());
                self.newline();
            }
            if let Some(exit) = &state.exit {
                self.write_indent();
                self.write("exit ");
                self.format_block(exit, self.source.len());
                self.newline();
            }
            self.indent -= 1;
            self.write_indent();
            self.write("}\n");
        } else if !own_fields.is_empty() {
            self.write(" {");
            for (fname, ty) in own_fields {
                self.write(" ");
                self.write(fname);
                self.write(": ");
                self.format_type_expr(&ty.0);
                self.write(";");
            }
            self.write(" }\n");
        } else {
            self.write(";\n");
        }
    }

    fn format_field_decl(&mut self, f: &FieldDecl) {
        self.write_outer_doc(f.doc_comment.as_ref());
        self.write_indent();
        self.write(if f.is_mutable { "var " } else { "let " });
        self.write(&f.name);
        self.write(": ");
        self.format_type_expr(&f.ty.0);
        if let Some(default) = &f.default {
            self.write(" = ");
            self.format_expr(&default.0);
        }
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

    fn format_attributes(&mut self, attrs: &[Attribute]) {
        for attr in attrs {
            self.write_indent();
            self.write("#[");
            self.write(&attr.name);
            if !attr.args.is_empty() {
                self.write("(");
                for (i, arg) in attr.args.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    match arg {
                        AttributeArg::Positional(s) => {
                            // If the value contains characters that are not valid
                            // in a bare identifier (e.g. `.` in `"math.sqrt"`),
                            // re-quote it as a string literal so the output round-trips.
                            let needs_quotes = s.chars().any(|c| !c.is_alphanumeric() && c != '_');
                            if needs_quotes {
                                self.write("\"");
                                self.write(s);
                                self.write("\"");
                            } else {
                                self.write(s);
                            }
                        }
                        AttributeArg::KeyValue { key, value } => {
                            self.write(key);
                            self.write(" = ");
                            self.write(value);
                        }
                        AttributeArg::Duration(ns) => self.write(&format_duration_ns(*ns)),
                    }
                }
                self.write(")");
            }
            self.write("]\n");
        }
    }

    fn format_receive_fn(&mut self, recv: &ReceiveFnDecl, scope_end: usize) {
        self.write_outer_doc(recv.doc_comment.as_ref());
        self.format_attributes(&recv.attributes);
        self.write_indent();

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
        // Emit the config-param clause when present: `supervisor App(config: T)`.
        // Without this, `hew fmt` silently drops the param and breaks all
        // config.field references in the body — a fail-open on the dev-tool surface.
        if !decl.params.is_empty() {
            self.write("(");
            self.format_params(&decl.params);
            self.write(")");
        }
        self.write(" {\n");
        self.indent += 1;

        // Always write `strategy:` explicitly. When the declaration omitted it,
        // the formatter materializes the default (`one_for_one`) so the restart
        // contract is never silently defaulted at the surface.
        self.write_indent();
        self.write("strategy: ");
        match decl.strategy.unwrap_or(SupervisorStrategy::OneForOne) {
            SupervisorStrategy::OneForOne => self.write("one_for_one"),
            SupervisorStrategy::OneForAll => self.write("one_for_all"),
            SupervisorStrategy::RestForOne => self.write("rest_for_one"),
            SupervisorStrategy::SimpleOneForOne => self.write("simple_one_for_one"),
        }
        self.write(";\n");
        if let Some(intensity) = &decl.intensity {
            self.write_indent();
            self.write("intensity: ");
            self.write(&intensity.restarts.to_string());
            self.write(" within ");
            self.write(&intensity.window);
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
        // `pool` vs `child` is load-bearing — a pool is a dynamic
        // simple_one_for_one child, not a static one. The old formatter always
        // wrote `child`, silently dropping pool-ness; preserve it here.
        self.write(if spec.is_pool { "pool " } else { "child " });
        self.write(&spec.name);
        self.write(": ");
        self.write(&spec.actor_type);
        if !spec.args.is_empty() {
            self.write("(");
            self.comma_sep(&spec.args, |f, (field_name, arg)| {
                f.write(field_name);
                f.write(": ");
                f.format_expr(&arg.0);
            });
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
        if let Some(shutdown) = &spec.shutdown {
            self.write(" shutdown: ");
            match shutdown {
                ShutdownDirective::Timeout(d) => self.write(d),
                ShutdownDirective::BrutalKill => self.write("brutal_kill"),
                ShutdownDirective::Infinity => self.write("infinity"),
            }
        }
        // `wired_to:` was silently dropped by the old formatter — preserve it.
        // HashMap iteration order is non-deterministic, so emit keys sorted to
        // keep the round-trip stable and idempotent.
        if let Some(wired_to) = &spec.wired_to {
            if !wired_to.is_empty() {
                let mut entries: Vec<(&String, &String)> = wired_to.iter().collect();
                entries.sort_by(|a, b| a.0.cmp(b.0));
                self.write(" wired_to: { ");
                self.comma_sep(&entries, |f, (key, sibling)| {
                    f.write(key);
                    f.write(": ");
                    f.write(sibling);
                });
                self.write(" }");
            }
        }
        self.write(";\n");
    }

    fn format_fn(&mut self, decl: &FnDecl, span_end: usize) {
        self.write_outer_doc(decl.doc_comment.as_ref());
        self.format_attributes(&decl.attributes);
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
        // An inherent-impl `consuming self` receiver is materialised as the
        // leading `self: Self` parameter; emit the `consuming self` spelling and
        // skip that synthetic first parameter, mirroring the type-body formatter.
        if decl.consumes_self {
            self.format_opt_type_params(decl.type_params.as_ref());
            self.write("(consuming self");
            let rest = decl.params.get(1..).unwrap_or(&[]);
            if !rest.is_empty() {
                self.write(", ");
            }
            self.format_params(rest);
            self.write(")");
            if let Some(ret) = decl.return_type.as_ref() {
                self.write(" -> ");
                self.format_type_expr(&ret.0);
            }
            self.format_opt_where_clause(decl.where_clause.as_ref());
        } else {
            self.format_fn_signature(
                decl.type_params.as_ref(),
                &decl.params,
                decl.return_type.as_ref(),
                decl.where_clause.as_ref(),
            );
        }
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
                self.write(")");
                if !matches!(return_type.0, TypeExpr::Tuple(ref elems) if elems.is_empty()) {
                    self.write(" -> ");
                    self.format_type_expr(&return_type.0);
                }
            }
            TypeExpr::Pointer {
                is_mutable,
                pointee,
            } => {
                self.write(if *is_mutable { "*mut " } else { "*const " });
                self.format_type_expr(&pointee.0);
            }
            TypeExpr::Borrow(inner) => {
                self.write("&");
                self.format_type_expr(&inner.0);
            }
            TypeExpr::TraitObject(bounds) => {
                self.write("dyn ");
                if bounds.len() == 1 {
                    self.format_trait_bound(&bounds[0]);
                } else {
                    self.write("(");
                    for (i, bound) in bounds.iter().enumerate() {
                        if i > 0 {
                            self.write(" + ");
                        }
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
        if bound.type_args.is_some() || !bound.assoc_type_bindings.is_empty() {
            self.write("<");
            let mut needs_comma = false;
            if let Some(args) = &bound.type_args {
                for arg in args {
                    if needs_comma {
                        self.write(", ");
                    }
                    self.format_type_expr(&arg.0);
                    needs_comma = true;
                }
            }
            for binding in &bound.assoc_type_bindings {
                if needs_comma {
                    self.write(", ");
                }
                self.write(&binding.name);
                self.write(" = ");
                self.format_type_expr(&binding.ty.0);
                needs_comma = true;
            }
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
            if p.name == "self"
                && matches!(
                    &p.ty.0,
                    TypeExpr::Named {
                        name,
                        type_args: None,
                    } if name == "Self"
                )
            {
                if p.is_mutable {
                    f.write("var ");
                }
                f.write("self");
                return;
            }
            // `consume` precedes `var` in the surface grammar
            // (`fn sink(consume var c: Conn)`); emit it first so the
            // formatted output reparses to the same ownership disposition.
            // The `self` fast-path above never reaches here, and `consume
            // self` is rejected by the parser, so an affine receiver is
            // never mis-printed with a `consume` modifier.
            if p.is_consume {
                f.write("consume ");
            }
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
        // Empty-block fast path: render `{}` on a single line when the block
        // has no statements, no trailing expression, and no comments fall
        // inside the block's source range. Multi-line `{\n}` is semantically
        // identical but stretches absolute byte offsets of every later
        // expression; that has surfaced a latent cross-module span-collision
        // bug in the type-checker → codegen `(start, end)` lookup tables
        // (see PR feat/actor-edges-phase-alpha-cow-envelopes / quic_service
        // smoke regression). Keeping empty blocks single-line also matches
        // common formatter conventions (rustfmt, gofmt) and is round-trip
        // stable.
        if block.stmts.is_empty() && block.trailing_expr.is_none() {
            let bytes = self.source.as_bytes();
            let from = self.prev_source_pos.min(bytes.len());
            // Some callers pass `self.source.len()` (no tighter bound), and
            // the parser has been observed to produce inverted spans for
            // empty function bodies (item.span.end < item.span.start). Be
            // defensive: clamp to source length and fall back to source end
            // when the supplied scope is degenerate.
            let to_raw = scope_end.min(bytes.len());
            let to = if to_raw > from { to_raw } else { bytes.len() };
            if from < to {
                // Locate this block's opening `{` and matching `}` in source.
                let open = self.source[from..to].find('{').map(|o| from + o);
                if let Some(open_idx) = open {
                    let close_idx = find_block_close(self.source, open_idx + 1, to);
                    // find_block_close returns `to` when no `}` was found in
                    // range; only collapse when we actually located the brace.
                    if close_idx < to {
                        // Check whether ANY comment falls inside the block's
                        // source range. We must scan forward from
                        // `next_comment` because some comments may have been
                        // logically classified earlier even though they sit
                        // inside this block in source (e.g. doc comments
                        // attached to earlier siblings can land here).
                        let mut comment_inside = false;
                        let mut i = self.next_comment;
                        while i < self.comments.len() {
                            let cs = self.comments[i].span.start;
                            if cs >= close_idx {
                                break;
                            }
                            if cs >= open_idx {
                                comment_inside = true;
                                break;
                            }
                            i += 1;
                        }
                        if !comment_inside {
                            self.write("{}");
                            self.prev_source_pos = close_idx + 1;
                            return;
                        }
                    }
                }
            }
        }
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

    fn format_gen_block(&mut self, body: &Block) {
        self.write("gen ");
        if self.can_format_gen_block_inline(body) {
            self.format_gen_block_inline(body);
        } else {
            self.format_block(body, self.source.len());
        }
    }

    fn can_format_gen_block_inline(&self, body: &Block) -> bool {
        let item_count = body.stmts.len() + usize::from(body.trailing_expr.is_some());
        item_count <= 2
            && !self.next_block_has_comments()
            && body
                .stmts
                .iter()
                .all(|(stmt, _)| Self::can_format_stmt_inline(stmt))
            && body
                .trailing_expr
                .as_deref()
                .is_none_or(|(expr, _)| Self::can_format_expr_inline(expr))
    }

    fn next_block_has_comments(&self) -> bool {
        if self.comments.is_empty() {
            return false;
        }
        let Some((open, close)) = self.next_block_bounds() else {
            return false;
        };
        self.comments[self.next_comment..]
            .iter()
            .any(|comment| comment.span.start > open && comment.span.start < close)
    }

    fn next_block_bounds(&self) -> Option<(usize, usize)> {
        let from = self.prev_source_pos.min(self.source.len());
        let open = self.source[from..].find('{').map(|off| from + off)?;
        let close = find_block_close(self.source, open + 1, self.source.len());
        (close < self.source.len()).then_some((open, close))
    }

    fn can_format_stmt_inline(stmt: &Stmt) -> bool {
        match stmt {
            Stmt::Let { value, .. } | Stmt::Var { value, .. } => value
                .as_ref()
                .is_none_or(|(expr, _)| Self::can_format_expr_inline(expr)),
            Stmt::Assign { target, value, .. } => {
                Self::can_format_expr_inline(&target.0) && Self::can_format_expr_inline(&value.0)
            }
            Stmt::Break { value, .. } | Stmt::Return(value) => value
                .as_ref()
                .is_none_or(|(expr, _)| Self::can_format_expr_inline(expr)),
            Stmt::Continue { .. } => true,
            Stmt::Defer(expr) => Self::can_format_expr_inline(&expr.0),
            Stmt::Expression(expr) => Self::can_format_expr_inline(&expr.0),
            Stmt::If { .. }
            | Stmt::IfLet { .. }
            | Stmt::Match { .. }
            | Stmt::Loop { .. }
            | Stmt::For { .. }
            | Stmt::While { .. }
            | Stmt::WhileLet { .. } => false,
        }
    }

    fn can_format_expr_inline(expr: &Expr) -> bool {
        match expr {
            Expr::Literal(_)
            | Expr::Identifier(_)
            | Expr::RegexLiteral(_)
            | Expr::ByteStringLiteral(_)
            | Expr::ByteArrayLiteral(_)
            | Expr::This
            | Expr::Yield(None)
            | Expr::Return(None) => true,
            Expr::Tuple(exprs) | Expr::Array(exprs) => exprs
                .iter()
                .all(|(expr, _)| Self::can_format_expr_inline(expr)),
            Expr::MapLiteral { entries } => entries.iter().all(|(key, value)| {
                Self::can_format_expr_inline(&key.0) && Self::can_format_expr_inline(&value.0)
            }),
            Expr::ArrayRepeat { value, count } => {
                Self::can_format_expr_inline(&value.0) && Self::can_format_expr_inline(&count.0)
            }
            Expr::Unary { operand, .. }
            | Expr::Clone(operand)
            | Expr::PostfixTry(operand)
            | Expr::Await(operand)
            | Expr::AwaitRestart(operand)
            | Expr::Yield(Some(operand))
            | Expr::Return(Some(operand)) => Self::can_format_expr_inline(&operand.0),
            Expr::Binary { left, right, .. }
            | Expr::Is {
                lhs: left,
                rhs: right,
            } => Self::can_format_expr_inline(&left.0) && Self::can_format_expr_inline(&right.0),
            Expr::Call { function, args, .. } => {
                Self::can_format_expr_inline(&function.0)
                    && args
                        .iter()
                        .all(|arg| Self::can_format_expr_inline(&arg.expr().0))
            }
            Expr::MethodCall { receiver, args, .. } => {
                Self::can_format_expr_inline(&receiver.0)
                    && args
                        .iter()
                        .all(|arg| Self::can_format_expr_inline(&arg.expr().0))
            }
            Expr::FieldAccess { object, .. } => Self::can_format_expr_inline(&object.0),
            Expr::Index { object, index } => {
                Self::can_format_expr_inline(&object.0) && Self::can_format_expr_inline(&index.0)
            }
            Expr::Cast { expr, .. } => Self::can_format_expr_inline(&expr.0),
            Expr::Range { start, end, .. } => {
                start
                    .as_ref()
                    .is_none_or(|expr| Self::can_format_expr_inline(&expr.0))
                    && end
                        .as_ref()
                        .is_none_or(|expr| Self::can_format_expr_inline(&expr.0))
            }
            Expr::StructInit { fields, base, .. } => {
                fields
                    .iter()
                    .all(|(_, expr)| Self::can_format_expr_inline(&expr.0))
                    && base
                        .as_ref()
                        .is_none_or(|expr| Self::can_format_expr_inline(&expr.0))
            }
            Expr::InterpolatedString(parts) => parts.iter().all(|part| match part {
                StringPart::Literal(_) => true,
                StringPart::Expr((expr, _)) => Self::can_format_expr_inline(expr),
            }),
            Expr::Block(_)
            | Expr::If { .. }
            | Expr::IfLet { .. }
            | Expr::Match { .. }
            | Expr::Lambda { .. }
            | Expr::Spawn { .. }
            | Expr::SpawnLambdaActor { .. }
            | Expr::Scope { .. }
            | Expr::ForkChild { .. }
            | Expr::ForkBlock { .. }
            | Expr::ScopeDeadline { .. }
            | Expr::Select { .. }
            | Expr::Join(_)
            | Expr::Timeout { .. }
            | Expr::UnsafeBlock(_)
            | Expr::MachineEmit { .. }
            | Expr::GenBlock { .. } => false,
        }
    }

    fn format_gen_block_inline(&mut self, body: &Block) {
        self.write("{");
        if !body.stmts.is_empty() || body.trailing_expr.is_some() {
            self.write(" ");
            for (stmt, _) in &body.stmts {
                self.format_stmt_inline(stmt);
                self.write(" ");
            }
            if let Some((expr, _)) = body.trailing_expr.as_deref() {
                self.format_expr(expr);
                self.write(" ");
            }
        }
        self.write("}");
    }

    fn format_stmt_inline(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let {
                pattern,
                ty,
                value,
                else_block,
            } => {
                self.write("let ");
                self.format_pattern(&pattern.0);
                if let Some(ty) = ty {
                    self.write(": ");
                    self.format_type_expr(&ty.0);
                }
                if let Some((expr, _)) = value {
                    self.write(" = ");
                    self.format_expr(expr);
                }
                if let Some(else_block) = else_block {
                    self.write(" else ");
                    self.format_block(else_block, self.source.len());
                }
                self.write(";");
            }
            Stmt::Var { name, ty, value } => {
                self.write("var ");
                self.write(name);
                if let Some(ty) = ty {
                    self.write(": ");
                    self.format_type_expr(&ty.0);
                }
                if let Some((expr, _)) = value {
                    self.write(" = ");
                    self.format_expr(expr);
                }
                self.write(";");
            }
            Stmt::Assign { target, op, value } => {
                self.format_expr(&target.0);
                if let Some(op) = op {
                    self.write(" ");
                    self.write(compound_assign_op_str(*op));
                    self.write(" ");
                } else {
                    self.write(" = ");
                }
                self.format_expr(&value.0);
                self.write(";");
            }
            Stmt::Break { label, value } => {
                self.write("break");
                if let Some(label) = label {
                    self.write(" @");
                    self.write(label);
                }
                if let Some((expr, _)) = value {
                    self.write(" ");
                    self.format_expr(expr);
                }
                self.write(";");
            }
            Stmt::Continue { label } => {
                self.write("continue");
                if let Some(label) = label {
                    self.write(" @");
                    self.write(label);
                }
                self.write(";");
            }
            Stmt::Return(value) => {
                self.write("return");
                if let Some((expr, _)) = value {
                    self.write(" ");
                    self.format_expr(expr);
                }
                self.write(";");
            }
            Stmt::Defer(expr) => {
                self.write("defer ");
                self.format_expr(&expr.0);
                self.write(";");
            }
            Stmt::Expression(expr) => {
                self.format_expr(&expr.0);
                self.write(";");
            }
            Stmt::If { .. }
            | Stmt::IfLet { .. }
            | Stmt::Match { .. }
            | Stmt::Loop { .. }
            | Stmt::For { .. }
            | Stmt::While { .. }
            | Stmt::WhileLet { .. } => {
                unreachable!("non-inline statement reached gen block inline formatter")
            }
        }
    }

    #[expect(clippy::too_many_lines, reason = "match on all Stmt variants")]
    fn format_stmt(&mut self, stmt: &Stmt) {
        match stmt {
            Stmt::Let {
                pattern,
                ty,
                value,
                else_block,
            } => {
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
                if let Some(else_block) = else_block {
                    self.write(" else ");
                    self.format_block(else_block, self.source.len());
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
                self.format_cond_expr(&condition.0);
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
                self.format_cond_expr(&scrutinee.0);
                self.write(" {\n");
                self.indent += 1;
                // advance past the opening `{` so the blank-line heuristic in
                // flush_comments_before counts only lines within the block;
                // search from the scrutinee's end to the first arm's pattern start
                if self.has_comments() {
                    let search_from = scrutinee.1.end.min(self.source.len());
                    let search_to = arms
                        .first()
                        .map_or(self.source.len(), |a| a.pattern.1.start);
                    if let Some(off) = self.source[search_from..search_to].find('{') {
                        self.prev_source_pos = search_from + off + 1;
                    }
                }
                for arm in arms {
                    self.flush_comments_before(arm.pattern.1.start);
                    self.format_match_arm(arm);
                    self.prev_source_pos = arm.body.1.end;
                }
                if self.has_comments() {
                    let close =
                        find_block_close(self.source, self.prev_source_pos, self.source.len());
                    self.flush_comments_before(close);
                    if close < self.source.len() {
                        self.prev_source_pos = close + 1;
                    }
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
                self.format_cond_expr(&condition.0);
                self.write(" ");
                self.format_block(body, self.source.len());
                self.newline();
            }
            Stmt::WhileLet {
                label,
                pattern,
                expr,
                body,
            } => {
                self.write_indent();
                if let Some(label) = label {
                    self.write("@");
                    self.write(label);
                    self.write(": ");
                }
                self.write("while let ");
                self.format_pattern(&pattern.0);
                self.write(" = ");
                self.format_expr(&expr.0);
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
                // Block expressions already end with `}`, no semicolon.
                if matches!(expr.0, Expr::Block(_)) {
                    self.write("\n");
                } else {
                    self.write(";\n");
                }
            }
        }
    }

    fn format_else_block(&mut self, eb: &ElseBlock) {
        if eb.is_if {
            if let Some(if_stmt) = &eb.if_stmt {
                self.write(" else ");
                // Print the inner `if` without leading indent (it's on the same line).
                // Only `Stmt::If` and `Stmt::IfLet` are valid here by parser construction
                // (see parser.rs: `else if`/`else if let` branches). Every other `Stmt`
                // variant is enumerated explicitly so that adding a new control-flow
                // statement forces a design decision at this dispatch site instead of
                // silently falling through to `format_stmt` and re-indenting.
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
                    Stmt::IfLet {
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
                    Stmt::Let { .. }
                    | Stmt::Var { .. }
                    | Stmt::Assign { .. }
                    | Stmt::Match { .. }
                    | Stmt::Loop { .. }
                    | Stmt::For { .. }
                    | Stmt::While { .. }
                    | Stmt::WhileLet { .. }
                    | Stmt::Break { .. }
                    | Stmt::Continue { .. }
                    | Stmt::Return(_)
                    | Stmt::Defer(_)
                    | Stmt::Expression(_) => {
                        // Parser invariant: `else if`/`else if let` are the only shapes
                        // that populate `ElseBlock::if_stmt`. Reaching this arm means the
                        // AST was hand-built or corrupted; fall back to `format_stmt` so
                        // we still produce valid source, and log via `debug_assert!` so
                        // tests surface the invariant break.
                        debug_assert!(
                            false,
                            "format_else_block: non-if stmt in else-if position: {:?}",
                            &if_stmt.0
                        );
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

    /// Return `true` when `expr` must be parenthesised before a postfix operator
    /// (`.field`, `.method()`, `[index]`, `?`).
    ///
    /// Postfix operators bind at the highest precedence in the Pratt loop.  Any
    /// expression at a strictly lower precedence level — binary ops, prefix unary
    /// ops, range, `is`, `await`, `clone` — must be wrapped in parens so that
    /// re-parsing produces the same AST.  Delimited forms (literals, identifiers,
    /// tuples, arrays, blocks, calls, other postfix) are already unambiguous.
    ///
    /// A `StructInit` receiver also needs parens: in `if`/`while` condition or
    /// `match` scrutinee position a bare struct literal is suppressed (the `{`
    /// opens the block), so `(Foo { a: 1 }).b` must keep its parens to re-parse
    /// as a field access. Emitting them unconditionally is always correct — in
    /// non-condition position the parens are merely redundant, not wrong.
    fn needs_receiver_parens(expr: &Expr) -> bool {
        matches!(
            expr,
            Expr::Binary { .. }
                | Expr::Unary { .. }
                | Expr::Clone(_)
                | Expr::Range { .. }
                | Expr::Is { .. }
                | Expr::Await(_)
                | Expr::AwaitRestart(_)
                | Expr::StructInit { .. }
        )
    }

    /// Format an expression that appears as the object/receiver of a postfix
    /// operation (`.`, `[]`, `?`), adding parentheses when required for correct
    /// re-parsing.
    fn format_receiver(&mut self, expr: &Expr) {
        if Self::needs_receiver_parens(expr) {
            self.write("(");
            self.format_expr(expr);
            self.write(")");
        } else {
            self.format_expr(expr);
        }
    }

    /// Format an `if`/`while` condition or `match` scrutinee, wrapping a direct
    /// struct literal in parens so it re-parses correctly.
    ///
    /// In condition/scrutinee position the parser suppresses bare struct
    /// literals (the `{` opens the block / loop body / match arms), so a struct
    /// literal that is the direct condition must keep its parens: `if (Foo {
    /// a: 1 }) {}` and `match (Foo { a: 1 }) { … }` would otherwise format to
    /// `if Foo { a: 1 } {}` / `match Foo { a: 1 } { … }`, which re-parse with
    /// the struct body swallowing the block. Only a direct `StructInit` needs
    /// this — every other condition form (binary ops, calls, blocks, nested
    /// `if`/`match`) re-parses unchanged.
    fn format_cond_expr(&mut self, expr: &Expr) {
        if matches!(expr, Expr::StructInit { .. }) {
            self.write("(");
            self.format_expr(expr);
            self.write(")");
        } else {
            self.format_expr(expr);
        }
    }

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
                    UnaryOp::RawDeref => self.write("*"),
                }
                let needs_parens = matches!(operand.0, Expr::Binary { .. });
                if needs_parens {
                    self.write("(");
                }
                self.format_expr(&operand.0);
                if needs_parens {
                    self.write(")");
                }
            }
            Expr::Literal(lit) => self.format_literal(lit),
            Expr::Identifier(name) => self.write(name),
            Expr::Clone(operand) => {
                self.write("clone ");
                self.format_expr(&operand.0);
            }
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
                self.format_cond_expr(&condition.0);
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
                self.format_cond_expr(&scrutinee.0);
                self.write(" {\n");
                self.indent += 1;
                // advance past the opening `{` so the blank-line heuristic in
                // flush_comments_before counts only lines within the block;
                // search from the scrutinee's end to the first arm's pattern start
                if self.has_comments() {
                    let search_from = scrutinee.1.end.min(self.source.len());
                    let search_to = arms
                        .first()
                        .map_or(self.source.len(), |a| a.pattern.1.start);
                    if let Some(off) = self.source[search_from..search_to].find('{') {
                        self.prev_source_pos = search_from + off + 1;
                    }
                }
                for arm in arms {
                    self.flush_comments_before(arm.pattern.1.start);
                    self.format_match_arm(arm);
                    self.prev_source_pos = arm.body.1.end;
                }
                if self.has_comments() {
                    let close =
                        find_block_close(self.source, self.prev_source_pos, self.source.len());
                    self.flush_comments_before(close);
                    if close < self.source.len() {
                        self.prev_source_pos = close + 1;
                    }
                }
                self.indent -= 1;
                self.write_indent();
                self.write("}");
            }
            Expr::Lambda {
                is_move,
                type_params,
                params,
                return_type,
                body,
            } => {
                if *is_move {
                    self.write("move ");
                }
                if type_params.is_some() {
                    self.format_opt_type_params(type_params.as_ref());
                    self.write("(");
                    self.format_lambda_params(params);
                    self.write(")");
                    if let Some(ret) = return_type {
                        self.write(" -> ");
                        self.format_type_expr(&ret.0);
                    }
                    self.write(" => ");
                    self.format_expr(&body.0);
                } else {
                    self.write("|");
                    self.format_lambda_params(params);
                    self.write("|");
                    if let Some(ret) = return_type {
                        self.write(" -> ");
                        self.format_type_expr(&ret.0);
                        self.write(" ");
                        if matches!(body.0, Expr::Block(_)) {
                            self.format_expr(&body.0);
                        } else {
                            self.write("{ ");
                            self.format_expr(&body.0);
                            self.write(" }");
                        }
                    } else {
                        self.write(" ");
                        self.format_expr(&body.0);
                    }
                }
            }
            Expr::Spawn {
                target,
                type_args,
                args,
            } => {
                self.write("spawn ");
                self.format_expr(&target.0);
                if !type_args.is_empty() {
                    self.write("<");
                    self.comma_sep(type_args, |f, (te, _)| {
                        f.format_type_expr(te);
                    });
                    self.write(">");
                }
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
                self.write("actor ");
                if *is_move {
                    self.write("move ");
                }
                self.write("|");
                self.format_lambda_params(params);
                self.write("|");
                if let Some(ret) = return_type {
                    self.write(" -> ");
                    self.format_type_expr(&ret.0);
                }
                self.write(" ");
                self.format_expr(&body.0);
            }
            Expr::Scope { body } => {
                self.write("scope ");
                self.format_block(body, self.source.len());
            }
            Expr::ForkChild { binding, expr } => {
                self.write("fork ");
                if let Some(name) = binding {
                    self.write(name);
                    self.write(" = ");
                }
                self.format_expr(&expr.0);
            }
            Expr::ForkBlock { body } => {
                self.write("fork ");
                self.format_block(body, self.source.len());
            }
            Expr::ScopeDeadline { duration, body } => {
                self.write("after(");
                self.format_expr(&duration.0);
                self.write(") ");
                self.format_block(body, self.source.len());
            }
            Expr::InterpolatedString(parts) => {
                self.write("f\"");
                for part in parts {
                    match part {
                        StringPart::Literal(s) => {
                            self.write(&escape_fstring_literal(s));
                        }
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
                // Add parens around a FieldAccess callee so that `(rec.f)(args)` — a
                // function call through a fn-typed field — formats as `(rec.f)(args)` and
                // re-parses as `Call { FieldAccess }`, not as a `MethodCall`. The two forms
                // are syntactically distinct (different AST nodes, different checker paths);
                // normalising them would break the round-trip property.
                let needs_callee_parens =
                    matches!(function.0, Expr::Lambda { .. } | Expr::FieldAccess { .. });
                if needs_callee_parens {
                    self.write("(");
                }
                self.format_expr(&function.0);
                if needs_callee_parens {
                    self.write(")");
                }
                if let Some(type_args) = type_args {
                    self.write("<");
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
                self.format_receiver(&receiver.0);
                self.write(".");
                self.write(method);
                self.write("(");
                self.format_call_args(args);
                self.write(")");
            }
            Expr::StructInit {
                name,
                fields,
                type_args,
                base,
            } => {
                self.write(name);
                if let Some(type_args) = type_args {
                    self.write("<");
                    self.comma_sep(type_args, |f, ta| f.format_type_expr(&ta.0));
                    self.write(">");
                }
                self.write(" { ");
                self.comma_sep(fields, |f, (fname, fval)| {
                    f.write(fname);
                    f.write(": ");
                    f.format_expr(&fval.0);
                });
                if let Some(base_expr) = base {
                    if !fields.is_empty() {
                        self.write(", ");
                    }
                    self.write("..");
                    self.format_expr(&base_expr.0);
                }
                self.write(" }");
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
            Expr::UnsafeBlock(block) => {
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
            Expr::Return(val) => {
                self.write("return");
                if let Some(val) = val {
                    self.write(" ");
                    self.format_expr(&val.0);
                }
            }
            Expr::This => self.write("this"),
            Expr::FieldAccess { object, field } => {
                self.format_receiver(&object.0);
                self.write(".");
                self.write(field);
            }
            Expr::Index { object, index } => {
                self.format_receiver(&object.0);
                self.write("[");
                self.format_expr(&index.0);
                self.write("]");
            }
            Expr::Cast { expr, ty } => {
                self.format_receiver(&expr.0);
                self.write(" as ");
                self.format_type_expr(&ty.0);
            }
            Expr::PostfixTry(expr) => {
                self.format_receiver(&expr.0);
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
            Expr::AwaitRestart(inner) => {
                self.write("await_restart ");
                self.format_expr(&inner.0);
            }
            Expr::RegexLiteral(pattern) => {
                self.write("re\"");
                self.write(&escape_regex_pattern(pattern));
                self.write("\"");
            }
            Expr::ByteStringLiteral(data) => {
                self.write("b\"");
                self.write(&escape_byte_string(data));
                self.write("\"");
            }
            Expr::ByteArrayLiteral(data) => {
                self.write("bytes [");
                for (i, &b) in data.iter().enumerate() {
                    if i > 0 {
                        self.write(", ");
                    }
                    self.write(&format!("0x{b:02x}"));
                }
                self.write("]");
            }
            Expr::MapLiteral { entries } => {
                self.write("{");
                self.comma_sep(entries, |f, (key, value)| {
                    f.format_expr(&key.0);
                    f.write(": ");
                    f.format_expr(&value.0);
                });
                self.write("}");
            }
            Expr::Is { lhs, rhs } => {
                // Precedence 9 — same as `==`/`!=`; no parens needed around operands
                // at lower precedence, but we do need them for nested `is`.
                self.format_expr_prec(&lhs.0, 9, false);
                self.write(" is ");
                self.format_expr_prec(&rhs.0, 9, true);
            }
            Expr::MachineEmit { event_name, fields } => {
                self.write("emit ");
                self.write(event_name);
                if fields.is_empty() {
                    self.write(" {}");
                } else {
                    self.write(" { ");
                    for (i, (name, val)) in fields.iter().enumerate() {
                        if i > 0 {
                            self.write(", ");
                        }
                        self.write(name);
                        self.write(": ");
                        self.format_expr(&val.0);
                    }
                    self.write(" }");
                }
            }
            Expr::GenBlock { body } => {
                self.format_gen_block(body);
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
                IntRadix::Hex => {
                    let _ = write!(self.output, "0x{value:X}");
                }
                IntRadix::Octal => {
                    let _ = write!(self.output, "0o{value:o}");
                }
                IntRadix::Binary => {
                    let _ = write!(self.output, "0b{value:b}");
                }
                IntRadix::Decimal => {
                    let _ = write!(self.output, "{value}");
                }
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
                let mut buf = String::new();
                escape_char_literal(*c, &mut buf);
                self.write("'");
                self.write(&buf);
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
            Pattern::RecordShorthand { fields } => {
                self.write("{ ");
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
            Pattern::Regex { pattern, .. } => {
                self.write("re\"");
                self.write(&escape_regex_pattern(pattern));
                self.write("\"");
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
        BinaryOp::WrappingAdd => "&+",
        BinaryOp::WrappingSub => "&-",
        BinaryOp::WrappingMul => "&*",
    }
}

/// Map binary operators to their precedence level (higher = tighter binding).
/// Values match the Pratt parser's binding powers in parser.rs.
fn binop_precedence(op: BinaryOp) -> u8 {
    match op {
        BinaryOp::Or => 3,
        BinaryOp::BitOr => 5,
        BinaryOp::BitXor => 7,
        BinaryOp::BitAnd => 9,
        BinaryOp::And => 11,
        BinaryOp::Equal | BinaryOp::NotEqual => 13,
        BinaryOp::Less | BinaryOp::LessEqual | BinaryOp::Greater | BinaryOp::GreaterEqual => 15,
        BinaryOp::Range | BinaryOp::RangeInclusive => 17,
        BinaryOp::Shl | BinaryOp::Shr => 19,
        // Wrapping add/sub at same precedence as plain add/sub
        BinaryOp::Add | BinaryOp::Subtract | BinaryOp::WrappingAdd | BinaryOp::WrappingSub => 21,
        // Wrapping mul at same precedence as plain mul
        BinaryOp::Multiply | BinaryOp::Divide | BinaryOp::Modulo | BinaryOp::WrappingMul => 23,
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

/// Returns `true` for non-ASCII Unicode scalars that should be emitted raw
/// (readable, unambiguous in source), following the rustfmt/gofmt convention.
///
/// The escape-vs-preserve decision is derived from the authoritative Unicode
/// `General_Category` so the predicate stays correct as new codepoints are
/// assigned and avoids the drift-prone hand-maintained range list it replaces.
///
/// Escapes (returns `false`) when the character belongs to any of:
/// - **Cc** (Control): U+0080–U+009F C1 controls and the ASCII controls
///   (the caller already gates `is_control()` before reaching here, but
///   `finl_unicode::is_control()` is the normative source for the Cc set).
/// - **Cf** (Format): soft hyphen, zero-width spaces, all `BiDi` controls
///   (U+202A–U+202E, U+2066–U+2069), deprecated format/shaping controls
///   (U+206A–U+206F), Mongolian vowel separator (U+180E), word joiners,
///   interlinear annotations, BOM, tag block — the Trojan-Source/confusable
///   class.
/// - **Co** (Private Use): U+E000–U+F8FF BMP PUA; supplementary PUA planes.
/// - **Cn** (Unassigned): no defined rendering — fail-closed.
/// - **Zl** (Line Separator): U+2028.
/// - **Zp** (Paragraph Separator): U+2029.
///
/// Also escapes (returns `false`) scalars that `General_Category` alone does
/// not catch but which still render as nothing or as a blank in source:
/// - **`Default_Ignorable_Code_Point`**: variation selectors (U+FE00–FE0F,
///   U+E0100–E01EF), the combining grapheme joiner (U+034F), Hangul fillers
///   (U+115F–1160), the tag block (U+E0000–E0FFF), and the rest of the
///   property. Many of these have a *readable* `General_Category` (Mn/Lo/So)
///   yet emit nothing, so a category-only predicate leaks them raw.
/// - **Blank-looking scalars**: BRAILLE PATTERN BLANK (U+2800) and the Hangul
///   fillers, which occupy width but show nothing — a confusable/space-spoof
///   vector.
///
/// Preserves (returns `true`) everything else: letters (L*), marks (M*),
/// numbers (N*), punctuation (P*), symbols (S*), and space separators (Zs) —
/// including accented letters (`é`), CJK (`世`), arrows (`→`), em dashes
/// (`—`), and similar readable Unicode.
///
/// Note: Cs (Surrogate) codepoints are not valid Rust `char` values and
/// therefore never reach this function.
pub fn is_printable_non_ascii(c: char) -> bool {
    debug_assert!(!c.is_ascii(), "only call for non-ASCII chars");

    // Escape the C categories that are invisible, confusable, or unrendered.
    if c.is_control() // Cc — C1 controls (U+0080–U+009F) and ASCII controls
        || c.is_format() // Cf — all Format characters (BiDi, soft-hyphen, ZWJ, shaping, tags, …)
        || c.is_private_use() // Co — Private Use Areas
        || c.is_unassigned() // Cn — no assigned rendering, fail-closed
        || c.is_separator_line() // Zl — U+2028
        || c.is_separator_paragraph() // Zp — U+2029
        || is_default_ignorable(c) // renders as nothing despite a readable category
        || is_blank_looking(c)
    // occupies width but shows nothing (Braille blank, Hangul fillers)
    {
        return false;
    }

    true
}

/// Unicode `Default_Ignorable_Code_Point` property (from
/// `DerivedCoreProperties.txt`).  `finl_unicode` 1.4 exposes only
/// `General_Category`, so this property is encoded explicitly.  These scalars
/// are rendered as nothing by conforming renderers; several (e.g. U+FE0F
/// VARIATION SELECTOR-16, U+034F COMBINING GRAPHEME JOINER) sit in *readable*
/// categories (Mn/Lo/So) and so escape the `General_Category` gate, yet still
/// emit raw and act as confusable/Trojan-Source vectors.  Escaping them keeps
/// the formatted source visually faithful.
fn is_default_ignorable(c: char) -> bool {
    matches!(c as u32,
        0x00AD                              // SOFT HYPHEN
        | 0x034F                            // COMBINING GRAPHEME JOINER
        | 0x061C                            // ARABIC LETTER MARK
        | 0x115F..=0x1160                   // HANGUL CHOSEONG/JUNGSEONG FILLER
        | 0x17B4..=0x17B5                   // KHMER VOWEL INHERENT AQ/AA
        | 0x180B..=0x180F                   // MONGOLIAN FVS1-4 + VOWEL SEPARATOR
        | 0x200B..=0x200F                   // ZERO WIDTH SPACE … RLM
        | 0x202A..=0x202E                   // BiDi embedding/override controls
        | 0x2060..=0x206F                   // WORD JOINER … deprecated format/shaping
        | 0x3164                            // HANGUL FILLER
        | 0xFE00..=0xFE0F                   // VARIATION SELECTOR-1 … -16
        | 0xFEFF                            // ZERO WIDTH NO-BREAK SPACE / BOM
        | 0xFFA0                            // HALFWIDTH HANGUL FILLER
        | 0xFFF0..=0xFFF8                   // unassigned specials (ignorable)
        | 0x1BCA0..=0x1BCA3                 // SHORTHAND FORMAT CONTROLS
        | 0x1D173..=0x1D17A                 // MUSICAL SYMBOL BEGIN/END controls
        | 0xE0000..=0xE0FFF                 // TAGS + VARIATION SELECTORS SUPPLEMENT
    )
}

/// Scalars that occupy advance width but render as a blank box of nothing and
/// are **not** already caught by `is_default_ignorable`.  The Hangul fillers
/// (U+115F/U+1160/U+3164/U+FFA0) are blank-looking too but carry the
/// `Default_Ignorable` property, so they are handled there; the only addition
/// here is the Braille blank, whose `General_Category` is the readable `So`.
fn is_blank_looking(c: char) -> bool {
    c == '\u{2800}' // BRAILLE PATTERN BLANK
}

/// Escape a single character for use inside a double-quoted string or
/// f-string literal part.
///
/// Covers the full escape set the parser resolves:
///   `\n \t \r \\ \" \0 \xNN \u{...}`
///
/// • Named escapes for the six ASCII control chars the parser recognises by
///   name (`\n`, `\t`, `\r`, `\\`, `\"`, `\0`).
/// • `\xNN` (two lowercase hex digits) for any other non-printable ASCII byte
///   (code point < 0x20 or == 0x7F).
/// • Printable ASCII and readable non-ASCII Unicode pass through verbatim.
///   "Readable" means: NOT a control character, NOT a Format/Separator
///   Unicode category, NOT a private-use codepoint — per `is_printable_non_ascii`.
/// • `\u{HHHH}` (uppercase hex, no leading zeros beyond the minimum) for
///   invisible/confusable non-ASCII (zero-width spaces, `BiDi` overrides, C1
///   controls, etc.).
///
/// This follows the rustfmt/gofmt convention: preserve readable Unicode
/// (`é`, `→`, `世`, `—`) verbatim; escape only what is genuinely invisible
/// or confusable in source text.
///
/// `fstring_braces`: when `true`, also escape `{` and `}` as `\{` / `\}` so
/// they survive round-trip inside an f-string interpolation boundary.
fn escape_str_char(c: char, out: &mut String, fstring_braces: bool) {
    match c {
        '\\' => out.push_str("\\\\"),
        '"' => out.push_str("\\\""),
        '\n' => out.push_str("\\n"),
        '\t' => out.push_str("\\t"),
        '\r' => out.push_str("\\r"),
        '\0' => out.push_str("\\0"),
        '{' if fstring_braces => out.push_str("\\{"),
        '}' if fstring_braces => out.push_str("\\}"),
        c if c.is_ascii() => {
            let b = c as u8;
            if b < 0x20 || b == 0x7f {
                // Non-printable ASCII not covered by a named escape above.
                let _ = write!(out, "\\x{b:02x}");
            } else {
                out.push(c);
            }
        }
        c if c.is_control() => {
            // Non-ASCII control chars (C1 range U+0080–U+009F, NEL U+0085).
            let cp = c as u32;
            let _ = write!(out, "\\u{{{cp:X}}}");
        }
        c if is_printable_non_ascii(c) => {
            // Readable non-ASCII: preserve verbatim (é, →, 世, —, …).
            out.push(c);
        }
        c => {
            // Invisible or confusable non-ASCII (zero-width, BiDi, private-use, …).
            let cp = c as u32;
            let _ = write!(out, "\\u{{{cp:X}}}");
        }
    }
}

fn escape_string(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 4);
    for c in s.chars() {
        escape_str_char(c, &mut out, false);
    }
    out
}

/// Escape the literal-text parts of an f-string (`f"…"`).
///
/// The lexer has already resolved all escape sequences to their byte values
/// (e.g. `\n` → a real newline) before storing them in `StringPart::Literal`.
/// The formatter must re-escape those bytes so that re-parsing yields the
/// same value.  An f-string literal part has the same escape set as a plain
/// string *plus* `{` and `}`, which delimit interpolation holes and must be
/// written as `\{` / `\}` to survive round-trip.
fn escape_fstring_literal(s: &str) -> String {
    let mut out = String::with_capacity(s.len() + 4);
    for c in s.chars() {
        escape_str_char(c, &mut out, true);
    }
    out
}

/// Escape a regex literal pattern (`re"…"`) for re-emission.
///
/// Regex backslash sequences (`\s`, `\d`, `\u{…}`, …) are passed through
/// verbatim because the pattern reaches the engine unchanged save for the
/// delimiter escape `\"` — see `normalize_regex_literal`. A raw invisible or
/// confusable scalar that appears literally in the pattern is rewritten to a
/// `\u{…}` regex escape: the `regex` engine treats `\u{202E}` as the U+202E
/// codepoint, so the match semantics are identical while the source stays
/// visually faithful. This routes regex literals through the same
/// invisible/confusable policy as string, f-string, byte-string, and char
/// literals, closing the bypass where a raw RLO inside `re"…"` would be emitted
/// verbatim.
fn escape_regex_pattern(pattern: &str) -> String {
    let mut out = String::with_capacity(pattern.len());
    let mut chars = pattern.chars();
    while let Some(c) = chars.next() {
        if c == '\\' {
            // Preserve the regex escape (both chars) verbatim.
            out.push('\\');
            if let Some(next) = chars.next() {
                out.push(next);
            } else {
                out.push('\\');
            }
        } else if c == '"' {
            out.push_str("\\\"");
        } else if c.is_ascii() {
            out.push(c);
        } else if c.is_control() || !is_printable_non_ascii(c) {
            // Non-ASCII control or invisible/confusable scalar: emit a \u{…}
            // regex escape, which the engine maps to the same codepoint.
            let cp = c as u32;
            let _ = write!(out, "\\u{{{cp:X}}}");
        } else {
            // Readable non-ASCII (é, →, 世, …): preserve verbatim.
            out.push(c);
        }
    }
    out
}

fn escape_byte_string(data: &[u8]) -> String {
    if let Ok(s) = std::str::from_utf8(data) {
        return escape_string(s);
    }

    let mut out = String::with_capacity(data.len());
    for &b in data {
        match b {
            b'\\' => out.push_str("\\\\"),
            b'"' => out.push_str("\\\""),
            b'\n' => out.push_str("\\n"),
            b'\t' => out.push_str("\\t"),
            b'\r' => out.push_str("\\r"),
            b'\0' => out.push_str("\\0"),
            b' '..=b'!' | b'#'..=b'[' | b']'..=b'~' => out.push(b as char),
            _ => {
                let _ = write!(out, "\\x{b:02x}");
            }
        }
    }
    out
}

/// Escape a single character for use inside a single-quoted char literal
/// (`'…'`).
///
/// Shares the invisible/confusable escape policy with [`escape_str_char`]: the
/// only differences are that `'` (not `"`) is the delimiter that must be
/// backslash-escaped, and `{`/`}` need no escaping (a char literal has no
/// interpolation). Routing char literals through the same policy closes the
/// bypass where a raw U+202E (RLO) or U+FE0F (VS-16) char literal would
/// otherwise be emitted verbatim.
fn escape_char_literal(c: char, out: &mut String) {
    match c {
        '\\' => out.push_str("\\\\"),
        '\'' => out.push_str("\\'"),
        '\n' => out.push_str("\\n"),
        '\t' => out.push_str("\\t"),
        '\r' => out.push_str("\\r"),
        '\0' => out.push_str("\\0"),
        c if c.is_ascii() => {
            let b = c as u8;
            if b < 0x20 || b == 0x7f {
                let _ = write!(out, "\\x{b:02x}");
            } else {
                out.push(c);
            }
        }
        c if c.is_control() => {
            // Non-ASCII control chars (C1 range U+0080–U+009F, NEL U+0085).
            let cp = c as u32;
            let _ = write!(out, "\\u{{{cp:X}}}");
        }
        c if is_printable_non_ascii(c) => {
            // Readable non-ASCII: preserve verbatim (é, →, 世, —, …).
            out.push(c);
        }
        c => {
            // Invisible or confusable non-ASCII (zero-width, BiDi, variation
            // selectors, Default_Ignorable, …).
            let cp = c as u32;
            let _ = write!(out, "\\u{{{cp:X}}}");
        }
    }
}

// ---------------------------------------------------------------------------
// Comment extraction
// ---------------------------------------------------------------------------

#[derive(Debug, Clone)]
pub struct Comment {
    pub text: String,
    pub span: Range<usize>,
}

/// Scan source text and extract all comments with their byte positions.
///
/// When `include_doc_comments` is false, doc-comments (`///` and `//!`) are
/// skipped because the parser captures their content into AST fields
/// (`doc_comment` / `module_doc`) and the formatter re-emits them via
/// `write_outer_doc` / `format_program`.
pub fn extract_comments(source: &str, include_doc_comments: bool) -> Vec<Comment> {
    let mut comments = Vec::new();
    let bytes = source.as_bytes();
    let mut i = 0;
    while i < bytes.len() {
        match bytes[i] {
            b'/' if i + 1 < bytes.len() => match bytes[i + 1] {
                b'/' => {
                    let start = i;
                    let is_doc_comment =
                        i + 2 < bytes.len() && (bytes[i + 2] == b'/' || bytes[i + 2] == b'!');
                    i += 2;
                    while i < bytes.len() && bytes[i] != b'\n' {
                        i += 1;
                    }
                    if include_doc_comments || !is_doc_comment {
                        comments.push(Comment {
                            text: source[start..i].to_string(),
                            span: start..i,
                        });
                    }
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

/// Source column (0-based) of `pos` within its line. Used by the block-end
/// comment dedent heuristic to decide whether a comment is at outer or inner
/// indent — a comment at or before the closing `}`'s column documents the
/// next branch in an if/else-if chain (or similar) and must be re-emitted at
/// outer indent so the chain idiom round-trips.
fn source_column(source: &str, pos: usize) -> usize {
    let bytes = source.as_bytes();
    let end = pos.min(bytes.len());
    let mut i = end;
    while i > 0 && bytes[i - 1] != b'\n' {
        i -= 1;
    }
    end - i
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
enum Colour {
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

    #[test]
    fn extern_rt_block_roundtrip() {
        let src = "\
extern \"rt\" {
    fn println(s: string);
    fn print(s: string);
    fn assert(cond: bool);
}
";
        let formatted = roundtrip(src);
        assert_eq!(formatted, src);
    }

    #[test]
    fn preserves_consume_modifier_on_extern_param() {
        // `consume` pins by-move ownership on an affine boundary parameter;
        // the formatter must emit it so the surface ownership disposition
        // survives a format → reparse round-trip. Regression: the modifier
        // was silently dropped, turning a boundary `consume` into an
        // inferred borrow and breaking the corpus round-trip (RAII-2 #1295).
        let src = "\
extern \"C\" {
    fn sink(consume c: Conn) -> i32;
}
";
        let formatted = roundtrip(src);
        assert_eq!(formatted, src);
    }

    #[test]
    fn preserves_consume_before_var_param_order() {
        // Surface grammar is `consume var name: T` — `consume` precedes
        // `var`. The formatter must reproduce that order so the reparse
        // recovers both the move disposition and the mutable binding.
        let src = "\
fn drain(consume var c: Conn) -> i32 {
    0
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
        // `await` expressions round-trip independently of the function modifier.
        let src = "fn main() {\n    let x = await foo();\n}\n";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn async_gen_fn_roundtrip() {
        // `async gen fn` is the only accepted async modifier; it round-trips cleanly.
        let src = "async gen fn stream() -> i32 {\n    yield 1;\n}\n";
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
enum Colour {
    Red;
    Green;
    Blue;
}
";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn wire_enum_roundtrips() {
        let src = "\
#[wire]
enum Status {
    Pending;
    Active;
    Completed;
}
";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn wire_enum_with_json_case_roundtrips() {
        let src = "\
#[json(camelCase)]
#[wire]
enum Status {
    PendingReview;
    ActiveNow;
    Completed;
}
";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn wire_type_since_roundtrips() {
        let src = "\
#[wire]
type Msg {
    added: String @2 repeated since 3 yaml(\"added\"),
}
";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn wire_type_roundtrips_byte_stably() {
        let src = "\
#[wire]
type Msg {
    id: i64 @1,
    name: String @2 optional since 2,
    tags: Vec<String> @3 repeated,
    reserved @4;
}
";
        let formatted = roundtrip(src);
        assert!(
            !formatted.contains("struct"),
            "formatted wire type should not contain struct: {formatted}"
        );
        assert_eq!(formatted, src);
        assert_eq!(roundtrip(&formatted), formatted);
    }

    #[test]
    fn enum_inline_comments_preserved() {
        let src = "\
pub enum IoError {
    // The target path does not exist.
    NotFound(int);
    // The process lacks permission for the operation.
    PermissionDenied(int);
    Other(int); // catch-all
}
";
        assert_eq!(roundtrip_source(src), src);
    }

    #[test]
    fn struct_field_comments_preserved() {
        let src = "\
type Config {
    // The server port to bind on.
    port: int;
    host: string; // hostname or IP
}
";
        assert_eq!(roundtrip_source(src), src);
    }

    #[test]
    fn match_arm_comments_preserved() {
        let src = "\
fn classify(e: IoError) -> string {
    match e {
        // file not found
        IoError::NotFound(_) => \"missing\",
        IoError::PermissionDenied(_) => \"denied\", // access error
        _ => \"other\",
    }
}
";
        assert_eq!(roundtrip_source(src), src);
    }

    #[test]
    fn match_arm_trailing_on_last_arm() {
        let src = "\
fn classify(e: IoError) -> string {
    match e {
        IoError::NotFound(_) => \"missing\",
        IoError::Other(_) => \"error\", // catch-all trailing
    }
}
";
        assert_eq!(roundtrip_source(src), src);
    }

    #[test]
    fn match_expr_arm_comments_preserved() {
        let src = "\
fn main() -> string {
    let msg = match get_error() {
        // success path
        IoError::NotFound(_) => \"not found\",
        _ => \"error\",
    };
    msg
}
";
        assert_eq!(roundtrip_source(src), src);
    }

    #[test]
    fn empty_block_collapses_to_single_line() {
        // Regression: multi-line empty blocks (e.g. `Ok(_) => {\n        },`)
        // shift absolute byte offsets of every later expression and have
        // surfaced cross-module span-collision crashes in codegen. Empty
        // blocks must be rendered as `{}` on one line, matching the rustfmt
        // / gofmt convention.
        let src = "\
fn assert_ok(result: Result<(), int>) {
    match result {
        Ok(_) => {
        },
        Err(e) => panic(\"op failed\"),
    }
}

fn empty_fn() {
}
";
        let expected = "\
fn assert_ok(result: Result<(), int>) {
    match result {
        Ok(_) => {},
        Err(e) => panic(\"op failed\"),
    }
}

fn empty_fn() {}
";
        assert_eq!(roundtrip_source(src), expected);
        // Idempotence on the collapsed form.
        assert_eq!(roundtrip_source(expected), expected);
    }

    #[test]
    fn combined_comments_idempotent() {
        let src = "\
// module-level comment
enum Status {
    // ok variant
    Ok;
    // error variant
    Err(int); // with payload
}

type Cfg {
    // host to connect to
    host: string;
    port: int; // default 8080
}

fn handle(s: Status) -> int {
    match s {
        // success
        Status::Ok => 0,
        // failure
        Status::Err(code) => code,
    }
}
";
        let once = roundtrip_source(src);
        assert_eq!(once, src, "first pass must preserve comments");
        let twice = roundtrip_source(&once);
        assert_eq!(twice, once, "second pass must be idempotent");
    }

    #[test]
    fn preserves_comment_above_else_if_branch_header() {
        let src = "\
fn classify(x: int) -> int {
    if x == 0 {
        0
    // documents the next branch
    } else if x == 1 {
        1
    } else {
        -1
    }
}
";
        let once = roundtrip_source(src);
        assert_eq!(once, src, "first pass must preserve comment");
        let twice = roundtrip_source(&once);
        assert_eq!(twice, once, "second pass must be idempotent");
    }

    #[test]
    fn preserves_comment_above_else_branch_header() {
        let src = "\
fn classify(x: int) -> int {
    if x == 0 {
        0
    // fallback case
    } else {
        -1
    }
}
";
        let once = roundtrip_source(src);
        assert_eq!(once, src, "first pass must preserve comment");
        let twice = roundtrip_source(&once);
        assert_eq!(twice, once, "second pass must be idempotent");
    }

    #[test]
    fn preserves_long_else_if_chain_comments() {
        let src = "\
fn lookup(errno: int) -> int {
    // ENOENT
    if errno == 2 {
        1
    // EACCES
    } else if errno == 13 {
        2
    // EEXIST
    } else if errno == 17 {
        3
    } else {
        0
    }
}
";
        let once = roundtrip_source(src);
        assert_eq!(once, src, "first pass must preserve comments");
        let twice = roundtrip_source(&once);
        assert_eq!(twice, once, "second pass must be idempotent");
    }

    #[test]
    fn preserves_comment_above_match_arm_block_close() {
        let src = "\
fn handle(x: int) -> int {
    match x {
        0 => {
            1
        // documents next arm
        },
        _ => 2,
    }
}
";
        let once = roundtrip_source(src);
        assert_eq!(once, src, "first pass must preserve comment");
        let twice = roundtrip_source(&once);
        assert_eq!(twice, once, "second pass must be idempotent");
    }

    #[test]
    fn preserves_trailing_comment_on_extern_fn() {
        let src = "\
extern \"C\" {
    fn foo(x: i32) -> i32; // returns the value untouched
    fn bar(y: i32);
}
";
        let once = roundtrip_source(src);
        assert_eq!(once, src, "first pass must preserve trailing comment");
        let twice = roundtrip_source(&once);
        assert_eq!(twice, once, "second pass must be idempotent");
    }

    #[test]
    fn preserves_trailing_comment_on_last_extern_fn() {
        let src = "\
extern \"C\" {
    fn foo(x: i32) -> i32;
    fn bar(y: i32) -> i32; // last fn trailing comment
}
";
        let once = roundtrip_source(src);
        assert_eq!(
            once, src,
            "first pass must preserve trailing comment on last fn"
        );
        let twice = roundtrip_source(&once);
        assert_eq!(twice, once, "second pass must be idempotent");
    }

    #[test]
    fn preserves_leading_comment_above_extern_fn() {
        let src = "\
extern \"C\" {
    fn first(x: i32) -> i32;
    // groups the read variants
    fn read_u8(buf: i32) -> i32;
    fn read_u16(buf: i32) -> i32;
}
";
        let once = roundtrip_source(src);
        assert_eq!(once, src, "first pass must preserve leading comment");
        let twice = roundtrip_source(&once);
        assert_eq!(twice, once, "second pass must be idempotent");
    }

    #[test]
    fn preserves_internal_abi_marker_on_extern_fn() {
        let src = "\
extern \"C\" {
    fn hew_stream_last_error() -> string;
    fn hew_stream_last_errno() -> i32; // INTERNAL-ABI: OS errno from thread-local; 0 when none recorded
}
";
        let once = roundtrip_source(src);
        assert_eq!(
            once, src,
            "INTERNAL-ABI marker must remain on the fn it documents"
        );
        let twice = roundtrip_source(&once);
        assert_eq!(twice, once, "second pass must be idempotent");
    }

    #[test]
    fn scope_block_roundtrips() {
        let src = "\
fn main() {
    let value = scope {
        1
    };
}
";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn fork_child_forms_roundtrip() {
        let src = "\
fn main() {
    fork child = run();
    fork run_other();
}
";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn nested_scope_roundtrips() {
        let src = "\
fn main() {
    let value = scope {
        fork worker = run();
        fork audit();
        worker
    };
}
";
        assert_eq!(roundtrip(src), src);
    }

    // ── StructInit with explicit type args (F1 formatter coverage) ────────

    #[test]
    fn struct_init_explicit_type_arg_formats_correctly() {
        // The formatter must emit `Name<T> { ... }` when type_args is Some.
        let src = "\
type Wrapper<T> {
    value: T;
}

fn main() {
    let w = Wrapper<String> { value: \"hello\" };
}
";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn struct_init_without_type_arg_formats_unchanged() {
        // The formatter must emit `Name { ... }` (no `<>`) when type_args is None.
        let src = "\
type Wrapper<T> {
    value: T;
}

fn main() {
    let w = Wrapper { value: \"hello\" };
}
";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn extern_symbol_attribute_round_trips_in_extern_block() {
        // `#[extern_symbol("…")]` on an `extern "C"` fn
        // must survive a parse/format round-trip. The string contains `{T}`
        // which is non-identifier-shaped, so the formatter re-quotes it.
        let src = "\
extern \"C\" {
    #[extern_symbol(\"hew_vec_push_{T}\")]
    fn hew_vec_push(v: ptr, x: ptr);
}
";
        assert_eq!(roundtrip(src), src);
    }

    #[test]
    fn extern_symbol_attribute_round_trips_on_impl_method() {
        let src = "\
impl<T> Vec<T> {
    #[extern_symbol(\"hew_vec_push_{T}\")]
    fn push(self, x: T) {
    }
}
";
        assert_eq!(roundtrip(src), src);
    }

    // ── escape_str_char: escape-sequence faithfulness (P0) ──────────────────

    /// Named escape sequences that the lexer resolved to their byte values must
    /// re-emit as the named escape, NOT as a raw byte.
    #[test]
    fn escape_string_roundtrips_named_escapes() {
        // The lexer stores a literal newline byte; escape_string must emit \n.
        assert_eq!(escape_string("\n"), "\\n");
        assert_eq!(escape_string("\t"), "\\t");
        assert_eq!(escape_string("\r"), "\\r");
        assert_eq!(escape_string("\\"), "\\\\");
        assert_eq!(escape_string("\""), "\\\"");
        assert_eq!(escape_string("\0"), "\\0");
    }

    /// \xNN escape for non-printable ASCII outside the named set.
    #[test]
    fn escape_string_roundtrips_xnn_control() {
        // U+0001 SOH — non-printable ASCII, not in named set.
        assert_eq!(escape_string("\x01"), "\\x01");
        // U+001F US — highest C0 below space, not in named set.
        assert_eq!(escape_string("\x1f"), "\\x1f");
        // U+007F DEL.
        assert_eq!(escape_string("\x7f"), "\\x7f");
    }

    /// \u{...} escape for non-ASCII invisible/confusable characters.
    #[test]
    fn escape_string_escapes_invisible_unicode() {
        // U+200B zero-width space (Format Cf) — must be escaped.
        assert_eq!(escape_string("\u{200B}"), "\\u{200B}");
        // U+202E right-to-left override (BiDi Cf) — must be escaped.
        assert_eq!(escape_string("\u{202E}"), "\\u{202E}");
        // U+FEFF byte order mark (Cf) — must be escaped.
        assert_eq!(escape_string("\u{FEFF}"), "\\u{FEFF}");
        // U+2028 line separator (Zl) — must be escaped.
        assert_eq!(escape_string("\u{2028}"), "\\u{2028}");
        // U+2029 paragraph separator (Zp) — must be escaped.
        assert_eq!(escape_string("\u{2029}"), "\\u{2029}");
        // U+00AD soft hyphen (Cf) — must be escaped.
        assert_eq!(escape_string("\u{AD}"), "\\u{AD}");
    }

    // ── escape_str_char: preserve-readable-Unicode (A176) ───────────────────

    /// Readable non-ASCII Unicode must pass through verbatim.
    #[test]
    fn escape_string_preserves_readable_unicode() {
        // Accented letter.
        assert_eq!(escape_string("é"), "é");
        // Arrow symbol.
        assert_eq!(escape_string("→"), "→");
        // CJK ideograph.
        assert_eq!(escape_string("世"), "世");
        // Em dash.
        assert_eq!(escape_string("—"), "—");
        // Mixed: readable Unicode adjacent to ASCII.
        assert_eq!(escape_string("café"), "café");
        assert_eq!(escape_string("résumé"), "résumé");
    }

    /// `is_printable_non_ascii` classifies readable chars as printable.
    #[test]
    fn is_printable_non_ascii_true_for_readable() {
        assert!(is_printable_non_ascii('é'), "accented letter");
        assert!(is_printable_non_ascii('→'), "arrow");
        assert!(is_printable_non_ascii('世'), "CJK");
        assert!(is_printable_non_ascii('—'), "em dash");
        assert!(is_printable_non_ascii('π'), "greek letter");
        assert!(is_printable_non_ascii('£'), "pound sign");
        assert!(is_printable_non_ascii('©'), "copyright sign");
    }

    /// `is_printable_non_ascii` classifies invisible/confusable chars as non-printable.
    #[test]
    fn is_printable_non_ascii_false_for_invisible() {
        assert!(!is_printable_non_ascii('\u{200B}'), "zero-width space");
        assert!(!is_printable_non_ascii('\u{200D}'), "zero-width joiner");
        assert!(!is_printable_non_ascii('\u{202E}'), "rtl override");
        assert!(!is_printable_non_ascii('\u{FEFF}'), "BOM");
        assert!(!is_printable_non_ascii('\u{2028}'), "line separator");
        assert!(!is_printable_non_ascii('\u{2029}'), "paragraph separator");
        assert!(!is_printable_non_ascii('\u{00AD}'), "soft hyphen");
        assert!(!is_printable_non_ascii('\u{E000}'), "private use start");
        assert!(!is_printable_non_ascii('\u{FFF0}'), "specials block");
        // Previously-missed Cf chars (deprecated format/shaping controls).
        assert!(
            !is_printable_non_ascii('\u{206A}'),
            "U+206A INHIBIT SYMMETRIC SWAPPING (Cf)"
        );
        assert!(
            !is_printable_non_ascii('\u{206B}'),
            "U+206B ACTIVATE SYMMETRIC SWAPPING (Cf)"
        );
        assert!(
            !is_printable_non_ascii('\u{206C}'),
            "U+206C INHIBIT ARABIC FORM SHAPING (Cf)"
        );
        assert!(
            !is_printable_non_ascii('\u{206D}'),
            "U+206D ACTIVATE ARABIC FORM SHAPING (Cf)"
        );
        assert!(
            !is_printable_non_ascii('\u{206E}'),
            "U+206E NATIONAL DIGIT SHAPES (Cf)"
        );
        assert!(
            !is_printable_non_ascii('\u{206F}'),
            "U+206F NOMINAL DIGIT SHAPES (Cf)"
        );
        assert!(
            !is_printable_non_ascii('\u{180E}'),
            "U+180E MONGOLIAN VOWEL SEPARATOR (Cf)"
        );
    }

    // ── property tests: category-derived escape/preserve classification ──────

    /// Every non-ASCII char in `General_Category` Cf (Format) must be escaped.
    /// Cf contains the Trojan-Source / confusable class — all must be escaped.
    #[test]
    fn property_all_cf_chars_escape() {
        let leaked: Vec<char> = (0x80u32..=0x0010_FFFFu32)
            .filter_map(char::from_u32)
            .filter(|c| c.is_format() && is_printable_non_ascii(*c))
            .collect();
        assert!(
            leaked.is_empty(),
            "Cf chars incorrectly marked printable: {:?}",
            leaked
                .iter()
                .map(|c| format!("U+{:04X}", *c as u32))
                .collect::<Vec<_>>()
        );
    }

    /// Every non-ASCII char in `General_Category` Cc (Control) must be escaped.
    #[test]
    fn property_all_cc_chars_escape() {
        let leaked: Vec<char> = (0x80u32..=0x0010_FFFFu32)
            .filter_map(char::from_u32)
            .filter(|c| c.is_control() && is_printable_non_ascii(*c))
            .collect();
        assert!(
            leaked.is_empty(),
            "Cc chars incorrectly marked printable: {:?}",
            leaked
                .iter()
                .map(|c| format!("U+{:04X}", *c as u32))
                .collect::<Vec<_>>()
        );
    }

    /// Every non-ASCII char in `General_Category` Co (Private Use) must be escaped.
    #[test]
    fn property_all_co_chars_escape() {
        let leaked: Vec<char> = (0x80u32..=0x0010_FFFFu32)
            .filter_map(char::from_u32)
            .filter(|c| c.is_private_use() && is_printable_non_ascii(*c))
            .collect();
        assert!(
            leaked.is_empty(),
            "Co (Private Use) chars incorrectly marked printable: {:?}",
            leaked
                .iter()
                .map(|c| format!("U+{:04X}", *c as u32))
                .collect::<Vec<_>>()
        );
    }

    /// Zl (U+2028) and Zp (U+2029) must be escaped.
    #[test]
    fn property_zl_zp_escape() {
        assert!(
            !is_printable_non_ascii('\u{2028}'),
            "U+2028 LINE SEPARATOR (Zl) must be escaped"
        );
        assert!(
            !is_printable_non_ascii('\u{2029}'),
            "U+2029 PARAGRAPH SEPARATOR (Zp) must be escaped"
        );
    }

    /// A sample of L* / N* / P* / S* / M* chars must be preserved raw.
    /// These are the readable categories that idiomatic source may contain.
    #[test]
    fn property_readable_categories_preserved() {
        let readable_samples: &[(char, &str)] = &[
            // Letters (L*)
            ('é', "U+00E9 LATIN SMALL LETTER E WITH ACUTE (Ll)"),
            ('π', "U+03C0 GREEK SMALL LETTER PI (Ll)"),
            ('世', "U+4E16 CJK UNIFIED IDEOGRAPH-4E16 (Lo)"),
            ('Ñ', "U+00D1 LATIN CAPITAL LETTER N WITH TILDE (Lu)"),
            (
                'ǅ',
                "U+01C5 LATIN CAPITAL LETTER D WITH SMALL LETTER Z WITH CARON (Lt)",
            ),
            ('ˈ', "U+02C8 MODIFIER LETTER VERTICAL LINE (Lm)"),
            // Numbers (N*)
            ('²', "U+00B2 SUPERSCRIPT TWO (No)"),
            ('Ⅲ', "U+2162 ROMAN NUMERAL THREE (Nl)"),
            ('١', "U+0661 ARABIC-INDIC DIGIT ONE (Nd)"),
            // Punctuation (P*)
            ('«', "U+00AB LEFT-POINTING DOUBLE ANGLE QUOTATION MARK (Pi)"),
            (
                '»',
                "U+00BB RIGHT-POINTING DOUBLE ANGLE QUOTATION MARK (Pf)",
            ),
            ('—', "U+2014 EM DASH (Pd)"),
            ('·', "U+00B7 MIDDLE DOT (Po)"),
            // Symbols (S*)
            ('©', "U+00A9 COPYRIGHT SIGN (So)"),
            ('→', "U+2192 RIGHTWARDS ARROW (Sm)"),
            ('£', "U+00A3 POUND SIGN (Sc)"),
            // Marks (M*)
            ('\u{0300}', "U+0300 COMBINING GRAVE ACCENT (Mn)"),
        ];
        for &(c, label) in readable_samples {
            assert!(
                is_printable_non_ascii(c),
                "{label} must be preserved raw (got escaped)"
            );
        }
    }

    // ── Default_Ignorable + blank-looking scalars (completeness) ─────────────

    /// Representatives of every `Default_Ignorable` range — plus the
    /// blank-looking Braille pattern — must escape, even the ones whose
    /// `General_Category` is readable (Mn/Lo/So) and would otherwise slip past
    /// the category gate.
    #[test]
    fn property_default_ignorable_and_blank_escape() {
        let must_escape: &[(char, &str)] = &[
            ('\u{00AD}', "U+00AD SOFT HYPHEN (Cf)"),
            (
                '\u{034F}',
                "U+034F COMBINING GRAPHEME JOINER (Mn, readable cat)",
            ),
            ('\u{061C}', "U+061C ARABIC LETTER MARK (Cf)"),
            ('\u{115F}', "U+115F HANGUL CHOSEONG FILLER (Lo, blank)"),
            ('\u{1160}', "U+1160 HANGUL JUNGSEONG FILLER (Lo, blank)"),
            ('\u{17B4}', "U+17B4 KHMER VOWEL INHERENT AQ (Cf)"),
            (
                '\u{180B}',
                "U+180B MONGOLIAN FREE VARIATION SELECTOR ONE (Mn)",
            ),
            ('\u{180E}', "U+180E MONGOLIAN VOWEL SEPARATOR (Cf)"),
            ('\u{200B}', "U+200B ZERO WIDTH SPACE (Cf)"),
            ('\u{200D}', "U+200D ZERO WIDTH JOINER (Cf)"),
            ('\u{202E}', "U+202E RIGHT-TO-LEFT OVERRIDE (Cf)"),
            ('\u{2060}', "U+2060 WORD JOINER (Cf)"),
            (
                '\u{2800}',
                "U+2800 BRAILLE PATTERN BLANK (So, blank-looking)",
            ),
            ('\u{3164}', "U+3164 HANGUL FILLER (Lo, blank)"),
            (
                '\u{FE0F}',
                "U+FE0F VARIATION SELECTOR-16 (Mn, readable cat)",
            ),
            ('\u{FE00}', "U+FE00 VARIATION SELECTOR-1 (Mn)"),
            ('\u{FEFF}', "U+FEFF ZERO WIDTH NO-BREAK SPACE / BOM (Cf)"),
            ('\u{FFA0}', "U+FFA0 HALFWIDTH HANGUL FILLER (Lo, blank)"),
            (
                '\u{1D173}',
                "U+1D173 MUSICAL SYMBOL BEGIN BEAM (Cf, supplementary)",
            ),
            (
                '\u{E0100}',
                "U+E0100 VARIATION SELECTOR-17 (Mn, supplement)",
            ),
            ('\u{E0001}', "U+E0001 LANGUAGE TAG (Cf, tag block)"),
        ];
        for &(c, label) in must_escape {
            assert!(
                !is_printable_non_ascii(c),
                "{label} must be escaped (got preserved)"
            );
            // The string-escape path must emit a \u{…} escape, never the raw char.
            let escaped = escape_string(&c.to_string());
            assert_eq!(
                escaped,
                format!("\\u{{{:X}}}", c as u32),
                "{label} must escape via escape_string"
            );
        }
    }

    /// Exhaustive: every scalar in the explicit `Default_Ignorable` ranges escapes.
    #[test]
    fn property_all_default_ignorable_ranges_escape() {
        let ranges: &[(u32, u32)] = &[
            (0x00AD, 0x00AD),
            (0x034F, 0x034F),
            (0x061C, 0x061C),
            (0x115F, 0x1160),
            (0x17B4, 0x17B5),
            (0x180B, 0x180F),
            (0x200B, 0x200F),
            (0x202A, 0x202E),
            (0x2060, 0x206F),
            (0x3164, 0x3164),
            (0xFE00, 0xFE0F),
            (0xFEFF, 0xFEFF),
            (0xFFA0, 0xFFA0),
            (0xFFF0, 0xFFF8),
            (0x1BCA0, 0x1BCA3),
            (0x1D173, 0x1D17A),
            (0xE0000, 0xE0FFF),
        ];
        let leaked: Vec<char> = ranges
            .iter()
            .flat_map(|&(lo, hi)| lo..=hi)
            .filter_map(char::from_u32)
            .filter(|c| !c.is_ascii() && is_printable_non_ascii(*c))
            .collect();
        assert!(
            leaked.is_empty(),
            "Default_Ignorable scalars incorrectly preserved: {:?}",
            leaked
                .iter()
                .map(|c| format!("U+{:04X}", *c as u32))
                .collect::<Vec<_>>()
        );
    }

    // ── char + regex literal escape parity (FIX 3: bypass surfaces) ──────────

    /// A char literal containing an invisible/confusable scalar must escape via
    /// the shared policy, not pass through raw.
    #[test]
    fn char_literal_escapes_invisible() {
        let mut out = String::new();
        escape_char_literal('\u{202E}', &mut out);
        assert_eq!(out, "\\u{202E}", "RLO in char literal must escape");

        out.clear();
        escape_char_literal('\u{FE0F}', &mut out);
        assert_eq!(out, "\\u{FE0F}", "VS-16 in char literal must escape");

        out.clear();
        escape_char_literal('\u{200B}', &mut out);
        assert_eq!(out, "\\u{200B}", "ZWSP in char literal must escape");
    }

    /// Char literals with readable Unicode and named escapes are preserved.
    #[test]
    fn char_literal_preserves_readable_and_named() {
        let cases: &[(char, &str)] = &[
            ('é', "é"),
            ('世', "世"),
            ('→', "→"),
            ('\n', "\\n"),
            ('\'', "\\'"),
            ('\\', "\\\\"),
            ('a', "a"),
        ];
        for &(c, expected) in cases {
            let mut out = String::new();
            escape_char_literal(c, &mut out);
            assert_eq!(out, expected, "char {c:?} formatted wrong");
        }
    }

    /// A char literal round-trips through parse → format with the invisible
    /// scalar escaped, and is idempotent.
    #[test]
    fn char_literal_invisible_round_trips_escaped() {
        let raw_src = "fn f() -> char {\n    '\u{202E}'\n}\n";
        let formatted = roundtrip(raw_src);
        assert!(
            formatted.contains("'\\u{202E}'"),
            "RLO char literal must format escaped; got: {formatted:?}"
        );
        let twice = roundtrip(&formatted);
        assert_eq!(twice, formatted, "char-literal escape must be idempotent");
    }

    /// A regex literal containing an invisible scalar must escape it to a
    /// `\u{…}` regex escape (semantically identical to the engine), while
    /// preserving readable Unicode and regex backslash escapes verbatim.
    #[test]
    fn regex_literal_escapes_invisible_preserves_escapes() {
        // Raw RLO inside the pattern → \u{202E}.
        assert_eq!(
            escape_regex_pattern("a\u{202E}b"),
            "a\\u{202E}b",
            "RLO in regex must escape to \\u{{202E}}"
        );
        // Readable Unicode preserved verbatim.
        assert_eq!(
            escape_regex_pattern("café→世"),
            "café→世",
            "readable Unicode in regex preserved"
        );
        // Regex backslash escapes preserved verbatim.
        assert_eq!(
            escape_regex_pattern("\\s+\\d*"),
            "\\s+\\d*",
            "regex backslash escapes preserved"
        );
        // Quote escaped, VS-16 escaped.
        assert_eq!(
            escape_regex_pattern("x\u{FE0F}\"y"),
            "x\\u{FE0F}\\\"y",
            "VS-16 escaped and quote escaped in regex"
        );
    }

    /// A regex literal with an invisible scalar round-trips through parse →
    /// format with the scalar escaped, and is idempotent.
    #[test]
    fn regex_literal_invisible_round_trips_escaped() {
        let raw_src = "fn f() {\n    let r = re\"a\u{202E}b\";\n}\n";
        let formatted = roundtrip(raw_src);
        assert!(
            formatted.contains("re\"a\\u{202E}b\""),
            "RLO regex literal must format escaped; got: {formatted:?}"
        );
        let twice = roundtrip(&formatted);
        assert_eq!(twice, formatted, "regex-literal escape must be idempotent");
    }

    // ── full parse/format round-trip for Unicode string literals ────────────

    /// A string literal containing readable Unicode must survive a parse/format
    /// cycle unchanged (formatter preserves raw Unicode rather than escaping it).
    #[test]
    fn string_literal_readable_unicode_round_trips() {
        let src = "fn greet() -> string {\n    \"café → résumé\"\n}\n";
        assert_eq!(roundtrip(src), src);
    }

    /// Idempotency: running the formatter twice on a source with readable Unicode
    /// must produce the same output both times.
    #[test]
    fn string_literal_readable_unicode_idempotent() {
        let src = "fn greet() -> string {\n    \"世界 — hello → world\"\n}\n";
        let once = roundtrip(src);
        assert_eq!(
            once, src,
            "first pass must preserve readable Unicode verbatim"
        );
        let twice = roundtrip(&once);
        assert_eq!(twice, once, "second pass must be idempotent");
    }

    /// A string literal containing an invisible Unicode character must have that
    /// character escaped after formatting, and remain stable on a second pass.
    #[test]
    fn string_literal_invisible_unicode_escaped_and_idempotent() {
        // Source contains a literal zero-width space (U+200B) inside the string.
        // After formatting, it must appear as \u{200B}.
        let raw_src = "fn test() -> string {\n    \"hello\u{200B}world\"\n}\n";
        let formatted = roundtrip(raw_src);
        assert!(
            formatted.contains("\\u{200B}"),
            "zero-width space must be escaped; got: {formatted:?}"
        );
        // Second pass must not change anything.
        let twice = roundtrip(&formatted);
        assert_eq!(twice, formatted, "must be idempotent after escaping");
    }

    /// f-string literals with readable Unicode survive round-trip.
    #[test]
    fn fstring_literal_readable_unicode_round_trips() {
        let src = "fn greet(name: string) -> string {\n    f\"bonjour {name} — café\"\n}\n";
        assert_eq!(roundtrip(src), src);
    }

    // ── supervisor config-param round-trip ───────────────────────────────────

    /// A supervisor with a config param must preserve the param and all
    /// config.field references in the body through `hew fmt`. The formatter
    /// previously dropped the `(config: T)` clause, silently breaking the body.
    #[test]
    fn supervisor_config_param_roundtrips() {
        let src = "\
record AppConfig { size: i64, label: string }

actor Cache {
    var capacity: i64;
    var name: string;
    receive fn get_cap(_n: i64) -> i64 {
        capacity;
        name.len()
    }
}

supervisor App(config: AppConfig) {
    strategy: one_for_one;
    intensity: 3 within 60s;

    child cache: Cache(capacity: config.size, name: config.label);
}

fn main() -> i64 {
    let cfg = AppConfig { size: 5, label: \"hi\" };
    let sup = spawn App(config: cfg);
    let _c = sup.cache;
    supervisor_stop(sup);
    0
}
";
        let formatted = roundtrip(src);
        assert!(
            formatted.contains("supervisor App(config: AppConfig)"),
            "hew fmt must preserve the supervisor config param; got:\n{formatted}"
        );
        assert!(
            formatted.contains("config.size"),
            "hew fmt must preserve config.size reference in body; got:\n{formatted}"
        );
        assert!(
            formatted.contains("config.label"),
            "hew fmt must preserve config.label reference in body; got:\n{formatted}"
        );
        // Idempotency: a second pass must not change the output.
        let twice = roundtrip(&formatted);
        assert_eq!(
            twice, formatted,
            "supervisor config-param format must be idempotent"
        );
    }

    /// A supervisor WITHOUT a config param must not emit an empty `()` clause —
    /// the formatter must be silent when there are no params.
    #[test]
    fn supervisor_without_config_param_no_parens() {
        let src = "\
supervisor Simple {
    strategy: one_for_one;
}
";
        let formatted = roundtrip(src);
        assert!(
            !formatted.contains("supervisor Simple("),
            "supervisor with no config params must not emit parens; got:\n{formatted}"
        );
    }
}
