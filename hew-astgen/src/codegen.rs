use std::collections::{HashMap, HashSet};
use std::fmt::Write;

use crate::model::*;
use crate::special_cases;
use crate::type_map::{self, TypeMap};

/// Types whose `Vec<Spanned<T>>` uses `unique_ptr` per element in C++.
/// These are heavyweight variant types where by-value vectors would be
/// too expensive. The Rust AST uses bare `Vec<Spanned<T>>` for all of
/// these, but the C++ header uses `vector<unique_ptr<Spanned<T>>>`.
const VEC_PTR_TYPES: &[&str] = &["Expr", "Stmt", "Pattern"];

/// Transitive dependency closures for forward-declared types.
///
/// For a forward-declared type F (e.g., Expr), `dep_closures[F]` contains all types
/// that F transitively depends on. If struct S ∈ dep_closures[F], then S is defined
/// before F in the C++ header, meaning F is incomplete at S's definition point.
/// Therefore, bare `Spanned<F>` in S's fields must use `unique_ptr`.
type DepClosures = HashMap<String, HashSet<String>>;

/// Generate the complete msgpack_reader_gen.cpp content.
pub fn generate(types: &[TypeDef], type_map: &TypeMap) -> String {
    let mut out = String::with_capacity(64 * 1024);

    // File header
    out.push_str(special_cases::file_header());
    out.push_str("\n\n");

    // Helper functions
    out.push_str(special_cases::helpers_preamble());
    out.push_str("\n\n");

    // Forward declarations for recursive types
    write_forward_decls(&mut out, type_map);
    out.push_str("\n");

    // Forward declarations for special-cased parsers that are emitted late
    // but referenced by auto-generated parsers
    write_special_forward_decls(&mut out);
    out.push_str("\n");

    // Template helpers (Span, Spanned, Vec, etc.)
    out.push_str(special_cases::template_helpers());
    out.push_str("\n\n");

    // Literal (special case - custom serde)
    out.push_str(
        "// ── Literals ────────────────────────────────────────────────────────────────\n\n",
    );
    out.push_str(special_cases::literal_parser());
    out.push_str("\n\n");

    // Build dependency order and dep closures for forward-declared types
    let ordered = dependency_order(types, type_map);
    let dep_closures = compute_dep_closures(types, type_map);

    // Generate parsers in dependency order
    for type_def in &ordered {
        if type_map.should_skip(type_def.name()) {
            continue;
        }
        // Special-cased types
        match type_def.name() {
            "Program" | "ModuleGraph" | "TypeDecl" | "TypeBodyItem" => continue,
            _ => {}
        }

        write_parser(&mut out, type_def, type_map, &dep_closures);
        out.push_str("\n");
    }

    // Special-cased parsers that must appear after their dependencies

    // TypeDecl (special case - method_storage)
    out.push_str(
        "// ── TypeDecl ────────────────────────────────────────────────────────────────\n\n",
    );
    out.push_str(special_cases::type_decl_parser());
    out.push_str("\n\n");

    // ModuleGraph (special case - dual-format HashMap)
    out.push_str(
        "// ── ModuleGraph ─────────────────────────────────────────────────────────────\n\n",
    );
    out.push_str(special_cases::module_graph_parser());
    out.push_str("\n\n");

    // Program & ExprTypeEntry (special cases)
    out.push_str(
        "// ── Program (top-level) ─────────────────────────────────────────────────────\n\n",
    );
    out.push_str(special_cases::expr_type_entry_parser());
    out.push_str("\n\n");
    out.push_str(special_cases::program_parser());
    out.push_str("\n\n");

    // Public API
    out.push_str(
        "// ── Public API ──────────────────────────────────────────────────────────────\n\n",
    );
    out.push_str(special_cases::public_api());
    out.push_str("\n\n");

    // Close namespace
    out.push_str("} // namespace hew\n");

    out
}

fn write_forward_decls(out: &mut String, type_map: &TypeMap) {
    out.push_str(
        "// ── Forward declarations ────────────────────────────────────────────────────\n\n",
    );
    for name in &type_map.forward_declared {
        let cpp_type = format!("ast::{name}");
        let fn_name = TypeMap::parse_fn_name(name);
        let _ = writeln!(
            out,
            "static {cpp_type} {fn_name}(const msgpack::object &obj);"
        );
    }
    out.push('\n');
}

/// Forward declarations for special-cased parsers that are emitted late
/// but called by auto-generated parsers (e.g., parseItem calls parseTypeDecl).
fn write_special_forward_decls(out: &mut String) {
    out.push_str(
        "// ── Forward declarations for special-cased parsers ──────────────────────────\n\n",
    );
    let _ = writeln!(
        out,
        "static ast::TypeDecl parseTypeDecl(const msgpack::object &obj);"
    );
    out.push('\n');
}

fn write_parser(
    out: &mut String,
    type_def: &TypeDef,
    type_map: &TypeMap,
    dep_closures: &DepClosures,
) {
    match type_def {
        TypeDef::SimpleEnum(e) => write_simple_enum(out, e),
        TypeDef::TaggedEnum(e) => write_tagged_enum(out, e, type_map, dep_closures),
        TypeDef::Struct(s) => write_struct_parser(out, s, dep_closures),
    }
}

/// Pattern A: String-to-enum for simple enums.
fn write_simple_enum(out: &mut String, e: &SimpleEnum) {
    let cpp_type = format!("ast::{}", e.name);
    let fn_name = TypeMap::parse_fn_name(&e.name);

    let _ = writeln!(
        out,
        "static {cpp_type} {fn_name}(const msgpack::object &obj) {{"
    );
    let _ = writeln!(out, "  auto s = getString(obj);");

    for variant in &e.variants {
        let _ = writeln!(
            out,
            "  if (s == \"{variant}\") return {cpp_type}::{variant};"
        );
    }

    let _ = writeln!(out, "  fail(\"unknown {}: \" + s);", e.name);
    let _ = writeln!(out, "}}");
}

/// Pattern B: Externally-tagged variant dispatch for tagged enums.
fn write_tagged_enum(
    out: &mut String,
    e: &TaggedEnum,
    type_map: &TypeMap,
    dep_closures: &DepClosures,
) {
    let cpp_type = format!("ast::{}", e.name);
    let fn_name = TypeMap::parse_fn_name(&e.name);

    let _ = writeln!(
        out,
        "static {cpp_type} {fn_name}(const msgpack::object &obj) {{"
    );
    let _ = writeln!(out, "  auto [name, payload] = getEnumVariant(obj);");
    out.push('\n');

    for variant in &e.variants {
        write_variant_handler(out, &e.name, &cpp_type, variant, type_map, dep_closures);
    }

    let _ = writeln!(out, "  fail(\"unknown {} variant: \" + name);", e.name);
    let _ = writeln!(out, "}}");
}

fn write_variant_handler(
    out: &mut String,
    enum_name: &str,
    cpp_enum_type: &str,
    variant: &EnumVariant,
    type_map: &TypeMap,
    dep_closures: &DepClosures,
) {
    let variant_name = variant.name();
    let cpp_struct = type_map.cpp_variant_struct(enum_name, variant);

    match variant {
        EnumVariant::Unit { .. } => {
            let _ = writeln!(
                out,
                "  if (name == \"{variant_name}\") return {cpp_enum_type}{{ast::{cpp_struct}{{}}}};",
            );
        }
        EnumVariant::Newtype { ty, .. } => {
            let parse_expr = gen_parse_expr(ty, "*payload", enum_name, dep_closures);
            // For Item enum, the variant struct IS the inner type, so we parse directly
            if enum_name == "Item" {
                let _ = writeln!(
                    out,
                    "  if (name == \"{variant_name}\") return {cpp_enum_type}{{{parse_expr}}};",
                );
            } else {
                let _ = writeln!(
                    out,
                    "  if (name == \"{variant_name}\") return {cpp_enum_type}{{ast::{cpp_struct}{{{parse_expr}}}}};",
                );
            }
        }
        EnumVariant::Struct { fields, .. } => {
            let _ = writeln!(out, "  if (name == \"{variant_name}\") {{");
            let _ = writeln!(out, "    ast::{cpp_struct} e;");
            for field in fields {
                if field.serde_skip {
                    continue;
                }
                write_field_parse(out, field, "e", "*payload", "    ", enum_name, dep_closures);
            }
            let _ = writeln!(out, "    return {cpp_enum_type}{{std::move(e)}};");
            let _ = writeln!(out, "  }}");
        }
        EnumVariant::Tuple { fields, .. } => {
            let _ = writeln!(out, "  if (name == \"{variant_name}\") {{");
            let _ = writeln!(out, "    uint32_t sz;");
            let _ = writeln!(out, "    const auto *arr = arrayData(*payload, sz);");
            let _ = writeln!(
                out,
                "    if (sz != {}) fail(\"{variant_name} expects {} elements\");",
                fields.len(),
                fields.len()
            );
            let _ = writeln!(out, "    ast::{cpp_struct} e;");
            for (i, field_ty) in fields.iter().enumerate() {
                let parse_expr =
                    gen_parse_expr(field_ty, &format!("arr[{i}]"), enum_name, dep_closures);
                // Use known field names for Pattern::Or
                if enum_name == "Pattern" && variant_name == "Or" {
                    let field_name = if i == 0 { "left" } else { "right" };
                    let _ = writeln!(out, "    e.{field_name} = {parse_expr};");
                } else {
                    let _ = writeln!(out, "    e.field{i} = {parse_expr};");
                }
            }
            let _ = writeln!(out, "    return {cpp_enum_type}{{std::move(e)}};");
            let _ = writeln!(out, "  }}");
        }
    }
}

/// Pattern C: Struct field extraction.
fn write_struct_parser(out: &mut String, s: &StructDef, dep_closures: &DepClosures) {
    let cpp_type = format!("ast::{}", s.name);
    let fn_name = TypeMap::parse_fn_name(&s.name);

    let _ = writeln!(
        out,
        "static {cpp_type} {fn_name}(const msgpack::object &obj) {{"
    );
    let _ = writeln!(out, "  {cpp_type} result;");

    for field in &s.fields {
        if field.serde_skip {
            continue;
        }
        write_field_parse(out, field, "result", "obj", "  ", &s.name, dep_closures);
    }

    let _ = writeln!(out, "  return result;");
    let _ = writeln!(out, "}}");
}

/// Generate code to parse a single field from a msgpack map.
fn write_field_parse(
    out: &mut String,
    field: &FieldDef,
    var: &str,
    obj: &str,
    indent: &str,
    current_type: &str,
    dep_closures: &DepClosures,
) {
    let key = field.serde_rename.as_deref().unwrap_or(&field.name);
    let field_name = &field.name;

    let is_optional = matches!(field.ty, RustType::Option(_)) || field.serde_default;

    if is_optional {
        write_optional_field_parse(
            out,
            field,
            var,
            obj,
            indent,
            key,
            field_name,
            current_type,
            dep_closures,
        );
    } else {
        write_required_field_parse(
            out,
            field,
            var,
            obj,
            indent,
            key,
            field_name,
            current_type,
            dep_closures,
        );
    }
}

fn write_optional_field_parse(
    out: &mut String,
    field: &FieldDef,
    var: &str,
    obj: &str,
    indent: &str,
    key: &str,
    field_name: &str,
    current_type: &str,
    dep_closures: &DepClosures,
) {
    match &field.ty {
        RustType::Option(inner) => {
            let _ = writeln!(
                out,
                "{indent}const auto *{field_name} = mapGet({obj}, \"{key}\");"
            );
            let _ = writeln!(out, "{indent}if ({field_name} && !isNil(*{field_name}))");

            // Generate the assignment based on inner type
            let parse =
                gen_parse_expr(inner, &format!("*{field_name}"), current_type, dep_closures);
            let _ = writeln!(out, "{indent}  {var}.{field_name} = {parse};");
        }
        _ => {
            // serde(default) on a non-Option field
            let _ = writeln!(
                out,
                "{indent}const auto *{field_name}_ = mapGet({obj}, \"{key}\");"
            );
            let _ = writeln!(out, "{indent}if ({field_name}_ && !isNil(*{field_name}_))");
            let parse = gen_parse_expr(
                &field.ty,
                &format!("*{field_name}_"),
                current_type,
                dep_closures,
            );
            let _ = writeln!(out, "{indent}  {var}.{field_name} = {parse};");
        }
    }
}

fn write_required_field_parse(
    out: &mut String,
    field: &FieldDef,
    var: &str,
    obj: &str,
    indent: &str,
    key: &str,
    field_name: &str,
    current_type: &str,
    dep_closures: &DepClosures,
) {
    let obj_expr = format!("mapReq({obj}, \"{key}\")");
    let parse = gen_parse_expr(&field.ty, &obj_expr, current_type, dep_closures);
    let _ = writeln!(out, "{indent}{var}.{field_name} = {parse};");
}

/// Generate a C++ parse expression for a Rust type given a msgpack object expression.
///
/// Wrapping rules:
/// - `Box<Spanned<T>>` → `unique_ptr<Spanned<T>>` (always)
/// - Bare `Spanned<T>` where T is incomplete at current_type's position → `unique_ptr`
/// - Bare `Spanned<T>` where T is complete → by-value `Spanned<T>`
/// - `Named(T)` where T is incomplete at current_type's position → `make_unique<T>`
/// - `Named(T)` where T is complete → by-value `parseT(obj)`
/// - `Vec<Spanned<T>>` where T ∈ VEC_PTR_TYPES → `vector<unique_ptr<Spanned<T>>>`
fn gen_parse_expr(
    ty: &RustType,
    obj: &str,
    current_type: &str,
    dep_closures: &DepClosures,
) -> String {
    match ty {
        RustType::String => format!("getString({obj})"),
        RustType::Bool => format!("getBool({obj})"),
        RustType::I64 => format!("getInt({obj})"),
        RustType::U64 | RustType::Usize => format!("getUint({obj})"),
        RustType::U32 => format!("static_cast<uint32_t>(getUint({obj}))"),
        RustType::F64 => format!("getFloat({obj})"),
        RustType::Char => format!("({obj}).type == msgpack::type::STR ? (getString({obj}).empty() ? '\\0' : getString({obj})[0]) : '\\0'"),
        RustType::PathBuf => format!("getString({obj})"),
        RustType::Named(name) => {
            let fn_name = TypeMap::parse_fn_name(name);
            if is_incomplete_at(name, current_type, dep_closures) {
                // Type incomplete at this struct's position → heap-allocate
                format!("std::make_unique<ast::{name}>({fn_name}({obj}))")
            } else {
                format!("{fn_name}({obj})")
            }
        }
        RustType::Range(_) => format!("parseSpan({obj})"),
        RustType::Spanned(inner) => {
            let cpp_inner = type_map::cpp_type(inner);
            let parse_fn = gen_parse_fn_ref(inner);
            let inner_name = match inner.as_ref() {
                RustType::Named(n) => n.as_str(),
                _ => "",
            };
            if is_incomplete_at(inner_name, current_type, dep_closures) {
                // Inner type incomplete at this struct's position → unique_ptr
                format!("parseSpannedPtr<{cpp_inner}>({obj}, {parse_fn})")
            } else {
                format!("parseSpanned<{cpp_inner}>({obj}, {parse_fn})")
            }
        }
        RustType::Box(inner) => match inner.as_ref() {
            RustType::Spanned(t) => {
                let cpp_t = type_map::cpp_type(t);
                let parse_fn = gen_parse_fn_ref(t);
                format!("parseSpannedPtr<{cpp_t}>({obj}, {parse_fn})")
            }
            _ => {
                let cpp_inner = type_map::cpp_type(inner);
                let parse_expr = gen_parse_expr(inner, obj, current_type, dep_closures);
                format!("std::make_unique<{cpp_inner}>({parse_expr})")
            }
        },
        RustType::Vec(inner) => gen_vec_parse(inner, obj),
        RustType::Option(inner) => {
            // For Option inside a required context (not a field), just parse the inner
            gen_parse_expr(inner, obj, current_type, dep_closures)
        }
        RustType::Tuple(elems) => {
            if elems.len() == 2 {
                gen_tuple2_parse(&elems[0], &elems[1], obj, current_type, dep_closures)
            } else {
                format!("/* unsupported tuple parse from {obj} */")
            }
        }
        RustType::HashMap(k, v) => {
            format!(
                "/* HashMap<{}, {}> parse from {obj} */",
                type_map::cpp_type(k),
                type_map::cpp_type(v)
            )
        }
    }
}

/// Check if `target_type` is incomplete at `current_type`'s definition point in the header.
///
/// A type T is incomplete at struct S's position if S is in T's transitive dependency
/// closure — meaning T depends (directly or transitively) on S, so S must be defined
/// before T in the header.
fn is_incomplete_at(target_type: &str, current_type: &str, dep_closures: &DepClosures) -> bool {
    if let Some(closure) = dep_closures.get(target_type) {
        closure.contains(current_type)
    } else {
        false
    }
}

/// Generate a reference to a parse function suitable for passing to template helpers.
/// Always generates by-value parsers (the caller handles ptr wrapping).
fn gen_parse_fn_ref(ty: &RustType) -> String {
    match ty {
        RustType::String => "[](const msgpack::object &o) { return getString(o); }".to_string(),
        RustType::Bool => "[](const msgpack::object &o) { return getBool(o); }".to_string(),
        RustType::I64 => "[](const msgpack::object &o) { return getInt(o); }".to_string(),
        RustType::U64 | RustType::Usize => {
            "[](const msgpack::object &o) { return getUint(o); }".to_string()
        }
        RustType::Named(name) => TypeMap::parse_fn_name(name),
        RustType::Spanned(inner) => {
            let cpp_inner = type_map::cpp_type(inner);
            let inner_fn = gen_parse_fn_ref(inner);
            format!(
                "[](const msgpack::object &o) {{ return parseSpanned<{cpp_inner}>(o, {inner_fn}); }}"
            )
        }
        _ => format!("/* unsupported parse fn ref for {:?} */", ty),
    }
}

/// Generate Vec<T> parsing.
///
/// For `Vec<Spanned<T>>` where T ∈ VEC_PTR_TYPES, uses parseVecPtr to produce
/// `vector<unique_ptr<Spanned<T>>>`. This is a C++ design choice for heavyweight
/// variant types (Expr, Stmt, Pattern) — Rust uses bare `Vec<Spanned<T>>` for all.
fn gen_vec_parse(inner: &RustType, obj: &str) -> String {
    match inner {
        RustType::Spanned(t) => {
            let inner_name = match t.as_ref() {
                RustType::Named(n) => n.as_str(),
                _ => "",
            };
            let cpp_t = type_map::cpp_type(t);
            let parse_fn = gen_parse_fn_ref(t);
            let cpp_inner = format!("ast::Spanned<{cpp_t}>");
            if VEC_PTR_TYPES.contains(&inner_name) {
                // Vec<Spanned<T>> → vector<unique_ptr<Spanned<T>>> via parseVecPtr
                format!(
                    "parseVecPtr<{cpp_inner}>({obj}, [](const msgpack::object &o) {{ return parseSpanned<{cpp_t}>(o, {parse_fn}); }})"
                )
            } else {
                format!(
                    "parseVec<{cpp_inner}>({obj}, [](const msgpack::object &o) {{ return parseSpanned<{cpp_t}>(o, {parse_fn}); }})"
                )
            }
        }
        RustType::Named(name) => {
            let cpp_type = format!("ast::{name}");
            let parse_fn = TypeMap::parse_fn_name(name);
            format!("parseVec<{cpp_type}>({obj}, {parse_fn})")
        }
        RustType::String => {
            format!(
                "parseVec<std::string>({obj}, [](const msgpack::object &o) {{ return getString(o); }})"
            )
        }
        RustType::Box(boxed_inner) => {
            // Vec<Box<T>> → vector<unique_ptr<T>> using parseVecPtr
            match boxed_inner.as_ref() {
                RustType::Spanned(t) => {
                    let cpp_t = type_map::cpp_type(t);
                    let parse_fn = gen_parse_fn_ref(t);
                    let cpp_inner = format!("ast::Spanned<{cpp_t}>");
                    format!(
                        "parseVecPtr<{cpp_inner}>({obj}, [](const msgpack::object &o) {{ return parseSpanned<{cpp_t}>(o, {parse_fn}); }})"
                    )
                }
                _ => {
                    let cpp_inner = type_map::cpp_type(boxed_inner);
                    let parse_fn = gen_parse_fn_ref(boxed_inner);
                    format!("parseVecPtr<{cpp_inner}>({obj}, {parse_fn})")
                }
            }
        }
        RustType::Tuple(elems) if elems.len() == 2 => {
            let cpp_inner = vec_element_cpp_type(inner);
            let parse_lambda = gen_vec_tuple2_lambda(&elems[0], &elems[1]);
            format!("parseVec<{cpp_inner}>({obj}, {parse_lambda})")
        }
        _ => {
            let cpp_inner = type_map::cpp_type(inner);
            let parse_fn = gen_parse_fn_ref(inner);
            format!("parseVec<{cpp_inner}>({obj}, {parse_fn})")
        }
    }
}

/// Generate parsing for a (A, B) tuple.
fn gen_tuple2_parse(
    a: &RustType,
    b: &RustType,
    obj: &str,
    current_type: &str,
    dep_closures: &DepClosures,
) -> String {
    let parse_a = gen_parse_expr(a, "arr[0]", current_type, dep_closures);
    let parse_b = gen_parse_expr(b, "arr[1]", current_type, dep_closures);
    format!(
        "[&]() {{ uint32_t sz; const auto *arr = arrayData({obj}, sz); \
         if (sz != 2) fail(\"tuple should have 2 elements\"); \
         return std::make_pair({parse_a}, {parse_b}); }}()"
    )
}

/// Generate a lambda for parsing (A, B) tuples inside parseVec.
///
/// Uses `vec_element_parse_expr` so that `Spanned<T>` where T ∈ VEC_PTR_TYPES
/// gets unique_ptr wrapping (the C++ header uses unique_ptr in vector elements
/// for heavyweight variant types even when Rust uses bare Spanned).
fn gen_vec_tuple2_lambda(a: &RustType, b: &RustType) -> String {
    let parse_a = vec_element_parse_expr(a, "arr[0]");
    let parse_b = vec_element_parse_expr(b, "arr[1]");
    format!(
        "[](const msgpack::object &o) {{ \
         uint32_t sz; const auto *arr = arrayData(o, sz); \
         if (sz != 2) fail(\"tuple should have 2 elements\"); \
         return std::make_pair({parse_a}, {parse_b}); }}"
    )
}

/// Generate a parse expression for a type used as a vector element.
///
/// Like `gen_parse_expr`, but applies VEC_PTR_TYPES wrapping to `Spanned<T>`:
/// inside vectors, `Spanned<Expr/Stmt/Pattern>` becomes unique_ptr even when
/// the Rust type is bare (no Box).
fn vec_element_parse_expr(ty: &RustType, obj: &str) -> String {
    // Vec element context: VEC_PTR_TYPES handles wrapping, no dep_closures needed.
    let empty_closures = HashMap::new();
    match ty {
        RustType::Spanned(inner) => {
            let inner_name = match inner.as_ref() {
                RustType::Named(n) => n.as_str(),
                _ => "",
            };
            if VEC_PTR_TYPES.contains(&inner_name) {
                let cpp_inner = type_map::cpp_type(inner);
                let parse_fn = gen_parse_fn_ref(inner);
                format!("parseSpannedPtr<{cpp_inner}>({obj}, {parse_fn})")
            } else {
                gen_parse_expr(ty, obj, "", &empty_closures)
            }
        }
        _ => gen_parse_expr(ty, obj, "", &empty_closures),
    }
}

/// Map a RustType to a C++ type string for use as a vector element.
///
/// Applies VEC_PTR_TYPES wrapping: `Spanned<Expr/Stmt/Pattern>` becomes
/// `unique_ptr<Spanned<T>>` in vector contexts.
fn vec_element_cpp_type(ty: &RustType) -> String {
    match ty {
        RustType::Spanned(inner) => {
            let inner_name = match inner.as_ref() {
                RustType::Named(n) => n.as_str(),
                _ => "",
            };
            if VEC_PTR_TYPES.contains(&inner_name) {
                format!(
                    "std::unique_ptr<ast::Spanned<{}>>",
                    type_map::cpp_type(inner)
                )
            } else {
                type_map::cpp_type(ty)
            }
        }
        RustType::Tuple(elems) if elems.len() == 2 => {
            format!(
                "std::pair<{}, {}>",
                vec_element_cpp_type(&elems[0]),
                vec_element_cpp_type(&elems[1])
            )
        }
        _ => type_map::cpp_type(ty),
    }
}

/// Order types by dependencies (topological sort).
fn dependency_order<'a>(types: &'a [TypeDef], type_map: &TypeMap) -> Vec<&'a TypeDef> {
    // Build adjacency: type_name → set of type_names it depends on
    let mut deps: HashMap<String, HashSet<String>> = HashMap::new();
    let type_names: HashSet<String> = types.iter().map(|t| t.name().to_string()).collect();

    for t in types {
        let mut my_deps = HashSet::new();
        collect_type_deps(t, &type_names, &mut my_deps);
        deps.insert(t.name().to_string(), my_deps);
    }

    // Topological sort
    let mut visited = HashSet::new();
    let mut result = Vec::new();
    let type_map_lookup: HashMap<&str, &TypeDef> = types.iter().map(|t| (t.name(), t)).collect();

    for t in types {
        topo_visit(
            t.name(),
            &deps,
            &type_map_lookup,
            &mut visited,
            &mut result,
            type_map,
        );
    }

    result
}

fn topo_visit<'a>(
    name: &str,
    deps: &HashMap<String, HashSet<String>>,
    lookup: &HashMap<&str, &'a TypeDef>,
    visited: &mut HashSet<String>,
    result: &mut Vec<&'a TypeDef>,
    type_map: &TypeMap,
) {
    if visited.contains(name) {
        return;
    }
    visited.insert(name.to_string());

    if let Some(my_deps) = deps.get(name) {
        for dep in my_deps {
            // Don't follow deps through forward-declared types (they break cycles)
            if !type_map.needs_forward_decl(dep) {
                topo_visit(dep, deps, lookup, visited, result, type_map);
            }
        }
    }

    if let Some(&t) = lookup.get(name) {
        result.push(t);
    }
}

fn collect_type_deps(t: &TypeDef, known_types: &HashSet<String>, deps: &mut HashSet<String>) {
    match t {
        TypeDef::SimpleEnum(_) => {} // no deps
        TypeDef::TaggedEnum(e) => {
            for v in &e.variants {
                match v {
                    EnumVariant::Newtype { ty, .. } => {
                        collect_rust_type_deps(ty, known_types, deps)
                    }
                    EnumVariant::Struct { fields, .. } => {
                        for f in fields {
                            collect_rust_type_deps(&f.ty, known_types, deps);
                        }
                    }
                    EnumVariant::Tuple { fields, .. } => {
                        for f in fields {
                            collect_rust_type_deps(f, known_types, deps);
                        }
                    }
                    EnumVariant::Unit { .. } => {}
                }
            }
        }
        TypeDef::Struct(s) => {
            for f in &s.fields {
                collect_rust_type_deps(&f.ty, known_types, deps);
            }
        }
    }
}

fn collect_rust_type_deps(
    ty: &RustType,
    known_types: &HashSet<String>,
    deps: &mut HashSet<String>,
) {
    match ty {
        RustType::Named(name) => {
            if known_types.contains(name) {
                deps.insert(name.clone());
            }
        }
        RustType::Vec(inner)
        | RustType::Option(inner)
        | RustType::Box(inner)
        | RustType::Spanned(inner)
        | RustType::Range(inner) => {
            collect_rust_type_deps(inner, known_types, deps);
        }
        RustType::Tuple(elems) => {
            for e in elems {
                collect_rust_type_deps(e, known_types, deps);
            }
        }
        RustType::HashMap(k, v) => {
            collect_rust_type_deps(k, known_types, deps);
            collect_rust_type_deps(v, known_types, deps);
        }
        _ => {}
    }
}

/// Compute the transitive dependency closure for every type in a dependency cycle.
///
/// For cyclic type F, `closures[F]` = all types that F transitively depends on.
/// If struct S ∈ closures[F], then S must be defined before F in the C++ header,
/// meaning F is incomplete at S's definition point. Therefore, bare `Spanned<F>`
/// or bare `Named(F)` in S's fields must use `unique_ptr`.
///
/// This handles both "big" cycles (Expr↔Stmt↔Block via forward declarations)
/// and "small" cycles (TypeExpr↔TraitBound via TypeTraitObject).
fn compute_dep_closures(types: &[TypeDef], type_map: &TypeMap) -> DepClosures {
    let type_names: HashSet<String> = types.iter().map(|t| t.name().to_string()).collect();

    // Build the full dependency graph
    let mut deps: HashMap<String, HashSet<String>> = HashMap::new();
    for t in types {
        let mut my_deps = HashSet::new();
        collect_type_deps(t, &type_names, &mut my_deps);
        deps.insert(t.name().to_string(), my_deps);
    }

    // Compute transitive closure for each type; keep entries for cyclic types.
    // A type is cyclic if it appears in its own transitive dependency closure.
    // Forward-declared types are always included (they're the known cycle breakers).
    let mut closures = HashMap::new();
    for name in &type_names {
        let mut closure = HashSet::new();
        transitive_deps(name, &deps, &mut closure);
        let is_cyclic = closure.contains(name);
        let is_forward = type_map.forward_declared.contains(name);
        if is_cyclic || is_forward {
            closures.insert(name.clone(), closure);
        }
    }
    closures
}

/// Walk the dependency graph to collect all transitive dependencies of `name`.
fn transitive_deps(
    name: &str,
    deps: &HashMap<String, HashSet<String>>,
    visited: &mut HashSet<String>,
) {
    if let Some(my_deps) = deps.get(name) {
        for dep in my_deps {
            if visited.insert(dep.clone()) {
                transitive_deps(dep, deps, visited);
            }
        }
    }
}
