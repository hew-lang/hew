//! `MessagePack` serialization for the Hew AST.
//!
//! Provides a compact binary serialization of the parsed (and optionally
//! type-enriched) AST using `rmp-serde`. The C++ codegen backend
//! (`hew-codegen/src/msgpack_reader.cpp`) deserializes this format.

use std::collections::{BTreeMap, HashMap};

use hew_parser::ast::{Spanned, TypeExpr};
use hew_parser::module::{Module, ModuleGraph, ModuleId};
use serde::{Deserialize, Serialize};

/// An entry in the expression type map: `(start, end)` → `TypeExpr`.
///
/// Carries the resolved type for a single expression, identified by its source
/// span. The C++ codegen uses this to look up expression types without
/// re-inferring them.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ExprTypeEntry {
    /// Byte offset of the expression start.
    pub start: usize,
    /// Byte offset of the expression end.
    pub end: usize,
    /// The resolved type, as a parser `TypeExpr` with a synthetic span.
    pub ty: Spanned<TypeExpr>,
}

/// Top-level serialization wrapper: the program AST plus the resolved
/// expression type map from the type checker.
///
/// Serialized as a `MessagePack` map with three keys: `"items"`,
/// `"module_doc"`, and `"expr_types"`. The C++ reader treats
/// `"expr_types"` as optional for backward compatibility.
#[derive(Debug, Serialize)]
struct TypedProgram<'a> {
    items: &'a Vec<Spanned<hew_parser::ast::Item>>,
    module_doc: &'a Option<String>,
    /// Resolved types for every expression the type checker annotated.
    expr_types: &'a [ExprTypeEntry],
    /// Names of all known handle types (e.g., `"http.Server"`, `"json.Value"`).
    /// Flows type metadata to C++ codegen so it doesn't need hardcoded type lists.
    handle_types: Vec<String>,
    /// Map of handle type name to its MLIR representation.
    /// Default is `"handle"` (opaque pointer via `HandleType`).
    /// `"i32"` means the type is represented as a 32-bit integer (e.g., file descriptors).
    handle_type_repr: HashMap<String, String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    module_graph: Option<&'a ModuleGraph>,
}

/// Serialize a [`Program`](hew_parser::ast::Program) to `MessagePack` bytes,
/// including the resolved expression type map.
///
/// Uses named fields (`to_vec_named`) so the format is self-describing and
/// tolerant of field additions.
///
/// # Panics
///
/// Panics if serialization fails, which should never happen for a valid AST.
#[must_use]
pub fn serialize_to_msgpack(
    program: &hew_parser::ast::Program,
    expr_types: Vec<ExprTypeEntry>,
    handle_types: Vec<String>,
    handle_type_repr: HashMap<String, String>,
) -> Vec<u8> {
    let typed = TypedProgram {
        items: &program.items,
        module_doc: &program.module_doc,
        expr_types: &expr_types,
        handle_types,
        handle_type_repr,
        module_graph: program.module_graph.as_ref(),
    };
    rmp_serde::to_vec_named(&typed).expect("AST MessagePack serialization failed")
}

/// JSON-friendly module graph: uses `ModuleId.to_string()` as map keys
/// instead of the struct form (which JSON cannot represent as object keys).
#[derive(Serialize)]
struct ModuleGraphJson<'a> {
    modules: BTreeMap<String, &'a Module>,
    root: &'a ModuleId,
    topo_order: &'a Vec<ModuleId>,
}

/// JSON-friendly top-level wrapper. Identical to [`TypedProgram`] except
/// `module_graph` uses string keys for the modules map.
#[derive(Serialize)]
struct TypedProgramJson<'a> {
    items: &'a Vec<Spanned<hew_parser::ast::Item>>,
    module_doc: &'a Option<String>,
    expr_types: &'a [ExprTypeEntry],
    handle_types: Vec<String>,
    handle_type_repr: HashMap<String, String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    module_graph: Option<ModuleGraphJson<'a>>,
}

/// Serialize a [`Program`](hew_parser::ast::Program) to pretty-printed JSON.
///
/// Produces the same `TypedProgram` structure as [`serialize_to_msgpack`], but
/// encoded as human-readable JSON for debugging. The module graph's modules
/// map uses `ModuleId` display strings (e.g. `"std::net::http"`) as keys
/// instead of the struct form, since JSON objects require string keys.
///
/// # Panics
///
/// Panics if serialization fails, which should never happen for a valid AST.
#[must_use]
pub fn serialize_to_json(
    program: &hew_parser::ast::Program,
    expr_types: Vec<ExprTypeEntry>,
    handle_types: Vec<String>,
    handle_type_repr: HashMap<String, String>,
) -> String {
    let module_graph_json = program.module_graph.as_ref().map(|mg| ModuleGraphJson {
        modules: mg.modules.iter().map(|(k, v)| (k.to_string(), v)).collect(),
        root: &mg.root,
        topo_order: &mg.topo_order,
    });
    let typed = TypedProgramJson {
        items: &program.items,
        module_doc: &program.module_doc,
        expr_types: &expr_types,
        handle_types,
        handle_type_repr,
        module_graph: module_graph_json,
    };
    serde_json::to_string_pretty(&typed).expect("AST JSON serialization failed")
}

/// Deserialize a [`Program`](hew_parser::ast::Program) from `MessagePack` bytes.
///
/// # Errors
///
/// Returns an error if the bytes do not represent a valid MessagePack-encoded
/// `Program`.
pub fn deserialize_from_msgpack(
    data: &[u8],
) -> Result<hew_parser::ast::Program, rmp_serde::decode::Error> {
    rmp_serde::from_slice(data)
}

#[cfg(test)]
mod tests {
    use super::*;
    use hew_parser::ast::{Block, Expr, FnDecl, IntRadix, Item, Literal, Program, Stmt};

    /// Round-trip: serialize → deserialize should produce an identical AST.
    #[test]
    fn round_trip_simple_program() {
        let program = Program {
            items: vec![(
                Item::Function(FnDecl {
                    attributes: vec![],
                    is_async: false,
                    is_generator: false,
                    is_pub: false,
                    is_pure: false,
                    name: "main".into(),
                    type_params: None,
                    params: vec![],
                    return_type: None,
                    where_clause: None,
                    body: Block {
                        stmts: vec![(
                            Stmt::Expression((
                                Expr::Literal(Literal::Integer {
                                    value: 42,
                                    radix: IntRadix::Decimal,
                                }),
                                10..12,
                            )),
                            5..15,
                        )],
                        trailing_expr: None,
                    },
                    doc_comment: None,
                }),
                0..50,
            )],
            module_doc: None,
            module_graph: None,
        };

        let bytes = serialize_to_msgpack(&program, vec![], vec![], HashMap::new());
        assert!(!bytes.is_empty());

        let restored = deserialize_from_msgpack(&bytes).expect("deserialization should succeed");
        assert_eq!(program, restored);
    }

    /// Verify that a Program with a ModuleGraph round-trips correctly.
    #[test]
    fn round_trip_with_module_graph() {
        use hew_parser::module::{Module, ModuleGraph, ModuleId, ModuleImport};
        use std::collections::HashMap;

        let root_id = ModuleId {
            path: vec!["root".into()],
        };
        let dep_id = ModuleId {
            path: vec!["dep".into()],
        };

        let mut modules = HashMap::new();
        modules.insert(
            root_id.clone(),
            Module {
                id: root_id.clone(),
                items: vec![],
                imports: vec![ModuleImport {
                    target: dep_id.clone(),
                    spec: None,
                    span: 0..0,
                }],
                source_path: None,
                doc: Some("root module".into()),
            },
        );
        modules.insert(
            dep_id.clone(),
            Module {
                id: dep_id.clone(),
                items: vec![],
                imports: vec![],
                source_path: None,
                doc: None,
            },
        );

        let graph = ModuleGraph {
            modules,
            root: root_id.clone(),
            topo_order: vec![dep_id, root_id],
        };

        let program = Program {
            items: vec![],
            module_doc: None,
            module_graph: Some(graph),
        };

        let bytes = serialize_to_msgpack(&program, vec![], vec![], HashMap::new());
        assert!(!bytes.is_empty());

        let restored = deserialize_from_msgpack(&bytes).expect("deserialization should succeed");
        assert_eq!(program, restored);
    }

    /// Verify that an empty program round-trips correctly.
    #[test]
    fn round_trip_empty_program() {
        let program = Program {
            items: vec![],
            module_doc: Some("Module doc".into()),
            module_graph: None,
        };

        let bytes = serialize_to_msgpack(&program, vec![], vec![], HashMap::new());
        let restored = deserialize_from_msgpack(&bytes).expect("deserialization should succeed");
        assert_eq!(program, restored);
    }
}
