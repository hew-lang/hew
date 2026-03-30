//! `MessagePack` serialization for the Hew AST.
//!
//! Provides a compact binary serialization of the parsed (and optionally
//! type-enriched) AST using `rmp-serde`. Hew's embedded C++ codegen backend
//! (`hew-codegen/src/msgpack_reader.cpp`) deserializes this format.

use std::collections::{BTreeMap, HashMap};

use hew_parser::ast::{Spanned, TypeExpr};
use hew_parser::module::{Module, ModuleId};
use serde::{Deserialize, Serialize};

/// Schema version for the msgpack AST boundary.
///
/// Increment when the serialized format changes in a way that older C++
/// codegen cannot understand. The embedded reader requires an explicit
/// `schema_version` field and rejects mismatches instead of carrying
/// fallback decoding for pre-versioned payloads.
pub const SCHEMA_VERSION: u32 = 2;

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

/// Top-level serialization wrapper: the program AST plus type-checker and
/// source metadata used by C++ codegen.
///
/// Serialized as a `MessagePack` map with these keys:
/// - `"schema_version"`
/// - `"items"`
/// - `"module_doc"`
/// - `"expr_types"`
/// - `"handle_types"`
/// - `"handle_type_repr"`
/// - `"module_graph"` when module graph data is available
/// - `"source_path"` when source path metadata is available
/// - `"line_map"` when line mapping metadata is available
#[derive(Debug, Serialize)]
struct TypedProgram<'a, ModuleGraphRepr> {
    /// Schema version — always serialized first so the C++ reader can
    /// reject incompatible payloads before parsing the rest of the AST.
    schema_version: u32,
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
    module_graph: Option<ModuleGraphRepr>,
    /// Absolute path to the source .hew file.
    /// Used for codegen diagnostics and, when enabled, DWARF debug info.
    #[serde(skip_serializing_if = "Option::is_none")]
    source_path: Option<&'a str>,
    /// Byte offset of the start of each line in the source file.
    /// `line_map`[0] = offset of line 1, `line_map`[1] = offset of line 2, etc.
    /// Used by codegen to convert byte-offset spans to line:column for
    /// diagnostics and DWARF.
    #[serde(skip_serializing_if = "Option::is_none")]
    line_map: Option<&'a [usize]>,
}

impl<'a, ModuleGraphRepr> TypedProgram<'a, ModuleGraphRepr> {
    fn new(
        program: &'a hew_parser::ast::Program,
        expr_types: &'a [ExprTypeEntry],
        handle_types: Vec<String>,
        handle_type_repr: HashMap<String, String>,
        module_graph: Option<ModuleGraphRepr>,
        source_path: Option<&'a str>,
        line_map: Option<&'a [usize]>,
    ) -> Self {
        Self {
            schema_version: SCHEMA_VERSION,
            items: &program.items,
            module_doc: &program.module_doc,
            expr_types,
            handle_types,
            handle_type_repr,
            module_graph,
            source_path,
            line_map,
        }
    }
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
#[expect(
    clippy::needless_pass_by_value,
    clippy::implicit_hasher,
    reason = "serialization consumes the map"
)]
pub fn serialize_to_msgpack(
    program: &hew_parser::ast::Program,
    expr_types: Vec<ExprTypeEntry>,
    handle_types: Vec<String>,
    handle_type_repr: HashMap<String, String>,
    source_path: Option<&str>,
    line_map: Option<&[usize]>,
) -> Vec<u8> {
    let typed = TypedProgram::new(
        program,
        &expr_types,
        handle_types,
        handle_type_repr,
        program.module_graph.as_ref(),
        source_path,
        line_map,
    );
    rmp_serde::to_vec_named(&typed).expect("AST MessagePack serialization failed")
}

/// JSON-friendly module graph: uses `ModuleId.to_string()` as map keys
/// instead of the struct form (which JSON cannot represent as object keys).
#[derive(Debug, Serialize)]
struct ModuleGraphJson<'a> {
    modules: BTreeMap<String, &'a Module>,
    root: &'a ModuleId,
    topo_order: &'a Vec<ModuleId>,
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
#[expect(
    clippy::needless_pass_by_value,
    clippy::implicit_hasher,
    reason = "serialization consumes the map"
)]
pub fn serialize_to_json(
    program: &hew_parser::ast::Program,
    expr_types: Vec<ExprTypeEntry>,
    handle_types: Vec<String>,
    handle_type_repr: HashMap<String, String>,
    source_path: Option<&str>,
    line_map: Option<&[usize]>,
) -> String {
    let module_graph_json = program.module_graph.as_ref().map(|mg| ModuleGraphJson {
        modules: mg.modules.iter().map(|(k, v)| (k.to_string(), v)).collect(),
        root: &mg.root,
        topo_order: &mg.topo_order,
    });
    let typed = TypedProgram::new(
        program,
        &expr_types,
        handle_types,
        handle_type_repr,
        module_graph_json,
        source_path,
        line_map,
    );
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
    use hew_parser::ast::{
        Block, Expr, FnDecl, IntRadix, Item, Literal, MachineDecl, MachineEvent, MachineState,
        MachineTransition, Program, Stmt, Visibility,
    };

    /// Round-trip: serialize → deserialize should produce an identical AST.
    #[test]
    fn round_trip_simple_program() {
        let program = Program {
            items: vec![(
                Item::Function(FnDecl {
                    attributes: vec![],
                    is_async: false,
                    is_generator: false,
                    visibility: Visibility::Private,
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

        let bytes = serialize_to_msgpack(&program, vec![], vec![], HashMap::new(), None, None);
        assert!(!bytes.is_empty());

        let restored = deserialize_from_msgpack(&bytes).expect("deserialization should succeed");
        assert_eq!(program, restored);
    }

    /// Verify that a Program with a `ModuleGraph` round-trips correctly.
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
                source_paths: Vec::new(),
                doc: Some("root module".into()),
            },
        );
        modules.insert(
            dep_id.clone(),
            Module {
                id: dep_id.clone(),
                items: vec![],
                imports: vec![],
                source_paths: Vec::new(),
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

        let bytes = serialize_to_msgpack(&program, vec![], vec![], HashMap::new(), None, None);
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

        let bytes = serialize_to_msgpack(&program, vec![], vec![], HashMap::new(), None, None);
        let restored = deserialize_from_msgpack(&bytes).expect("deserialization should succeed");
        assert_eq!(program, restored);
    }

    /// Round-trip a `MachineDecl` through `MessagePack`.
    #[test]
    fn round_trip_machine_decl() {
        let program = Program {
            items: vec![(
                Item::Machine(MachineDecl {
                    visibility: Visibility::Pub,
                    name: "TrafficLight".into(),
                    has_default: false,
                    states: vec![
                        MachineState {
                            name: "Red".into(),
                            fields: vec![],
                        },
                        MachineState {
                            name: "Green".into(),
                            fields: vec![(
                                "duration".into(),
                                (
                                    TypeExpr::Named {
                                        name: "Int".into(),
                                        type_args: None,
                                    },
                                    10..13,
                                ),
                            )],
                        },
                    ],
                    events: vec![MachineEvent {
                        name: "Timer".into(),
                        fields: vec![],
                    }],
                    transitions: vec![MachineTransition {
                        event_name: "Timer".into(),
                        source_state: "Red".into(),
                        target_state: "Green".into(),
                        guard: None,
                        body: (
                            Expr::Block(Block {
                                stmts: vec![],
                                trailing_expr: Some(Box::new((
                                    Expr::Literal(Literal::Integer {
                                        value: 0,
                                        radix: IntRadix::Decimal,
                                    }),
                                    20..21,
                                ))),
                            }),
                            15..25,
                        ),
                    }],
                }),
                0..100,
            )],
            module_doc: None,
            module_graph: None,
        };

        let bytes = serialize_to_msgpack(&program, vec![], vec![], HashMap::new(), None, None);
        assert!(!bytes.is_empty());

        let restored = deserialize_from_msgpack(&bytes).expect("deserialization should succeed");
        assert_eq!(program, restored);
    }

    /// Verify that the serialized msgpack contains a `schema_version` field
    /// set to `SCHEMA_VERSION`, and that a round-trip still produces an
    /// identical `Program` (the version field is metadata, not part of `Program`).
    #[test]
    fn schema_version_round_trip() {
        let program = Program {
            items: vec![],
            module_doc: None,
            module_graph: None,
        };

        let bytes = serialize_to_msgpack(&program, vec![], vec![], HashMap::new(), None, None);

        // Deserialize into a serde_json::Value to inspect the schema_version field.
        // rmp_serde can deserialize msgpack into any Deserialize type, and
        // serde_json::Value is a convenient untyped representation.
        let value: serde_json::Value =
            rmp_serde::from_slice(&bytes).expect("should deserialize to generic value");

        let obj = value.as_object().expect("top-level should be a map");
        let sv = obj
            .get("schema_version")
            .expect("schema_version key should be present");
        assert_eq!(
            sv.as_u64(),
            Some(u64::from(SCHEMA_VERSION)),
            "schema_version should equal SCHEMA_VERSION"
        );

        // The round-trip should still produce the same Program (schema_version
        // is extra metadata that Program doesn't carry, so it's silently ignored).
        let restored = deserialize_from_msgpack(&bytes).expect("deserialization should succeed");
        assert_eq!(program, restored);
    }
}
