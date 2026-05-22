use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SourcePosition {
    pub line: usize,
    pub column: usize,
    pub byte_offset: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SourceFile {
    pub id: String,
    pub path: String,
    pub content_sha256: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SpanEntry {
    pub id: String,
    pub source_id: String,
    pub start: SourcePosition,
    pub end: SourcePosition,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SourceMap {
    pub sources: Vec<SourceFile>,
    pub spans: Vec<SpanEntry>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ModuleGraph {
    pub entry: String,
    pub modules: Vec<Module>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Module {
    pub id: String,
    pub path: String,
    pub source_id: String,
    pub imports: Vec<ImportEdge>,
    pub functions: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ImportEdge {
    pub path: String,
    pub resolved_module: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Layouts {
    pub types: Vec<TypeLayout>,
    pub records: Vec<RecordLayout>,
    pub enums: Vec<EnumLayout>,
    pub actors: Vec<ActorLayout>,
    pub supervisors: Vec<SupervisorLayout>,
    pub machines: Vec<MachineLayout>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct TypeLayout {
    pub id: String,
    pub kind: String,
    pub name: String,
    #[serde(skip_serializing_if = "Vec::is_empty", default)]
    pub parameters: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RecordLayout {
    pub id: String,
    pub name: String,
    pub fields: Vec<FieldLayout>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct FieldLayout {
    pub name: String,
    #[serde(rename = "type")]
    pub ty: String,
    pub index: usize,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct EnumLayout {
    pub id: String,
    pub name: String,
    pub variants: Vec<VariantLayout>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct VariantLayout {
    pub name: String,
    pub tag: usize,
    pub payload: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct ActorLayout {
    pub id: String,
    pub name: String,
    pub state_fields: Vec<FieldLayout>,
    pub handlers: Vec<HandlerLayout>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct HandlerLayout {
    pub name: String,
    pub function: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct SupervisorLayout {
    pub id: String,
    pub name: String,
    pub strategy: String,
    pub children: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MachineLayout {
    pub id: String,
    pub name: String,
    pub states: Vec<String>,
    pub events: Vec<String>,
    pub transitions: Vec<MachineTransition>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct MachineTransition {
    pub event: String,
    pub from: String,
    pub to: String,
    pub span: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct StdlibSymbol {
    pub id: String,
    pub module: String,
    pub name: String,
    pub params: Vec<String>,
    pub result: String,
    pub capability: Option<String>,
    pub admission: String,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Capability {
    pub id: String,
    pub disposition: String,
    pub reason: String,
    pub required_by: Vec<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Function {
    pub id: String,
    pub module: String,
    pub name: String,
    pub params: Vec<String>,
    pub result: String,
    pub locals: Vec<Local>,
    pub blocks: Vec<Block>,
    pub span: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Local {
    pub id: String,
    pub name: Option<String>,
    #[serde(rename = "type")]
    pub ty: String,
    pub mutable: bool,
    pub span: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Block {
    pub id: String,
    pub params: Vec<String>,
    pub instructions: Vec<Instruction>,
    pub terminator: Terminator,
    pub span: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Instruction {
    pub op: String,
    pub dst: Option<String>,
    pub args: Vec<Operand>,
    pub span: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub metadata: Option<Value>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Terminator {
    pub op: String,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub target: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub else_target: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub condition: Option<Operand>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub function: Option<String>,
    pub args: Vec<Operand>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub trap_kind: Option<String>,
    pub span: Option<String>,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Operand {
    pub kind: String,
    pub value: Value,
}

#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct SandboxBytecodePackage {
    pub schema_version: String,
    pub package_id: String,
    pub hew_version: String,
    pub compiler_version: String,
    pub profile: String,
    pub source_map: SourceMap,
    pub module_graph: ModuleGraph,
    pub layouts: Layouts,
    pub stdlib_symbols: Vec<StdlibSymbol>,
    pub capabilities: Vec<Capability>,
    pub functions: Vec<Function>,
}

impl Operand {
    pub fn local(id: impl Into<String>) -> Self {
        Self {
            kind: "local".to_string(),
            value: Value::String(id.into()),
        }
    }

    pub fn literal(value: impl Into<Value>) -> Self {
        Self {
            kind: "literal".to_string(),
            value: value.into(),
        }
    }

    pub fn function(id: impl Into<String>) -> Self {
        Self {
            kind: "function".to_string(),
            value: Value::String(id.into()),
        }
    }

    pub fn ty(id: impl Into<String>) -> Self {
        Self {
            kind: "type".to_string(),
            value: Value::String(id.into()),
        }
    }

    pub fn symbol(id: impl Into<String>) -> Self {
        Self {
            kind: "symbol".to_string(),
            value: Value::String(id.into()),
        }
    }

    pub fn trap(kind: impl Into<String>) -> Self {
        Self {
            kind: "trap".to_string(),
            value: Value::String(kind.into()),
        }
    }
}

impl Terminator {
    pub fn ret(args: Vec<Operand>, span: Option<String>) -> Self {
        Self {
            op: "return".to_string(),
            target: None,
            else_target: None,
            condition: None,
            function: None,
            args,
            trap_kind: None,
            span,
        }
    }

    pub fn br(target: impl Into<String>, args: Vec<Operand>, span: Option<String>) -> Self {
        Self {
            op: "br".to_string(),
            target: Some(target.into()),
            else_target: None,
            condition: None,
            function: None,
            args,
            trap_kind: None,
            span,
        }
    }

    pub fn br_if(
        condition: Operand,
        target: impl Into<String>,
        else_target: impl Into<String>,
        args: Vec<Operand>,
        span: Option<String>,
    ) -> Self {
        Self {
            op: "br_if".to_string(),
            target: Some(target.into()),
            else_target: Some(else_target.into()),
            condition: Some(condition),
            function: None,
            args,
            trap_kind: None,
            span,
        }
    }

    pub fn trap(kind: impl Into<String>, span: Option<String>) -> Self {
        let kind = kind.into();
        Self {
            op: "trap".to_string(),
            target: None,
            else_target: None,
            condition: None,
            function: None,
            args: vec![Operand::trap(kind.clone())],
            trap_kind: Some(kind),
            span,
        }
    }
}
