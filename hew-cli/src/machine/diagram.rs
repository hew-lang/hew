//! Authored machine presentation. This projection records scopes and source
//! logic; transition selection remains the checker's/evaluator's responsibility.
use std::collections::HashMap;
use std::fmt::Write as _;

use hew_parser::ast::{
    sym, Block, Expr, Ident, MachineDecl, MachineTransition, MachineTransitionBodyForm,
    MachineTrigger,
};
use serde_json::{json, Value};

use hew_cabi::host_error::{HewError, HewText};

#[derive(Clone, Copy, PartialEq, Eq)]
enum Kind {
    State,
    Compound,
    Region,
}

struct Node {
    id: String,
    name: String,
    kind: Kind,
    parent: Option<usize>,
    position: usize,
    fields: Vec<String>,
    entry: Option<String>,
    exit: Option<String>,
    parallel: bool,
    is_final: bool,
    initial: Option<(usize, Option<String>)>,
}

struct Edge<'a> {
    from: String,
    to: String,
    scope: Option<usize>,
    rule: &'a MachineTransition,
}

struct Auxiliary {
    id: String,
    label: String,
    computed: bool,
}

pub(super) struct Chart<'a> {
    machine: &'a MachineDecl,
    name: &'a str,
    nodes: Vec<Node>,
    edges: Vec<Edge<'a>>,
    auxiliary: Vec<Auxiliary>,
}

fn block_text(block: &Block) -> String {
    hew_parser::fmt::format_expression(&(Expr::Block(block.clone()), 0..0))
}

fn line(out: &mut String, depth: usize, text: impl AsRef<str>) {
    out.push_str(&"    ".repeat(depth));
    out.push_str(text.as_ref());
    out.push('\n');
}

impl<'a> Chart<'a> {
    #[expect(
        clippy::too_many_lines,
        reason = "one projection assigns declaration identity before connecting scoped edges"
    )]
    pub(super) fn new(machine: &'a MachineDecl, name: &'a str) -> Self {
        let mut nodes = Vec::new();
        let mut declarations = HashMap::new();
        for (index, state) in machine.states.iter().enumerate() {
            declarations.insert(state.name, nodes.len());
            nodes.push(Node {
                id: format!("state_{index}"),
                name: state.name.to_string(),
                kind: Kind::State,
                parent: None,
                position: state.span.start,
                fields: state
                    .fields
                    .iter()
                    .map(|(name, _)| name.to_string())
                    .collect(),
                entry: state.entry.as_ref().map(block_text),
                exit: state.exit.as_ref().map(block_text),
                parallel: false,
                is_final: state.is_final,
                initial: None,
            });
        }
        let compound_base = nodes.len();
        for (index, group) in machine.composite_groups.iter().enumerate() {
            declarations.insert(group.name, nodes.len());
            nodes.push(Node {
                id: format!("compound_{index}"),
                name: group.name.to_string(),
                kind: Kind::Compound,
                parent: None,
                position: group.span.start,
                fields: group
                    .fields
                    .iter()
                    .map(|(name, _)| name.to_string())
                    .collect(),
                entry: group.entry.as_ref().map(block_text),
                exit: group.exit.as_ref().map(block_text),
                parallel: !group.regions.is_empty(),
                is_final: false,
                initial: None,
            });
        }
        let mut regions = Vec::new();
        for (index, group) in machine.composite_groups.iter().enumerate() {
            let parent = compound_base + index;
            for member in &group.members {
                if let Some(child) = declarations.get(member) {
                    nodes[*child].parent = Some(parent);
                }
            }
            nodes[parent].initial = group
                .initial
                .and_then(|initial| declarations.get(&initial).copied())
                .map(|initial| {
                    (
                        initial,
                        group
                            .initial_value
                            .as_ref()
                            .map(hew_parser::fmt::format_expression),
                    )
                });
            for (ordinal, region) in group.regions.iter().enumerate() {
                let region_index = nodes.len();
                nodes.push(Node {
                    id: format!("region_{index}_{ordinal}"),
                    name: region.name.to_string(),
                    kind: Kind::Region,
                    parent: Some(parent),
                    position: region.span.start,
                    fields: Vec::new(),
                    entry: None,
                    exit: None,
                    parallel: false,
                    is_final: false,
                    initial: declarations.get(&region.initial).copied().map(|initial| {
                        (
                            initial,
                            region
                                .initial_value
                                .as_ref()
                                .map(hew_parser::fmt::format_expression),
                        )
                    }),
                });
                for member in &region.members {
                    if let Some(child) = declarations.get(member) {
                        nodes[*child].parent = Some(region_index);
                    }
                }
                regions.push((region_index, region));
            }
        }
        let mut chart = Self {
            machine,
            name,
            nodes,
            edges: Vec::new(),
            auxiliary: Vec::new(),
        };
        for rule in &machine.transitions {
            chart.push_rule(rule, None, &declarations);
        }
        for (index, group) in machine.composite_groups.iter().enumerate() {
            for rule in &group.parent_transitions {
                chart.push_rule(rule, Some(compound_base + index), &declarations);
            }
        }
        for (index, region) in regions {
            for rule in &region.parent_transitions {
                chart.push_rule(rule, Some(index), &declarations);
            }
        }
        chart
    }

    fn push_rule(
        &mut self,
        rule: &'a MachineTransition,
        scope: Option<usize>,
        declarations: &HashMap<Ident, usize>,
    ) {
        let ordinal = self.edges.len();
        let from = if rule.source_state.name == sym::UNDERSCORE {
            scope.map_or_else(
                || {
                    if !self.auxiliary.iter().any(|node| node.id == "any_state") {
                        self.auxiliary.push(Auxiliary {
                            id: "any_state".into(),
                            label: "any state (fallback)".into(),
                            computed: false,
                        });
                    }
                    "any_state".to_string()
                },
                |scope| self.nodes[scope].id.clone(),
            )
        } else {
            self.endpoint(
                rule.source_state,
                &format!("source_{ordinal}"),
                declarations,
            )
        };
        let to = if rule.target_state.name == sym::UNDERSCORE {
            let id = format!("computed_{ordinal}");
            self.auxiliary.push(Auxiliary {
                id: id.clone(),
                label: "computed target".into(),
                computed: true,
            });
            id
        } else {
            self.endpoint(
                rule.target_state,
                &format!("target_{ordinal}"),
                declarations,
            )
        };
        self.edges.push(Edge {
            from,
            to,
            scope,
            rule,
        });
    }

    fn endpoint(
        &mut self,
        name: Ident,
        fallback: &str,
        declarations: &HashMap<Ident, usize>,
    ) -> String {
        if let Some(index) = declarations.get(&name) {
            return self.nodes[*index].id.clone();
        }
        // --no-check can inspect an invalid draft, but must not invent a state.
        self.auxiliary.push(Auxiliary {
            id: fallback.into(),
            label: format!("unresolved: {name}"),
            computed: false,
        });
        fallback.into()
    }

    fn children(&self, parent: Option<usize>) -> Vec<usize> {
        let mut children = self
            .nodes
            .iter()
            .enumerate()
            .filter(|(_, node)| node.parent == parent)
            .map(|(index, _)| index)
            .collect::<Vec<_>>();
        children.sort_by_key(|index| self.nodes[*index].position);
        children
    }

    fn path(&self, index: usize) -> Vec<&str> {
        let mut names = Vec::new();
        let mut current = Some(index);
        while let Some(index) = current {
            names.push(self.nodes[index].name.as_str());
            current = self.nodes[index].parent;
        }
        names.reverse();
        names
    }

    fn label(rule: &MachineTransition) -> String {
        let mut label = rule.trigger.to_string();
        if !rule.event_bindings.is_empty() {
            let _ = write!(
                label,
                " {{ {} }}",
                rule.event_bindings
                    .iter()
                    .map(|(name, _)| name.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            );
        }
        if let Some(guard) = &rule.guard {
            let _ = write!(label, " [{}]", hew_parser::fmt::format_expression(guard));
        }
        if rule.history {
            label.push_str(" [history]");
        }
        if rule.reenter {
            label.push_str(" [reenter]");
        }
        if rule.target_state.name == sym::UNDERSCORE {
            label.push_str(" [external]");
        }
        if rule.body_form != MachineTransitionBodyForm::Implicit {
            let _ = write!(
                label,
                " / {}",
                hew_parser::fmt::format_expression(&rule.body)
            );
        }
        label
    }

    // This private view shares declaration identities with the public projection.
    // Source expressions are already formatted; graph syntax belongs to Hew.
    fn render(&self, format: &str) -> String {
        let input = json!({
            "format": format,
            "name": self.name,
            "parameters": self.machine.type_params.iter().map(|p| p.name.to_string())
                .chain(self.machine.const_params.iter().map(|p| p.name.to_string())).collect::<Vec<_>>(),
            "has_default": self.machine.has_default,
            "emits": self.machine.emits.iter().map(|event| event.name.to_string()).collect::<Vec<_>>(),
            "roots": self.children(None),
            "nodes": self.nodes.iter().enumerate().map(|(index, node)| json!({
                "id": node.id, "name": node.name,
                "kind": match node.kind { Kind::State => "state", Kind::Compound => "compound", Kind::Region => "region" },
                "parent": node.parent.map_or(-1, |index| i64::try_from(index).expect("node index")),
                "children": self.children(Some(index)),
                "fields": node.fields, "entry": node.entry, "exit": node.exit,
                "parallel": node.parallel, "is_final": node.is_final,
                "initial": node.initial.as_ref().map_or(-1, |(index, _)| i64::try_from(*index).expect("initial index")),
                "initial_value": node.initial.as_ref().and_then(|(_, value)| value.as_deref()),
            })).collect::<Vec<_>>(),
            "edges": self.edges.iter().map(|edge| json!({
                "from": edge.from, "to": edge.to, "label": Self::label(edge.rule),
            })).collect::<Vec<_>>(),
            "auxiliary": self.auxiliary.iter().map(|node| json!({
                "id": node.id, "label": node.label, "computed": node.computed,
            })).collect::<Vec<_>>(),
        });
        render_text(&input.to_string())
    }

    pub(super) fn mermaid(&self) -> String {
        self.render("mermaid")
    }

    pub(super) fn dot(&self) -> String {
        self.render("dot")
    }

    pub(super) fn json(&self) -> Value {
        let fields = |fields: &[(Ident, _)]| {
            fields
                .iter()
                .map(|(name, _)| name.to_string())
                .collect::<Vec<_>>()
        };
        json!({
            "name": self.machine.name.to_string(), "qualifiedName": self.name,
            "hasDefault": self.machine.has_default,
            "typeParams": self.machine.type_params.iter().map(|parameter| parameter.name.to_string()).collect::<Vec<_>>(),
            "emits": self.machine.emits.iter().map(|event| event.name.to_string()).collect::<Vec<_>>(),
            "outputs": self.machine.emits.iter().map(|event| json!({"name": event.name.to_string(), "fields": fields(&event.fields)})).collect::<Vec<_>>(),
            "events": self.machine.events.iter().map(|event| json!({"name": event.name.to_string(), "fields": fields(&event.fields)})).collect::<Vec<_>>(),
            "states": self.nodes.iter().enumerate().filter(|(_, node)| node.kind == Kind::State).map(|(index, node)| self.node_json(index, node)).collect::<Vec<_>>(),
            "composites": self.nodes.iter().enumerate().filter(|(_, node)| node.kind == Kind::Compound).map(|(index, node)| self.node_json(index, node)).collect::<Vec<_>>(),
            "regions": self.nodes.iter().enumerate().filter(|(_, node)| node.kind == Kind::Region).map(|(index, node)| self.node_json(index, node)).collect::<Vec<_>>(),
            "transitions": self.edges.iter().map(|edge| json!({
                "trigger": match edge.rule.trigger { MachineTrigger::Event(_) => "event", MachineTrigger::Completion => "completion", MachineTrigger::Always => "always" },
                "event": edge.rule.trigger.event_name().map(|name| name.to_string()), "from": edge.rule.source_state.to_string(), "to": edge.rule.target_state.to_string(),
                "fromId": edge.from, "toId": edge.to, "scope": edge.scope.map(|scope| self.nodes[scope].id.as_str()),
                "fromPath": edge.rule.source_path.iter().map(ToString::to_string).collect::<Vec<_>>(), "toPath": edge.rule.target_path.iter().map(ToString::to_string).collect::<Vec<_>>(),
                "guarded": edge.rule.guard.is_some(), "guard": edge.rule.guard.as_ref().map(hew_parser::fmt::format_expression),
                "action": (edge.rule.body_form != MachineTransitionBodyForm::Implicit).then(|| hew_parser::fmt::format_expression(&edge.rule.body)),
                "history": edge.rule.history, "reenter": edge.rule.reenter, "selfTransition": edge.rule.source_state == edge.rule.target_state && edge.rule.target_state.name != sym::UNDERSCORE,
                "external": edge.rule.reenter || edge.rule.target_state != edge.rule.source_state || edge.rule.target_state.name == sym::UNDERSCORE,
            })).collect::<Vec<_>>(),
        })
    }

    fn node_json(&self, index: usize, node: &Node) -> Value {
        json!({
            "id": node.id, "name": node.name, "path": self.path(index), "fields": node.fields,
            "parent": node.parent.map(|parent| self.nodes[parent].name.as_str()), "parentId": node.parent.map(|parent| self.nodes[parent].id.as_str()),
            "parallel": node.parallel, "final": node.is_final, "initial": node.initial.as_ref().map(|(initial, _)| self.nodes[*initial].name.as_str()),
            "initialId": node.initial.as_ref().map(|(initial, _)| self.nodes[*initial].id.as_str()),
            "initialValue": node.initial.as_ref().and_then(|(_, value)| value.as_deref()),
            "members": self.children(Some(index)).iter().filter(|child| self.nodes[**child].kind != Kind::Region).map(|child| self.nodes[*child].name.as_str()).collect::<Vec<_>>(),
            "regions": self.children(Some(index)).iter().filter(|child| self.nodes[**child].kind == Kind::Region).map(|child| self.node_json(*child, &self.nodes[*child])).collect::<Vec<_>>(),
            "hasEntry": node.entry.is_some(), "hasExit": node.exit.is_some(), "entry": node.entry, "exit": node.exit,
        })
    }

    pub(super) fn list(&self) -> String {
        let mut out = format!("machine {} {{\n  States:\n", self.name);
        for child in self.children(None) {
            self.list_node(&mut out, child, 1);
        }
        out.push_str("  Events:\n");
        for event in &self.machine.events {
            let fields = event
                .fields
                .iter()
                .map(|(name, _)| name.to_string())
                .collect::<Vec<_>>();
            line(
                &mut out,
                1,
                if fields.is_empty() {
                    event.name.to_string()
                } else {
                    format!("{} {{ {} }}", event.name, fields.join(", "))
                },
            );
        }
        let _ = writeln!(out, "  Transitions: {}", self.edges.len());
        if self.machine.has_default {
            out.push_str("  Default: unhandled events stay in current state\n");
        }
        if !self.machine.emits.is_empty() {
            let _ = writeln!(
                out,
                "  Emits: {}",
                self.machine
                    .emits
                    .iter()
                    .map(|event| event.name.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            );
        }
        out.push_str("}\n\n");
        out
    }

    fn list_node(&self, out: &mut String, index: usize, depth: usize) {
        let node = &self.nodes[index];
        let kind = match node.kind {
            Kind::State if node.is_final => "final ",
            Kind::State => "",
            Kind::Compound if node.parallel => "parallel ",
            Kind::Compound => "compound ",
            Kind::Region => "region ",
        };
        let fields = if node.fields.is_empty() {
            String::new()
        } else {
            format!(" {{ {} }}", node.fields.join(", "))
        };
        line(out, depth, format!("{kind}{}{fields}", node.name));
        for child in self.children(Some(index)) {
            self.list_node(out, child, depth + 1);
        }
    }
}

// The checked export and its C declaration are generated together in build.rs.
// This synchronous string -> string signature requires no scheduler handle.
#[allow(
    improper_ctypes,
    reason = "HewError is an opaque C owner; its Rust contents never cross the boundary"
)]
unsafe extern "C" {
    fn machine_render_text(
        input: *const HewText,
        output: *mut *mut HewText,
        error: *mut *mut HewError,
    ) -> i32;
}

fn render_text(input: &str) -> String {
    // SAFETY: build.rs checks this generated export's borrowed-string contract.
    unsafe { crate::host_text::call(input, machine_render_text) }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn final_and_internal_rules_use_their_authored_trigger_kinds() {
        let parsed = hew_parser::parse(
            r"
machine Delivery {
    events { Completion; }
    state Ready;
    state Running { initial state Waiting; final state Sent; }
    final state Finished;
    on Completion: Ready => Running;
    on completion: Running => Finished;
    always: Ready => Finished when false;
    default { state }
}
",
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let hew_parser::ast::Item::Machine(machine) = &parsed.program.items[0].0 else {
            panic!("machine");
        };
        let chart = Chart::new(machine, "Delivery");
        let json = chart.json();
        let rules = json["transitions"].as_array().unwrap();
        assert_eq!(rules[0]["trigger"], "event");
        assert_eq!(rules[0]["event"], "Completion");
        assert_eq!(rules[1]["trigger"], "completion");
        assert_eq!(rules[2]["trigger"], "always");
        assert!(rules[1]["event"].is_null() && rules[2]["event"].is_null());
        assert!(json["states"]
            .as_array()
            .unwrap()
            .iter()
            .any(|node| node["name"] == "Sent" && node["final"] == true));
        let mermaid = chart.mermaid();
        assert!(mermaid.contains(": completion"), "{mermaid}");
        assert!(mermaid.contains(": always [false]"), "{mermaid}");
        assert!(chart.dot().contains("peripheries=2"));
        assert!(chart.list().contains("final Sent"));
    }

    #[test]
    fn source_logic_is_escaped_at_each_render_boundary() {
        let parsed = hew_parser::parse(
            r#"
machine Quoted<T, const N: usize> {
    events { Send { text: string; } }
    state Idle { entry { let text = "a\"b<|%"; } }
    state Done;
    on Send { text }: Idle => Done when text == "a\"b<|%" { Done }
    default { state }
}
"#,
        );
        assert!(parsed.errors.is_empty(), "{:?}", parsed.errors);
        let hew_parser::ast::Item::Machine(machine) = &parsed.program.items[0].0 else {
            panic!("machine");
        };
        let chart = Chart::new(machine, "Quoted");
        let json = chart.json();
        let guard = json["transitions"][0]["guard"].as_str().unwrap();
        assert!(guard.contains("a\\\"b<|%"), "{guard}");
        assert!(json["states"][0]["entry"]
            .as_str()
            .unwrap()
            .contains("let text"));
        let mermaid = chart.mermaid();
        assert!(mermaid.contains("title: \"Quoted<T, N>\""), "{mermaid}");
        assert!(mermaid.contains("Send { text }"), "{mermaid}");
        assert!(mermaid.contains("&lt;&#124;&#37;"), "{mermaid}");
        assert!(mermaid.contains("&quot;"), "{mermaid}");
        let dot = chart.dot();
        assert!(dot.contains("\\\""), "{dot}");
        assert!(!dot.contains("initial_root"));
    }
}
