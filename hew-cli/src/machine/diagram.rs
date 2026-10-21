//! Authored machine presentation. This projection records scopes and source
//! logic; transition selection remains the checker's/evaluator's responsibility.
use std::collections::HashMap;
use std::fmt::Write as _;

use hew_parser::ast::{
    sym, Block, Expr, Ident, MachineDecl, MachineTransition, MachineTransitionBodyForm,
    MachineTrigger,
};
use serde_json::{json, Value};

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

/// Both renderers receive the same formatted source. Escape only at the output
/// boundary so source quotes, comments and control punctuation stay data.
fn mermaid_text(text: &str) -> String {
    text.replace('&', "&amp;")
        .replace('"', "&quot;")
        .replace('<', "&lt;")
        .replace('>', "&gt;")
        .replace('%', "&#37;")
        .replace('|', "&#124;")
        .replace('\n', "<br/>")
        .replace('\r', "")
}

fn dot_text(text: &str) -> String {
    text.replace('\\', "\\\\")
        .replace('"', "\\\"")
        .replace('\n', "\\n")
        .replace('\r', "")
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

    fn annotations(&self, index: usize) -> Vec<String> {
        let node = &self.nodes[index];
        let mut lines = Vec::new();
        if node.is_final {
            lines.push("final".into());
        }
        if !node.fields.is_empty() {
            lines.push(format!("fields: {}", node.fields.join(", ")));
        }
        if let Some(entry) = &node.entry {
            lines.push(format!("entry {entry}"));
        }
        if let Some(exit) = &node.exit {
            lines.push(format!("exit {exit}"));
        }
        lines
    }

    fn legend(&self) -> Vec<String> {
        let mut lines = Vec::new();
        if self.machine.has_default {
            lines.push("unhandled events stay in current state".into());
        }
        if !self.machine.emits.is_empty() {
            lines.push(format!(
                "Emits: {}",
                self.machine
                    .emits
                    .iter()
                    .map(|event| event.name.to_string())
                    .collect::<Vec<_>>()
                    .join(", ")
            ));
        }
        lines
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

    pub(super) fn mermaid(&self) -> String {
        let mut out = String::new();
        let parameters = self
            .machine
            .type_params
            .iter()
            .map(|parameter| parameter.name.to_string())
            .chain(
                self.machine
                    .const_params
                    .iter()
                    .map(|parameter| parameter.name.to_string()),
            )
            .collect::<Vec<_>>();
        let title = if parameters.is_empty() {
            self.name.to_string()
        } else {
            format!("{}<{}>", self.name, parameters.join(", "))
        };
        line(&mut out, 0, "---");
        line(&mut out, 0, format!("title: {}", json!(title)));
        line(&mut out, 0, "---");
        line(&mut out, 0, "stateDiagram-v2");
        for child in self.children(None) {
            self.mermaid_node(&mut out, child, 1);
        }
        for node in &self.auxiliary {
            line(
                &mut out,
                1,
                format!("state \"{}\" as {}", mermaid_text(&node.label), node.id),
            );
            if node.computed {
                line(&mut out, 1, format!("state {} <<choice>>", node.id));
            }
        }
        let legend = self.legend();
        if !legend.is_empty() {
            line(&mut out, 1, "state \"Machine rules\" as machine_rules");
            line(&mut out, 1, "note right of machine_rules");
            line(&mut out, 2, mermaid_text(&legend.join("\n")));
            line(&mut out, 1, "end note");
        }
        for edge in &self.edges {
            line(
                &mut out,
                1,
                format!(
                    "{} --> {} : {}",
                    edge.from,
                    edge.to,
                    mermaid_text(&Self::label(edge.rule))
                ),
            );
        }
        for (index, node) in self.nodes.iter().enumerate() {
            let annotations = self.annotations(index);
            if !annotations.is_empty() {
                line(&mut out, 1, format!("note right of {}", node.id));
                line(&mut out, 2, mermaid_text(&annotations.join("\n")));
                line(&mut out, 1, "end note");
            }
        }
        out
    }

    fn mermaid_node(&self, out: &mut String, index: usize, depth: usize) {
        let node = &self.nodes[index];
        let label = if node.kind == Kind::Region {
            format!("region {}", node.name)
        } else {
            node.name.clone()
        };
        if node.kind == Kind::State {
            line(
                out,
                depth,
                format!("state \"{}\" as {}", mermaid_text(&label), node.id),
            );
            return;
        }
        line(
            out,
            depth,
            format!("state \"{}\" as {} {{", mermaid_text(&label), node.id),
        );
        if let Some((initial, value)) = &node.initial {
            let label = value
                .as_ref()
                .map_or(String::new(), |value| format!(" : {}", mermaid_text(value)));
            line(
                out,
                depth + 1,
                format!("[*] --> {}{label}", self.nodes[*initial].id),
            );
        }
        for (ordinal, child) in self.children(Some(index)).into_iter().enumerate() {
            if node.parallel && ordinal > 0 {
                line(out, depth + 1, "--");
            }
            self.mermaid_node(out, child, depth + 1);
        }
        line(out, depth, "}");
    }

    pub(super) fn dot(&self) -> String {
        let mut out = String::new();
        line(
            &mut out,
            0,
            format!("digraph \"{}\" {{", dot_text(self.name)),
        );
        line(
            &mut out,
            1,
            "rankdir=LR; compound=true; node [shape=box, style=rounded];",
        );
        line(
            &mut out,
            1,
            format!("label=\"{}\"; labelloc=t;", dot_text(self.name)),
        );
        for child in self.children(None) {
            self.dot_node(&mut out, child, 1);
        }
        for node in &self.auxiliary {
            line(
                &mut out,
                1,
                format!(
                    "{} [label=\"{}\", shape={}];",
                    node.id,
                    dot_text(&node.label),
                    if node.computed { "diamond" } else { "note" }
                ),
            );
        }
        let legend = self.legend();
        if !legend.is_empty() {
            line(
                &mut out,
                1,
                format!(
                    "machine_rules [shape=note, label=\"{}\"];",
                    dot_text(&legend.join("\n"))
                ),
            );
        }
        for edge in &self.edges {
            line(
                &mut out,
                1,
                format!(
                    "{} -> {} [label=\"{}\" {} {}];",
                    edge.from,
                    edge.to,
                    dot_text(&Self::label(edge.rule)),
                    self.flow_cluster_attribute("ltail", &edge.from, &edge.to),
                    self.flow_cluster_attribute("lhead", &edge.to, &edge.from)
                ),
            );
        }
        line(&mut out, 0, "}");
        out
    }

    fn cluster_attribute(&self, key: &str, id: &str) -> String {
        self.nodes
            .iter()
            .find(|node| node.id == id && node.kind != Kind::State)
            .map_or(String::new(), |node| format!("{key}=cluster_{}", node.id))
    }

    fn flow_cluster_attribute(&self, key: &str, id: &str, other: &str) -> String {
        if let Some(index) = self.nodes.iter().position(|node| node.id == id) {
            let mut current = self.nodes.iter().position(|node| node.id == other);
            while let Some(other) = current {
                if index == other {
                    return String::new();
                }
                current = self.nodes[other].parent;
            }
        }
        self.cluster_attribute(key, id)
    }

    fn dot_node(&self, out: &mut String, index: usize, depth: usize) {
        let node = &self.nodes[index];
        let mut annotations = vec![if node.kind == Kind::Region {
            format!("region {}", node.name)
        } else {
            node.name.clone()
        }];
        if node.parallel {
            annotations.push("parallel".into());
        }
        annotations.extend(self.annotations(index));
        let label = dot_text(&annotations.join("\n"));
        if node.kind == Kind::State {
            line(out, depth, format!("{} [label=\"{label}\"];", node.id));
            if node.is_final {
                line(out, depth, format!("{} [peripheries=2];", node.id));
            }
            return;
        }
        line(out, depth, format!("subgraph cluster_{} {{", node.id));
        line(
            out,
            depth + 1,
            format!(
                "label=\"{label}\"; style=\"{}\";",
                if node.kind == Kind::Region {
                    "dashed"
                } else {
                    "rounded"
                }
            ),
        );
        line(
            out,
            depth + 1,
            format!(
                "{} [label=\"\", shape=point, width=0.01, style=invis];",
                node.id
            ),
        );
        if let Some((initial, value)) = &node.initial {
            line(
                out,
                depth + 1,
                format!("initial_{} [shape=point, width=0.1];", node.id),
            );
            line(
                out,
                depth + 1,
                format!(
                    "initial_{} -> {} [label=\"{}\" {}];",
                    node.id,
                    self.nodes[*initial].id,
                    dot_text(value.as_deref().unwrap_or("")),
                    self.cluster_attribute("lhead", &self.nodes[*initial].id)
                ),
            );
        }
        for child in self.children(Some(index)) {
            self.dot_node(out, child, depth + 1);
        }
        line(out, depth, "}");
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
machine Quoted {
    events { Send { text: string; } }
    state Idle { entry { let text = "a\"b<|%"; } }
    state Done;
    on Send: Idle => Done when event.text == "a\"b<|%" { Done }
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
        assert!(mermaid.contains("&lt;&#124;&#37;"), "{mermaid}");
        assert!(mermaid.contains("&quot;"), "{mermaid}");
        let dot = chart.dot();
        assert!(dot.contains("\\\""), "{dot}");
        assert!(!dot.contains("initial_root"));
    }
}
