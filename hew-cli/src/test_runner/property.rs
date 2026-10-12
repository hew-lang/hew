//! Structural strategies use proptest's seeded runner and shrinking engine.

use hew_codec::property::{Key, Node, Shape};
use hew_codec::Value;
use proptest::prelude::*;

fn key_value(key: &Key) -> Value {
    key.tag.map_or_else(
        || Value::Str(key.name.clone()),
        |tag| Value::Int(i128::from(tag)),
    )
}

pub(super) fn strategy(shape: &Shape) -> Result<BoxedStrategy<Value>, String> {
    at(shape, shape.root, 64, &[])
}

#[expect(
    clippy::too_many_lines,
    reason = "one exhaustive shape match preserves the recursive generation budget"
)]
fn at(
    shape: &Shape,
    id: usize,
    budget: usize,
    ancestors: &[usize],
) -> Result<BoxedStrategy<Value>, String> {
    let node = shape
        .nodes
        .get(id)
        .ok_or("property shape has an invalid child")?;
    let mut ancestors = ancestors.to_vec();
    ancestors.push(id);
    let remaining = budget.saturating_sub(1);
    // Split the expansion budget among siblings, so recursive collections do
    // not multiply it. Required acyclic fields remain representable at zero.
    let child = |id, allowance| {
        if ancestors.contains(&id) && allowance == 0 {
            Err("property shape has no bounded value".to_string())
        } else {
            at(shape, id, allowance, &ancestors)
        }
    };
    Ok(match node {
        Node::Unit => Just(Value::Null).boxed(),
        Node::Bool => any::<bool>().prop_map(Value::Bool).boxed(),
        Node::Signed { min, max } => (*min..=*max)
            .prop_map(|n| Value::Int(i128::from(n)))
            .boxed(),
        Node::Unsigned { max } => (0..=*max).prop_map(|n| Value::Int(i128::from(n))).boxed(),
        Node::Float { bits: 32 } => any::<f32>()
            .prop_map(|value| Value::Float(f64::from(value)))
            .boxed(),
        Node::Float { bits: 64 } => any::<f64>().prop_map(Value::Float).boxed(),
        Node::Float { .. } => return Err("property shape has an invalid float width".into()),
        Node::Char => any::<char>()
            .prop_map(|ch| Value::Str(ch.to_string()))
            .boxed(),
        Node::String => proptest::collection::vec(any::<char>(), 0..32)
            .prop_map(|chars| Value::Str(chars.into_iter().collect()))
            .boxed(),
        Node::Bytes => proptest::collection::vec(any::<u8>(), 0..32)
            .prop_map(Value::Bytes)
            .boxed(),
        Node::Tuple(fields) => fields
            .iter()
            .map(|id| child(*id, remaining / fields.len().max(1)))
            .collect::<Result<Vec<_>, _>>()?
            .prop_map(Value::Seq)
            .boxed(),
        Node::Array { len: 0, .. } => Just(Value::Seq(Vec::new())).boxed(),
        Node::Array { element, len } => {
            proptest::collection::vec(child(*element, remaining / len)?, *len)
                .prop_map(Value::Seq)
                .boxed()
        }
        Node::Sequence(element) | Node::Set(element) => {
            let length = remaining.min(7);
            if let Some(element) = (length > 0)
                .then(|| child(*element, remaining / length))
                .and_then(Result::ok)
            {
                let unique = matches!(node, Node::Set(_));
                proptest::collection::vec(element, 0..=length)
                    .prop_map(move |values| {
                        let mut elements = Vec::new();
                        for value in values {
                            if !unique || !elements.contains(&value) {
                                elements.push(value);
                            }
                        }
                        Value::Seq(elements)
                    })
                    .boxed()
            } else {
                Just(Value::Seq(Vec::new())).boxed()
            }
        }
        Node::Map { key, value } => {
            let length = (remaining / 2).min(7);
            let children = (length > 0).then(|| {
                let allowance = remaining / (length * 2);
                Ok::<_, String>((child(*key, allowance)?, child(*value, allowance)?))
            });
            if let Some(Ok(children)) = children {
                proptest::collection::vec(children, 0..=length)
                    .prop_map(|entries| {
                        let mut unique = Vec::new();
                        for (key, value) in entries {
                            if !unique.iter().any(|(other, _)| other == &key) {
                                unique.push((key, value));
                            }
                        }
                        Value::Map(unique)
                    })
                    .boxed()
            } else {
                Just(Value::Map(Vec::new())).boxed()
            }
        }
        Node::Record(fields) => {
            let keys = fields
                .iter()
                .map(|(key, _)| key_value(key))
                .collect::<Vec<_>>();
            fields
                .iter()
                .map(|(_, id)| child(*id, remaining / fields.len().max(1)))
                .collect::<Result<Vec<_>, _>>()?
                .prop_map(move |values| Value::Map(keys.iter().cloned().zip(values).collect()))
                .boxed()
        }
        Node::Variant(variants) => {
            let choices = variants
                .iter()
                .filter_map(|(key, payload)| {
                    let key = key_value(key);
                    Some(match payload {
                        None => Just(key).boxed(),
                        Some(id) => child(*id, remaining)
                            .ok()?
                            .prop_map(move |value| Value::Map(vec![(key.clone(), value)]))
                            .boxed(),
                    })
                })
                .collect::<Vec<_>>();
            if choices.is_empty() {
                return Err("property enum has no bounded variant".into());
            }
            proptest::strategy::Union::new(choices).boxed()
        }
        Node::Option(element) => {
            if let Ok(element) = child(*element, remaining) {
                proptest::option::of(element)
                    .prop_map(|value| value.unwrap_or(Value::Null))
                    .boxed()
            } else {
                Just(Value::Null).boxed()
            }
        }
    })
}
