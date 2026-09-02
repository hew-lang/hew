//! Publication of one declared record shape into the layout tables.
//!
//! The layout tables are keyed by resolved SPELLING, while a declaration has
//! one identity. Those coincide for every ordinary module, but a peer file of
//! a directory module that is also importable in its own right is reachable
//! as `pkg.Response` and as `pkg.file.Response`, and which spelling reaches a
//! use site depends on the import route the program took. Publishing under
//! both keeps the field-order and layout lookups total without teaching every
//! consumer to re-resolve a name.

use hew_types::ResolvedTy;

use crate::model::RecordLayout;

/// The spelling a declaration's defining module publishes beside its own
/// render, when the two differ.
pub(super) fn defining_module_alias(
    defining_module: Option<&String>,
    name: &str,
    key: &str,
) -> Option<String> {
    defining_module
        .map(|module| format!("{module}.{name}"))
        .filter(|alias| alias != key)
}

/// Publish one monomorphic record shape under `key` and, when the declaration
/// answers to a second spelling, under that too.
pub(super) fn publish<S: std::hash::BuildHasher>(
    key: String,
    alias: Option<String>,
    fields: Vec<(String, ResolvedTy)>,
    with_layout: bool,
    record_layouts: &mut Vec<RecordLayout>,
    record_field_orders: &mut std::collections::HashMap<String, Vec<(String, ResolvedTy)>, S>,
) {
    let layout = |name: String| RecordLayout {
        name,
        field_tys: fields.iter().map(|(_, ty)| ty.clone()).collect(),
        field_names: fields.iter().map(|(name, _)| name.clone()).collect(),
    };
    if with_layout {
        record_layouts.push(layout(key.clone()));
    }
    if let Some(alias) = alias {
        if with_layout {
            record_layouts.push(layout(alias.clone()));
        }
        record_field_orders.insert(alias, fields.clone());
    }
    record_field_orders.insert(key, fields);
}
