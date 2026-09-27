//! Textual dumps of checker-owned tables for `hew tool compile --dump-*`.

use std::fmt::Write as _;

use crate::DefTable;

/// Render every module and declaration row of `defs` in mint order: the
/// `--dump-defs` inspection. The output depends only on the source set, so
/// two compiles of one program print the same text.
#[must_use]
pub fn dump_defs(defs: &DefTable) -> String {
    let mut out = String::from("modules\n");
    for (module, path) in defs.modules() {
        writeln!(out, "  {module:?} {path}").expect("write to string");
    }
    out.push_str("defs\n");
    for id in defs.ids() {
        write!(out, "  {id:?} {:?} {}", defs.kind(id), defs.name(id)).expect("write to string");
        if let Some(module) = defs.module(id) {
            write!(out, " module={module:?}").expect("write to string");
        }
        if let Some(owner) = defs.owner(id) {
            write!(out, " owner={owner:?}").expect("write to string");
        }
        if let Some(site) = defs.site(id) {
            let span = site.span();
            write!(out, " site={}..{}", span.start, span.end).expect("write to string");
        }
        writeln!(out, " path={}", defs.path(id)).expect("write to string");
    }
    out
}
