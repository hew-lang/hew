//! `--explain-cow` renderer.
//!
//! Actor-send mechanism facts are authored by MIR. The checker-owned renderer
//! was removed with its stale alias/copy side table; MIR mode rendering is
//! installed with outbound mode resolution.

/// Keep the command surface inert until MIR-authored outbound modes are
/// available to the check pipeline.
pub fn render_explain_cow(_source: &str, _filename: &str, _out: &mut dyn std::io::Write) {}
