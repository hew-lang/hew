//! Typed authority and checked-output generator for Hew's WASM capability policy.
//!
//! `wasm-capability-manifest.toml` is the only editable source of reject/warn
//! feature identity, module exclusions, and curated playground WASI overrides.
//! This crate validates that authority and renders the Rust, JSON, and
//! documentation-table consumers.

use serde::Deserialize;
use std::collections::{BTreeMap, BTreeSet};
use std::fmt::Write as _;
use std::path::{Path, PathBuf};

/// Current manifest schema version.
pub const MANIFEST_VERSION: u32 = 1;

/// Repository-relative path of the generated checker authority.
pub const RUST_OUTPUT: &str = "hew-types/src/wasm_capabilities_generated.rs";

/// Repository-relative path of the generated playground WASI decisions.
pub const PLAYGROUND_OUTPUT: &str = "examples/playground/wasm-capabilities.json";

/// Repository-relative path whose feature-policy table is generated.
pub const MATRIX_OUTPUT: &str = "docs/wasm-capability-matrix.md";

/// Fully typed manifest schema.
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Manifest {
    /// Schema version.
    pub manifest_version: u32,
    /// Target tier descriptors.
    #[serde(default, rename = "tier")]
    pub tiers: Vec<Tier>,
    /// Feature disposition rows.
    #[serde(default, rename = "feature")]
    pub features: Vec<Feature>,
    /// WASM backlog rows.
    #[serde(default, rename = "backlog")]
    pub backlog: Vec<Backlog>,
    /// Curated playground entries that are not currently WASI-runnable.
    #[serde(default, rename = "playground_wasi")]
    pub playground_wasi: Vec<PlaygroundWasi>,
}

/// A compilation target tier.
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Tier {
    pub id: String,
    pub name: String,
    #[serde(rename = "crate")]
    pub crate_name: String,
    pub target: String,
    pub target_alias: Option<String>,
    pub toolchain: Option<String>,
    pub use_case: String,
    pub runnable: bool,
    pub capability: Option<String>,
}

/// Checker disposition of a feature row.
#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum CheckerDisposition {
    Pass,
    Warn,
    Reject,
    Todo,
}

impl CheckerDisposition {
    fn is_checker_feature(self) -> bool {
        matches!(self, Self::Warn | Self::Reject)
    }
}

/// Runtime support state documented for a feature.
#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum RuntimeDisposition {
    Implemented,
    HostDependent,
    NativeOnly,
    Trap,
    Unimplemented,
    Cooperative,
    Unavailable,
}

/// A feature-disposition row.
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Feature {
    pub id: String,
    pub enum_variant: Option<String>,
    pub label: String,
    pub prose_label: Option<String>,
    pub reason: Option<String>,
    pub checker: CheckerDisposition,
    pub runtime: RuntimeDisposition,
    pub runtime_status: String,
    #[serde(default)]
    pub categories: Vec<String>,
    pub checker_detail: Option<String>,
    pub tracking: Option<String>,
    pub tracking_label: Option<String>,
    pub note: Option<String>,
    #[serde(default)]
    pub native_only_modules: Vec<String>,
    /// Exact `module.function` entries whose runtime substrate is absent on
    /// wasm32 even though their containing module is otherwise supported.
    #[serde(default)]
    pub native_only_functions: Vec<String>,
}

/// Source-marker policy for a backlog row.
#[derive(Debug, Clone, Copy, Default, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "kebab-case")]
pub enum MarkerDisposition {
    #[default]
    Source,
    NonSource,
}

/// A WASM-TODO backlog row.
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct Backlog {
    pub id: String,
    pub gap: String,
    pub blocker: String,
    pub tracking_label: String,
    #[serde(default)]
    pub marker_disposition: MarkerDisposition,
    pub non_source_reason: Option<String>,
}

/// Manifest-owned non-runnable playground decision.
#[derive(Debug, Clone, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct PlaygroundWasi {
    pub id: String,
    pub status: PlaygroundWasiStatus,
    pub capability: Option<String>,
    pub reason: String,
}

/// Playground WASI status. Pass/runnable rows are deliberately not admitted:
/// they remain proven by the real E2E loop rather than declared by authority.
#[derive(Debug, Clone, Copy, Deserialize, PartialEq, Eq)]
#[serde(rename_all = "snake_case")]
pub enum PlaygroundWasiStatus {
    Unsupported,
}

/// A generated repository output.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GeneratedOutput {
    pub path: &'static str,
    pub contents: String,
}

impl Manifest {
    /// Parse and validate a manifest.
    ///
    /// # Errors
    ///
    /// Returns a TOML schema or cross-row validation diagnostic.
    pub fn parse(src: &str) -> Result<Self, String> {
        let manifest: Self = toml::from_str(src).map_err(|err| err.to_string())?;
        manifest.validate()?;
        Ok(manifest)
    }

    /// Validate cross-row invariants before any output is rendered.
    ///
    /// # Errors
    ///
    /// Returns the first schema-version, identity, disposition, module, marker,
    /// or playground-reference invariant violation.
    #[allow(
        clippy::too_many_lines,
        reason = "authority validation is a single ordered fail-closed pass over the four row families"
    )]
    pub fn validate(&self) -> Result<(), String> {
        if self.manifest_version != MANIFEST_VERSION {
            return Err(format!(
                "manifest_version must be {MANIFEST_VERSION}, got {}",
                self.manifest_version
            ));
        }
        unique(self.tiers.iter().map(|row| row.id.as_str()), "tier id")?;
        unique(
            self.features.iter().map(|row| row.id.as_str()),
            "feature id",
        )?;
        unique(self.backlog.iter().map(|row| row.id.as_str()), "backlog id")?;
        unique(
            self.playground_wasi.iter().map(|row| row.id.as_str()),
            "playground_wasi id",
        )?;

        let mut variants = BTreeSet::new();
        let mut modules = BTreeSet::new();
        let mut functions = BTreeSet::new();
        for feature in &self.features {
            validate_kebab_id(&feature.id, "feature id")?;
            match feature.checker {
                CheckerDisposition::Warn | CheckerDisposition::Reject => {
                    let variant = feature.enum_variant.as_deref().ok_or_else(|| {
                        format!(
                            "feature `{}` with {:?} disposition requires enum_variant",
                            feature.id, feature.checker
                        )
                    })?;
                    validate_rust_variant(variant)?;
                    if !variants.insert(variant) {
                        return Err(format!("duplicate checker enum_variant `{variant}`"));
                    }
                    if feature.reason.as_deref().is_none_or(str::is_empty) {
                        return Err(format!(
                            "feature `{}` with {:?} disposition requires reason",
                            feature.id, feature.checker
                        ));
                    }
                }
                CheckerDisposition::Pass | CheckerDisposition::Todo => {
                    if let Some(variant) = &feature.enum_variant {
                        return Err(format!(
                            "feature `{}` with {:?} disposition must not declare enum_variant `{variant}`",
                            feature.id, feature.checker
                        ));
                    }
                    if feature.checker == CheckerDisposition::Todo
                        && (feature.note.as_deref().is_none_or(str::is_empty)
                            || feature.tracking.as_deref().is_none_or(str::is_empty))
                    {
                        return Err(format!(
                            "todo feature `{}` requires note and tracking",
                            feature.id
                        ));
                    }
                }
            }
            match feature.checker {
                CheckerDisposition::Warn if feature.runtime != RuntimeDisposition::Cooperative => {
                    return Err(format!(
                        "warning feature `{}` must have cooperative runtime disposition",
                        feature.id
                    ));
                }
                CheckerDisposition::Reject
                    if matches!(
                        feature.runtime,
                        RuntimeDisposition::Implemented
                            | RuntimeDisposition::HostDependent
                            | RuntimeDisposition::Cooperative
                    ) =>
                {
                    return Err(format!(
                        "reject feature `{}` has runnable runtime disposition {:?}",
                        feature.id, feature.runtime
                    ));
                }
                _ => {}
            }
            if (!feature.native_only_modules.is_empty()
                || !feature.native_only_functions.is_empty())
                && feature.checker != CheckerDisposition::Reject
            {
                return Err(format!(
                    "feature `{}` declares native-only source identities but is not reject",
                    feature.id
                ));
            }
            for module in &feature.native_only_modules {
                if module.is_empty()
                    || !module
                        .chars()
                        .all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
                {
                    return Err(format!(
                        "feature `{}` has invalid native-only module `{module}`",
                        feature.id
                    ));
                }
                if !modules.insert(module) {
                    return Err(format!("duplicate native-only module `{module}`"));
                }
            }
            for function in &feature.native_only_functions {
                let Some((module, method)) = function.rsplit_once('.') else {
                    return Err(format!(
                        "feature `{}` has invalid native-only function `{function}`",
                        feature.id
                    ));
                };
                if module.is_empty()
                    || method.is_empty()
                    || !module.split('.').all(|segment| {
                        !segment.is_empty()
                            && segment
                                .chars()
                                .all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
                    })
                    || !method
                        .chars()
                        .all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
                {
                    return Err(format!(
                        "feature `{}` has invalid native-only function `{function}`",
                        feature.id
                    ));
                }
                if !functions.insert(function) {
                    return Err(format!("duplicate native-only function `{function}`"));
                }
            }
        }

        for backlog in &self.backlog {
            validate_kebab_id(&backlog.id, "backlog id")?;
            match (backlog.marker_disposition, &backlog.non_source_reason) {
                (MarkerDisposition::Source, Some(_)) => {
                    return Err(format!(
                        "source backlog `{}` must not declare non_source_reason",
                        backlog.id
                    ));
                }
                (MarkerDisposition::NonSource, None) => {
                    return Err(format!(
                        "non-source backlog `{}` requires non_source_reason",
                        backlog.id
                    ));
                }
                _ => {}
            }
        }

        let known_ids: BTreeSet<&str> = self
            .features
            .iter()
            .map(|row| row.id.as_str())
            .chain(self.backlog.iter().map(|row| row.id.as_str()))
            .collect();
        for row in &self.playground_wasi {
            if row.reason.trim().is_empty() {
                return Err(format!(
                    "playground_wasi `{}` requires a non-empty reason",
                    row.id
                ));
            }
            if let Some(capability) = row.capability.as_deref() {
                if !known_ids.contains(capability) {
                    return Err(format!(
                        "playground_wasi `{}` references unknown capability `{capability}`",
                        row.id
                    ));
                }
                let pass_only = self.features.iter().any(|feature| {
                    feature.id == capability && feature.checker == CheckerDisposition::Pass
                }) && !self.backlog.iter().any(|entry| entry.id == capability);
                if pass_only {
                    return Err(format!(
                        "playground_wasi `{}` cannot use pass capability `{capability}` as an unsupported decision",
                        row.id
                    ));
                }
            }
        }
        Ok(())
    }

    /// Render every checked-in consumer of the manifest.
    #[must_use]
    pub fn generated_outputs(&self) -> Vec<GeneratedOutput> {
        vec![
            GeneratedOutput {
                path: RUST_OUTPUT,
                contents: self.render_rust(),
            },
            GeneratedOutput {
                path: PLAYGROUND_OUTPUT,
                contents: self.render_playground_json(),
            },
        ]
    }

    /// Render the complete manifest-owned feature-policy table.
    ///
    /// Every cell is derived from typed manifest data. The surrounding prose
    /// remains hand-authored, while [`write_outputs`] replaces this table and
    /// [`stale_outputs`] checks it byte-for-byte.
    ///
    /// # Panics
    ///
    /// Panics only if called on a manually constructed, unvalidated manifest
    /// whose warn/reject row omits its required enum variant. [`Manifest::parse`]
    /// rejects that state.
    #[must_use]
    pub fn render_feature_policy_table(&self) -> String {
        let mut out = String::from(
            "| ID | Feature surface | Diagnostic label | Checker disposition | Diagnostic reason | Runtime status | Tracking |\n\
             |----|-----------------|------------------|---------------------|-------------------|----------------|----------|\n",
        );
        for feature in &self.features {
            let surface = feature.prose_label.as_deref().unwrap_or(&feature.label);
            let reason = feature.reason.as_deref().unwrap_or("—");
            let tracking = feature.tracking_label.as_deref().unwrap_or("—");
            let disposition = feature.checker_detail.clone().unwrap_or_else(|| {
                let variant = feature.enum_variant.as_deref();
                match feature.checker {
                    CheckerDisposition::Pass => "Pass".to_string(),
                    CheckerDisposition::Todo => "WASM-TODO (not checker-gated)".to_string(),
                    CheckerDisposition::Warn => {
                        format!("Warn (`{}`)", variant.expect("validated warn variant"))
                    }
                    CheckerDisposition::Reject => {
                        format!("Reject (`{}`)", variant.expect("validated reject variant"))
                    }
                }
            });
            writeln!(
                out,
                "| `{}` | {} | {} | {} | {} | {} | {} |",
                markdown_cell(&feature.id),
                markdown_cell(surface),
                markdown_cell(&feature.label),
                markdown_cell(&disposition),
                markdown_cell(reason),
                markdown_cell(&feature.runtime_status),
                markdown_cell(tracking),
            )
            .expect("String write");
        }
        out
    }

    fn checker_features(&self) -> impl Iterator<Item = &Feature> {
        self.features
            .iter()
            .filter(|feature| feature.checker.is_checker_feature())
    }

    #[allow(
        clippy::too_many_lines,
        reason = "the generated Rust carrier is intentionally rendered in one deterministic pass"
    )]
    fn render_rust(&self) -> String {
        let checker_features: Vec<_> = self.checker_features().collect();
        let mut out = String::from(
            "// @generated by `cargo run -p hew-capability-gen`; do not edit.\n\
             // Source: wasm-capability-manifest.toml\n\n\
             #[rustfmt::skip]\n\
             mod generated {\n\
             /// Stable identity of a manifest-owned WASM capability.\n\
             #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]\n\
             pub struct WasmCapabilityId(&'static str);\n\n\
             impl WasmCapabilityId {\n\
                 /// Return the stable kebab-case manifest identity.\n\
                 #[must_use]\n\
                 pub const fn as_str(self) -> &'static str { self.0 }\n\
             }\n\n\
             impl std::fmt::Display for WasmCapabilityId {\n\
                 fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {\n\
                     f.write_str(self.0)\n\
                 }\n\
             }\n\n\
             /// Manifest-owned checker disposition.\n\
             #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]\n\
             pub enum WasmFeatureDisposition { Warn, Reject }\n\n\
             /// Exhaustive reject/warn feature carrier generated from the manifest.\n\
             #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]\n\
             pub enum WasmUnsupportedFeature {\n",
        );
        for feature in &checker_features {
            let variant = feature.enum_variant.as_deref().expect("validated variant");
            writeln!(out, "    {variant},").expect("String write");
        }
        out.push_str("}\n\n#[allow(clippy::match_same_arms, reason = \"one generated arm per manifest row keeps identity and mutation coverage explicit\")]\nimpl WasmUnsupportedFeature {\n    /// Every reject/warn variant, in manifest order.\n    pub const ALL: &'static [Self] = &[\n");
        for feature in &checker_features {
            writeln!(
                out,
                "        Self::{},",
                feature.enum_variant.as_deref().expect("validated variant")
            )
            .expect("String write");
        }
        out.push_str("    ];\n\n    /// Stable manifest identity.\n    #[must_use]\n    pub const fn capability_id(self) -> WasmCapabilityId {\n        match self {\n");
        for feature in &checker_features {
            writeln!(
                out,
                "            Self::{} => WasmCapabilityId({:?}),",
                feature.enum_variant.as_deref().expect("validated variant"),
                feature.id
            )
            .expect("String write");
        }
        out.push_str("        }\n    }\n\n    /// Checker disposition supplied by the manifest.\n    #[must_use]\n    pub const fn disposition(self) -> WasmFeatureDisposition {\n        match self {\n");
        for feature in &checker_features {
            let disposition = match feature.checker {
                CheckerDisposition::Warn => "Warn",
                CheckerDisposition::Reject => "Reject",
                _ => unreachable!("filtered checker disposition"),
            };
            writeln!(
                out,
                "            Self::{} => WasmFeatureDisposition::{disposition},",
                feature.enum_variant.as_deref().expect("validated variant")
            )
            .expect("String write");
        }
        out.push_str("        }\n    }\n\n    /// Stable diagnostic label.\n    #[must_use]\n    pub const fn label(self) -> &'static str {\n        match self {\n");
        for feature in &checker_features {
            writeln!(
                out,
                "            Self::{} => {:?},",
                feature.enum_variant.as_deref().expect("validated variant"),
                feature.label
            )
            .expect("String write");
        }
        out.push_str("        }\n    }\n\n    /// Stable diagnostic reason.\n    #[must_use]\n    pub const fn reason(self) -> &'static str {\n        match self {\n");
        for feature in &checker_features {
            writeln!(
                out,
                "            Self::{} => {:?},",
                feature.enum_variant.as_deref().expect("validated variant"),
                feature.reason.as_deref().expect("validated reason")
            )
            .expect("String write");
        }
        out.push_str("        }\n    }\n}\n\n");

        let mut capability_ids = BTreeMap::new();
        for id in self
            .features
            .iter()
            .filter(|row| row.checker.is_checker_feature())
            .map(|row| row.id.as_str())
            .chain(self.backlog.iter().map(|row| row.id.as_str()))
        {
            capability_ids.entry(const_name(id)).or_insert(id);
        }
        out.push_str("/// Typed constants for every reject/warn feature and backlog identity.\npub mod wasm_capability_ids {\n    use super::WasmCapabilityId;\n");
        for (name, id) in capability_ids {
            writeln!(
                out,
                "    pub const {name}: WasmCapabilityId = WasmCapabilityId({id:?});"
            )
            .expect("String write");
        }
        out.push_str("}\n\n/// A native-only stdlib module and its manifest feature identity.\n#[derive(Debug, Clone, Copy, PartialEq, Eq)]\npub struct WasmModuleRejection {\n    pub module: &'static str,\n    pub feature: WasmUnsupportedFeature,\n}\n\n/// A native-only stdlib function in an otherwise supported module.\n#[derive(Debug, Clone, Copy, PartialEq, Eq)]\npub struct WasmFunctionRejection {\n    pub module: &'static str,\n    pub function: &'static str,\n    pub feature: WasmUnsupportedFeature,\n}\n\n/// Generated module rejection classification.\npub const NATIVE_ONLY_WASM_MODULE_REJECTIONS: &[WasmModuleRejection] = &[\n");
        for feature in &checker_features {
            for module in &feature.native_only_modules {
                writeln!(
                    out,
                    "    WasmModuleRejection {{ module: {module:?}, feature: WasmUnsupportedFeature::{} }},",
                    feature.enum_variant.as_deref().expect("validated variant")
                )
                .expect("String write");
            }
        }
        out.push_str(
            "];

/// Generated exact-function rejection classification.
pub const NATIVE_ONLY_WASM_FUNCTION_REJECTIONS: &[WasmFunctionRejection] = &[
",
        );
        for feature in &checker_features {
            for function in &feature.native_only_functions {
                let (module, method) = function
                    .rsplit_once('.')
                    .expect("validated native-only function identity");
                writeln!(
                    out,
                    "    WasmFunctionRejection {{ module: {module:?}, function: {method:?}, feature: WasmUnsupportedFeature::{} }},",
                    feature.enum_variant.as_deref().expect("validated variant")
                )
                .expect("String write");
            }
        }
        out.push_str(
            "];

/// Generated native-only module short-names for sandbox and checker consumers.
pub const NATIVE_ONLY_WASM_MODULES: &[&str] = &[
",
        );
        for feature in &checker_features {
            for module in &feature.native_only_modules {
                writeln!(out, "    {module:?},").expect("String write");
            }
        }
        out.push_str(
            "];

}

pub use generated::*;
",
        );
        out
    }

    fn render_playground_json(&self) -> String {
        let mut out = String::from("[\n");
        for (index, row) in self.playground_wasi.iter().enumerate() {
            out.push_str("  {\n");
            writeln!(out, "    \"id\": {},", json_string(&row.id)).expect("String write");
            writeln!(out, "    \"status\": \"unsupported\",").expect("String write");
            if let Some(capability) = &row.capability {
                writeln!(out, "    \"capability\": {},", json_string(capability))
                    .expect("String write");
            }
            writeln!(out, "    \"reason\": {}", json_string(&row.reason)).expect("String write");
            if index + 1 == self.playground_wasi.len() {
                out.push_str("  }\n");
            } else {
                out.push_str("  },\n");
            }
        }
        out.push_str("]\n");
        out
    }
}

/// Read, parse, and validate the authority at `root`.
///
/// # Errors
///
/// Returns an I/O, TOML schema, or authority-validation diagnostic.
pub fn load_manifest(root: &Path) -> Result<Manifest, String> {
    let path = root.join("wasm-capability-manifest.toml");
    let src =
        std::fs::read_to_string(&path).map_err(|err| format!("read {}: {err}", path.display()))?;
    Manifest::parse(&src).map_err(|err| format!("{}: {err}", path.display()))
}

/// Write all generated outputs beneath `root`.
///
/// # Errors
///
/// Returns an I/O diagnostic when an output cannot be written.
pub fn write_outputs(root: &Path, manifest: &Manifest) -> Result<(), String> {
    for output in manifest.generated_outputs() {
        let path = root.join(output.path);
        std::fs::write(&path, output.contents)
            .map_err(|err| format!("write {}: {err}", path.display()))?;
    }
    let matrix_path = root.join(MATRIX_OUTPUT);
    let matrix = std::fs::read_to_string(&matrix_path)
        .map_err(|err| format!("read {}: {err}", matrix_path.display()))?;
    let rendered = replace_feature_policy_table(&matrix, manifest)?;
    std::fs::write(&matrix_path, rendered)
        .map_err(|err| format!("write {}: {err}", matrix_path.display()))?;
    Ok(())
}

/// Check all generated outputs byte-for-byte and return every stale path.
///
/// # Errors
///
/// Returns an I/O diagnostic for failures other than an absent output, which
/// is reported as stale.
pub fn stale_outputs(root: &Path, manifest: &Manifest) -> Result<Vec<PathBuf>, String> {
    let mut stale = Vec::new();
    for output in manifest.generated_outputs() {
        let path = root.join(output.path);
        match std::fs::read_to_string(&path) {
            Ok(existing) if existing == output.contents => {}
            Ok(_) => stale.push(path),
            Err(err) if err.kind() == std::io::ErrorKind::NotFound => stale.push(path),
            Err(err) => return Err(format!("read {}: {err}", path.display())),
        }
    }
    let matrix_path = root.join(MATRIX_OUTPUT);
    match std::fs::read_to_string(&matrix_path) {
        Ok(existing) => {
            if replace_feature_policy_table(&existing, manifest)? != existing {
                stale.push(matrix_path);
            }
        }
        Err(err) if err.kind() == std::io::ErrorKind::NotFound => stale.push(matrix_path),
        Err(err) => return Err(format!("read {}: {err}", matrix_path.display())),
    }
    Ok(stale)
}

fn replace_feature_policy_table(source: &str, manifest: &Manifest) -> Result<String, String> {
    const HEADING: &str = "## Feature disposition table";
    let lines: Vec<&str> = source.split_inclusive('\n').collect();
    let heading = lines
        .iter()
        .position(|line| line.trim_end_matches(['\r', '\n']) == HEADING)
        .ok_or_else(|| format!("{MATRIX_OUTPUT}: missing `{HEADING}` heading"))?;
    let table_start = lines
        .iter()
        .enumerate()
        .skip(heading + 1)
        .take_while(|(_, line)| !line.starts_with("## "))
        .find_map(|(index, line)| line.starts_with('|').then_some(index))
        .ok_or_else(|| format!("{MATRIX_OUTPUT}: missing feature policy table"))?;
    let table_end = lines
        .iter()
        .enumerate()
        .skip(table_start)
        .find_map(|(index, line)| (!line.starts_with('|')).then_some(index))
        .unwrap_or(lines.len());

    let mut rendered = String::with_capacity(source.len());
    for line in &lines[..table_start] {
        rendered.push_str(line);
    }
    rendered.push_str(&manifest.render_feature_policy_table());
    for line in &lines[table_end..] {
        rendered.push_str(line);
    }
    Ok(rendered)
}

fn markdown_cell(value: &str) -> String {
    value
        .replace('&', "&amp;")
        .replace('|', "&#124;")
        .replace(['\r', '\n'], "<br>")
}

fn unique<'a>(values: impl Iterator<Item = &'a str>, what: &str) -> Result<(), String> {
    let mut seen = BTreeSet::new();
    for value in values {
        if !seen.insert(value) {
            return Err(format!("duplicate {what} `{value}`"));
        }
    }
    Ok(())
}

fn validate_kebab_id(value: &str, what: &str) -> Result<(), String> {
    let valid = !value.is_empty()
        && !value.starts_with('-')
        && !value.ends_with('-')
        && value
            .chars()
            .all(|ch| ch == '-' || ch.is_ascii_lowercase() || ch.is_ascii_digit());
    if valid {
        Ok(())
    } else {
        Err(format!("invalid {what} `{value}`; expected kebab-case"))
    }
}

fn validate_rust_variant(value: &str) -> Result<(), String> {
    let mut chars = value.chars();
    let valid = chars.next().is_some_and(|ch| ch.is_ascii_uppercase())
        && chars.all(|ch| ch == '_' || ch.is_ascii_alphanumeric());
    if valid {
        Ok(())
    } else {
        Err(format!(
            "invalid checker enum_variant `{value}`; expected a Rust-style variant"
        ))
    }
}

fn const_name(id: &str) -> String {
    id.chars()
        .map(|ch| {
            if ch.is_ascii_alphanumeric() {
                ch.to_ascii_uppercase()
            } else {
                '_'
            }
        })
        .collect()
}

fn json_string(value: &str) -> String {
    let mut out = String::from("\"");
    for ch in value.chars() {
        match ch {
            '\"' => out.push_str("\\\""),
            '\\' => out.push_str("\\\\"),
            '\n' => out.push_str("\\n"),
            '\r' => out.push_str("\\r"),
            '\t' => out.push_str("\\t"),
            ch if ch.is_control() => write!(out, "\\u{:04x}", ch as u32).expect("String write"),
            ch => out.push(ch),
        }
    }
    out.push('\"');
    out
}
