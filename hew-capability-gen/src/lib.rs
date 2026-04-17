//! Capability manifest scaffolding.
//!
//! This crate is the future home of the generator that reads
//! `wasm-capability-manifest.toml` and emits the checker feature enum, the
//! `CMake` exclusion list, the playground capability block, and a rendered
//! copy of `docs/wasm-capability-matrix.md`.
//!
//! For now the crate only exposes the manifest shape needed to assert the TOML
//! and the prose matrix stay in cardinality lock-step. Subsequent changes fill
//! in validation, rendering, and diffing on top of this same entry point.

use serde::Deserialize;

/// Top-level manifest shape used for cardinality checks.
///
/// Only the tables that the row-count check needs are deserialised strictly;
/// richer fields are retained via a captured [`toml::Table`] so a later
/// revision can add validation without rewriting call sites.
#[derive(Debug, Deserialize)]
pub struct Manifest {
    /// Schema version of the manifest file. Intentionally read so that
    /// future versions can gate parsing logic.
    pub manifest_version: u32,

    /// Target tier descriptors (Tier 1 / Tier 2 today).
    #[serde(default, rename = "tier")]
    pub tiers: Vec<Tier>,

    /// Feature disposition rows — one per row of the "Feature disposition
    /// table" in `docs/wasm-capability-matrix.md`.
    #[serde(default, rename = "feature")]
    pub features: Vec<Feature>,

    /// WASM-TODO backlog rows — one per row of the backlog table.
    #[serde(default, rename = "backlog")]
    pub backlog: Vec<Backlog>,
}

/// A compilation target tier.
///
/// Fields beyond `id` are captured opaquely to avoid locking the schema
/// prematurely; a strongly typed record replaces this once the generator
/// needs to read the full row.
#[derive(Debug, Deserialize)]
pub struct Tier {
    /// Stable identifier for the tier (e.g. `"tier1"`).
    pub id: String,
    /// Remaining fields retained verbatim until the generator owns the schema.
    #[serde(flatten)]
    pub extra: toml::Table,
}

/// A feature-disposition row.
#[derive(Debug, Deserialize)]
pub struct Feature {
    /// Stable kebab-case feature identifier (e.g. `"supervision-trees"`).
    pub id: String,
    /// Remaining fields retained verbatim until the generator owns the schema.
    #[serde(flatten)]
    pub extra: toml::Table,
}

/// A WASM-TODO backlog row.
#[derive(Debug, Deserialize)]
pub struct Backlog {
    /// Stable kebab-case backlog identifier (e.g. `"channels"`).
    pub id: String,
    /// Remaining fields retained verbatim until the generator owns the schema.
    #[serde(flatten)]
    pub extra: toml::Table,
}

impl Manifest {
    /// Parses a TOML string into a [`Manifest`].
    ///
    /// # Errors
    ///
    /// Returns the underlying [`toml::de::Error`] if the source is not valid
    /// TOML or does not match the manifest shape.
    pub fn parse(src: &str) -> Result<Self, toml::de::Error> {
        toml::from_str(src)
    }
}
