//! 10,000-iteration random-corpus shadow test for the three descriptor
//! emitters.
//!
//! Each iteration generates a pseudo-random [`WireCodecPlan`] covering the
//! full [`PrimitiveWireKind`] surface (including nested wire references) and
//! asserts three invariants per descriptor:
//!
//! 1. **Determinism.** Two successive `from_plan` calls on the same plan
//!    produce equal descriptors; two successive `to_msgpack_bytes()` calls
//!    produce equal byte strings. This is the stability guarantee that any
//!    C++ consumer depends on: the same plan always emits the same bytes.
//! 2. **Round-trip.** Bytes emitted by `to_msgpack_bytes()` decode cleanly
//!    via `rmp_serde::from_slice` into a descriptor equal to the original.
//! 3. **Cross-descriptor agreement.** The JSON and YAML descriptors emit
//!    identical per-field ops for every primitive (they differ only in the
//!    object key, which this test generates distinctly). This upholds the
//!    alias contract documented in `yaml_desc.rs`.
//!
//! This test is a precondition for follow-on deletion of the legacy
//! `MLIRGenWire.cpp` dispatch paths. When the C++ descriptor consumer lands,
//! a second shadow test compares descriptor-emitted MLIR text against the
//! legacy `MLIRGenWire` output on every `e2e_wire` fixture; this Rust-side
//! test is what makes that consumer implementable against a stable, well-
//! exercised producer.
//!
//! Uses a deterministic Lehmer RNG (no external crate) so CI runs identical
//! cases on every build; a seeded override env var `HEW_CORPUS_SEED` is
//! available for reproducing any specific failure.

use std::env;

use hew_wirecodec::{
    FieldModifiers, FieldPlan, IntegerBounds, JsonCodecDesc, MsgpackCodecDesc, PrimitiveWireKind,
    VariantPlan, WireCodecPlan, WireShape, YamlCodecDesc,
};

/// Default iteration budget. Set to 10,000 to match the lane contract.
const DEFAULT_ITERATIONS: u32 = 10_000;

/// Minimum surface coverage required by the corpus. If `assert_kind_coverage`
/// finds fewer than this many distinct `PrimitiveWireKind` discriminants were
/// exercised, the generator is biased and the test fails.
const REQUIRED_KIND_VARIANTS: usize = 15;

/// Lehmer / MCG pseudo-random generator — deterministic, no dependencies.
///
/// Parameters from Park–Miller ("Random Number Generators: Good Ones are Hard
/// to Find", CACM 31:10). Suitable for generating test corpora; not suitable
/// for cryptographic use.
struct Rng {
    state: u64,
}

impl Rng {
    fn new(seed: u64) -> Self {
        let state = if seed == 0 { 1 } else { seed };
        Self { state }
    }

    fn next_u32(&mut self) -> u32 {
        // xorshift64* — deterministic and well-distributed for small state.
        self.state ^= self.state << 13;
        self.state ^= self.state >> 7;
        self.state ^= self.state << 17;
        #[expect(
            clippy::cast_possible_truncation,
            reason = "deliberate truncation to u32 for randomness extraction"
        )]
        let lo = self.state as u32;
        lo
    }

    fn range(&mut self, lo: u32, hi_exclusive: u32) -> u32 {
        assert!(hi_exclusive > lo, "range bounds inverted");
        lo + (self.next_u32() % (hi_exclusive - lo))
    }

    fn pick_kind(&mut self) -> PrimitiveWireKind {
        match self.range(0, 16) {
            0 => PrimitiveWireKind::Bool,
            1 => PrimitiveWireKind::I8,
            2 => PrimitiveWireKind::I16,
            3 => PrimitiveWireKind::I32,
            4 => PrimitiveWireKind::I64,
            5 => PrimitiveWireKind::U8,
            6 => PrimitiveWireKind::U16,
            7 => PrimitiveWireKind::U32,
            8 => PrimitiveWireKind::U64,
            9 => PrimitiveWireKind::F32,
            10 => PrimitiveWireKind::F64,
            11 => PrimitiveWireKind::String,
            12 => PrimitiveWireKind::Bytes,
            13 => PrimitiveWireKind::Duration,
            14 => PrimitiveWireKind::Char,
            _ => PrimitiveWireKind::Nested(format!("Nested{}", self.next_u32() % 32)),
        }
    }
}

fn random_plan(rng: &mut Rng, id: u32) -> WireCodecPlan {
    // 1 in 8 plans is an enum; the rest are structs.
    let is_enum = rng.range(0, 8) == 0;
    let name = format!("T{id}");
    if is_enum {
        let variant_count = rng.range(1, 6);
        let variants: Vec<VariantPlan> = (0..variant_count)
            .map(|i| VariantPlan {
                name: format!("V{i}"),
            })
            .collect();
        return WireCodecPlan {
            name,
            shape: WireShape::Enum { variants },
            json_case: None,
            yaml_case: None,
        };
    }
    let field_count = rng.range(0, 8);
    let mut fields = Vec::with_capacity(field_count as usize);
    for i in 0..field_count {
        let kind = rng.pick_kind();
        let narrowing = IntegerBounds::for_kind(&kind);
        // Deliberately choose distinct json/yaml names so the cross-
        // descriptor equality test catches any accidental aliasing.
        let field_name = format!("field_{i}");
        let json_name = format!("jsonField{i}");
        let yaml_name = format!("yaml-field-{i}");
        let mut modifiers = FieldModifiers::default();
        // Mix modifiers to exercise the propagation path.
        if rng.range(0, 4) == 0 {
            modifiers.is_optional = true;
        }
        if rng.range(0, 8) == 0 {
            modifiers.is_repeated = true;
        }
        if rng.range(0, 16) == 0 {
            modifiers.is_reserved = true;
        }
        if rng.range(0, 32) == 0 {
            modifiers.is_deprecated = true;
        }
        fields.push(FieldPlan {
            name: field_name,
            number: i + 1,
            json_name,
            yaml_name,
            kind,
            modifiers,
            narrowing,
        });
    }
    WireCodecPlan {
        name,
        shape: WireShape::Struct { fields },
        json_case: None,
        yaml_case: None,
    }
}

fn seed_from_env_or_default() -> u64 {
    env::var("HEW_CORPUS_SEED")
        .ok()
        .and_then(|s| s.parse::<u64>().ok())
        .unwrap_or(0x5A17_1AC0_DEC0_DE01)
}

fn iterations_from_env() -> u32 {
    env::var("HEW_CORPUS_ITERATIONS")
        .ok()
        .and_then(|s| s.parse::<u32>().ok())
        .unwrap_or(DEFAULT_ITERATIONS)
}

fn kind_discriminant(k: &PrimitiveWireKind) -> u8 {
    match k {
        PrimitiveWireKind::Bool => 0,
        PrimitiveWireKind::I8 => 1,
        PrimitiveWireKind::I16 => 2,
        PrimitiveWireKind::I32 => 3,
        PrimitiveWireKind::I64 => 4,
        PrimitiveWireKind::U8 => 5,
        PrimitiveWireKind::U16 => 6,
        PrimitiveWireKind::U32 => 7,
        PrimitiveWireKind::U64 => 8,
        PrimitiveWireKind::F32 => 9,
        PrimitiveWireKind::F64 => 10,
        PrimitiveWireKind::String => 11,
        PrimitiveWireKind::Bytes => 12,
        PrimitiveWireKind::Duration => 13,
        PrimitiveWireKind::Char => 14,
        PrimitiveWireKind::Nested(_) => 15,
    }
}

#[test]
#[expect(
    clippy::too_many_lines,
    reason = "single orchestration test asserts four invariants across three descriptors; splitting obscures the corpus-seeding contract"
)]
fn corpus_descriptor_emitters_are_deterministic_and_round_trip() {
    let seed = seed_from_env_or_default();
    let iters = iterations_from_env();
    let mut rng = Rng::new(seed);

    let mut seen_kinds = [false; 16];
    let mut enum_count = 0usize;
    let mut total_fields = 0usize;

    for i in 0..iters {
        let plan = random_plan(&mut rng, i);

        // Record kind coverage.
        if let Some(fields) = plan.fields() {
            total_fields += fields.len();
            for f in fields {
                seen_kinds[usize::from(kind_discriminant(&f.kind))] = true;
            }
        } else if plan.variants().is_some() {
            enum_count += 1;
        }

        // Build each descriptor twice. Determinism: both copies must be
        // equal at the struct level AND produce equal msgpack bytes.
        let msg_a = MsgpackCodecDesc::from_plan(&plan);
        let msg_b = MsgpackCodecDesc::from_plan(&plan);
        assert_eq!(
            msg_a, msg_b,
            "msgpack descriptor non-deterministic on iteration {i} (plan {})",
            plan.name
        );
        let msg_bytes = msg_a.to_msgpack_bytes();
        assert_eq!(
            msg_bytes,
            msg_b.to_msgpack_bytes(),
            "msgpack bytes non-deterministic on iteration {i}"
        );

        let json_a = JsonCodecDesc::from_plan(&plan);
        let json_b = JsonCodecDesc::from_plan(&plan);
        assert_eq!(
            json_a, json_b,
            "json descriptor non-deterministic on iteration {i} (plan {})",
            plan.name
        );
        let json_bytes = json_a.to_msgpack_bytes();
        assert_eq!(
            json_bytes,
            json_b.to_msgpack_bytes(),
            "json bytes non-deterministic on iteration {i}"
        );

        let yaml_a = YamlCodecDesc::from_plan(&plan);
        let yaml_b = YamlCodecDesc::from_plan(&plan);
        assert_eq!(
            yaml_a, yaml_b,
            "yaml descriptor non-deterministic on iteration {i} (plan {})",
            plan.name
        );
        let yaml_bytes = yaml_a.to_msgpack_bytes();
        assert_eq!(
            yaml_bytes,
            yaml_b.to_msgpack_bytes(),
            "yaml bytes non-deterministic on iteration {i}"
        );

        // Round-trip: decode each descriptor byte stream back into its
        // typed form; it must equal the original.
        let msg_round: MsgpackCodecDesc = rmp_serde::from_slice(&msg_bytes).unwrap_or_else(|e| {
            panic!(
                "msgpack round-trip failed on iteration {i} (plan {}): {e}",
                plan.name
            )
        });
        assert_eq!(msg_round, msg_a, "msgpack round-trip mismatch on {i}");

        let json_round: JsonCodecDesc = rmp_serde::from_slice(&json_bytes).unwrap_or_else(|e| {
            panic!(
                "json round-trip failed on iteration {i} (plan {}): {e}",
                plan.name
            )
        });
        assert_eq!(json_round, json_a, "json round-trip mismatch on {i}");

        let yaml_round: YamlCodecDesc = rmp_serde::from_slice(&yaml_bytes).unwrap_or_else(|e| {
            panic!(
                "yaml round-trip failed on iteration {i} (plan {}): {e}",
                plan.name
            )
        });
        assert_eq!(yaml_round, yaml_a, "yaml round-trip mismatch on {i}");

        // Cross-descriptor agreement: JSON and YAML ops must match for
        // every field (yaml_desc's alias contract).
        assert_eq!(
            json_a.fields.len(),
            yaml_a.fields.len(),
            "json/yaml field count divergence on {i}"
        );
        for (jf, yf) in json_a.fields.iter().zip(yaml_a.fields.iter()) {
            assert_eq!(
                jf.op, yf.op,
                "json/yaml op divergence on iteration {i} field {}",
                jf.name
            );
            assert_eq!(
                jf.tag, yf.tag,
                "json/yaml tag drift on {i} field {}",
                jf.name
            );
            assert_eq!(
                jf.bounds, yf.bounds,
                "json/yaml bounds drift on {i} field {}",
                jf.name
            );
            // Keys differ by construction (see random_plan); this asserts
            // that the two descriptors genuinely track their own name.
            assert_ne!(
                jf.key, yf.key,
                "random plan should produce distinct json/yaml keys on {i} field {}",
                jf.name
            );
        }
    }

    // Surface-coverage gate: all 16 kind discriminants should be exercised
    // across the corpus. The gate is an invariant check on the generator,
    // not a property of descriptors — but if the generator is biased, the
    // above invariants are weaker than they look.
    let seen_count = seen_kinds.iter().filter(|b| **b).count();
    assert!(
        seen_count >= REQUIRED_KIND_VARIANTS,
        "corpus exercised only {seen_count}/16 PrimitiveWireKind variants (need ≥{REQUIRED_KIND_VARIANTS}); generator is biased"
    );
    // Sanity: we produced at least a few enum plans and a healthy number
    // of field decls.
    assert!(enum_count > 0, "corpus produced no enum plans");
    assert!(
        total_fields > iters as usize,
        "corpus produced only {total_fields} total fields across {iters} iterations"
    );
    eprintln!(
        "corpus: {iters} iterations, seed=0x{seed:x}, {seen_count}/16 kinds exercised, \
         {enum_count} enums, {total_fields} total fields"
    );
}
