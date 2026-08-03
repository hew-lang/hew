use hew_capability_gen::{
    stale_outputs, write_outputs, Manifest, MATRIX_OUTPUT, PLAYGROUND_MANIFEST, PLAYGROUND_OUTPUT,
    RUST_OUTPUT,
};
use std::path::{Path, PathBuf};

type SourceMutation = fn(String) -> String;

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .parent()
        .expect("crate dir has a parent")
        .to_path_buf()
}

fn manifest_source() -> String {
    std::fs::read_to_string(repo_root().join("wasm-capability-manifest.toml"))
        .expect("read manifest")
}

fn parsed() -> Manifest {
    Manifest::parse(&manifest_source()).expect("typed authority parses and validates")
}

fn seed_checked_outputs(root: &Path, manifest: &Manifest) {
    for output in manifest.generated_outputs() {
        let path = root.join(output.path);
        std::fs::create_dir_all(path.parent().expect("output parent")).expect("create parent");
    }
    let matrix_path = root.join(MATRIX_OUTPUT);
    std::fs::create_dir_all(matrix_path.parent().expect("matrix parent")).expect("create docs");
    std::fs::write(
        &matrix_path,
        std::fs::read_to_string(repo_root().join(MATRIX_OUTPUT)).expect("read live matrix"),
    )
    .expect("seed matrix");
    let playground_manifest_path = root.join(PLAYGROUND_MANIFEST);
    std::fs::create_dir_all(
        playground_manifest_path
            .parent()
            .expect("playground manifest parent"),
    )
    .expect("create playground manifest parent");
    std::fs::write(
        &playground_manifest_path,
        std::fs::read_to_string(repo_root().join(PLAYGROUND_MANIFEST))
            .expect("read live playground manifest"),
    )
    .expect("seed playground manifest");
    write_outputs(root, manifest).expect("seed generated outputs");
}

#[test]
fn checked_outputs_are_current() {
    let stale = stale_outputs(&repo_root(), &parsed()).expect("check generated outputs");
    assert!(
        stale.is_empty(),
        "generated WASM capability outputs are stale: {stale:?}"
    );
}

#[test]
fn structural_coverage_ratchet_is_pinned() {
    let manifest = parsed();
    let variants: Vec<_> = manifest
        .features
        .iter()
        .filter_map(|feature| feature.enum_variant.as_deref())
        .collect();

    assert_eq!(
        manifest.features.len(),
        35,
        "review every feature row change"
    );
    assert_eq!(
        variants,
        [
            "SupervisionTrees",
            "LinkMonitor",
            "StructuredConcurrency",
            "Tasks",
            "BlockingChannelRecv",
            "BlockingSemaphoreAcquire",
            "Timers",
            "PeriodicTimers",
            "Streams",
            "FilesystemStreams",
            "HttpClient",
            "Smtp",
            "WebSocket",
            "HttpServer",
            "TcpNetworking",
            "ProcessExecution",
            "Tls",
            "Quic",
            "Dns",
            "OsEnv",
            "Distributed",
            "CryptoRandom",
            "CryptoEncrypt",
            "CryptoSign",
        ],
        "checker coverage changed; review the complete bidirectional authority"
    );
    assert_eq!(
        manifest.backlog.len(),
        40,
        "review every backlog row change"
    );
    assert_eq!(
        manifest
            .features
            .iter()
            .map(|feature| feature.native_only_modules.len())
            .sum::<usize>(),
        13,
        "review every generated native-only module exclusion"
    );
    assert_eq!(
        manifest
            .features
            .iter()
            .map(|feature| feature.native_only_functions.len())
            .sum::<usize>(),
        1,
        "review every generated native-only function exclusion"
    );
    assert_eq!(
        manifest.playground_wasi.len(),
        7,
        "review every curated WASI exclusion"
    );
}

#[test]
fn reject_warn_variants_are_bijective_and_exhaustive_in_generated_rust() {
    let manifest = parsed();
    let rust = manifest
        .generated_outputs()
        .into_iter()
        .find(|output| output.path == RUST_OUTPUT)
        .expect("Rust output")
        .contents;
    let variants: Vec<&str> = manifest
        .features
        .iter()
        .filter_map(|feature| feature.enum_variant.as_deref())
        .collect();
    for variant in &variants {
        assert_eq!(
            rust.matches(&format!("    {variant},\n")).count(),
            1,
            "enum declaration must contain `{variant}` exactly once"
        );
        assert!(
            rust.contains(&format!("Self::{variant} => WasmCapabilityId")),
            "identity match must be exhaustive for `{variant}`"
        );
        assert!(
            rust.contains(&format!("Self::{variant} => WasmFeatureDisposition::")),
            "disposition match must be exhaustive for `{variant}`"
        );
    }
    assert!(
        !rust.contains("pub const BASIC_ACTORS"),
        "pass feature identities must not be available for codegen exclusion classification"
    );
}

#[test]
fn authority_mutations_fail_closed() {
    let source = manifest_source();

    let duplicate = source.replacen(
        "enum_variant = \"LinkMonitor\"",
        "enum_variant = \"SupervisionTrees\"",
        1,
    );
    assert!(Manifest::parse(&duplicate)
        .expect_err("duplicate enum variant must fail")
        .contains("duplicate checker enum_variant"));

    let omitted = source.replacen("enum_variant = \"LinkMonitor\"\n", "", 1);
    assert!(Manifest::parse(&omitted)
        .expect_err("reject row omitted from checker list must fail")
        .contains("requires enum_variant"));

    let disposition_mismatch = source.replacen(
        "enum_variant = \"LinkMonitor\"",
        "enum_variant = \"LinkMonitor\"\nchecker = \"pass\"",
        1,
    );
    assert!(
        Manifest::parse(&disposition_mismatch).is_err(),
        "duplicate/mismatched checker disposition must fail parse or validation"
    );

    let unknown_field = source.replacen(
        "enum_variant = \"LinkMonitor\"",
        "enum_variant = \"LinkMonitor\"\nunknown_authority = true",
        1,
    );
    assert!(Manifest::parse(&unknown_field)
        .expect_err("unknown authority field must fail")
        .contains("unknown field"));

    let runtime_mismatch = source.replacen(
        "checker = \"reject\"\nruntime = \"native-only\"",
        "checker = \"warn\"\nruntime = \"native-only\"",
        1,
    );
    assert!(Manifest::parse(&runtime_mismatch)
        .expect_err("warning/native-only mismatch must fail")
        .contains("must have cooperative runtime disposition"));
}

#[test]
fn playground_authority_cannot_declare_pass() {
    let manifest = parsed();
    let json = manifest
        .generated_outputs()
        .into_iter()
        .find(|output| output.path == PLAYGROUND_OUTPUT)
        .expect("playground output")
        .contents;
    assert!(
        !json.contains("runnable"),
        "generated authority must contain exclusions only; runnable stays E2E-proven"
    );

    let source = manifest_source().replacen("status = \"unsupported\"", "status = \"runnable\"", 1);
    assert!(
        Manifest::parse(&source).is_err(),
        "typed manifest must reject declarative playground pass"
    );
}

#[test]
fn every_checked_output_gate_detects_mutation() {
    let manifest = parsed();
    let temp = tempfile::tempdir().expect("tempdir");
    seed_checked_outputs(temp.path(), &manifest);
    assert!(stale_outputs(temp.path(), &manifest)
        .expect("clean check")
        .is_empty());

    for path in [RUST_OUTPUT, PLAYGROUND_OUTPUT, MATRIX_OUTPUT] {
        write_outputs(temp.path(), &manifest).expect("restore outputs");
        let output = temp.path().join(path);
        let mut contents = std::fs::read_to_string(&output).expect("read generated output");
        if path == MATRIX_OUTPUT {
            contents = contents.replacen("| `basic-actors` |", "| `mutated-basic-actors` |", 1);
        } else {
            contents.push_str("MUTATION\n");
        }
        std::fs::write(&output, contents).expect("mutate generated output");
        let stale = stale_outputs(temp.path(), &manifest).expect("mutated check");
        assert_eq!(stale, vec![output], "{path} mutation must turn gate red");
    }
}

#[test]
fn playground_wasi_summary_mutations_turn_the_freshness_gate_red() {
    let manifest = parsed();
    let temp = tempfile::tempdir().expect("tempdir");
    seed_checked_outputs(temp.path(), &manifest);
    let matrix_path = temp.path().join(MATRIX_OUTPUT);
    let playground_manifest_path = temp.path().join(PLAYGROUND_MANIFEST);

    let matrix = std::fs::read_to_string(&matrix_path).expect("read generated matrix");
    let mutated_matrix = matrix.replacen(
        "| `types/wire_types` | `runnable` |",
        "| `types/wire_types` | `unsupported` |",
        1,
    );
    assert_ne!(mutated_matrix, matrix, "summary mutation must alter matrix");
    std::fs::write(&matrix_path, mutated_matrix).expect("write matrix mutation");
    assert_eq!(
        stale_outputs(temp.path(), &manifest).expect("check summary mutation"),
        vec![matrix_path.clone()],
        "WASI summary drift must turn the matrix authority gate red"
    );

    seed_checked_outputs(temp.path(), &manifest);
    let playground_manifest =
        std::fs::read_to_string(&playground_manifest_path).expect("read playground manifest");
    let mutated_manifest = playground_manifest.replacen(
        "\"id\": \"types/wire_types\"",
        "\"id\": \"types/wire_types_mutated\"",
        1,
    );
    assert_ne!(
        mutated_manifest, playground_manifest,
        "runnable truth mutation must alter playground manifest"
    );
    std::fs::write(&playground_manifest_path, mutated_manifest)
        .expect("write playground manifest mutation");
    assert_eq!(
        stale_outputs(temp.path(), &manifest).expect("check runnable truth mutation"),
        vec![matrix_path.clone()],
        "runnable playground truth drift must turn the generated summary gate red"
    );

    seed_checked_outputs(temp.path(), &manifest);
    let playground_manifest =
        std::fs::read_to_string(&playground_manifest_path).expect("read playground manifest");
    let contradictory_manifest =
        playground_manifest.replacen("\"wasi\": \"unsupported\"", "\"wasi\": \"runnable\"", 1);
    assert_ne!(
        contradictory_manifest, playground_manifest,
        "status contradiction must alter playground manifest"
    );
    std::fs::write(&playground_manifest_path, contradictory_manifest)
        .expect("write contradictory playground manifest");
    let error = stale_outputs(temp.path(), &manifest)
        .expect_err("runnable status must contradict the typed unsupported row");
    assert!(
        error.contains("is runnable but [[playground_wasi]] declares it unsupported"),
        "unexpected contradiction diagnostic: {error}"
    );
}

#[test]
fn checker_variant_mutations_turn_the_freshness_gate_red() {
    let manifest = parsed();
    let temp = tempfile::tempdir().expect("tempdir");
    seed_checked_outputs(temp.path(), &manifest);

    let mutations: &[(&str, SourceMutation)] = &[
        ("omitted variant", |source| {
            source.replacen("    LinkMonitor,\n", "", 1)
        }),
        ("unknown variant", |source| {
            source.replacen(
                "    LinkMonitor,\n",
                "    LinkMonitor,\n    UnknownCapability,\n",
                1,
            )
        }),
        ("disposition mismatch", |source| {
            source.replacen(
                "Self::LinkMonitor => WasmFeatureDisposition::Reject",
                "Self::LinkMonitor => WasmFeatureDisposition::Warn",
                1,
            )
        }),
    ];
    for (name, mutate) in mutations {
        write_outputs(temp.path(), &manifest).expect("restore outputs");
        let rust_path = temp.path().join(RUST_OUTPUT);
        let source = std::fs::read_to_string(&rust_path).expect("read generated Rust");
        let mutated = mutate(source.clone());
        assert_ne!(mutated, source, "{name} mutation must alter generated Rust");
        std::fs::write(&rust_path, mutated).expect("write mutation");
        assert_eq!(
            stale_outputs(temp.path(), &manifest).expect("check mutated output"),
            vec![rust_path],
            "{name} must turn the checker authority gate red"
        );
    }
}

#[test]
fn matrix_policy_field_mutations_turn_the_freshness_gate_red() {
    let manifest = parsed();
    let temp = tempfile::tempdir().expect("tempdir");
    seed_checked_outputs(temp.path(), &manifest);
    let mutations = [
        ("id", "`supervision-trees`", "`changed-id`"),
        (
            "feature surface",
            "Supervision trees (`supervisor`, `supervisor_child`, `supervisor_stop`)",
            "Changed feature surface",
        ),
        (
            "diagnostic label",
            "Supervision tree operations",
            "Changed diagnostic label",
        ),
        (
            "checker disposition",
            "Reject (`SupervisionTrees`)",
            "Warn (`SupervisionTrees`)",
        ),
        (
            "diagnostic reason",
            "they require OS threads for restart strategies and child supervision",
            "changed diagnostic reason",
        ),
        (
            "runtime status",
            "Educational sandbox subset implements deterministic restart trees; native runtime parity remains gated",
            "changed runtime status",
        ),
        (
            "tracking label",
            "WASM-TODO(supervision):",
            "WASM-TODO(changed-supervision):",
        ),
    ];

    for (name, old, new) in mutations {
        write_outputs(temp.path(), &manifest).expect("restore outputs");
        let matrix_path = temp.path().join(MATRIX_OUTPUT);
        let source = std::fs::read_to_string(&matrix_path).expect("read matrix");
        let mutated = source.replacen(old, new, 1);
        assert_ne!(mutated, source, "{name} mutation must alter matrix");
        std::fs::write(&matrix_path, mutated).expect("write matrix mutation");
        assert_eq!(
            stale_outputs(temp.path(), &manifest).expect("check mutated matrix"),
            vec![matrix_path],
            "{name} drift must turn the matrix authority gate red"
        );
    }
}
