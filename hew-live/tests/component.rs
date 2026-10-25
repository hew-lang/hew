#![cfg(feature = "component")]

use hew_live::component::{bindings, write_guest_bindings};
use std::path::{Path, PathBuf};
use std::process::Command;
use std::sync::{Arc, Mutex};
use wasmtime::component::{Component, Linker, ResourceTable};
use wasmtime::{Config, Engine, Store};
use wasmtime_wasi::{WasiCtx, WasiCtxBuilder, WasiCtxView, WasiView};

type Delivery = bindings::hew::live_router::events::Delivery;
type Event = bindings::hew::live_router::events::Event;
type Snapshot = bindings::hew::live_router::events::Snapshot;

fn run(command: &mut Command) {
    let output = command.output().expect("run component build tool");
    assert!(
        output.status.success(),
        "{command:?}\n{}\n{}",
        String::from_utf8_lossy(&output.stdout),
        String::from_utf8_lossy(&output.stderr)
    );
}

fn environment(name: &str) -> PathBuf {
    std::env::var_os(name).map_or_else(
        || panic!("{name} must be set by make test-live-component"),
        PathBuf::from,
    )
}

fn build_component(directory: &Path) -> Vec<u8> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let fixture = root.join("tests/fixtures");
    write_guest_bindings(directory).unwrap();
    run(Command::new(environment("HEW_LIVE_COMPILER"))
        .args(["build", "--target", "wasm32-wasip1", "--emit-obj"])
        .args(["--export-c", "create=guest_create"])
        .args(["--export-c-async", "route=guest_route"])
        .args(["--export-c-async", "snapshot=guest_snapshot"])
        .args(["--export-c-async", "dispose=guest_dispose"])
        .arg(fixture.join("router.hew"))
        .arg("-o")
        .arg(directory.join("guest.o")));
    let sdk = environment("HEW_WASI_SDK");
    run(Command::new(sdk.join("bin/clang"))
        .args(["-mexec-model=reactor", "-O2", "-Wall", "-Wextra", "-Werror"])
        .arg("-I")
        .arg(directory)
        .arg("-I")
        .arg(root.join("../hew-cabi/include"))
        .arg(directory.join("router.c"))
        .arg(directory.join("router_component_type.o"))
        .arg(fixture.join("router_adapter.c"))
        .arg(directory.join("guest.o"))
        .arg("-L")
        .arg(environment("HEW_LIVE_WASM_LIB_DIR"))
        .args(["-lhew_runtime", "-lhew_std", "-o"])
        .arg(directory.join("router.core.wasm")));
    component_bytes(&std::fs::read(directory.join("router.core.wasm")).unwrap())
}

fn component_bytes(core: &[u8]) -> Vec<u8> {
    wit_component::ComponentEncoder::default()
        .module(core)
        .unwrap()
        .adapter(
            "wasi_snapshot_preview1",
            wasi_preview1_component_adapter_provider::WASI_SNAPSHOT_PREVIEW1_REACTOR_ADAPTER,
        )
        .unwrap()
        .validate(true)
        .encode()
        .unwrap()
}

struct Host {
    wasi: WasiCtx,
    resources: ResourceTable,
}

impl WasiView for Host {
    fn ctx(&mut self) -> WasiCtxView<'_> {
        WasiCtxView {
            ctx: &mut self.wasi,
            table: &mut self.resources,
        }
    }
}

async fn activate(
    engine: &Engine,
    component: &Component,
    node: &'static str,
    deliveries: Arc<Mutex<Vec<Delivery>>>,
) -> (Store<Host>, bindings::Router) {
    let mut linker = Linker::new(engine);
    wasmtime_wasi::p2::add_to_linker_async(&mut linker).unwrap();
    let mut services = linker.instance("hew:live-router/services@0.1.0").unwrap();
    services
        .func_wrap_concurrent("destination", move |_accessor, (source,): (String,)| {
            Box::pin(async move {
                tokio::time::sleep(std::time::Duration::from_millis(5)).await;
                let reply: Result<Option<String>, String> = match source.as_str() {
                    "missing" => Ok(None),
                    "denied" => Err("destination grant denied".into()),
                    _ => Ok(Some(format!("{node}/{source}"))),
                };
                Ok((reply,))
            })
        })
        .unwrap();
    services
        .func_wrap_concurrent("emit", move |_accessor, (message,): (Delivery,)| {
            let deliveries = deliveries.clone();
            Box::pin(async move {
                tokio::time::sleep(std::time::Duration::from_millis(5)).await;
                deliveries.lock().unwrap().push(message);
                Ok((Ok::<(), String>(()),))
            })
        })
        .unwrap();
    let mut store = Store::new(
        engine,
        Host {
            wasi: WasiCtxBuilder::new().build(),
            resources: ResourceTable::new(),
        },
    );
    store.set_fuel(100_000_000).unwrap();
    let router = bindings::Router::instantiate_async(&mut store, component, &linker)
        .await
        .unwrap();
    (store, router)
}

#[tokio::test(flavor = "current_thread")]
async fn hew_and_foreign_components_share_services_and_snapshot_contracts() {
    let directory = tempfile::tempdir().unwrap();
    let bytes = build_component(directory.path());
    let mut config = Config::new();
    config.wasm_component_model_async(true).consume_fuel(true);
    let engine = Engine::new(&config).unwrap();
    // Preparation compiles the immutable component before any service is granted.
    let component = Component::new(&engine, bytes).unwrap();
    tokio::time::timeout(
        std::time::Duration::from_secs(30),
        exercise_router(&engine, &component),
    )
    .await
    .expect("Hew component lifecycle must make progress");
    let core = std::fs::read(environment("HEW_LIVE_FOREIGN_COMPONENT")).unwrap();
    let foreign = Component::new(&engine, component_bytes(&core)).unwrap();
    exercise_router(&engine, &foreign).await;
}

#[allow(
    clippy::too_many_lines,
    reason = "one lifecycle oracle checks admission, faults, resume, snapshot and destination grants"
)]
async fn exercise_router(engine: &Engine, component: &Component) {
    let deliveries = Arc::new(Mutex::new(Vec::new()));
    let (mut store, router) = activate(engine, component, "first", deliveries.clone()).await;
    let snapshot = store
        .run_concurrent(async |accessor| {
            let handler = router.hew_live_router_handler();
            handler
                .call_initialize(accessor, Snapshot { accepted: 40 })
                .await
                .unwrap()
                .unwrap();
            let (delivered, missing) = tokio::join!(
                handler.call_route(
                    accessor,
                    Event {
                        source: "sensor".into(),
                        body: "hé\0雪".into()
                    }
                ),
                handler.call_route(
                    accessor,
                    Event {
                        source: "missing".into(),
                        body: String::new()
                    }
                ),
            );
            assert!(delivered.unwrap().unwrap());
            assert!(!missing.unwrap().unwrap());
            let denied = handler
                .call_route(
                    accessor,
                    Event {
                        source: "denied".into(),
                        body: "forbidden".into(),
                    },
                )
                .await
                .unwrap();
            assert!(denied.unwrap_err().contains("destination grant denied"));
            let snapshot = handler.call_quiesce(accessor).await.unwrap().unwrap();
            assert_eq!(snapshot.accepted, 41);
            let refused = handler
                .call_route(
                    accessor,
                    Event {
                        source: "paused".into(),
                        body: "forbidden".into(),
                    },
                )
                .await
                .unwrap();
            assert!(refused.unwrap_err().contains("not accepting"));
            // A rejected replacement can resume this exact retained actor state.
            handler.call_resume(accessor).await.unwrap().unwrap();
            assert!(handler
                .call_route(
                    accessor,
                    Event {
                        source: "again".into(),
                        body: String::new()
                    }
                )
                .await
                .unwrap()
                .unwrap());
            let snapshot = handler.call_quiesce(accessor).await.unwrap().unwrap();
            assert_eq!(snapshot.accepted, 42);
            handler.call_dispose(accessor).await.unwrap().unwrap();
            snapshot
        })
        .await
        .unwrap();
    let (mut second, router) = activate(engine, component, "second", deliveries.clone()).await;
    second
        .run_concurrent(async |accessor| {
            let handler = router.hew_live_router_handler();
            handler
                .call_initialize(accessor, snapshot)
                .await
                .unwrap()
                .unwrap();
            assert!(handler
                .call_route(
                    accessor,
                    Event {
                        source: "sensor".into(),
                        body: "restored".into()
                    }
                )
                .await
                .unwrap()
                .unwrap());
            let snapshot = handler.call_quiesce(accessor).await.unwrap().unwrap();
            assert_eq!(snapshot.accepted, 43);
            handler.call_dispose(accessor).await.unwrap().unwrap();
        })
        .await
        .unwrap();
    let delivered = deliveries.lock().unwrap();
    assert_eq!(delivered.len(), 3);
    assert_eq!(delivered[0].destination, "first/sensor");
    assert_eq!(delivered[0].body, "hé\0雪");
    assert_eq!(delivered[1].destination, "first/again");
    assert_eq!(delivered[1].body, "");
    assert_eq!(delivered[2].destination, "second/sensor");
    assert_eq!(delivered[2].body, "restored");
}
