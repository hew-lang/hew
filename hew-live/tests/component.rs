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

fn build_component(directory: &Path, constructor: &str) -> Vec<u8> {
    build_fixture(directory, "router.hew", constructor)
}

fn build_fixture(directory: &Path, source: &str, constructor: &str) -> Vec<u8> {
    let root = Path::new(env!("CARGO_MANIFEST_DIR"));
    let fixture = root.join("tests/fixtures");
    write_guest_bindings(directory).unwrap();
    std::fs::copy(fixture.join(source), directory.join("router_app.hew")).unwrap();
    run(Command::new(environment("HEW_LIVE_COMPILER"))
        .args(["build", "--target", "wasm32-wasip1", "--emit-obj"])
        .args(["--export-c", &format!("{constructor}=guest_create")])
        .args(["--export-c-async", "route=guest_route"])
        .args(["--export-c-async", "snapshot=guest_snapshot"])
        .args(["--export-c-async", "dispose=guest_dispose"])
        .arg(directory.join("router_app.hew"))
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
        .arg(directory.join("router_adapter.c"))
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
    store.fuel_async_yield_interval(Some(100_000)).unwrap();
    let router = bindings::Router::instantiate_async(&mut store, component, &linker)
        .await
        .unwrap();
    (store, router)
}

#[tokio::test(flavor = "current_thread")]
async fn hew_and_foreign_components_share_services_and_snapshot_contracts() {
    let directory = tempfile::tempdir().unwrap();
    let bytes = build_component(directory.path(), "create");
    let mut config = Config::new();
    config.wasm_component_model_async(true).consume_fuel(true);
    let engine = Engine::new(&config).unwrap();
    // Preparation compiles the immutable component before any service is granted.
    let component = Component::new(&engine, &bytes).unwrap();
    tokio::time::timeout(
        std::time::Duration::from_secs(30),
        exercise_router(&engine, &component),
    )
    .await
    .expect("Hew component lifecycle must make progress");
    let core = std::fs::read(environment("HEW_LIVE_FOREIGN_COMPONENT")).unwrap();
    let foreign = Component::new(&engine, component_bytes(&core)).unwrap();
    exercise_router(&engine, &foreign).await;
    let stalled_directory = tempfile::tempdir().unwrap();
    let stalled = build_component(stalled_directory.path(), "create_stalled");
    tokio::time::timeout(
        std::time::Duration::from_mins(2),
        exercise_deployment(&bytes, &stalled),
    )
    .await
    .expect("host replacement must make progress independently of the guest");
}

struct DeploymentServices {
    idle: Arc<tokio::sync::Notify>,
    idle_calls: Arc<std::sync::atomic::AtomicUsize>,
    started: Arc<tokio::sync::Notify>,
    release: Arc<tokio::sync::Notify>,
    delivered: Arc<Mutex<Vec<Delivery>>>,
}

impl DeploymentServices {
    async fn observe_idle_turn(&self) {
        use std::sync::atomic::Ordering;
        let previous = self.idle_calls.load(Ordering::Acquire);
        tokio::time::timeout(std::time::Duration::from_secs(2), async {
            while self.idle_calls.load(Ordering::Acquire) == previous {
                self.idle.notified().await;
            }
        })
        .await
        .expect("autonomous actor must call its service without host ingress");
    }
}

impl hew_live::component::host::Services for DeploymentServices {
    fn destination(
        &self,
        source: String,
    ) -> hew_live::component::host::ServiceCall<Option<String>> {
        let idle = Arc::clone(&self.idle);
        let idle_calls = Arc::clone(&self.idle_calls);
        let started = Arc::clone(&self.started);
        let release = Arc::clone(&self.release);
        Box::pin(async move {
            if source == "idle" {
                idle_calls.fetch_add(1, std::sync::atomic::Ordering::Release);
                idle.notify_one();
            }
            if source == "held" {
                started.notify_one();
                release.notified().await;
            }
            Ok(Some(source))
        })
    }

    fn emit(&self, message: Delivery) -> hew_live::component::host::ServiceCall<()> {
        let delivered = Arc::clone(&self.delivered);
        Box::pin(async move {
            delivered.lock().unwrap().push(message);
            Ok(())
        })
    }
}

fn policy_services() -> Arc<DeploymentServices> {
    Arc::new(DeploymentServices {
        idle: Arc::default(),
        idle_calls: Arc::default(),
        started: Arc::default(),
        release: Arc::default(),
        delivered: Arc::default(),
    })
}

fn foreign_bytes() -> Vec<u8> {
    component_bytes(&std::fs::read(environment("HEW_LIVE_FOREIGN_COMPONENT")).unwrap())
}

#[tokio::test(flavor = "current_thread")]
async fn failed_rollback_resume_closes_the_retained_instance() {
    use hew_live::component::host::{Error, Host};

    let bytes = component_bytes(
        &std::fs::read(environment("HEW_LIVE_FOREIGN_ROLLBACK_COMPONENT")).unwrap(),
    );
    let ordinary = foreign_bytes();
    for (active, refuses_resume) in [(&ordinary, false), (&bytes, true)] {
        let services = policy_services();
        let host = Arc::new(Host::new(services.clone()).unwrap());
        let deployment = host
            .activate(host.prepare(active).unwrap(), Snapshot { accepted: 0 })
            .await
            .unwrap();
        assert!(deployment
            .route(Event {
                source: "ok".into(),
                body: "accepted".into(),
            })
            .await
            .unwrap());
        assert!(matches!(
            deployment
                .replace(
                    host.prepare(&bytes).unwrap(),
                    1,
                    std::time::Duration::from_secs(5)
                )
                .await,
            Err(Error::Guest(_))
        ));
        for _ in 0..2 {
            let result = deployment
                .route(Event {
                    source: "ok".into(),
                    body: "after rollback".into(),
                })
                .await;
            if refuses_resume {
                let Err(Error::Engine(error)) = result else {
                    panic!("expected a terminated instance: {result:?}");
                };
                assert!(error.to_string().contains("foreign resumption refused"));
            } else {
                assert!(result.unwrap());
            }
        }
        assert_eq!(
            services.delivered.lock().unwrap().len(),
            if refuses_resume { 1 } else { 3 }
        );
    }
}

#[tokio::test(flavor = "current_thread")]
#[allow(
    clippy::too_many_lines,
    reason = "one admission lifecycle proves retention through caller loss and actual Store disposal"
)]
async fn policy_admission_retains_routes_after_caller_loss_and_refunds_preparation() {
    use hew_live::component::host::{
        Error, Host, HostPolicy, PolicyLimit, Preview, ServiceFixture,
    };
    let bytes = foreign_bytes();
    let services = policy_services();
    let host = Arc::new(
        Host::with_policy(
            services.clone(),
            HostPolicy {
                max_component_bytes: bytes.len(),
                max_prepared_components: 1,
                max_instances: 1,
                queue_capacity: 1,
                max_routes: 1,
                max_payload_bytes: 128,
                max_preview_fixtures: 1,
                ..HostPolicy::default()
            },
        )
        .unwrap(),
    );
    assert!(matches!(
        host.prepare(&vec![0; bytes.len() + 1]),
        Err(Error::Refused(PolicyLimit::ComponentBytes))
    ));
    assert!(matches!(
        host.prepare(b"invalid component"),
        Err(Error::Engine(_))
    ));
    let prepared = host.prepare(&bytes).unwrap();
    assert!(matches!(
        host.prepare(&bytes),
        Err(Error::Refused(PolicyLimit::PreparedComponents))
    ));
    let preview = Preview {
        state: Snapshot { accepted: 0 },
        input: Event {
            source: "held".into(),
            body: String::new(),
        },
        fixtures: vec![
            ServiceFixture::Destination {
                source: "held".into(),
                result: Ok(None)
            };
            2
        ],
    };
    assert!(matches!(
        host.preview(prepared.clone(), preview, std::time::Duration::from_secs(1))
            .await,
        Err(Error::Refused(PolicyLimit::PreviewFixtures))
    ));
    let deployment = Arc::new(
        host.activate(prepared.clone(), Snapshot { accepted: 0 })
            .await
            .unwrap(),
    );
    assert!(matches!(
        host.activate(prepared, Snapshot { accepted: 0 }).await,
        Err(Error::Refused(PolicyLimit::Instances))
    ));
    assert!(matches!(
        deployment
            .route(Event {
                source: "oversized".into(),
                body: "x".repeat(129)
            })
            .await,
        Err(Error::Refused(PolicyLimit::PayloadBytes))
    ));
    let caller = Arc::clone(&deployment);
    let pending = tokio::spawn(async move {
        caller
            .route(Event {
                source: "held".into(),
                body: "one".into(),
            })
            .await
    });
    services.started.notified().await;
    pending.abort();
    let _ = pending.await;
    assert!(matches!(
        deployment
            .route(Event {
                source: "next".into(),
                body: String::new()
            })
            .await,
        Err(Error::Refused(PolicyLimit::Routes))
    ));
    services.release.notify_one();
    tokio::time::timeout(std::time::Duration::from_secs(2), async {
        loop {
            match deployment
                .route(Event {
                    source: "next".into(),
                    body: String::new(),
                })
                .await
            {
                Err(Error::Refused(PolicyLimit::Routes)) => tokio::task::yield_now().await,
                result => {
                    assert!(result.unwrap());
                    break;
                }
            }
        }
    })
    .await
    .unwrap();
    assert_eq!(services.delivered.lock().unwrap().len(), 2);
    Arc::try_unwrap(deployment)
        .unwrap()
        .dispose()
        .await
        .unwrap()
        .unwrap();
    // Store destruction, rather than loss of a caller, returns retention permits.
    tokio::time::timeout(std::time::Duration::from_secs(2), async {
        loop {
            match host.prepare(&bytes) {
                Err(Error::Refused(PolicyLimit::PreparedComponents)) => {
                    tokio::task::yield_now().await;
                }
                result => {
                    result.unwrap();
                    break;
                }
            }
        }
    })
    .await
    .unwrap();
}

#[tokio::test(flavor = "current_thread")]
async fn policy_service_exhaustion_is_an_admitted_fault() {
    use hew_live::component::host::{Error, Host, HostPolicy};
    let bytes = foreign_bytes();
    for (index, policy) in [
        HostPolicy {
            max_service_calls: 1,
            ..HostPolicy::default()
        },
        HostPolicy {
            max_output_bytes: 3,
            ..HostPolicy::default()
        },
        HostPolicy {
            host_operation_timeout: std::time::Duration::from_millis(10),
            ..HostPolicy::default()
        },
    ]
    .into_iter()
    .enumerate()
    {
        let services = policy_services();
        let host = Arc::new(Host::with_policy(services.clone(), policy).unwrap());
        let deployment = host
            .activate(host.prepare(&bytes).unwrap(), Snapshot { accepted: 0 })
            .await
            .unwrap();
        let error = deployment.route(Event {
            source: if index == 2 { "held" } else { "ok" }.into(),
            body: "body".into(),
        });
        // The first two policies fail on traffic before any provider effect;
        // the timeout policy enters its provider and expires there.
        let result = tokio::time::timeout(std::time::Duration::from_secs(2), error)
            .await
            .unwrap();
        let Err(Error::Engine(error)) = result else {
            panic!("expected an admitted execution fault: {result:?}")
        };
        let expected = ["ServiceCalls", "OutputBytes", "expired"][index];
        assert!(format!("{error:#}").contains(expected), "{error:#}");
        assert!(services.delivered.lock().unwrap().is_empty());
        drop(deployment);
    }
}

#[tokio::test(flavor = "current_thread")]
async fn policy_limits_cover_child_allocations_and_guest_fuel() {
    use hew_live::component::host::{Error, Host, HostPolicy};
    let directory = tempfile::tempdir().unwrap();
    let bytes = build_fixture(directory.path(), "policy_router.hew", "create");
    for (limit, succeeds) in [(32 * 1024 * 1024, false), (128 * 1024 * 1024, true)] {
        let host = Arc::new(
            Host::with_policy(
                policy_services(),
                HostPolicy {
                    memory_bytes: limit,
                    ..HostPolicy::default()
                },
            )
            .unwrap(),
        );
        let deployment = host
            .activate(host.prepare(&bytes).unwrap(), Snapshot { accepted: 0 })
            .await
            .unwrap();
        assert!(deployment
            .route(Event {
                source: "small".into(),
                body: String::new()
            })
            .await
            .unwrap());
        let result = deployment
            .route(Event {
                source: "allocate".into(),
                body: String::new(),
            })
            .await;
        if succeeds {
            assert!(result.unwrap());
            deployment.dispose().await.unwrap().unwrap();
        } else {
            let Err(Error::Engine(error)) = result else {
                panic!("expected a child memory fault: {result:?}")
            };
            assert!(format!("{error:#}").contains("memory"), "{error:#}");
        }
    }
    let host = Arc::new(
        Host::with_policy(
            policy_services(),
            HostPolicy {
                fuel: 0,
                ..HostPolicy::default()
            },
        )
        .unwrap(),
    );
    let error = host
        .activate(host.prepare(&bytes).unwrap(), Snapshot { accepted: 0 })
        .await
        .unwrap_err();
    let Error::Engine(error) = error else {
        panic!("expected a fuel fault: {error}")
    };
    assert!(format!("{error:#}").contains("fuel"), "{error:#}");

    let host = Arc::new(
        Host::with_policy(
            policy_services(),
            HostPolicy {
                max_host_resources: 0,
                ..HostPolicy::default()
            },
        )
        .unwrap(),
    );
    let result = match host
        .activate(host.prepare(&bytes).unwrap(), Snapshot { accepted: 0 })
        .await
    {
        Ok(deployment) => {
            deployment
                .route(Event {
                    source: "resources".into(),
                    body: String::new(),
                })
                .await
        }
        Err(error) => Err(error),
    };
    let Err(Error::Engine(error)) = result else {
        panic!("expected a WASI resource fault: {result:?}")
    };
    assert!(format!("{error:#}").contains("resource"), "{error:#}");
}

#[allow(
    clippy::too_many_lines,
    reason = "one replacement oracle checks admission, expiry, autonomous recovery and retirement"
)]
async fn exercise_deployment(bytes: &[u8], stalled: &[u8]) {
    use hew_live::component::host::{Error, Host, Stage};
    use std::time::Duration;

    let services = Arc::new(DeploymentServices {
        idle: Arc::default(),
        idle_calls: Arc::default(),
        started: Arc::default(),
        release: Arc::default(),
        delivered: Arc::default(),
    });
    let observation = Arc::new(Observed::default());
    let host = Arc::new(
        // This oracle exercises deadline revocation independently of fuel
        // exhaustion, which has its own explicit policy control above.
        Host::with_policy(
            services.clone(),
            hew_live::component::host::HostPolicy {
                fuel: 1_000_000_000_000,
                ..hew_live::component::host::HostPolicy::default()
            },
        )
        .unwrap()
        .with_observer(observation.clone()),
    );
    let cache_directory = tempfile::tempdir().unwrap();
    let identity = {
        let cache =
            hew_live::component::cache::ArtifactCache::open(cache_directory.path()).unwrap();
        host.prepare_cached(&cache, bytes).unwrap().digest()
    };
    let cache = hew_live::component::cache::ArtifactCache::open(cache_directory.path()).unwrap();
    let prepared = host.restore(&cache, identity).unwrap();
    let stalled = host.prepare(stalled).unwrap();
    let deployment = Arc::new(
        host.activate(prepared.clone(), Snapshot { accepted: 40 })
            .await
            .unwrap(),
    );
    services.observe_idle_turn().await;
    assert!(deployment
        .route(Event {
            source: "first".into(),
            body: "owned".into()
        })
        .await
        .unwrap());

    let caller = {
        let deployment = Arc::clone(&deployment);
        tokio::spawn(async move {
            deployment
                .route(Event {
                    source: "held".into(),
                    body: "admitted".into(),
                })
                .await
        })
    };
    services.started.notified().await;
    caller.abort();
    assert!(caller.await.unwrap_err().is_cancelled());
    assert!(matches!(
        deployment
            .replace(prepared.clone(), 1, Duration::from_millis(1))
            .await,
        Err(Error::Expired(Stage::Admission))
    ));
    assert_eq!(deployment.epoch().await, 1);
    services.release.notify_one();

    let committed = deployment
        .replace(prepared.clone(), 1, Duration::from_secs(5))
        .await
        .unwrap();
    assert_eq!(committed.snapshot.accepted, 42);
    assert_eq!(committed.epoch, 2);
    assert_eq!(
        deployment.status().await,
        hew_live::component::host::Status {
            epoch: 2,
            digest: identity
        }
    );
    committed.retirement.await.unwrap().unwrap();

    // The ordinary Hew snapshot sleeps for five milliseconds. A shorter
    // attempt expires while retaining that old Store, regardless of which
    // executor happens to observe the deadline first.
    let expired = deployment
        .replace(prepared.clone(), 2, Duration::from_millis(1))
        .await
        .unwrap_err();
    assert!(matches!(expired, Error::Expired(_)), "{expired}");
    assert!(deployment
        .route(Event {
            source: "resumed".into(),
            body: "same state".into()
        })
        .await
        .unwrap());

    // Pure candidate initialization loops forever. Fuel yields allow the host
    // deadline to revoke commit authority without trapping the old deployment.
    let expired = deployment
        .replace(stalled, 2, Duration::from_secs(5))
        .await
        .unwrap_err();
    assert!(
        matches!(expired, Error::Expired(Stage::Initialization)),
        "{expired}"
    );
    assert_eq!(deployment.epoch().await, 2);
    services.observe_idle_turn().await;
    assert!(deployment
        .route(Event {
            source: "after-expiry".into(),
            body: "still owned".into()
        })
        .await
        .unwrap());
    let committed = deployment
        .replace(prepared.clone(), 2, Duration::from_secs(5))
        .await
        .unwrap();
    assert_eq!(committed.snapshot.accepted, 44);
    assert_eq!(committed.epoch, 3);
    committed.retirement.await.unwrap().unwrap();
    assert!(matches!(
        deployment
            .replace(prepared, 1, Duration::from_secs(5))
            .await,
        Err(Error::PredecessorChanged)
    ));
    assert_eq!(services.delivered.lock().unwrap().len(), 4);
    Arc::try_unwrap(deployment)
        .unwrap()
        .dispose()
        .await
        .unwrap()
        .unwrap();
    let events = observation.0.lock().unwrap();
    for admitted in events.iter().filter(|event| event.phase == "admitted") {
        let terminal: Vec<_> = events
            .iter()
            .filter(|event| {
                event.deployment_id == admitted.deployment_id
                    && event.invocation_id == admitted.invocation_id
                    && matches!(event.phase.as_str(), "completed" | "faulted")
            })
            .collect();
        assert_eq!(
            terminal.len(),
            1,
            "caller cancellation must retain terminal attribution"
        );
        assert_eq!(terminal[0].code_id, admitted.code_id);
        assert_eq!(terminal[0].activation_epoch, admitted.activation_epoch);
    }
    assert!(events.iter().any(|event| event.phase == "rejected"));
    assert!(events
        .iter()
        .any(|event| event.phase == "committed" && event.activation_epoch == 3));
}

#[derive(Default)]
struct Observed(Mutex<Vec<hew_observe_protocol::LiveExecution>>);

impl hew_live::component::host::Observer for Observed {
    fn record(&self, event: &hew_observe_protocol::LiveExecution) {
        self.0.lock().unwrap().push(event.clone());
    }
}

#[tokio::test(flavor = "current_thread")]
async fn controlled_preview_requires_matching_services_and_never_calls_production() {
    use hew_live::component::host::{Host, Preview, ServiceFixture};
    use std::time::Duration;
    struct NoProduction;
    impl hew_live::component::host::Services for NoProduction {
        fn destination(&self, _: String) -> hew_live::component::host::ServiceCall<Option<String>> {
            panic!("preview reached a production service");
        }
        fn emit(&self, _: Delivery) -> hew_live::component::host::ServiceCall<()> {
            panic!("preview emitted a production delivery");
        }
    }
    let observed = Arc::new(Observed::default());
    let host = Arc::new(
        Host::new(Arc::new(NoProduction))
            .unwrap()
            .with_observer(observed.clone()),
    );
    let bytes = std::fs::read(environment("HEW_LIVE_FOREIGN_COMPONENT")).unwrap();
    let prepared = host.prepare(&component_bytes(&bytes)).unwrap();
    let preview = |fixtures| Preview {
        state: Snapshot { accepted: 9 },
        input: Event {
            source: "private-source".into(),
            body: "private-body".into(),
        },
        fixtures,
    };
    let destination = ServiceFixture::Destination {
        source: "private-source".into(),
        result: Ok(Some("test-route".into())),
    };
    let emit = ServiceFixture::Emit {
        message: Delivery {
            destination: "test-route".into(),
            body: "private-body".into(),
        },
        result: Ok(()),
    };
    let matched = host
        .preview(
            prepared.clone(),
            preview(vec![destination.clone(), emit.clone()]),
            Duration::from_secs(10),
        )
        .await
        .unwrap();
    assert_eq!(matched.accepted, Some(true));
    assert!(matched.needs_input.is_none());
    assert_eq!(matched.used_fixtures, 2);
    let missing = host
        .preview(
            prepared.clone(),
            preview(vec![destination]),
            Duration::from_secs(10),
        )
        .await
        .unwrap();
    assert_eq!(missing.accepted, None);
    assert!(missing.needs_input.unwrap().starts_with("emit("));
    assert_eq!(missing.used_fixtures, 1);
    let mismatch = host
        .preview(prepared, preview(vec![emit]), Duration::from_secs(10))
        .await
        .unwrap();
    assert_eq!(mismatch.accepted, None);
    assert!(mismatch.needs_input.unwrap().starts_with("destination("));
    assert_eq!(mismatch.used_fixtures, 0);
    let records = observed.0.lock().unwrap();
    assert!(records.iter().all(|event| event.mode == "preview"));
    let recorded = serde_json::to_string(&*records).unwrap();
    assert!(!recorded.contains("private-source"));
    assert!(!recorded.contains("private-body"));
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
            let (first, second) = tokio::join!(
                handler.call_quiesce(accessor),
                handler.call_quiesce(accessor),
            );
            let snapshot = match (first.unwrap(), second.unwrap()) {
                (Ok(snapshot), Err(error)) | (Err(error), Ok(snapshot)) => {
                    assert!(error.contains("drain admitted calls"));
                    snapshot
                }
                other => panic!("exactly one quiescence attempt must own admission: {other:?}"),
            };
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
            // Resume also withdraws an in-progress attempt. Completion may win
            // the race; either way the old instance must accept the next call.
            let (abandoned, resumed) = tokio::join!(
                handler.call_quiesce(accessor),
                handler.call_resume(accessor),
            );
            if let Ok(snapshot) = abandoned.unwrap() {
                assert_eq!(snapshot.accepted, 41);
            }
            resumed.unwrap().unwrap();
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
