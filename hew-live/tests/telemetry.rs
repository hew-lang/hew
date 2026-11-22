#![cfg(feature = "component")]

use std::path::PathBuf;
use std::sync::Arc;
use std::time::Duration;

use hew_live::component::build::{build_telemetry, Toolchain};
use hew_live::component::host::{Error, Observer, Prepared, ServiceCall};
use hew_live::component::telemetry::{Calibration, Host, Sample, Snapshot};
use hew_observe_protocol::LiveExecution;
use tokio::sync::Notify;

const MONITOR: &str = include_str!("../examples/telemetry.hew");
const REFERENCE: &str = "const LIMIT_DIVISOR: u32 = 1;";
const TIGHTER: &str = "const LIMIT_DIVISOR: u32 = 2;";
const CONSTRUCTOR: &str = "pub fn create(accepted: u64, previous: u32, last: u32) -> Monitor {";
const REFUSAL: &str = "    assert(accepted == 0, \"this firmware requires an empty window\");";

#[derive(Default)]
struct CalibrationLog {
    in_flight: Notify,
    release: Arc<Notify>,
}

impl Calibration for CalibrationLog {
    fn limit(&self, sequence: u64) -> ServiceCall<u32> {
        if sequence == 3 {
            let release = Arc::clone(&self.release);
            self.in_flight.notify_one();
            Box::pin(async move {
                release.notified().await;
                Ok(250)
            })
        } else {
            Box::pin(async move {
                if sequence == 6 {
                    Err("calibration unavailable".into())
                } else {
                    Ok(250)
                }
            })
        }
    }
}

#[derive(Default)]
struct ReplacementWaiting(Notify);

impl Observer for ReplacementWaiting {
    fn record(&self, event: &LiveExecution) {
        assert_eq!(event.semantic_profile, "hew:live-telemetry@0.1.0");
        if event.phase == "waiting" {
            self.0.notify_one();
        }
    }
}

fn tool_path(name: &str) -> PathBuf {
    std::env::var_os(name).map_or_else(|| panic!("set {name} to exercise the live SDK"), Into::into)
}

fn reading(sequence: u64, vibration: u32) -> Sample {
    Sample {
        sequence,
        vibration,
    }
}

fn prepare_generations(host: &Host) -> (Prepared, Prepared, Prepared) {
    let root = tempfile::tempdir().unwrap();
    let reference = root.path().join("reference.hew");
    let tightened = root.path().join("tightened.hew");
    let empty_only = root.path().join("empty_only.hew");
    assert!(MONITOR.contains(REFERENCE));
    std::fs::write(&reference, MONITOR).unwrap();
    std::fs::write(&tightened, MONITOR.replacen(REFERENCE, TIGHTER, 1)).unwrap();
    assert!(MONITOR.contains(CONSTRUCTOR));
    let refuses_warm = format!("{CONSTRUCTOR}\n{REFUSAL}");
    std::fs::write(&empty_only, MONITOR.replacen(CONSTRUCTOR, &refuses_warm, 1)).unwrap();

    let tools = Toolchain {
        compiler: tool_path("HEW_LIVE_COMPILER"),
        wasi_sdk: tool_path("HEW_WASI_SDK"),
        wasm_lib_dir: tool_path("HEW_LIVE_WASM_LIB_DIR"),
    };
    let first = host
        .prepare(&build_telemetry(&reference, &tools).unwrap())
        .unwrap();
    let second = host
        .prepare(&build_telemetry(&tightened, &tools).unwrap())
        .unwrap();
    let rejected = host
        .prepare(&build_telemetry(&empty_only, &tools).unwrap())
        .unwrap();
    (first, second, rejected)
}

#[tokio::test(flavor = "current_thread")]
async fn vibration_monitor_retains_its_window_after_replacement_and_refusal() {
    let calibration = Arc::new(CalibrationLog::default());
    let replacement_waiting = Arc::new(ReplacementWaiting::default());
    let host = Host::new(calibration.clone())
        .unwrap()
        .with_observer(replacement_waiting.clone());
    let (first, second, rejected) = prepare_generations(&host);
    assert_ne!(first.digest(), second.digest());
    let second_digest = second.digest();
    let within = Duration::from_secs(15);

    tokio::time::timeout(Duration::from_secs(45), async {
        let deployment = Arc::new(
            host.activate(
                first,
                Snapshot {
                    accepted: 0,
                    previous: 0,
                    last: 0,
                },
            )
            .await
            .unwrap(),
        );
        let initial = deployment.status().await;
        assert_eq!(initial.epoch, 1);
        assert!(!deployment.observe(reading(1, 1_000)).await.unwrap().ready);
        assert!(!deployment.observe(reading(2, 1_100)).await.unwrap().ready);

        let working = deployment.clone();
        let admitted = tokio::spawn(async move { working.observe(reading(3, 1_250)).await });
        calibration.in_flight.notified().await;
        let upgrading = deployment.clone();
        let replacing =
            tokio::spawn(async move { upgrading.replace(second, initial.epoch, within).await });
        replacement_waiting.0.notified().await;
        assert!(
            !replacing.is_finished(),
            "the admitted sample still has its grant"
        );
        calibration.release.notify_one();

        let old_reading = admitted.await.unwrap().unwrap();
        assert!(old_reading.ready);
        assert_eq!(old_reading.baseline, 1_050);
        assert!(!old_reading.anomaly);
        let committed = replacing.await.unwrap().unwrap();
        assert_eq!(committed.epoch, 2);
        assert_eq!(committed.digest, second_digest);
        assert_eq!(committed.snapshot.accepted, 3);
        assert_eq!(committed.snapshot.previous, 1_100);
        assert_eq!(committed.snapshot.last, 1_250);
        committed.retirement.await.unwrap().unwrap();
        let active = deployment.status().await;

        let new_reading = deployment.observe(reading(4, 1_325)).await.unwrap();
        assert!(new_reading.ready && new_reading.anomaly);
        assert_eq!(new_reading.baseline, 1_175);

        let refused = deployment
            .replace(rejected, active.epoch, within)
            .await
            .unwrap_err();
        assert!(
            matches!(refused, Error::Guest(ref text) if text.contains("requires an empty window")),
            "{refused}"
        );
        assert_eq!(deployment.status().await, active);
        let recovered = deployment.observe(reading(5, 1_330)).await.unwrap();
        assert_eq!(recovered.baseline, 1_287);
        assert!(!recovered.anomaly);

        let refused = deployment.observe(reading(6, 9_000)).await.unwrap_err();
        assert!(
            matches!(refused, Error::Guest(ref text) if text.contains("calibration unavailable")),
            "{refused}"
        );
        let recovered = deployment.observe(reading(7, 1_335)).await.unwrap();
        assert_eq!(recovered.baseline, 1_327);
        assert!(!recovered.anomaly);
        Arc::try_unwrap(deployment)
            .unwrap()
            .dispose()
            .await
            .unwrap()
            .unwrap();
    })
    .await
    .expect("the fenced replacement and cleanup must finish");
}
