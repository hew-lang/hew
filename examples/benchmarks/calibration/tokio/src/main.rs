//! Runtime construction, actor setup and one warm-up task precede timing.
//! Fanout includes request/reply clients' task lifetime, with one client per actor.
use std::{
    env,
    time::{Duration, Instant},
};
use tokio::sync::{mpsc, oneshot};

type Sender = mpsc::Sender<oneshot::Sender<i64>>;

fn counter() -> (Sender, tokio::task::JoinHandle<()>) {
    let (tx, mut rx) = mpsc::channel::<oneshot::Sender<i64>>(1);
    let task = tokio::spawn(async move {
        let mut count = 0;
        while let Some(reply) = rx.recv().await {
            count += 1;
            reply.send(count).expect("caller remains alive");
        }
    });
    (tx, task)
}

async fn increment(actor: &Sender) -> i64 {
    let (tx, rx) = oneshot::channel();
    actor.send(tx).await.expect("actor remains alive");
    rx.await.expect("actor replied")
}

async fn stream_worker(
    mut input: mpsc::Receiver<i64>,
    output: mpsc::Sender<i64>,
    ready: mpsc::Sender<()>,
    gate: oneshot::Receiver<()>,
    n: i64,
) {
    ready.send(()).await.expect("stream worker ready");
    gate.await.expect("stream worker start");
    for _ in 0..n {
        let value = input.recv().await.expect("stream request");
        output.send(value + 1).await.expect("stream reply");
    }
}

async fn stream_client(
    requests: mpsc::Sender<i64>,
    mut returned: mpsc::Receiver<i64>,
    ready: mpsc::Sender<()>,
    gate: oneshot::Receiver<()>,
    n: i64,
    sample_latency: bool,
) -> (i64, Vec<u128>, u128) {
    ready.send(()).await.expect("stream client ready");
    gate.await.expect("stream client start");
    let start = Instant::now();
    let mut checksum = 0;
    let mut samples = Vec::new();
    for index in 0..n {
        if sample_latency && index < 640_000 && index % 64 == 0 {
            let before = Instant::now();
            requests.send(index).await.expect("stream request");
            assert_eq!(returned.recv().await.expect("stream reply"), index + 1);
            samples.push(before.elapsed().as_nanos());
        } else {
            requests.send(index).await.expect("stream request");
            assert_eq!(returned.recv().await.expect("stream reply"), index + 1);
        }
        checksum += index;
    }
    (checksum, samples, start.elapsed().as_nanos())
}

async fn pipeline_worker(
    output: mpsc::Sender<i64>,
    ready: mpsc::Sender<()>,
    gate: oneshot::Receiver<()>,
    n: i64,
    id: i64,
    pairs: i64,
) {
    ready.send(()).await.expect("pipeline readiness");
    gate.await.expect("pipeline start");
    for index in 0..n {
        output
            .send(index * pairs + id)
            .await
            .expect("pipeline output");
    }
}

async fn run(workload: &str, n: i64, size: i64) {
    tokio::spawn(async { 0 }).await.unwrap();
    let mut actors = Vec::new();
    let count = match workload {
        "actor_roundtrip" | "actor_roundtrip_alloc" => 1,
        "actor_fanout" => size,
        "stream_roundtrip" | "stream_roundtrip_alloc" => 0,
        "stream_pipeline" | "stream_pipeline_alloc" => 0,
        _ => 0,
    };
    for _ in 0..count {
        actors.push(counter());
    }
    let mut checksum = 0_i64;
    let mut latencies = Vec::<u128>::new();
    let mut start = Instant::now();
    let mut measured_elapsed = None;
    match workload {
        "task_ready" => {
            let mut offset = 0;
            while offset < n {
                let end = (offset + size).min(n);
                let mut tasks = Vec::new();
                for index in offset..end {
                    tasks.push(tokio::spawn(async move { index % 251 }));
                }
                for task in tasks {
                    checksum += task.await.unwrap();
                }
                offset = end;
            }
        }
        "actor_roundtrip" => {
            for index in 0..n {
                let before = Instant::now();
                checksum += increment(&actors[0].0).await;
                let elapsed = before.elapsed().as_nanos();
                if index < 10000 {
                    latencies.push(elapsed);
                }
            }
        }
        "actor_roundtrip_alloc" => {
            for _ in 0..n {
                checksum += increment(&actors[0].0).await;
            }
        }
        "actor_fanout" => {
            let mut clients = Vec::new();
            for (actor, _) in &actors {
                let actor = actor.clone();
                clients.push(tokio::spawn(async move {
                    let mut sum = 0;
                    for _ in 0..n {
                        sum += increment(&actor).await;
                    }
                    sum
                }));
            }
            for client in clients {
                checksum += client.await.unwrap();
            }
        }
        "timer" => {
            // Like Hew, sleep runs on a worker task rather than the root executor.
            latencies = tokio::spawn(async move {
                let mut samples = Vec::new();
                for index in 0..n {
                    let before = Instant::now();
                    tokio::time::sleep(Duration::from_millis(1)).await;
                    let overshoot = before.elapsed().as_nanos().saturating_sub(1_000_000);
                    if index < 10000 {
                        samples.push(overshoot);
                    }
                }
                samples
            })
            .await
            .unwrap();
            checksum = n;
        }
        "stream_roundtrip" | "stream_roundtrip_alloc" => {
            let (requests, incoming) = mpsc::channel::<i64>(1);
            let (replies, returned) = mpsc::channel::<i64>(1);
            let (ready_tx, mut ready_rx) = mpsc::channel::<()>(2);
            let (worker_gate_tx, worker_gate_rx) = oneshot::channel();
            let (client_gate_tx, client_gate_rx) = oneshot::channel();
            let worker = tokio::spawn(stream_worker(
                incoming,
                replies,
                ready_tx.clone(),
                worker_gate_rx,
                n,
            ));
            let sample_latency = workload == "stream_roundtrip";
            let client = tokio::spawn(stream_client(
                requests,
                returned,
                ready_tx,
                client_gate_rx,
                n,
                sample_latency,
            ));
            for _ in 0..2 {
                ready_rx.recv().await.expect("stream ready");
            }
            worker_gate_tx.send(()).expect("worker gate");
            client_gate_tx.send(()).expect("client gate");
            start = Instant::now();
            let (sum, samples, elapsed) = client.await.unwrap();
            checksum = sum;
            latencies = samples;
            measured_elapsed = Some(elapsed);
            worker.await.unwrap();
        }
        "stream_pipeline" | "stream_pipeline_alloc" => {
            let (ready_tx, mut ready_rx) = mpsc::channel::<()>(size as usize * 2);
            let mut inputs = Vec::new();
            let mut workers = Vec::new();
            let mut consumers = Vec::new();
            let mut gates = Vec::new();
            let sample_latency = workload == "stream_pipeline";
            for pair in 0..size {
                let (tx, rx) = mpsc::channel::<i64>(1);
                let (worker_gate_tx, worker_gate_rx) = oneshot::channel();
                let (consumer_gate_tx, consumer_gate_rx) = oneshot::channel();
                gates.push(worker_gate_tx);
                gates.push(consumer_gate_tx);
                workers.push(tokio::spawn(pipeline_worker(
                    tx.clone(),
                    ready_tx.clone(),
                    worker_gate_rx,
                    n,
                    pair,
                    size,
                )));
                inputs.push(tx);
                let consumer_ready = ready_tx.clone();
                consumers.push(tokio::spawn(async move {
                    consumer_ready
                        .send(())
                        .await
                        .expect("pipeline consumer readiness");
                    consumer_gate_rx.await.expect("pipeline consumer start");
                    let mut input = rx;
                    let mut sum = 0;
                    let mut samples = Vec::new();
                    for index in 0..n {
                        let expected = index * size + pair;
                        let value = if sample_latency && expected < 640_000 && expected % 64 == 0 {
                            let before = Instant::now();
                            let item = input.recv().await.expect("pipeline output");
                            samples.push(before.elapsed().as_nanos());
                            item
                        } else {
                            input.recv().await.expect("pipeline output")
                        };
                        assert_eq!(value, expected, "pipeline sequence mismatch");
                        sum += value % 251;
                    }
                    (sum, samples)
                }));
            }
            drop(inputs);
            drop(ready_tx);
            for _ in 0..(size * 2) {
                ready_rx.recv().await.expect("pipeline endpoint ready");
            }
            start = Instant::now();
            for gate in gates {
                gate.send(()).expect("pipeline start gate");
            }
            for worker in workers {
                worker.await.unwrap();
            }
            for consumer in consumers {
                let (sum, samples) = consumer.await.unwrap();
                checksum += sum;
                latencies.extend(samples);
            }
        }
        _ => panic!("unknown concurrency workload"),
    }
    let elapsed = measured_elapsed.unwrap_or_else(|| start.elapsed().as_nanos());
    for (actor, task) in actors {
        drop(actor);
        task.await.unwrap();
    }
    println!("{{\"workload\":\"{workload}\",\"iterations\":{n},\"size\":{size},\"elapsed_ns\":{elapsed},\"checksum\":{checksum},\"latencies_ns\":{latencies:?}}}");
}

fn main() {
    let args: Vec<_> = env::args().collect();
    assert_eq!(args.len(), 4, "usage: concurrency WORKLOAD ITERATIONS SIZE");
    let n: i64 = args[2].parse().expect("positive iterations");
    let size: i64 = args[3].parse().expect("positive size");
    assert!(n > 0 && size > 0);
    let workers: usize = env::var("BENCH_WORKERS")
        .unwrap_or_else(|_| "1".into())
        .parse()
        .expect("positive worker count");
    assert!(workers > 0);
    tokio::runtime::Builder::new_multi_thread()
        .worker_threads(workers)
        .enable_time()
        .build()
        .unwrap()
        .block_on(run(&args[1], n, size));
}
