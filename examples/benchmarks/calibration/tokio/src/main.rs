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

async fn run(workload: &str, n: i64, size: i64) {
    tokio::spawn(async { 0 }).await.unwrap();
    let mut actors = Vec::new();
    let count = match workload {
        "actor_roundtrip" | "actor_roundtrip_alloc" => 1,
        "actor_fanout" => size,
        _ => 0,
    };
    for _ in 0..count {
        actors.push(counter());
    }
    let mut checksum = 0_i64;
    let mut latencies = Vec::<u128>::new();
    let start = Instant::now();
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
        _ => panic!("unknown concurrency workload"),
    }
    let elapsed = start.elapsed().as_nanos();
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
