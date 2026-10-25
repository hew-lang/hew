use std::{convert::Infallible, env, io, net::SocketAddr, sync::Arc, time::Duration};

use http_body_util::{BodyExt, Full, Limited};
use hyper::{
    Method, Request, Response, StatusCode,
    body::Bytes,
    header::{ACCEPT_ENCODING, CONNECTION, CONTENT_LENGTH, HOST},
    server::conn::http1,
    service::service_fn,
};
use hyper_util::rt::TokioIo;
use tokio::{
    net::{TcpListener, TcpStream},
    sync::Semaphore,
};

const BODY_SIZE: usize = 64;
const WORK_UNITS: usize = 200_000;

fn env_workers() -> Result<usize, io::Error> {
    match env::var("BENCH_WORKERS") {
        Ok(value) => value
            .parse::<usize>()
            .ok()
            .filter(|workers| *workers > 0)
            .ok_or_else(|| {
                io::Error::new(
                    io::ErrorKind::InvalidInput,
                    "BENCH_WORKERS must be a positive integer",
                )
            }),
        Err(env::VarError::NotPresent) => Ok(2),
        Err(error) => Err(io::Error::new(io::ErrorKind::InvalidInput, error)),
    }
}

fn compute(seed: u8, start: usize, count: usize) -> u64 {
    let mut total = 0_u64;
    for i in start..start + count {
        total += ((usize::from(seed) + i) % 251) as u64;
    }
    total
}

fn response(status: StatusCode, body: Vec<u8>) -> Response<Full<Bytes>> {
    Response::builder()
        .status(status)
        .header(CONNECTION, "close")
        .header(CONTENT_LENGTH, body.len())
        .body(Full::new(Bytes::from(body)))
        .expect("valid response")
}

async fn handle(
    request: Request<hyper::body::Incoming>,
) -> Result<Response<Full<Bytes>>, Infallible> {
    if request.method() != Method::POST {
        return Ok(response(
            StatusCode::METHOD_NOT_ALLOWED,
            b"method not allowed".to_vec(),
        ));
    }
    let path = request.uri().path().to_owned();
    if !matches!(
        path.as_str(),
        "/echo" | "/wait" | "/compute" | "/fanout" | "/io"
    ) {
        return Ok(response(StatusCode::NOT_FOUND, b"not found".to_vec()));
    }
    let body = match Limited::new(request.into_body(), BODY_SIZE + 1)
        .collect()
        .await
    {
        Ok(collected) => collected.to_bytes(),
        Err(_) => return Ok(response(StatusCode::BAD_REQUEST, b"invalid body".to_vec())),
    };
    if body.len() != BODY_SIZE || !body.iter().all(u8::is_ascii) {
        return Ok(response(
            StatusCode::BAD_REQUEST,
            b"expected exactly 64 ASCII bytes".to_vec(),
        ));
    }
    let input = body.to_vec();
    let result = match path.as_str() {
        "/echo" => input,
        "/wait" => {
            tokio::time::sleep(Duration::from_millis(5)).await;
            input
        }
        "/io" => {
            if let Err(error) = backend_echo(&input).await {
                return Ok(response(StatusCode::BAD_GATEWAY, error.into_bytes()));
            }
            input
        }
        "/compute" => {
            let seed = input[0];
            let total = compute(seed, 0, WORK_UNITS);
            [input, format!(":{total}").into_bytes()].concat()
        }
        "/fanout" => {
            let seed = input[0];
            let mut tasks = tokio::task::JoinSet::new();
            for part in 0..8 {
                tasks.spawn(async move { compute(seed, part * (WORK_UNITS / 8), WORK_UNITS / 8) });
            }
            let mut total = 0_u64;
            for _ in 0..8 {
                total += tasks
                    .join_next()
                    .await
                    .expect("all compute partitions were spawned")
                    .expect("compute task panicked");
            }
            [input, format!(":{total}").into_bytes()].concat()
        }
        _ => unreachable!(),
    };
    Ok(response(StatusCode::OK, result))
}

async fn backend_echo(body: &[u8]) -> Result<(), String> {
    let operation = async {
        let backend = env::var("BENCH_BACKEND")
            .map_err(|_| "BENCH_BACKEND is required for /io".to_owned())?;
        let stream = TcpStream::connect(&backend)
            .await
            .map_err(|error| error.to_string())?;
        let (mut sender, connection) = hyper::client::conn::http1::handshake(TokioIo::new(stream))
            .await
            .map_err(|error| error.to_string())?;
        let mut tasks = tokio::task::JoinSet::new();
        tasks.spawn(async move { connection.await });
        let request = Request::builder()
            .method(Method::POST)
            .uri("/echo")
            .header(HOST, &backend)
            .header(CONNECTION, "close")
            .header(ACCEPT_ENCODING, "identity")
            .body(Full::new(Bytes::copy_from_slice(body)))
            .map_err(|error| error.to_string())?;
        let response = sender
            .send_request(request)
            .await
            .map_err(|error| error.to_string())?;
        let status = response.status();
        let echoed = response
            .into_body()
            .collect()
            .await
            .map_err(|error| error.to_string())?
            .to_bytes();
        if status != StatusCode::OK || echoed.as_ref() != body {
            return Err(format!(
                "backend returned status {status} and {} body bytes",
                echoed.len()
            ));
        }
        Ok::<(), String>(())
    };
    tokio::time::timeout(Duration::from_secs(10), operation)
        .await
        .map_err(|_| "backend request exceeded 10 seconds".to_owned())?
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let workers = env_workers()?;
    tokio::runtime::Builder::new_multi_thread()
        .worker_threads(workers)
        .enable_all()
        .build()?
        .block_on(serve())
}

async fn serve() -> Result<(), Box<dyn std::error::Error>> {
    let bind: SocketAddr = env::var("BENCH_BIND")
        .unwrap_or_else(|_| "127.0.0.1:18080".to_owned())
        .parse()?;
    let listener = TcpListener::bind(bind).await?;
    println!("READY {}", listener.local_addr()?);

    let semaphore = Arc::new(Semaphore::new(32));
    loop {
        let (stream, _) = listener.accept().await?;
        let io = TokioIo::new(stream);
        let semaphore = Arc::clone(&semaphore);
        tokio::spawn(async move {
            let service = service_fn(move |request| {
                let semaphore = Arc::clone(&semaphore);
                async move {
                    let _permit = semaphore.acquire_owned().await.expect("semaphore is open");
                    handle(request).await
                }
            });
            let _ = http1::Builder::new()
                .keep_alive(false)
                .serve_connection(io, service)
                .await;
        });
    }
}
