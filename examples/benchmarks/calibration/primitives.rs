use std::hint::black_box;
use std::sync::Arc;
use std::time::Instant;

fn main() {
    let args: Vec<String> = std::env::args().collect();
    assert_eq!(args.len(), 4, "usage: primitives WORKLOAD ITERATIONS SIZE");
    let workload = &args[1];
    let iterations: usize = args[2].parse().expect("positive iteration count");
    let size: usize = args[3].parse().expect("positive size");
    assert!(iterations > 0 && size > 0);
    let values: Vec<i64> = if workload == "vector_scan" {
        (0..size).map(|i| (i % 251) as i64).collect()
    } else {
        Vec::new()
    };
    let original: Arc<[u8]> = if workload.starts_with("bytes_") {
        (0..size).map(|i| (i % 251) as u8).collect()
    } else {
        Arc::from([])
    };
    let start = Instant::now();
    let mut checksum: i64 = 0;
    match workload.as_str() {
        "scalar" => {
            checksum = 1;
            for _ in 0..iterations {
                checksum = (checksum * 48271) % 2147483647;
            }
        }
        "vector_scan" => {
            for _ in 0..iterations {
                for value in black_box(&values) {
                    checksum += value;
                }
            }
        }
        "vector_build" => {
            for _ in 0..iterations {
                let mut values = Vec::new();
                for i in 0..size {
                    values.push((i % 251) as i64);
                }
                checksum += black_box(&values).iter().sum::<i64>();
            }
        }
        "bytes_shared" => {
            for r in 0..iterations {
                let snapshot = Arc::clone(black_box(&original));
                checksum += black_box(&snapshot)[r % size] as i64;
            }
        }
        "bytes_cow" => {
            for r in 0..iterations {
                let mut snapshot = Arc::clone(black_box(&original));
                let index = r % size;
                Arc::make_mut(&mut snapshot)[index] = (original[index] + 1) % 251;
                checksum += black_box(&snapshot)[index] as i64 + original[index] as i64;
            }
        }
        _ => panic!("unknown workload"),
    }
    let elapsed = start.elapsed().as_nanos();
    println!("{{\"workload\":\"{workload}\",\"iterations\":{iterations},\"size\":{size},\"elapsed_ns\":{elapsed},\"checksum\":{checksum}}}");
}
