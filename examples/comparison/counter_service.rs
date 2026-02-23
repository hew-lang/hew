// Rust: Counter Service
// Same problem as counter_service.hew but in Rust.
// Notice: Arc<Mutex<>> everywhere, no actors, no supervision,
// no wire types without serde + format crate.
use std::sync::{Arc, Mutex};

// Rust has no actor model. You use Arc<Mutex<>> for shared state.
// Safe, but verbose and easy to deadlock.

struct Counter {
    count: i32,
}

impl Counter {
    fn new() -> Self {
        Counter { count: 0 }
    }

    fn increment(&mut self, amount: i32) -> i32 {
        self.count += amount;
        self.count
    }

    fn get_count(&self) -> i32 {
        self.count
    }
}

// To share across threads, you need Arc<Mutex<Counter>>
type SharedCounter = Arc<Mutex<Counter>>;

fn new_shared_counter() -> SharedCounter {
    Arc::new(Mutex::new(Counter::new()))
}

fn shared_increment(counter: &SharedCounter, amount: i32) -> i32 {
    let mut c = counter.lock().unwrap(); // Can panic on poison
    c.increment(amount)
}

// Rust has no supervision trees. You build your own.
struct CounterPool {
    counters: Vec<SharedCounter>,
    max_restarts: i32,
    restart_count: i32,
}

impl CounterPool {
    fn new(size: usize) -> Self {
        let counters = (0..size).map(|_| new_shared_counter()).collect();
        CounterPool {
            counters,
            max_restarts: 5,
            restart_count: 0,
        }
    }

    // No language support for restart strategies.
    fn restart_child(&mut self, idx: usize) {
        self.restart_count += 1;
        if self.restart_count > self.max_restarts {
            eprintln!("Exceeded restart limit");
            return;
        }
        self.counters[idx] = new_shared_counter();
    }
}

// Rust has no wire types. You need serde + a format crate (bincode, etc.)
// Plus derive macros. No compile-time schema evolution checking.
// #[derive(Serialize, Deserialize)]  // Needs serde dependency
struct CounterUpdate {
    counter_id: u32,
    new_count: i32,
    timestamp: u64,
}

fn fibonacci(n: i32) -> i32 {
    if n <= 1 {
        n
    } else {
        fibonacci(n - 1) + fibonacci(n - 2)
    }
}

fn main() {
    println!("Rust counter service: Arc<Mutex<>>, no supervision, no wire types");
    println!("Lines of domain logic: 90+");
    println!("External dependencies: serde, bincode (or similar) for wire types");
    println!("Runtime overhead: native binary, no GC (advantage shared with Hew)");
    let result = fibonacci(10);
    println!("fibonacci(10) = {}", result);
}
