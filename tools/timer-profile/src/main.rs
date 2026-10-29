//! Isolated optimized host for the production timer wheel and platform poller.
#![allow(dead_code, unused_imports, unexpected_cfgs, unsafe_op_in_unsafe_fn)]
use std::time::{Duration, Instant};
macro_rules! cabi_guard {
    ($condition:expr) => {
        if $condition {
            return;
        }
    };
    ($condition:expr, $result:expr) => {
        if $condition {
            return $result;
        }
    };
}
#[path = "../../../hew-runtime/src/duration.rs"]
mod duration;
#[path = "../../../hew-runtime/src/io_time.rs"]
mod io_time;
#[path = "../../../hew-runtime/src/monotonic.rs"]
mod monotonic;
#[path = "../../../hew-runtime/src/runtime_sync.rs"]
mod runtime_sync;
#[path = "../../../hew-runtime/src/timer_deadline.rs"]
mod timer_deadline;
#[path = "../../../hew-runtime/src/timer_wheel.rs"]
mod timer_wheel;
mod clock {
    pub unsafe fn hew_now_ms() -> u64 {
        crate::monotonic::monotonic_ms()
    }
    pub unsafe fn hew_instant_now() -> i64 {
        crate::monotonic::monotonic_ns() as i64
    }
}
mod file_io {
    pub unsafe fn hew_file_read(
        _: *const hew_cabi::string::HewString,
    ) -> *mut hew_cabi::string::HewString {
        unreachable!("not a file-I/O benchmark")
    }
}
fn set_last_error(error: impl AsRef<str>) {
    panic!("{}", error.as_ref());
}
fn cpu_ns() -> u64 {
    let mut value = libc::timespec {
        tv_sec: 0,
        tv_nsec: 0,
    };
    assert_eq!(
        unsafe { libc::clock_gettime(libc::CLOCK_THREAD_CPUTIME_ID, &mut value) },
        0
    );
    value.tv_sec as u64 * 1_000_000_000 + value.tv_nsec as u64
}
fn quantile(values: &[u64], percent: usize) -> u64 {
    values[(values.len() - 1) * percent / 100]
}
fn kernel_compare() {
    // This preflight compares the existing kernel primitive against its
    // high-resolution counterpart before changing production code.
    let epfd = unsafe { libc::epoll_create1(libc::EPOLL_CLOEXEC) };
    assert!(epfd >= 0);
    let mut event = libc::epoll_event { events: 0, u64: 0 };
    for delay in [50_000u64, 250_000, 1_000_000, 10_000_000] {
        for precise in [false, true] {
            let mut late = Vec::new();
            let cpu = cpu_ns();
            let started = Instant::now();
            for _ in 0..300 {
                let start = Instant::now();
                let rc = unsafe {
                    if precise {
                        let spec = libc::timespec {
                            tv_sec: 0,
                            tv_nsec: delay as _,
                        };
                        libc::syscall(
                            libc::SYS_epoll_pwait2,
                            epfd,
                            &mut event,
                            1,
                            &spec,
                            std::ptr::null::<libc::sigset_t>(),
                            8,
                        )
                    } else {
                        libc::epoll_wait(epfd, &mut event, 1, delay.div_ceil(1_000_000) as i32) as _
                    }
                };
                assert_eq!(rc, 0, "{}", std::io::Error::last_os_error());
                late.push((start.elapsed().as_nanos() as u64).saturating_sub(delay));
            }
            let cpu = cpu_ns() - cpu;
            let wall = started.elapsed().as_nanos();
            late.sort_unstable();
            println!("kernel precise={precise} delay_ns={delay} n={} late_p50_ns={} late_p99_ns={} cpu_ns_per_wait={} wall_ns={wall}", late.len(), quantile(&late,50), quantile(&late,99), cpu/late.len() as u64);
        }
    }
    assert_eq!(unsafe { libc::close(epfd) }, 0);
}
fn main() {
    println!(
        "timer_entry_bytes={}",
        std::mem::size_of::<timer_wheel::HewTimerEntry>()
    );
    match std::env::args().nth(1).as_deref() {
        Some("--help" | "-h") => println!("Usage: hew-timer-profile [latency|exact|throughput|kernel]\nRun in an otherwise idle environment; latency also starts four CPU load threads."),
        Some("kernel") => kernel_compare(),
        Some("throughput") => {
            throughput(false);
            throughput(true);
        }
        Some("exact") => {
            for loaded in [false, true] { wheel_latency(true, loaded); }
            idle();
        }
        None | Some("latency") => {
            for loaded in [false, true] {
                for precise in [false, true] {
                    wheel_latency(precise, loaded);
                }
            }
            idle();
        }
        Some(mode) => panic!("unknown profile: {mode}; use --help"),
    }
}

unsafe extern "C" fn observed(data: *mut std::ffi::c_void) {
    let at = unsafe { &mut *data.cast::<u64>() };
    *at = monotonic::monotonic_ns();
}
fn wheel_latency(precise: bool, loaded: bool) {
    let stop = std::sync::Arc::new(std::sync::atomic::AtomicBool::new(false));
    let workers: Vec<_> = (0..if loaded { 4 } else { 0 })
        .map(|_| {
            let stop = stop.clone();
            std::thread::spawn(move || {
                while !stop.load(std::sync::atomic::Ordering::Relaxed) {
                    std::hint::spin_loop();
                }
            })
        })
        .collect();
    let poller = io_time::Poller::new().unwrap();
    let mut events = Vec::new();
    let wheel = unsafe { timer_wheel::hew_timer_wheel_new() };
    for delay in [50_000u64, 250_000, 1_000_000, 10_000_000] {
        let cpu = cpu_ns();
        let wall = Instant::now();
        let mut late = Vec::new();
        let mut waits = 0;
        for _ in 0..300 {
            let now = monotonic::monotonic_ns();
            let requested = now + delay;
            let deadline = if precise {
                timer_deadline::Deadline::from_nanos(requested)
            } else {
                timer_deadline::Deadline::from_millis(requested.div_ceil(1_000_000))
            };
            let mut fired_at = 0u64;
            let handle = unsafe {
                timer_wheel::timer_wheel_schedule_precise_handle(
                    wheel,
                    deadline,
                    observed,
                    (&raw mut fired_at).cast(),
                )
            };
            assert!(!handle.entry.is_null());
            while fired_at == 0 {
                let next = unsafe { timer_wheel::timer_wheel_next_deadline(wheel) }.unwrap();
                let remaining = next.remaining(timer_wheel::now_for_wheel());
                let remaining = if precise {
                    remaining
                } else {
                    Duration::from_millis(remaining.as_nanos().div_ceil(1_000_000) as u64)
                };
                poller.wait_for(Some(remaining), &mut events).unwrap();
                waits += 1;
                unsafe { timer_wheel::hew_timer_wheel_tick(wheel) };
            }
            assert!(fired_at >= requested, "early timeout");
            late.push(fired_at - requested);
        }
        let cpu = cpu_ns() - cpu;
        let wall = wall.elapsed().as_nanos();
        late.sort_unstable();
        println!("wheel precise={precise} loaded={loaded} delay_ns={delay} n={} late_p50_ns={} late_p99_ns={} cpu_ns_per_timer={} waits={waits} wall_ns={wall}", late.len(), quantile(&late,50), quantile(&late,99), cpu/late.len() as u64);
    }
    unsafe { timer_wheel::hew_timer_wheel_free(wheel) };
    stop.store(true, std::sync::atomic::Ordering::Relaxed);
    for worker in workers {
        worker.join().unwrap();
    }
}
unsafe extern "C" fn noop(_: *mut std::ffi::c_void) {}
fn throughput(precise: bool) {
    for count in [32u64, 512, 8192] {
        let mut samples = Vec::new();
        for _ in 0..25 {
            let wheel = unsafe { timer_wheel::hew_timer_wheel_new() };
            let base = monotonic::monotonic_ns() + 1_000_000;
            let start = Instant::now();
            for index in 0..count {
                // All within L0. Fractions exercise eligibility without shrinking
                // the wheel's range or inflating its slot count.
                let ns = base + index * 30_001;
                let deadline = if precise {
                    timer_deadline::Deadline::from_nanos(ns)
                } else {
                    timer_deadline::Deadline::from_millis(ns.div_ceil(1_000_000))
                };
                let handle = unsafe {
                    timer_wheel::timer_wheel_schedule_precise_handle(
                        wheel,
                        deadline,
                        noop,
                        std::ptr::null_mut(),
                    )
                };
                assert!(!handle.entry.is_null());
            }
            std::hint::black_box(unsafe { timer_wheel::timer_wheel_next_deadline(wheel) });
            let end = timer_deadline::Deadline::from_nanos(base + count * 30_001);
            assert_eq!(
                unsafe {
                    timer_wheel::timer_wheel_tick_at(
                        wheel,
                        timer_deadline::Deadline::from_millis(end.ceil_millis().unwrap()),
                    )
                },
                count as i32
            );
            samples.push(start.elapsed().as_nanos() as u64 / count);
            unsafe { timer_wheel::hew_timer_wheel_free(wheel) };
        }
        samples.sort_unstable();
        println!("throughput precise={precise} timers={count} repeats=25 median_ns_per_timer={} p99_ns_per_timer={}",quantile(&samples,50),quantile(&samples,99));
    }
}
fn idle() {
    let poller = std::sync::Arc::new(io_time::Poller::new().unwrap());
    let wake = poller.clone();
    let waker = std::thread::spawn(move || {
        std::thread::sleep(Duration::from_millis(250));
        wake.wake();
    });
    let cpu = cpu_ns();
    let wall = Instant::now();
    poller.wait_for(None, &mut Vec::new()).unwrap();
    println!(
        "idle wall_ns={} cpu_ns={} waits=1",
        wall.elapsed().as_nanos(),
        cpu_ns() - cpu
    );
    waker.join().unwrap();
}
