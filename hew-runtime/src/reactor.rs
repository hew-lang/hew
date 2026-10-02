// native-only: the reactor uses a platform readiness poller and an OS thread,
// neither available on WASM. The WASM build fails closed through the type
// checker's `WasmUnsupportedFeature::TcpNetworking` gate.
//! The I/O reactor: one thread that reports readiness and drives the timer
//! wheel.
//!
//! Every waitable handle lives in one slot table keyed by its handle, a
//! monotonic number the runtime never reuses while the slot lives. A slot owns
//! its OS object, so a descriptor stays open while any operation still holds
//! the slot, and the reactor never names a descriptor number.
//!
//! An operation runs its syscall on the waiting task's worker. It tries first
//! and registers only when the syscall would block: it stores itself as the
//! slot's reader or writer and arms one readiness report. Arming is one-shot on
//! every OS (`EPOLLONESHOT`, `EV_ONESHOT`, one `AFD_POLL`), so one arm gives at
//! most one wake, and the task re-arms after its next attempt would block. The
//! reactor thread only moves readiness to the waiting operation and wakes its
//! task; it performs no socket syscall. A spurious report costs one attempt
//! that meets `WouldBlock` again.
//!
//! The timer wheel has no thread of its own. The reactor's poll timeout is the
//! wheel's next deadline, and a timer inserted ahead of that deadline wakes the
//! reactor through the poller's wake source.

use std::collections::{HashMap, VecDeque};
use std::ffi::c_int;
use std::io;
use std::net::{TcpListener, TcpStream};
use std::sync::atomic::{AtomicBool, AtomicI32, AtomicU64, AtomicUsize, Ordering};
use std::sync::{Arc, LazyLock, Mutex, OnceLock};
use std::thread::JoinHandle;
use std::time::Duration;

use crate::async_io::{HewAsyncIo, IoFailure};
use crate::io_time::{Event, Poller, HEW_IO_ERROR, HEW_IO_HUP, HEW_IO_READ, HEW_IO_WRITE};
use crate::timer_wheel::HewTimerWheel;
use crate::util::MutexExt;

// ---------------------------------------------------------------------------
// Slots
// ---------------------------------------------------------------------------

/// The OS object a slot owns.
#[derive(Debug)]
pub(crate) enum IoObject {
    TcpStream(TcpStream),
    TcpListener(TcpListener),
    /// Standard input: descriptor 0 on Unix, which the slot never owns or
    /// closes. Its one slot lives outside the table; see [`stdin_slot`].
    Stdin,
}

/// Which waiter an operation occupies. A socket slot has at most one of each,
/// so one task can read a connection while another writes it; standard input
/// queues its readers instead.
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub(crate) enum Direction {
    Read,
    Write,
}

#[derive(Default)]
struct SlotState {
    reader: Option<Arc<HewAsyncIo>>,
    writer: Option<Arc<HewAsyncIo>>,
    /// Standard input's readers; socket slots use `reader`.
    readers: Readers,
    /// The descriptor is in the epoll set (Linux); later arms modify it.
    #[cfg(unix)]
    added: bool,
    closed: bool,
    nonblocking: bool,
    /// Interest of the in-flight AFD poll, zero when none is in flight.
    #[cfg(windows)]
    armed: c_int,
}

/// Standard input's line reads in arrival order. The front holds the turn to
/// take input; the rest wait for it without attempting.
#[derive(Default)]
struct Readers {
    queue: VecDeque<Arc<HewAsyncIo>>,
    /// The front was woken and has not attempted since, so nothing arms
    /// readiness until it does.
    front_woken: bool,
}

impl SlotState {
    fn interest(&self) -> c_int {
        let mut interest = 0;
        if self.reader.is_some() || (!self.readers.queue.is_empty() && !self.readers.front_woken) {
            interest |= HEW_IO_READ;
        }
        if self.writer.is_some() {
            interest |= HEW_IO_WRITE;
        }
        interest
    }

    fn waiter(&mut self, direction: Direction) -> &mut Option<Arc<HewAsyncIo>> {
        match direction {
            Direction::Read => &mut self.reader,
            Direction::Write => &mut self.writer,
        }
    }
}

/// One waitable handle: its OS object and the operations waiting on it.
pub(crate) struct Slot {
    handle: c_int,
    object: IoObject,
    state: Mutex<SlotState>,
    /// The poll buffers the kernel writes while an AFD poll is in flight.
    /// Touched only under `state` while no poll is in flight, or by the
    /// reactor after it dequeued the completion.
    #[cfg(windows)]
    afd: std::cell::UnsafeCell<crate::io_time::AfdPoll>,
}

// SAFETY: the AFD buffers are accessed only under the protocol described on
// the field; every other field is Send + Sync.
#[cfg(windows)]
unsafe impl Send for Slot {}
// SAFETY: as above.
#[cfg(windows)]
unsafe impl Sync for Slot {}

impl std::fmt::Debug for Slot {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("Slot")
            .field("handle", &self.handle)
            .field("object", &self.object)
            .finish_non_exhaustive()
    }
}

/// Operations stored as a reader or writer, including those the reactor has
/// taken but not yet woken. Shutdown's idle probe reads this.
static WAITERS: AtomicUsize = AtomicUsize::new(0);

fn waiter_added() {
    WAITERS.fetch_add(1, Ordering::SeqCst);
    crate::observe::record_reactor_registration();
}

fn waiters_removed(count: usize) {
    if count != 0 {
        WAITERS.fetch_sub(count, Ordering::SeqCst);
        crate::observe::record_reactor_unregistration(count as u64);
    }
}

fn busy() -> IoFailure {
    IoFailure::from_io(
        "TCP handle already has pending I/O",
        &io::Error::from_raw_os_error(libc::EBUSY),
    )
}

pub(crate) fn cancelled_failure(operation: &str) -> IoFailure {
    IoFailure::from_io(operation, &io::Error::from_raw_os_error(libc::ECANCELED))
}

impl Slot {
    pub(crate) fn handle(&self) -> c_int {
        self.handle
    }

    pub(crate) fn stream(&self) -> Option<&TcpStream> {
        match &self.object {
            IoObject::TcpStream(stream) => Some(stream),
            IoObject::TcpListener(_) | IoObject::Stdin => None,
        }
    }

    pub(crate) fn listener(&self) -> Option<&TcpListener> {
        match &self.object {
            IoObject::TcpListener(listener) => Some(listener),
            IoObject::TcpStream(_) | IoObject::Stdin => None,
        }
    }

    pub(crate) fn is_stdin(&self) -> bool {
        matches!(self.object, IoObject::Stdin)
    }

    /// Whether another operation already waits in `direction`.
    pub(crate) fn has_waiter(&self, direction: Direction) -> bool {
        self.state.lock_or_recover().waiter(direction).is_some()
    }

    pub(crate) fn is_closed(&self) -> bool {
        self.state.lock_or_recover().closed
    }

    /// The socket's timeout for waits in `direction`. The socket option is
    /// the one authority: duplicated handles, such as stream halves, share it.
    pub(crate) fn timeout(&self, direction: Direction) -> Option<Duration> {
        let stream = self.stream()?;
        match direction {
            Direction::Read => stream.read_timeout(),
            Direction::Write => stream.write_timeout(),
        }
        .ok()
        .flatten()
    }

    /// Switch the socket to non-blocking mode at its first waiting operation.
    pub(crate) fn ensure_nonblocking(&self) -> io::Result<()> {
        let mut state = self.state.lock_or_recover();
        if !state.nonblocking {
            match &self.object {
                IoObject::TcpStream(stream) => stream.set_nonblocking(true)?,
                IoObject::TcpListener(listener) => listener.set_nonblocking(true)?,
                // Descriptor 0's open file description is shared with the
                // terminal and parent shell; its reads are gated on `poll`.
                IoObject::Stdin => {}
            }
            state.nonblocking = true;
        }
        Ok(())
    }

    #[cfg(unix)]
    fn fd(&self) -> std::os::fd::RawFd {
        use std::os::fd::AsRawFd;
        match &self.object {
            IoObject::TcpStream(stream) => stream.as_raw_fd(),
            IoObject::TcpListener(listener) => listener.as_raw_fd(),
            IoObject::Stdin => libc::STDIN_FILENO,
        }
    }

    /// Store `operation` as this slot's waiter for `direction` and arm one
    /// readiness report. The waiter and the arm share the slot lock with the
    /// reactor's hand-off, so a report can never find the slot half-armed.
    pub(crate) fn wait(
        self: &Arc<Self>,
        direction: Direction,
        operation: &Arc<HewAsyncIo>,
    ) -> Result<(), IoFailure> {
        let poller = poller().map_err(|error| IoFailure::from_io("start I/O reactor", &error))?;
        if !ensure_reactor_started() {
            return Err(IoFailure::from_io(
                "start I/O reactor",
                &io::Error::other("reactor unavailable"),
            ));
        }
        let mut state = self.state.lock_or_recover();
        if state.closed {
            return Err(cancelled_failure("wait on closed TCP handle"));
        }
        if self.is_stdin() {
            // Only the turn holder waits here; it keeps its place at the front.
            if state.readers.queue.is_empty() {
                state.readers.queue.push_back(Arc::clone(operation));
                waiter_added();
            }
            state.readers.front_woken = false;
            // An arm failure completes the reader, and `forget` passes its turn.
            return self
                .arm(poller, &mut state)
                .map_err(|error| IoFailure::from_io("arm standard input readiness", &error));
        }
        match state.waiter(direction) {
            Some(current) if !Arc::ptr_eq(current, operation) => return Err(busy()),
            Some(_) => {}
            slot @ None => {
                *slot = Some(Arc::clone(operation));
                waiter_added();
            }
        }
        if let Err(error) = self.arm(poller, &mut state) {
            if state.waiter(direction).take().is_some() {
                waiters_removed(1);
            }
            return Err(IoFailure::from_io("arm TCP readiness", &error));
        }
        Ok(())
    }

    #[cfg(unix)]
    fn arm(self: &Arc<Self>, poller: &Poller, state: &mut SlotState) -> io::Result<()> {
        let interest = state.interest();
        if interest == 0 {
            return Ok(());
        }
        poller.arm(
            self.fd(),
            u64::from(self.handle.unsigned_abs()),
            interest,
            state.added,
        )?;
        state.added = true;
        Ok(())
    }

    #[cfg(windows)]
    fn arm(self: &Arc<Self>, poller: &Poller, state: &mut SlotState) -> io::Result<()> {
        if self.is_stdin() {
            // The reader thread reports readiness through `report_stdin_ready`.
            crate::async_io::ensure_stdin_reader();
            return Ok(());
        }
        let interest = state.interest();
        if interest == 0 || state.armed & interest == interest {
            return Ok(());
        }
        if state.armed != 0 {
            // The in-flight poll lacks a direction. Cancel it; its completion
            // re-arms with the interest the waiters then hold.
            // SAFETY: a poll is in flight on these buffers.
            unsafe { poller.cancel(self.afd.get()) };
            return Ok(());
        }
        let context = Arc::into_raw(Arc::clone(self)) as usize;
        // SAFETY: no poll is in flight; the retained slot keeps the buffers
        // live until the reactor dequeues this completion.
        match unsafe { poller.arm(self.afd.get(), interest, context) } {
            Ok(()) => {
                state.armed = interest;
                AFD_IN_FLIGHT.fetch_add(1, Ordering::SeqCst);
                Ok(())
            }
            Err(error) => {
                // SAFETY: the failed arm did not retain the context.
                drop(unsafe { Arc::from_raw(context as *const Self) });
                Err(error)
            }
        }
    }

    /// Whether a standard-input read may take input now: no reader is queued
    /// or it holds the turn. Otherwise it joins the back of the queue.
    pub(crate) fn take_turn(&self, operation: &Arc<HewAsyncIo>) -> bool {
        let mut state = self.state.lock_or_recover();
        match state.readers.queue.front() {
            None => true,
            Some(front) if Arc::ptr_eq(front, operation) => {
                state.readers.front_woken = false;
                true
            }
            Some(_) => {
                if !state
                    .readers
                    .queue
                    .iter()
                    .any(|queued| Arc::ptr_eq(queued, operation))
                {
                    state.readers.queue.push_back(Arc::clone(operation));
                    waiter_added();
                }
                false
            }
        }
    }

    /// Withdraw `operation` if it is still a waiter. Completion, cancellation
    /// and deadline expiry all end here; a later report finds no waiter. A
    /// standard-input reader leaving the front passes the turn to the next,
    /// which attempts at once: input it needs may already be buffered.
    pub(crate) fn forget(&self, operation: &HewAsyncIo) {
        let mut next = None;
        let removed = {
            let mut state = self.state.lock_or_recover();
            let mut removed = Vec::new();
            if let Some(index) = state
                .readers
                .queue
                .iter()
                .position(|queued| std::ptr::eq(Arc::as_ptr(queued), operation))
            {
                removed.push(state.readers.queue.remove(index));
                if index == 0 {
                    next = state.readers.queue.front().cloned();
                    state.readers.front_woken = next.is_some();
                }
            }
            for direction in [Direction::Read, Direction::Write] {
                let slot = state.waiter(direction);
                if slot
                    .as_ref()
                    .is_some_and(|current| std::ptr::eq(Arc::as_ptr(current), operation))
                {
                    removed.push(slot.take());
                }
            }
            removed
        };
        waiters_removed(removed.len());
        drop(removed);
        if let Some(next) = next {
            next.signal_ready();
        }
    }

    /// Hand a readiness report to the waiters it can unblock and re-arm for
    /// any waiter it cannot. Runs on the reactor thread.
    fn fire(self: &Arc<Self>, events: c_int) {
        let (ready, turn) = {
            let mut state = self.state.lock_or_recover();
            #[cfg(windows)]
            {
                state.armed = 0;
            }
            let mut ready = Vec::with_capacity(2);
            let mut turn = None;
            if events & (HEW_IO_READ | HEW_IO_HUP | HEW_IO_ERROR) != 0 {
                ready.extend(state.reader.take());
                // The front reader stays queued; it leaves when it completes.
                if !state.readers.front_woken {
                    turn = state.readers.queue.front().cloned();
                    state.readers.front_woken = turn.is_some();
                }
            }
            if events & (HEW_IO_WRITE | HEW_IO_HUP | HEW_IO_ERROR) != 0 {
                ready.extend(state.writer.take());
            }
            if !state.closed && state.interest() != 0 {
                if let Ok(poller) = poller() {
                    if let Err(error) = self.arm(poller, &mut state) {
                        // Nothing can report readiness for the remaining
                        // waiter now; wake it so its attempt reports the fault.
                        eprintln!("hew: re-arm I/O readiness failed: {error}");
                        ready.extend(state.reader.take());
                        ready.extend(state.writer.take());
                    }
                }
            }
            (ready, turn)
        };
        for operation in ready.iter().chain(&turn) {
            crate::observe::record_reactor_ready_event();
            operation.signal_ready();
        }
        waiters_removed(ready.len());
    }

    /// Mark the slot closed and complete its waiters with `ECANCELED`. The OS
    /// object closes when the last operation holding the slot releases it.
    fn close(&self) {
        let waiters = {
            let mut state = self.state.lock_or_recover();
            state.closed = true;
            #[cfg(unix)]
            if state.added {
                if let Ok(poller) = poller() {
                    poller.remove(self.fd());
                }
                state.added = false;
            }
            #[cfg(windows)]
            if state.armed != 0 {
                if let Ok(poller) = poller() {
                    // SAFETY: a poll is in flight on these buffers.
                    unsafe { poller.cancel(self.afd.get()) };
                }
            }
            [state.reader.take(), state.writer.take()]
        };
        for operation in waiters.into_iter().flatten() {
            operation.complete(Err(cancelled_failure("TCP handle closed while waiting")));
            waiters_removed(1);
        }
    }

    fn take_waiters(&self) -> Vec<Arc<HewAsyncIo>> {
        let mut state = self.state.lock_or_recover();
        state.readers.front_woken = false;
        let mut waiters: Vec<_> = state.readers.queue.drain(..).collect();
        waiters.extend(state.reader.take());
        waiters.extend(state.writer.take());
        waiters
    }
}

// ---------------------------------------------------------------------------
// The slot table
// ---------------------------------------------------------------------------

const SHARDS: usize = 64;

/// Handle to slot, sharded so operations on different handles never share a
/// lock. A shard lock is held only to clone or move a slot reference.
struct Table {
    shards: [Mutex<HashMap<c_int, Arc<Slot>>>; SHARDS],
}

static TABLE: LazyLock<Table> = LazyLock::new(|| Table {
    shards: std::array::from_fn(|_| Mutex::new(HashMap::new())),
});
static NEXT_HANDLE: AtomicI32 = AtomicI32::new(1);

fn shard(handle: c_int) -> &'static Mutex<HashMap<c_int, Arc<Slot>>> {
    &TABLE.shards[handle.unsigned_abs() as usize % SHARDS]
}

/// Give `object` a slot and return its handle. Handles count up and wrap to 1
/// past `i32::MAX`, skipping any still in use.
pub(crate) fn register(object: IoObject) -> c_int {
    let mut object = Some(object);
    loop {
        let handle = NEXT_HANDLE
            .fetch_update(Ordering::Relaxed, Ordering::Relaxed, |next| {
                Some(if next == i32::MAX { 1 } else { next + 1 })
            })
            .expect("handle update always succeeds");
        let mut shard = shard(handle).lock_or_recover();
        if let std::collections::hash_map::Entry::Vacant(entry) = shard.entry(handle) {
            crate::observe::record_io_handle_opened();
            entry.insert(Arc::new(new_slot(
                handle,
                object.take().expect("object is placed once"),
            )));
            return handle;
        }
    }
}

fn new_slot(handle: c_int, object: IoObject) -> Slot {
    #[cfg(windows)]
    let afd = {
        use std::os::windows::io::AsRawSocket;
        let socket = match &object {
            IoObject::TcpStream(stream) => stream.as_raw_socket() as usize,
            IoObject::TcpListener(listener) => listener.as_raw_socket() as usize,
            // Never armed through AFD.
            IoObject::Stdin => usize::MAX,
        };
        std::cell::UnsafeCell::new(crate::io_time::AfdPoll::new(socket))
    };
    Slot {
        handle,
        object,
        state: Mutex::new(SlotState::default()),
        #[cfg(windows)]
        afd,
    }
}

/// Standard input's handle. `register` never mints it.
const STDIN_HANDLE: c_int = 0;

/// Standard input's slot. It is never closed, so it serves every runtime
/// generation; it is not a program-owned handle and stays out of the table
/// and the leak count.
static STDIN_SLOT: OnceLock<Arc<Slot>> = OnceLock::new();

pub(crate) fn stdin_slot() -> Arc<Slot> {
    Arc::clone(STDIN_SLOT.get_or_init(|| Arc::new(new_slot(STDIN_HANDLE, IoObject::Stdin))))
}

/// Hand new standard input to its waiter. The Windows reader thread is the
/// readiness source for its slot.
#[cfg(windows)]
pub(crate) fn report_stdin_ready() {
    if let Some(slot) = STDIN_SLOT.get() {
        slot.fire(HEW_IO_READ);
    }
}

/// The live slot for `handle`.
pub(crate) fn lookup(handle: c_int) -> Option<Arc<Slot>> {
    shard(handle).lock_or_recover().get(&handle).cloned()
}

/// Remove `handle` from the table and close its slot. Waiting operations
/// complete with `ECANCELED`; the returned reference keeps the OS object open
/// until the caller drops it.
pub(crate) fn unregister(handle: c_int) -> Option<Arc<Slot>> {
    let slot = shard(handle).lock_or_recover().remove(&handle)?;
    crate::observe::record_io_handle_closed();
    slot.close();
    Some(slot)
}

pub(crate) fn slots() -> Vec<Arc<Slot>> {
    TABLE
        .shards
        .iter()
        .flat_map(|shard| {
            shard
                .lock_or_recover()
                .values()
                .cloned()
                .collect::<Vec<_>>()
        })
        .chain(STDIN_SLOT.get().cloned())
        .collect()
}

// ---------------------------------------------------------------------------
// Poller and reactor thread
// ---------------------------------------------------------------------------

static POLLER: OnceLock<Result<Poller, String>> = OnceLock::new();

/// The process poller. It is created once and never freed, so a slot armed by
/// any runtime generation stays valid.
fn poller() -> io::Result<&'static Poller> {
    POLLER
        .get_or_init(|| Poller::new().map_err(|error| error.to_string()))
        .as_ref()
        .map_err(|error| io::Error::other(error.clone()))
}

static REACTOR_RUNNING: AtomicBool = AtomicBool::new(false);
static REACTOR_STOP: AtomicBool = AtomicBool::new(false);
static REACTOR_HANDLE: Mutex<Option<JoinHandle<()>>> = Mutex::new(None);

/// When the reactor next wakes on its own, in wheel milliseconds: `AWAKE`
/// while it runs, `u64::MAX` when it sleeps with no deadline.
static SLEEP_UNTIL: AtomicU64 = AtomicU64::new(AWAKE);
const AWAKE: u64 = 0;

/// AFD polls in flight; each owns one retained slot reference.
#[cfg(windows)]
static AFD_IN_FLIGHT: AtomicUsize = AtomicUsize::new(0);

/// Whether shutdown has closed listener admission. An accept attempt refuses
/// to begin once this is set, so no accepted connection can land behind the
/// drain's final idle sample.
static LISTENER_ADMISSION_CLOSED: AtomicBool = AtomicBool::new(false);

pub(crate) fn close_listener_admission() {
    LISTENER_ADMISSION_CLOSED.store(true, Ordering::SeqCst);
}

pub(crate) fn reset_listener_admission() {
    LISTENER_ADMISSION_CLOSED.store(false, Ordering::SeqCst);
}

pub(crate) fn listener_admission_closed() -> bool {
    LISTENER_ADMISSION_CLOSED.load(Ordering::SeqCst)
}

/// Whether no operation waits on readiness or is being handed its readiness.
pub(crate) fn drain_is_idle() -> bool {
    WAITERS.load(Ordering::SeqCst) == 0
}

/// Start the reactor thread if it is not running. Returns false when the
/// poller or the thread cannot be created.
pub(crate) fn ensure_reactor_started() -> bool {
    if REACTOR_RUNNING.load(Ordering::Acquire) {
        return true;
    }
    let mut handle = REACTOR_HANDLE.lock_or_recover();
    if REACTOR_RUNNING.load(Ordering::Acquire) {
        return true;
    }
    if poller().is_err() {
        crate::set_last_error("hew I/O reactor: no readiness poller");
        return false;
    }
    if let Some(finished) = handle.take() {
        crate::util::report_join_panic("hew I/O reactor thread", finished.join());
    }
    REACTOR_STOP.store(false, Ordering::SeqCst);
    match std::thread::Builder::new()
        .name("hew-io-reactor".into())
        .spawn(reactor_loop)
    {
        Ok(thread) => {
            *handle = Some(thread);
            REACTOR_RUNNING.store(true, Ordering::Release);
            true
        }
        Err(error) => {
            crate::set_last_error(format!("hew I/O reactor: spawn failed: {error}"));
            false
        }
    }
}

/// Wake the reactor when `deadline_ms` falls before its current sleep limit.
/// Called after every insert into the process wheel.
pub(crate) fn timer_inserted(deadline_ms: u64) {
    if deadline_ms < SLEEP_UNTIL.load(Ordering::SeqCst) {
        if let Ok(poller) = poller() {
            poller.wake();
        }
    }
}

/// How long the next wait may sleep. Publishing the sleep limit before
/// re-reading the wheel pairs with `timer_inserted`: an insert either sees the
/// published limit and wakes the poller, or this re-read sees the insert.
fn next_timeout(wheel: *mut HewTimerWheel) -> c_int {
    if wheel.is_null() {
        SLEEP_UNTIL.store(u64::MAX, Ordering::SeqCst);
        return -1;
    }
    // SAFETY: the wheel stays live until the reactor has been joined.
    let mut gap = unsafe { crate::timer_wheel::hew_timer_wheel_next_deadline_ms(wheel) };
    loop {
        let until = u64::try_from(gap).map_or(u64::MAX, |gap| {
            // SAFETY: hew_now_ms has no preconditions on native targets.
            unsafe { crate::clock::hew_now_ms() }.saturating_add(gap)
        });
        SLEEP_UNTIL.store(until, Ordering::SeqCst);
        // SAFETY: as above.
        let again = unsafe { crate::timer_wheel::hew_timer_wheel_next_deadline_ms(wheel) };
        let sooner = again >= 0 && (gap < 0 || again < gap);
        if !sooner {
            return c_int::try_from(gap).unwrap_or(c_int::MAX).max(-1);
        }
        gap = again;
    }
}

fn reactor_loop() {
    let Ok(poller) = poller() else {
        REACTOR_RUNNING.store(false, Ordering::Release);
        return;
    };
    let mut events: Vec<Event> = Vec::with_capacity(256);
    while !REACTOR_STOP.load(Ordering::Acquire) {
        let wheel = crate::timer_periodic::process_wheel();
        let timeout = next_timeout(wheel);
        let waited = poller.wait(timeout, &mut events);
        SLEEP_UNTIL.store(AWAKE, Ordering::SeqCst);
        #[cfg(test)]
        LOOP_TURNS.fetch_add(1, Ordering::SeqCst);
        match waited {
            Ok(()) => {}
            Err(error) if error.kind() == io::ErrorKind::Interrupted => {}
            Err(error) => {
                fail_reactor(&error);
                return;
            }
        }
        for event in events.drain(..) {
            dispatch(event);
        }
        if !wheel.is_null() {
            // SAFETY: the wheel stays live until the reactor has been joined.
            unsafe { crate::timer_wheel::hew_timer_wheel_tick(wheel) };
        }
    }
}

#[cfg(unix)]
fn dispatch(event: Event) {
    let Ok(handle) = c_int::try_from(event.token) else {
        return;
    };
    let slot = if handle == STDIN_HANDLE {
        STDIN_SLOT.get().cloned()
    } else {
        lookup(handle)
    };
    if let Some(slot) = slot {
        slot.fire(event.events);
    }
}

#[cfg(windows)]
fn dispatch(event: Event) {
    // SAFETY: the arm retained exactly this slot reference as its context.
    let slot = unsafe { Arc::from_raw(event.token as usize as *const Slot) };
    AFD_IN_FLIGHT.fetch_sub(1, Ordering::SeqCst);
    // SAFETY: the completion has been dequeued; the kernel is done writing.
    let events = unsafe { (*slot.afd.get()).report() } | event.events;
    slot.fire(events);
}

/// A poll error other than an interrupt leaves no way to learn readiness.
/// Report it once, fail every waiter with it and stop; the next registration
/// starts a fresh reactor.
fn fail_reactor(error: &io::Error) {
    eprintln!("hew: I/O reactor poll failed: {error}");
    crate::set_last_error(format!("hew I/O reactor poll failed: {error}"));
    for slot in slots() {
        for operation in slot.take_waiters() {
            operation.complete(Err(IoFailure::from_io("wait for TCP readiness", error)));
            waiters_removed(1);
        }
    }
    REACTOR_RUNNING.store(false, Ordering::Release);
}

/// Cancel and wake every waiting operation. Shutdown uses this so parked
/// tasks take their cancellation edge before workers are joined.
pub(crate) fn cancel_all() -> usize {
    let mut cancelled = 0;
    for slot in slots() {
        for operation in slot.take_waiters() {
            cancelled += usize::from(operation.cancel_and_wake());
            waiters_removed(1);
        }
    }
    cancelled
}

/// Resolve every wait that was parked when shutdown began.
pub(crate) fn reactor_cancel_parked_waits_for_shutdown() -> usize {
    cancel_all()
}

/// Stop and join the reactor thread, then cancel what still waits. Runtime
/// cleanup calls this before it frees actors and the process wheel.
pub(crate) fn reactor_shutdown() {
    REACTOR_STOP.store(true, Ordering::SeqCst);
    if let Ok(poller) = poller() {
        poller.wake();
    }
    let handle = REACTOR_HANDLE.lock_or_recover().take();
    if let Some(handle) = handle {
        crate::util::report_join_panic("hew I/O reactor thread", handle.join());
    }
    REACTOR_RUNNING.store(false, Ordering::Release);
    cancel_all();
    #[cfg(windows)]
    drain_afd_polls();
    REACTOR_STOP.store(false, Ordering::SeqCst);
}

/// Cancel every in-flight AFD poll and dequeue each completion, so no slot
/// buffer is released while the kernel may still write it.
#[cfg(windows)]
fn drain_afd_polls() {
    let Ok(poller) = poller() else {
        return;
    };
    for slot in slots() {
        let state = slot.state.lock_or_recover();
        if state.armed != 0 {
            // SAFETY: a poll is in flight on these buffers.
            unsafe { poller.cancel(slot.afd.get()) };
        }
    }
    let mut events = Vec::new();
    while AFD_IN_FLIGHT.load(Ordering::SeqCst) != 0 {
        if poller.wait(1000, &mut events).is_err() {
            break;
        }
        for event in events.drain(..) {
            dispatch(event);
        }
    }
}

/// Process exit status when the I/O balance check finds open handles or
/// waiting operations after runtime cleanup. Distinct from the actor-leak
/// status (93) and every trap or signal status.
pub const HEW_EXIT_IO_LEAK: i32 = 94;

/// With `HEW_IO_LEAK_CHECK=1`, report handles still open or operations still
/// waiting once runtime cleanup has finished, and exit [`HEW_EXIT_IO_LEAK`].
/// Every resource a program owns is closed by then, so anything left is a
/// leak. Runs after the runtime drops its blocking pool, whose stop joins
/// every job.
pub(crate) fn io_leak_verdict_after_runtime_cleanup() {
    if std::env::var("HEW_IO_LEAK_CHECK").as_deref() != Ok("1") {
        return;
    }
    let handles: usize = TABLE
        .shards
        .iter()
        .map(|shard| shard.lock_or_recover().len())
        .sum();
    let waiting = WAITERS.load(Ordering::SeqCst);
    if handles == 0 && waiting == 0 {
        return;
    }
    eprintln!(
        "hew: I/O leak: {handles} handle(s) open and {waiting} operation(s) waiting after \
         runtime cleanup"
    );
    std::process::exit(HEW_EXIT_IO_LEAK);
}

#[cfg(test)]
pub(crate) fn reactor_running() -> bool {
    REACTOR_RUNNING.load(Ordering::Acquire)
}

#[cfg(test)]
static LOOP_TURNS: AtomicUsize = AtomicUsize::new(0);

#[cfg(test)]
pub(crate) fn loop_turns_for_test() -> usize {
    LOOP_TURNS.load(Ordering::SeqCst)
}

#[cfg(test)]
pub(crate) fn waiter_count() -> usize {
    WAITERS.load(Ordering::SeqCst)
}

#[cfg(test)]
pub(crate) static REACTOR_TEST_MUTEX: Mutex<()> = Mutex::new(());
