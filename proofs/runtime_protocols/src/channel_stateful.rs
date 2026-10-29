//! Generated histories against the full runtime and its real release walker.
use super::*;
use proptest::prelude::*;
use std::ffi::c_void;
use std::sync::atomic::{AtomicUsize, Ordering};

struct Ledger {
    released: [AtomicUsize; 64],
    delivered: [AtomicUsize; 64],
}
impl Ledger {
    fn new() -> Self {
        Self {
            released: std::array::from_fn(|_| AtomicUsize::new(0)),
            delivered: std::array::from_fn(|_| AtomicUsize::new(0)),
        }
    }
}
#[repr(C)]
struct Message {
    ledger: *const Ledger,
    id: usize,
    payload: *mut usize,
}
fn message(ledger: &Ledger, id: usize) -> Vec<u8> {
    let message = Message {
        ledger,
        id,
        payload: Box::into_raw(Box::new(id ^ 0xa55a)),
    };
    let mut bytes = vec![0; size_of::<Message>()];
    // SAFETY: the envelope moves this initialized message's three scalar fields.
    unsafe {
        std::ptr::copy_nonoverlapping(
            (&raw const message).cast::<u8>(),
            bytes.as_mut_ptr(),
            bytes.len(),
        );
    }
    bytes
}
#[expect(
    clippy::needless_pass_by_value,
    reason = "this callback consumes the envelope owner, including its raw payload"
)]
unsafe fn release(message: Message, delivered: bool) {
    // SAFETY: the history retains its ledger until all queue owners are retired.
    let ledger = unsafe { &*message.ledger };
    assert_eq!(
        ledger.released[message.id].fetch_add(1, Ordering::Relaxed),
        0,
        "message owner released twice"
    );
    // SAFETY: the first retirement owns the original allocation exclusively.
    let payload = unsafe { Box::from_raw(message.payload) };
    assert_eq!(
        *payload,
        message.id ^ 0xa55a,
        "message contents changed during transfer"
    );
    if delivered {
        ledger.delivered[message.id].fetch_add(1, Ordering::Relaxed);
    }
}
unsafe extern "C-unwind" fn discard(slot: *mut c_void) {
    // SAFETY: the runtime's descriptor walker lends one initialized aligned slot.
    unsafe { release(slot.cast::<Message>().read(), false) };
}
fn retire(bytes: Vec<u8>, delivered: bool) -> usize {
    assert_eq!(bytes.len(), size_of::<Message>());
    // SAFETY: this envelope owns the initialized fields, with byte alignment.
    let message = unsafe { bytes.as_ptr().cast::<Message>().read_unaligned() };
    let id = message.id;
    drop(bytes);
    // SAFETY: the caller consumes this envelope exactly once.
    unsafe { release(message, delivered) };
    id
}
unsafe extern "C" fn ignore(_: *mut c_void) {}
fn waker() -> HewWaker {
    HewWaker {
        context: std::ptr::null_mut(),
        wake: ignore,
        retain: ignore,
        release: ignore,
    }
}

proptest! {
    #![proptest_config(ProptestConfig{cases:512,max_shrink_iters:10000,..ProptestConfig::default()})]
    #[test]
    fn generated_channel_failures_conserve_every_message(capacity in 0usize..5, operations in prop::collection::vec(0u8..9,1..64)){
        let ledger=Ledger::new();
        let core=ChannelCore::new(capacity);
        core.stamp_elem_layout(&HewValueLayout{size:size_of::<Message>(),align:align_of::<Message>(),ownership_kind:HewTypeOwnershipKind::LayoutManaged,clone_fn:None,drop_fn:Some(discard),release_start:None,code_owner:std::ptr::null(),managed:std::ptr::null()});
        let mut queue=VecDeque::new();let mut sinks=1usize;let mut reader_closed=false;let mut faulted=false;let mut created=0;
        let mut delivered=Vec::new();
        for operation in operations {
            match operation {
                0|1=>{
                    let id=created;created+=1;
                    let terminal=reader_closed || sinks==0 || faulted;
                    let full=queue.len()>=capacity.max(1);
                    if operation==0 {
                        let status=core.try_send(message(&ledger,id));
                        prop_assert_eq!(status,if terminal{TrySendResult::Closed}else if full{TrySendResult::Full}else{TrySendResult::Accepted});
                    }else{
                        // SAFETY: callbacks accept concurrent retain/release and keep no borrowed context.
                        let (status,returned)=unsafe{core.poll_send_envelope(&waker(),message(&ledger,id))};
                        prop_assert_eq!(status,if terminal{2}else{i32::from(!full)});
                        if terminal||full {prop_assert_eq!(retire(returned.unwrap(),false),id);}else{prop_assert!(returned.is_none());}
                    }
                    if !terminal && !full{queue.push_back(id);}
                }
                2 if !reader_closed=>{
                    // SAFETY: the no-op notification descriptor has no borrowed context.
                    let (status,value)=unsafe{core.poll_recv_envelope(&waker())};
                    if let Some(expected)=queue.pop_front(){
                        prop_assert_eq!(status,1);prop_assert_eq!(retire(value.unwrap(),true),expected);delivered.push(expected);
                    }else{prop_assert_eq!(status,if faulted{3}else if sinks==0{2}else{0});prop_assert!(value.is_none());}
                }
                3 if sinks>0=>{core.clone_sink();sinks+=1;}
                4 if sinks>0=>{core.close_sink();sinks-=1;}
                5=>{core.close_stream();reader_closed=true;queue.clear();}
                6=>{core.fault_close(17);faulted=true;}
                7 if !reader_closed=>{
                    // SAFETY: the descriptor's callbacks are valid no-ops.
                    let observer=Arc::new(unsafe{OwnedWaker::retain(&waker())});
                    prop_assert_eq!(core.poll_recv_ready(&observer),if !queue.is_empty(){1}else if faulted{3}else if sinks==0{2}else{0});
                    drop(observer);
                }
                _=>{}
            }
        }
        drop(core);
        for id in 0..created {
            prop_assert_eq!(ledger.released[id].load(Ordering::Relaxed),1,"message {} lost or released twice",id);
            prop_assert_eq!(ledger.delivered[id].load(Ordering::Relaxed),usize::from(delivered.contains(&id)),"message {} delivered more than once",id);
        }
    }
}

proptest! {
    #![proptest_config(ProptestConfig{cases:128,max_shrink_iters:10000,..ProptestConfig::default()})]
    #[test]
    fn generated_blocked_send_cancellation_keeps_committed_owners(
        producers in 1usize..5, actions in prop::collection::vec((any::<bool>(),0usize..4),0..20)
    ){
        let scheduler=crate::scheduler::NoWorkerSchedulerForTest::install();
        let actor=crate::test_actor::TrackedTestActor::install_parked();
        let ledger=Ledger::new();
        let core=ChannelCore::new(1);
        core.stamp_elem_layout(&HewValueLayout{size:size_of::<Message>(),align:align_of::<Message>(),ownership_kind:HewTypeOwnershipKind::LayoutManaged,clone_fn:None,drop_fn:Some(discard),release_start:None,code_owner:std::ptr::null(),managed:std::ptr::null()});
        prop_assert_eq!(core.try_send(message(&ledger,0)),TrySendResult::Accepted);
        let mut slots=Vec::new();let mut waiting=VecDeque::new();let mut queued=Some(0);
        for id in 1..=producers {
            let slot=crate::read_slot::hew_read_slot_new();
            // SAFETY: the tracked actor and creator-owned slot outlive all registration use.
            prop_assert_eq!(unsafe{core.await_send(actor.ptr(),slot,message(&ledger,id))},STREAM_AWAIT_SUSPEND);
            slots.push(slot);waiting.push_back(id);
        }
        for (cancel,index) in actions {
            if cancel {
                let id=1+index%producers;let slot=slots[id-1];
                // SAFETY: the creator reference remains held through the entire history.
                unsafe{crate::read_slot::hew_read_slot_cancel(slot);core.detach_producer(slot);}
                if let Some(position)=waiting.iter().position(|&waiting_id|waiting_id==id){waiting.remove(position);}
            }else{
                let actual=core.pop();
                match queued.take(){
                    Some(id)=>{prop_assert_eq!(retire(actual.unwrap(),true),id);queued=waiting.pop_front();}
                    None=>prop_assert!(actual.is_none()),
                }
            }
        }
        core.close_stream();drop(core);
        while let Some(queued_actor)=scheduler.pop_global(){prop_assert_eq!(queued_actor,actor.ptr());}
        for slot in slots {
            // SAFETY: core closure relinquished every independent in-flight slot reference.
            unsafe{crate::read_slot::hew_read_slot_free(slot)};
        }
        for id in 0..=producers{prop_assert_eq!(ledger.released[id].load(Ordering::Relaxed),1,"message {} leaked or was reclaimed twice",id);}
    }
}
