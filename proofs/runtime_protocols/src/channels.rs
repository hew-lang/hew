use crate::channel_core::ChannelCore;
use crate::runtime_sync::{AtomicUsize, Mutex};
use crate::wake::{HewWaker, OwnedWaker};
use hew_cabi::value::HewValueLayout;
use hew_cabi::vec::HewTypeOwnershipKind;
use std::ffi::c_void;
use std::ptr;
use std::sync::atomic::Ordering;
use std::sync::Arc;

struct Audit { released: [AtomicUsize; 4], delivered: [AtomicUsize;4] }
impl Audit {
    fn new() -> Self { Self { released: std::array::from_fn(|_| AtomicUsize::new(0)), delivered: std::array::from_fn(|_| AtomicUsize::new(0)) } }
}
#[repr(C)]
struct Token { audit: *const Audit, id: usize, value: *mut usize }
fn envelope(audit: &Audit, id: usize) -> Vec<u8> {
    let token = Token { audit, id, value: Box::into_raw(Box::new(id + 31)) };
    let mut bytes = vec![0; size_of::<Token>()];
    unsafe { ptr::copy_nonoverlapping((&raw const token).cast::<u8>(), bytes.as_mut_ptr(), bytes.len()) };
    bytes
}
unsafe fn release(token: Token, delivered: bool) {
    let audit = &*token.audit;
    assert_eq!(audit.released[token.id].fetch_add(1, Ordering::Relaxed), 0, "channel: message released twice");
    assert_eq!(*token.value, token.id + 31, "channel: message contents changed");
    drop(Box::from_raw(token.value));
    if delivered { audit.delivered[token.id].fetch_add(1,Ordering::Relaxed); }
}
unsafe extern "C-unwind" fn discard(slot: *mut c_void) { release(slot.cast::<Token>().read_unaligned(), false); }
fn consume(bytes: Vec<u8>) -> usize {
    assert_eq!(bytes.len(),size_of::<Token>(),"channel: envelope width changed");
    let token = unsafe { bytes.as_ptr().cast::<Token>().read_unaligned() };
    let id=token.id;
    unsafe { release(token,true) }; id
}
fn new_core(capacity:usize)->ChannelCore {
    let core=ChannelCore::new(capacity);
    core.stamp_elem_layout(&HewValueLayout {
        size:size_of::<Token>(), align:align_of::<Token>(), ownership_kind:HewTypeOwnershipKind::LayoutManaged,
        clone_fn:None,drop_fn:Some(discard),release_start:None,managed:ptr::null(),code_owner:ptr::null(),
    });
    core
}
struct Wake { count: AtomicUsize, reenter: Mutex<Option<std::sync::Weak<ChannelCore>>> }
unsafe extern "C" fn retain(p:*mut c_void){Arc::increment_strong_count(p.cast::<Wake>());}
unsafe extern "C" fn free(p:*mut c_void){Arc::decrement_strong_count(p.cast::<Wake>());}
unsafe extern "C" fn notify(p:*mut c_void){
    let wake=&*p.cast::<Wake>();
    wake.count.fetch_add(1,Ordering::Relaxed);
    if let Some(core)=wake.reenter.lock().unwrap().as_ref().and_then(std::sync::Weak::upgrade) {
        let _=core.fault_message();
    }
}
fn descriptor(wake:&Arc<Wake>)->HewWaker { HewWaker {context:Arc::as_ptr(wake).cast_mut().cast(),retain,release:free,wake:notify} }
fn wake()->Arc<Wake>{Arc::new(Wake{count:AtomicUsize::new(0),reenter:Mutex::new(None)})}

#[cfg(feature="channel_loom")]
pub fn verify(selection:Option<&str>) {
    use loom::thread;
    let names=["handoff","backpressure","close_send","fault_drain","detach_promotion","observation","last_sink","reentrant"];
    assert!(selection.is_none_or(|name|names.contains(&name)),"channel: unknown scenario");
    for name in names {
        if selection.is_some_and(|selected| selected != name) {continue;}
        let mut model=loom::model::Builder::new();
        model.preemption_bound=None;
        model.max_permutations=None;
        model.max_duration=None;
        model.checkpoint_file=None;
        model.expect_explicit_explore=false;
        model.check(move|| {
            let audit=Arc::new(Audit::new());
            let core=Arc::new(new_core(1));
            let reader=wake();let writer=wake();
            let mut created=1;
            match name {
                "handoff" | "reentrant" => {
                    if name=="reentrant" {*reader.reenter.lock().unwrap()=Some(Arc::downgrade(&core));}
                    let c=core.clone();let a=audit.clone();let w=writer.clone();
                    let send=thread::spawn(move||unsafe{c.poll_send_envelope(&descriptor(&w),envelope(&a,0))});
                    let c=core.clone();let w=reader.clone();
                    let receive=thread::spawn(move||unsafe{c.poll_recv_envelope(&descriptor(&w))});
                    let (sent,returned)=send.join().unwrap();
                    assert_eq!(sent,1,"channel: empty queue refused send");assert!(returned.is_none(),"channel: accepted send retained a second owner");
                    let (status,value)=receive.join().unwrap();
                    let value=if status==0 {
                        assert!(value.is_none());
                        assert_eq!(reader.count.load(Ordering::Relaxed),1,"channel: committed message stranded receiver");
                        let (status,value)=unsafe{core.poll_recv_envelope(&descriptor(&reader))};
                        assert_eq!(status,1,"channel: notified receiver cannot retrieve message");value.unwrap()
                    }else{assert_eq!(status,1);value.unwrap()};
                    assert_eq!(consume(value),0,"channel: wrong message delivered");
                }
                "backpressure" => {
                    created=2;
                    assert_eq!(core.try_send(envelope(&audit,0)),hew_cabi::sink::TrySendResult::Accepted);
                    let probe=wake();
                    let (status,returned)=unsafe{core.poll_send_envelope(&descriptor(&probe),envelope(&audit,1))};
                    assert_eq!(status,0,"channel: full queue consumed caller message");
                    let pending=returned.expect("channel: backpressure lost caller ownership");
                    let c=core.clone();let a=audit.clone();let w=writer.clone();
                    let send=thread::spawn(move||unsafe{let _keep_ledger_live=a;c.poll_send_envelope(&descriptor(&w),pending)});
                    let c=core.clone();let w=reader.clone();
                    let receive=thread::spawn(move||unsafe{c.poll_recv_envelope(&descriptor(&w))});
                    let (status,value)=receive.join().unwrap();assert_eq!(status,1);assert_eq!(consume(value.unwrap()),0,"channel: bounded queue reordered accepted messages");
                    let (status,returned)=send.join().unwrap();
                    if status==0 {
                        assert_eq!(writer.count.load(Ordering::Relaxed),1,"channel: freed capacity stranded blocked producer");
                        let (status,returned)=unsafe{core.poll_send_envelope(&descriptor(&writer),returned.unwrap())};
                        assert_eq!(status,1);assert!(returned.is_none());
                    } else {assert_eq!(status,1);assert!(returned.is_none());}
                    assert_eq!(consume(core.pop().unwrap()),1,"channel: refused or accepted message lost");
                }
                "close_send" => {
                    created=2;let _=core.try_send(envelope(&audit,0));
                    let c=core.clone();let a=audit.clone();let w=writer.clone();
                    let send=thread::spawn(move||unsafe{c.poll_send_envelope(&descriptor(&w),envelope(&a,1))});
                    let c=core.clone();let close=thread::spawn(move||c.close_stream());
                    let (status,returned)=send.join().unwrap();close.join().unwrap();
                    assert!(status==0 || status==2,"channel: send committed after reader closed");
                    if status==0 {assert_eq!(writer.count.load(Ordering::Relaxed),1,"channel: closure stranded producer");}
                    assert_eq!(consume(returned.unwrap()),1,"channel: rejected send lost caller ownership");
                    assert_eq!(audit.delivered[0].load(Ordering::Relaxed),0,"channel: closed reader received message");
                }
                "fault_drain" => {
                    let _=core.try_send(envelope(&audit,0));
                    let c=core.clone();let fault=thread::spawn(move||c.fault_close(71));
                    let c=core.clone();let w=reader.clone();let receive=thread::spawn(move||unsafe{c.poll_recv_envelope(&descriptor(&w))});
                    let (status,value)=receive.join().unwrap();fault.join().unwrap();
                    assert_eq!(status,1,"channel: fault erased accepted message");assert_eq!(consume(value.unwrap()),0);
                    let (status,value)=unsafe{core.poll_recv_envelope(&descriptor(&reader))};
                    assert_eq!(status,3,"channel: producer fault was masked as EOF or pending");assert!(value.is_none());
                    core.fault_close(72);assert!(core.fault_message().contains("71"),"channel: later fault replaced original cause");
                }
                "detach_promotion" => unsafe {
                    created=2;let _=core.try_send(envelope(&audit,0));
                    let mut actor=crate::actor::HewActor{id:1};
                    let slot=crate::read_slot::hew_read_slot_new();
                    assert_eq!(core.await_send(&mut actor,slot,envelope(&audit,1)),0);
                    let c=core.clone();let raw=slot as usize;
                    let cancel=thread::spawn(move||c.detach_producer(raw as *mut _));
                    let c=core.clone();let pop=thread::spawn(move||c.pop());
                    assert_eq!(consume(pop.join().unwrap().unwrap()),0);
                    cancel.join().unwrap();
                    if let Some(value)=core.pop() {assert_eq!(consume(value),1);}
                    assert_eq!(audit.released[1].load(Ordering::Relaxed),1,"channel: cancelled or promoted message lost its owner");
                    crate::read_slot::hew_read_slot_free(slot);
                },
                "observation" => {
                    let first=wake();let first_owner=Arc::new(unsafe{OwnedWaker::retain(&descriptor(&first))});
                    assert_eq!(core.poll_recv_ready(&first_owner),0);
                    let successor=Arc::new(unsafe{OwnedWaker::retain(&descriptor(&reader))});
                    assert_eq!(core.poll_recv_ready(&successor),0);
                    let cancel=thread::spawn(move||drop(first_owner));
                    let c=core.clone();let a=audit.clone();let send=thread::spawn(move||c.try_send(envelope(&a,0)));
                    cancel.join().unwrap();assert_eq!(send.join().unwrap(),hew_cabi::sink::TrySendResult::Accepted);
                    assert_eq!(first.count.load(Ordering::Relaxed),0,"channel: cancelled observer was reawakened");
                    assert_eq!(reader.count.load(Ordering::Relaxed),1,"channel: cancellation disarmed successor");
                    assert_eq!(consume(core.pop().unwrap()),0,"channel: readiness observation consumed data");
                    drop(successor);
                }
                "last_sink" => {
                    created=0;core.clone_sink();
                    let c=core.clone();let close=thread::spawn(move||c.close_sink());
                    let c=core.clone();let w=reader.clone();let receive=thread::spawn(move||unsafe{c.poll_recv_envelope(&descriptor(&w))});
                    let (status,value)=receive.join().unwrap();close.join().unwrap();
                    assert_eq!(status,0,"channel: one closed sink published premature EOF");assert!(value.is_none());
                    core.close_sink();
                    assert_eq!(reader.count.load(Ordering::Relaxed),1,"channel: last producer failed to wake EOF waiter");
                    assert_eq!(unsafe{core.poll_recv_envelope(&descriptor(&reader))}.0,2,"channel: last sink did not publish EOF");
                }
                _=>unreachable!(),
            }
            drop(core);
            for id in 0..created {assert_eq!(audit.released[id].load(Ordering::Relaxed),1,"channel: message owner leaked after terminal cleanup");}
            assert_eq!(Arc::strong_count(&reader),1,"channel: reader registration leaked");
            assert_eq!(Arc::strong_count(&writer),1,"channel: writer registration leaked");
        });
        println!("Verified channel {name} (all interleavings)");
    }
}
