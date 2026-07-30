use super::is_handle_borrowing_call_abi;
use hew_types::runtime_call::RuntimeCallFamily as F;

#[test]
fn channel_sender_send_is_a_borrow_of_the_endpoint() {
    assert!(
        is_handle_borrowing_call_abi(Some(F::ChannelSendLayout)),
        "channel send queues the payload but leaves Sender<T> owned by its caller"
    );
}

#[test]
fn channel_receive_and_try_receive_borrow_the_receiver_endpoint() {
    assert!(
        is_handle_borrowing_call_abi(Some(F::ChannelRecvLayout)),
        "blocking receive decodes a payload but leaves Receiver<T> owned by its caller"
    );
    assert!(
        is_handle_borrowing_call_abi(Some(F::ChannelTryRecvLayout)),
        "try-receive decodes a payload but leaves Receiver<T> owned by its caller"
    );
}
