//! Typed provider transfer is a separate protocol, never silently modeled here.
pub(super) use super::item::QueueItem;
pub(super) struct State;
impl super::ChannelCore {
    pub(crate) fn is_typed(&self) -> bool {
        assert!(
            self.locked().typed.is_none(),
            "channel: typed transfer outside model"
        );
        false
    }
    pub(crate) fn close_typed_sync(&self) {
        panic!("channel: typed transfer outside model");
    }
}
