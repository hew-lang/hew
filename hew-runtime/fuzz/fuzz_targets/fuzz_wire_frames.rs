//! Untrusted-input decoders: wire frames, control payloads, the handshake and
//! the checked-protocol records in every codec format.
//!
//! The first byte selects a decoder from `hew_runtime::fuzzing::DECODERS`; the
//! rest is its input. Each decoder must not panic, must keep its heap bounded
//! by the input it was given, and must be a fixed point of its encoder on
//! accepted input.
#![no_main]

mod support;

use hew_runtime::fuzzing::DECODERS;
use libfuzzer_sys::fuzz_target;

#[global_allocator]
static ALLOC: support::Counting = support::Counting;

fuzz_target!(|data: &[u8]| {
    let Some((&selector, input)) = data.split_first() else {
        return;
    };
    let decoder = &DECODERS[usize::from(selector) % DECODERS.len()];
    support::bounded(decoder.name, input.len(), || (decoder.check)(input));
});
