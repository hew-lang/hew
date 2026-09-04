//! Cross-crate behaviour oracle for runtime ask tags at the public Result ABI.

use hew_runtime::internal::types::{
    translate_ask_error_tag_for_public_result, AskError, PublicAskResultTag, HEW_ASK_RESULT_OK_TAG,
};

fn surface_askerror_variants() -> Vec<&'static str> {
    let catalog = hew_types::builtin_enums::monomorphic_builtin_enums();
    let entry = catalog
        .iter()
        .find(|entry| entry.name == "AskError")
        .expect("hew-types monomorphic catalog must contain AskError");
    entry.variants.iter().map(|variant| variant.name).collect()
}

#[test]
fn runtime_success_sentinel_translates_to_public_ok() {
    assert_eq!(
        translate_ask_error_tag_for_public_result(AskError::None as i32),
        Ok(PublicAskResultTag::Ok),
    );
    assert_eq!(
        hew_runtime::internal::types::hew_ask_error_translate_for_public_result(
            AskError::None as i32,
        ),
        HEW_ASK_RESULT_OK_TAG,
    );
}

#[test]
fn runtime_ask_errors_translate_to_matching_public_variants() {
    let surface = surface_askerror_variants();
    for (runtime, expected_surface) in [
        (AskError::NodeNotRunning, "NodeNotRunning"),
        (AskError::RoutingFailed, "RoutingFailed"),
        (AskError::EncodeFailed, "EncodeFailed"),
        (AskError::SendFailed, "SendFailed"),
        (AskError::Timeout, "Timeout"),
        (AskError::ConnectionDropped, "ConnectionDropped"),
        (AskError::PayloadSizeMismatch, "PayloadSizeMismatch"),
        (AskError::WorkerAtCapacity, "WorkerAtCapacity"),
        (AskError::ActorStopped, "ActorStopped"),
        (AskError::MailboxFull, "MailboxFull"),
        (AskError::OrphanedAsk, "OrphanedAsk"),
        (AskError::NoRunnableWork, "NoRunnableWork"),
        (AskError::DecodeFailure, "DecodeFailure"),
        (AskError::Partition, "Partition"),
        (AskError::StaleRef, "StaleRef"),
        (AskError::Cancelled, "Cancelled"),
        (AskError::LocalShutdown, "LocalShutdown"),
        (AskError::VersionMismatch, "VersionMismatch"),
        (AskError::Unauthorized, "Unauthorized"),
        (AskError::Backpressure, "Backpressure"),
        (AskError::MonitorLost, "MonitorLost"),
    ] {
        let public_tag = match translate_ask_error_tag_for_public_result(runtime as i32) {
            Ok(PublicAskResultTag::Err(tag)) => tag,
            other => panic!("runtime tag {runtime:?} did not translate to public Err: {other:?}"),
        };
        assert_eq!(
            surface.get(public_tag as usize),
            Some(&expected_surface),
            "runtime tag {runtime:?} did not select its public AskError variant",
        );
    }
}

#[test]
fn unmapped_runtime_ask_tag_is_refused() {
    assert_eq!(
        translate_ask_error_tag_for_public_result(i32::MAX),
        Err(i32::MAX),
    );
}
