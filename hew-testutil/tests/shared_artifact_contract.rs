//! Contract between the shared test artifact inventory, its Make builder, and
//! the verify-only paths used by integration tests.

#[test]
#[ignore = "run after `make stdlib` by the shared artifact contract target"]
fn builder_produces_every_inventoried_artifact() {
    let verified = hew_testutil::verify_shared_test_artifacts()
        .expect("the builder must produce every artifact the verifier can demand");
    assert!(
        verified.len() >= 5,
        "the concrete inventory unexpectedly shrank: {verified:?}"
    );
}
