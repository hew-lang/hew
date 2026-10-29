fn main() {
    let selection = std::env::args().nth(1);
    hew_runtime_protocol_verification::verify_concurrent_compositions(selection.as_deref());
}
