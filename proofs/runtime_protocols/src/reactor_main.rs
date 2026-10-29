fn main() {
    hew_runtime_protocol_verification::verify_reactor(std::env::args().nth(1).as_deref());
}
