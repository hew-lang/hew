fn main() {
    hew_runtime_protocol_verification::verify(std::env::args().nth(1).as_deref());
}
