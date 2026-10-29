// Compile the production file itself; there is no copied gate implementation.
#[cfg(test)]
#[allow(dead_code)]
#[path = "../../../hew-runtime/src/ready_state.rs"]
mod production;

#[cfg(test)]
#[path = "../concurrency.rs"]
mod concurrency;
