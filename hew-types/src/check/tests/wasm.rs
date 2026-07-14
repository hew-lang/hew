#[allow(
    clippy::wildcard_imports,
    reason = "submodules mirror the legacy check namespace during the split"
)]
pub(super) use super::*;

// ── WASM compile-time reject tests ──────────────────────────────────────────
//
// These tests verify that `Channels`, `Semaphore`, `Timers`, and `Streams` features are
// rejected as compile-time errors (not warnings) when the WASM target is
// enabled.  The reject path is exercised by setting `checker.enable_wasm_target()`
// before calling `check_program`.
//
// Coverage:
//  - channel.new / send / try_recv → allowed on wasm32 bounded subset
//  - Receiver<T>::recv / `for await ... in Receiver<T>` → BlockingChannelRecv error
//  - semaphore.new / try_acquire / release / count / close → allowed on wasm32
//  - Semaphore::acquire / Semaphore::acquire_timeout → BlockingSemaphoreAcquire error
//  - sleep_ms → now an undefined-function error (removed; use sleep(duration))
//  - sleep → Timers warning
//  - Stream<T>::next → Streams error
//  - stream.* module constructor call → Streams error
//  - Non-wasm target: none of the above fire
mod wasm_rejects {
    use super::*;

    /// Parse `source`, enable the WASM target, run the type checker, and
    /// return the resulting output.
    fn check_wasm(source: &str) -> TypeCheckOutput {
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors in wasm_rejects test: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.enable_wasm_target();
        checker.check_program(&result.program)
    }

    /// Parse `source` without the WASM target and return the output.
    fn check_native(source: &str) -> TypeCheckOutput {
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors in wasm_rejects test (native): {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        checker.check_program(&result.program)
    }

    fn has_platform_limitation_error(output: &TypeCheckOutput) -> bool {
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::PlatformLimitation)
    }

    fn has_platform_limitation_warning(output: &TypeCheckOutput) -> bool {
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::PlatformLimitation)
    }

    fn platform_error_contains(output: &TypeCheckOutput, fragment: &str) -> bool {
        output
            .errors
            .iter()
            .any(|e| e.kind == TypeErrorKind::PlatformLimitation && e.message.contains(fragment))
    }

    fn platform_warning_contains(output: &TypeCheckOutput, fragment: &str) -> bool {
        output
            .warnings
            .iter()
            .any(|w| w.kind == TypeErrorKind::PlatformLimitation && w.message.contains(fragment))
    }

    // ── sleep / sleep_until ──────────────────────────────────────────────────

    #[test]
    fn wasm_warns_sleep_duration() {
        let output = check_wasm("fn main() { sleep(100ms); }");
        assert!(
            has_platform_limitation_warning(&output),
            "sleep(duration) should emit a PlatformLimitation warning on WASM; got warnings: {:?}",
            output.warnings
        );
        assert!(
            platform_warning_contains(&output, "Timer"),
            "warning message should mention Timer feature; got: {:?}",
            output.warnings
        );
        assert!(
            !has_platform_limitation_error(&output),
            "sleep should NOT be a compile-time error on WASM (cooperative semantics); got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_warns_sleep_until() {
        let output = check_wasm("fn main() { let t = instant::now(); sleep_until(t); }");
        assert!(
            has_platform_limitation_warning(&output),
            "sleep_until should emit a PlatformLimitation warning on WASM; got warnings: {:?}",
            output.warnings
        );
        assert!(
            !has_platform_limitation_error(&output),
            "sleep_until should NOT be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_sleep_no_platform_error() {
        let output = check_native("fn main() { sleep(100ms); }");
        assert!(
            !has_platform_limitation_error(&output),
            "sleep should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
    }

    /// Regression for PR #920: `reject_if_wasm_incompatible_call` matched on
    /// the *raw* callee identifier before any resolution, so an ordinary
    /// user function bare-named `sleep`/`sleep_until` (nothing to do with
    /// the stdlib timer primitive) was flagged with the same WASM
    /// `PlatformLimitation` warning as the real builtin, purely by name.
    /// A user-defined `sleep`/`sleep_until` must compile on WASM with no
    /// diagnostic at all — it shadows the builtin entirely, and the
    /// unrelated body/signature proves this isn't the timer primitive.
    #[test]
    fn wasm_user_defined_sleep_does_not_warn() {
        let output = check_wasm("fn sleep(x: i64) -> i64 { x + 1 } fn main() { sleep(41); }");
        assert!(
            !has_platform_limitation_warning(&output),
            "a user-defined `sleep` function must not trigger the Timer PlatformLimitation warning; got warnings: {:?}",
            output.warnings
        );
        assert!(
            !has_platform_limitation_error(&output),
            "a user-defined `sleep` function must not trigger any PlatformLimitation error; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_user_defined_sleep_until_does_not_warn() {
        let output =
            check_wasm("fn sleep_until(x: i64) -> i64 { x } fn main() { sleep_until(1); }");
        assert!(
            !has_platform_limitation_warning(&output),
            "a user-defined `sleep_until` function must not trigger the Timer PlatformLimitation warning; got warnings: {:?}",
            output.warnings
        );
        assert!(
            !has_platform_limitation_error(&output),
            "a user-defined `sleep_until` function must not trigger any PlatformLimitation error; got errors: {:?}",
            output.errors
        );
    }

    /// `sleep_ms` was removed in the `sleep(duration)` migration. Calling it
    /// must produce an undefined-function error, not silently compile.
    #[test]
    fn sleep_ms_is_undefined_function_error() {
        let output = check_native("fn main() { sleep_ms(100); }");
        assert!(
            !output.errors.is_empty(),
            "sleep_ms should be rejected as undefined; got no errors"
        );
        let msg = output
            .errors
            .iter()
            .map(|e| e.message.as_str())
            .collect::<Vec<_>>()
            .join("; ");
        assert!(
            msg.contains("sleep_ms") || msg.contains("undefined") || msg.contains("unknown"),
            "error should mention `sleep_ms` or 'undefined'; got: {msg}"
        );
    }

    #[test]
    fn wasm_warns_on_every_attribute() {
        let output =
            check_wasm("actor Ticker { #[every(10ms)] receive fn tick() {} } fn main() {}");
        assert!(
            has_platform_limitation_warning(&output),
            "#[every] should emit a PlatformLimitation warning on WASM; got warnings: {:?}",
            output.warnings
        );
        assert!(
            platform_warning_contains(&output, "Timer"),
            "#[every] warning should mention Timer operations; got: {:?}",
            output.warnings
        );
        assert!(
            !has_platform_limitation_error(&output),
            "#[every] should NOT be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_every_attribute_no_platform_error() {
        let output =
            check_native("actor Ticker { #[every(10ms)] receive fn tick() {} } fn main() {}");
        assert!(
            !has_platform_limitation_error(&output),
            "#[every] should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
    }

    // ── channel.new ──────────────────────────────────────────────────────────

    #[test]
    fn wasm_allows_bounded_channel_subset() {
        let source = concat!(
            "import std::channel::channel;\n",
            "fn main() {\n",
            "    let (tx, rx) = channel.new(1);\n",
            "    tx.send(\"hello\");\n",
            "    let _ = rx.try_recv();\n",
            "    tx.close();\n",
            "    rx.close();\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        checker.enable_wasm_target();
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "bounded channel.new/send/try_recv subset should be allowed on WASM; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_channel_new_no_platform_error() {
        let source = concat!(
            "import std::channel::channel;\n",
            "fn main() {\n",
            "    let pair = channel.new(0);\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "channel.new should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn receive_fn_await_channel_recv_does_not_warn_blocking() {
        let source = concat!(
            "import std::channel::channel;\n",
            "actor Worker {\n",
            "    receive fn run() {\n",
            "        let (tx, rx): (channel.Sender<string>, channel.Receiver<string>) = channel.new(1);\n",
            "        tx.send(\"hello\");\n",
            "        tx.close();\n",
            "        let _ = await rx.recv();\n",
            "        rx.close();\n",
            "    }\n",
            "}\n",
            "fn main() {\n",
            "    let w = spawn Worker;\n",
            "    w.run();\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            output.errors.is_empty(),
            "awaited receive-fn recv fixture should type-check cleanly: {:?}",
            output.errors
        );
        assert!(
            !output
                .warnings
                .iter()
                .any(|w| w.kind == TypeErrorKind::BlockingCallInReceiveFn),
            "await rx.recv() suspends and must not warn as blocking: {:?}",
            output.warnings
        );
    }

    #[test]
    fn receive_fn_bare_channel_recv_still_warns_blocking() {
        let source = concat!(
            "import std::channel::channel;\n",
            "actor Worker {\n",
            "    receive fn run() {\n",
            "        let (_tx, rx): (channel.Sender<string>, channel.Receiver<string>) = channel.new(1);\n",
            "        let _ = rx.recv();\n",
            "        rx.close();\n",
            "    }\n",
            "}\n",
            "fn main() {\n",
            "    let w = spawn Worker;\n",
            "    w.run();\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            output
                .warnings
                .iter()
                .any(|w| w.kind == TypeErrorKind::BlockingCallInReceiveFn),
            "bare rx.recv() in a receive fn must still warn as blocking: {:?}",
            output.warnings
        );
    }

    #[test]
    fn wasm_rejects_blocking_channel_recv() {
        let source = concat!(
            "import std::channel::channel;\n",
            "fn main() {\n",
            "    let (_tx, rx) = channel.new(1);\n",
            "    let _ = rx.recv();\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        checker.enable_wasm_target();
        let output = checker.check_program(&result.program);
        assert!(
            has_platform_limitation_error(&output),
            "blocking recv should still be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "Blocking channel receive"),
            "error message should mention blocking channel receive; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_for_await_receiver() {
        let source = concat!(
            "import std::channel::channel;\n",
            "fn main() {\n",
            "    let (tx, rx) = channel.new(1);\n",
            "    tx.send(\"hello\");\n",
            "    tx.close();\n",
            "    for await item in rx {\n",
            "        println(item);\n",
            "    }\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        checker.enable_wasm_target();
        let output = checker.check_program(&result.program);
        assert!(
            has_platform_limitation_error(&output),
            "`for await` over Receiver<T> should be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "Blocking channel receive"),
            "error message should mention blocking channel receive; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_for_await_stream() {
        let source = concat!(
            "import std::stream;\n",
            "fn main() {\n",
            "    let (sink, input) = stream.bytes_pipe(1);\n",
            "    sink.close();\n",
            "    for await item in input {\n",
            "        println(item.to_string());\n",
            "    }\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        checker.enable_wasm_target();
        let output = checker.check_program(&result.program);
        assert!(
            has_platform_limitation_error(&output),
            "`for await` over Stream<T> should be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "Stream operations"),
            "error message should mention Stream operations; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_for_await_receiver_no_platform_error() {
        let source = concat!(
            "import std::channel::channel;\n",
            "fn main() {\n",
            "    let (tx, rx) = channel.new(1);\n",
            "    tx.send(\"hello\");\n",
            "    tx.close();\n",
            "    for await item in rx {\n",
            "        println(item);\n",
            "    }\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "`for await` over Receiver<T> should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_allows_non_blocking_semaphore_subset() {
        let source = concat!(
            "import std::semaphore;\n",
            "fn main() {\n",
            "    let sem = semaphore.new(1);\n",
            "    let _ = sem.count();\n",
            "    let _ = sem.try_acquire();\n",
            "    sem.release();\n",
            "    sem.close();\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        checker.enable_wasm_target();
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "non-blocking semaphore subset should be allowed on WASM; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_blocking_semaphore_methods() {
        let source = concat!(
            "import std::semaphore;\n",
            "fn main() {\n",
            "    let sem = semaphore.new(1);\n",
            "    sem.acquire();\n",
            "    let _ = sem.acquire_timeout(10);\n",
            "    sem.close();\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        checker.enable_wasm_target();
        let output = checker.check_program(&result.program);
        let reject_count = output
            .errors
            .iter()
            .filter(|error| {
                error.kind == TypeErrorKind::PlatformLimitation
                    && error.message.contains("Blocking semaphore acquire")
            })
            .count();
        assert!(
            reject_count >= 2,
            "blocking semaphore methods should be rejected on WASM; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_blocking_semaphore_methods_no_platform_error() {
        let source = concat!(
            "import std::semaphore;\n",
            "fn main() {\n",
            "    let sem = semaphore.new(1);\n",
            "    sem.acquire();\n",
            "    let _ = sem.acquire_timeout(10);\n",
            "    sem.release();\n",
            "    sem.close();\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "blocking semaphore methods should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
    }

    // ── Stream<T> methods ────────────────────────────────────────────────────

    #[test]
    fn wasm_rejects_stream_method() {
        // Use a function that accepts a Stream<string> and calls .next().
        // The stream module must be imported to register Stream types.
        let source = concat!(
            "import std::stream;\n",
            "fn consume(s: stream.Stream<string>) -> string {\n",
            "    s.next()\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        checker.enable_wasm_target();
        let output = checker.check_program(&result.program);
        assert!(
            has_platform_limitation_error(&output),
            "Stream<T>::next should be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "Stream"),
            "error message should mention Stream feature; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_stream_method_no_platform_error() {
        let source = concat!(
            "import std::stream;\n",
            "fn consume(s: stream.Stream<string>) -> string {\n",
            "    s.next()\n",
            "}\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "Stream<T>::next should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
    }

    // ── TLS / QUIC / DNS / OS / CryptoRandom reject ────────────────────────

    fn check_wasm_with_registry(source: &str) -> TypeCheckOutput {
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors in wasm_rejects test: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        checker.enable_wasm_target();
        checker.check_program(&result.program)
    }

    #[test]
    fn wasm_rejects_tls_module_call() {
        let source = concat!(
            "import std::net::tls;\n",
            "fn main() { tls.connect(\"host\", 443); }\n",
        );
        let output = check_wasm_with_registry(source);
        assert!(
            has_platform_limitation_error(&output),
            "tls.connect should be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "std::net::tls"),
            "error message should mention TLS feature; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_quic_module_call() {
        let source = concat!(
            "import std::net::quic;\n",
            "fn main() { quic.new_client(); }\n",
        );
        let output = check_wasm_with_registry(source);
        assert!(
            has_platform_limitation_error(&output),
            "quic.* should be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "std::net::quic"),
            "error message should mention QUIC feature; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_dns_module_call() {
        let source = concat!(
            "import std::net::dns;\n",
            "fn main() { dns.resolve(\"example.com\"); }\n",
        );
        let output = check_wasm_with_registry(source);
        assert!(
            has_platform_limitation_error(&output),
            "dns.resolve should be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "std::net::dns"),
            "error message should mention DNS feature; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_os_module_call() {
        let source = concat!("import std::os;\n", "fn main() { os.env(\"HOME\"); }\n",);
        let output = check_wasm_with_registry(source);
        assert!(
            has_platform_limitation_error(&output),
            "os.* should be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "std::os"),
            "error message should mention OS feature; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_crypto_random_bytes() {
        // crypto.random_bytes requires secure entropy. On wasm32, no secure
        // implementation is linked, so the checker must fail closed.
        let source = concat!(
            "import std::crypto::crypto;\n",
            "fn main() { crypto.random_bytes(16); }\n",
        );
        let output = check_wasm_with_registry(source);
        assert!(
            has_platform_limitation_error(&output),
            "crypto.random_bytes should be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "random_bytes"),
            "error message should mention crypto.random_bytes; got: {:?}",
            output.errors
        );
        assert!(
            !has_platform_limitation_warning(&output),
            "crypto.random_bytes should not emit a PlatformLimitation warning on WASM; got warnings: {:?}",
            output.warnings
        );
    }

    /// Regression for PR #920: the bare-identifier `"random_bytes"` arm in
    /// `reject_if_wasm_incompatible_call` fired on raw name alone, so an
    /// ordinary user function bare-named `random_bytes` (nothing to do with
    /// `std::crypto.random_bytes`) hard-failed a WASM build with the
    /// crypto-entropy diagnostic pointed at the user's own call — not just a
    /// spurious warning, a genuine compile break for an unrelated function.
    #[test]
    fn wasm_user_defined_random_bytes_does_not_reject() {
        let output =
            check_wasm("fn random_bytes(n: i64) -> i64 { n * 2 } fn main() { random_bytes(21); }");
        assert!(
            !has_platform_limitation_error(&output),
            "a user-defined `random_bytes` function must not trigger the CryptoRandom PlatformLimitation error; got errors: {:?}",
            output.errors
        );
        assert!(
            !has_platform_limitation_warning(&output),
            "a user-defined `random_bytes` function must not trigger any PlatformLimitation warning either; got warnings: {:?}",
            output.warnings
        );
    }

    // ── Issue #2135: value-position (first-class fn reference) wasm rejects ──

    #[test]
    fn wasm_rejects_crypto_random_bytes_value_position() {
        // Taking crypto.random_bytes as a first-class function value on wasm32
        // must be rejected with PlatformLimitation, not silently allowed.
        let source = concat!(
            "import std::crypto::crypto;\n",
            "fn main() { let _f = crypto.random_bytes; }\n",
        );
        let output = check_wasm_with_registry(source);
        assert!(
            has_platform_limitation_error(&output),
            "crypto.random_bytes value-position reference must be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "random_bytes"),
            "error message should mention random_bytes; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_crypto_random_bytes_value_position_no_error() {
        // The same value-position reference must be accepted on native (non-wasm)
        // targets so the guard does not break native builds.
        let source = concat!(
            "import std::crypto::crypto;\n",
            "fn main() { let _f = crypto.random_bytes; }\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "crypto.random_bytes value-position must not error on native target; got: {:?}",
            output.errors
        );
    }

    // ── Native sibling tests: no platform error on non-wasm target ────────

    // ── Additional value-position coverage: net, http_client, encrypt (#2135) ─
    // These tests verify that the NATIVE_ONLY_WASM_MODULE_REJECTIONS table
    // covers a representative subset of the 12 native-only modules in the
    // value-position path so a future accidental deletion is caught.

    #[test]
    fn wasm_rejects_net_connect_value_position() {
        let source = concat!(
            "import std::net;\n",
            "fn main() { let _f = net.connect; }\n",
        );
        let output = check_wasm_with_registry(source);
        assert!(
            has_platform_limitation_error(&output),
            "net.connect value-position reference must be a compile-time error on WASM; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_http_client_request_value_position() {
        let source = concat!(
            "import std::net::http::http_client;\n",
            "fn main() { let _f = http_client.request; }\n",
        );
        let output = check_wasm_with_registry(source);
        assert!(
            has_platform_limitation_error(&output),
            "http_client.request value-position reference must be a compile-time error on WASM; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_encrypt_seal_value_position() {
        let source = concat!(
            "import std::crypto::encrypt;\n",
            "fn main() { let _f = encrypt.seal; }\n",
        );
        let output = check_wasm_with_registry(source);
        assert!(
            has_platform_limitation_error(&output),
            "encrypt.seal value-position reference must be a compile-time error on WASM; got: {:?}",
            output.errors
        );
    }

    // ── Over-reject regression: local bindings / params must not trigger guard ─

    #[test]
    fn wasm_admits_local_binding_named_net() {
        // A local let-binding named `net` must NOT trigger the value-position
        // guard because receiver_is_binding=true for local variables — the type
        // checker records a type for the object and check_field_access takes the
        // normal path.
        let source = r"
type Conn { connect: i64; }
fn main() {
    let net = Conn { connect: 42 };
    println(net.connect);
}
";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        checker.enable_wasm_target();
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "local binding named `net` must not produce PlatformLimitation on WASM; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_admits_function_param_named_stream() {
        // A function parameter named `stream` must NOT trigger the guard.
        let source = r"
type Packet { value: i64; }
fn process(stream: Packet) -> i64 {
    stream.value
}
fn main() {
    let p = Packet { value: 7 };
    println(process(p));
}
";
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        checker.enable_wasm_target();
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "function param named `stream` must not produce PlatformLimitation on WASM; got: {:?}",
            output.errors
        );
    }

    // ── Native sibling tests: no platform error on non-wasm target ────────

    #[test]
    fn native_tls_no_platform_error() {
        let source = concat!(
            "import std::net::tls;\n",
            "fn main() { tls.connect(\"host\", 443); }\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "tls.connect should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
        assert!(
            !has_platform_limitation_warning(&output),
            "tls.connect should not emit PlatformLimitation warning on native target; got: {:?}",
            output.warnings
        );
    }

    #[test]
    fn native_quic_no_platform_error() {
        let source = concat!(
            "import std::net::quic;\n",
            "fn main() { quic.new_client(); }\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "quic.* should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
        assert!(
            !has_platform_limitation_warning(&output),
            "quic.* should not emit PlatformLimitation warning on native target; got: {:?}",
            output.warnings
        );
    }

    #[test]
    fn native_dns_no_platform_error() {
        let source = concat!(
            "import std::net::dns;\n",
            "fn main() { dns.resolve(\"example.com\"); }\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "dns.resolve should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
        assert!(
            !has_platform_limitation_warning(&output),
            "dns.resolve should not emit PlatformLimitation warning on native target; got: {:?}",
            output.warnings
        );
    }

    #[test]
    fn native_os_no_platform_error() {
        let source = concat!("import std::os;\n", "fn main() { os.env(\"HOME\"); }\n",);
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "os.* should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
        assert!(
            !has_platform_limitation_warning(&output),
            "os.* should not emit PlatformLimitation warning on native target; got: {:?}",
            output.warnings
        );
    }

    #[test]
    fn native_crypto_random_bytes_no_platform_error() {
        let source = concat!(
            "import std::crypto::crypto;\n",
            "fn main() { crypto.random_bytes(16); }\n",
        );
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(test_registry());
        let output = checker.check_program(&result.program);
        assert!(
            !has_platform_limitation_error(&output),
            "crypto.random_bytes should not emit PlatformLimitation error on native target; got: {:?}",
            output.errors
        );
        assert!(
            !has_platform_limitation_warning(&output),
            "crypto.random_bytes should not emit PlatformLimitation warning on native target; got: {:?}",
            output.warnings
        );
    }

    // ── Deduplication: same call site emits only one error ─────────────────

    #[test]
    fn wasm_reject_deduplicates_same_span() {
        // Two consecutive calls at different call sites should produce two
        // warnings, not one (each span is unique).
        let output = check_wasm("fn main() { sleep(100ms); sleep(200ms); }");
        let count = output
            .warnings
            .iter()
            .filter(|w| w.kind == TypeErrorKind::PlatformLimitation && w.message.contains("Timer"))
            .count();
        assert_eq!(
            count, 2,
            "two distinct sleep call sites should produce two warnings; got: {:?}",
            output.warnings
        );
    }

    // ── Reject-level features now fail closed on WASM ────────────────────────

    fn supervisor_calls_source() -> &'static str {
        // `pool` is a reserved keyword from S-A (supervisor `pool` child decls).
        // Use `wp` for the local WorkerPool ref to avoid the keyword conflict.
        r"
            actor Worker {
                receive fn ping() {}
            }

            supervisor WorkerPool {
                strategy: one_for_one,
                intensity: 1 within 10s,
                child w1: Worker
            }

            fn main() {
                let wp = spawn WorkerPool;
                let worker = supervisor_child(wp, 0);
                supervisor_stop(wp);
                worker.ping();
            }
        "
    }

    fn link_monitor_calls_source() -> &'static str {
        r"
            actor Worker {
                receive fn ping() {}
            }

            fn main() {
                let worker = spawn Worker;
                let m: MonitorRef = monitor(worker);
                link(worker);
                let _ = m.close();
            }
        "
    }

    fn monitor_result_is_not_int_source() -> &'static str {
        r"
            actor Worker {
                receive fn ping() {}
            }

            fn main() {
                let worker = spawn Worker;
                let _ok: MonitorRef = monitor(worker);
                let x: i64 = monitor(worker);
                println(x);
            }
        "
    }

    fn monitor_ref_use_after_close_source() -> &'static str {
        r"
            actor Worker {
                receive fn ping() {}
            }

            fn main() {
                let worker = spawn Worker;
                let m: MonitorRef = monitor(worker);
                let _ = m.close();
                let _ = m.close();
            }
        "
    }

    fn remote_monitor_returns_typed_result_source() -> &'static str {
        r"
            actor Worker {
                receive fn ping() {}
            }

            fn main() {
                let remote: RemotePid<Worker> = RemotePid::from_raw(2, 1);
                let result: Result<MonitorRef, MonitorError> = monitor(remote);
                match result {
                    Ok(m) => {
                        m.close();
                    },
                    Err(_) => {},
                }
            }
        "
    }

    fn structured_concurrency_scope_source() -> &'static str {
        "fn main() { let result = scope { 1 + 2 }; println(result); }"
    }

    fn scope_tasks_source() -> &'static str {
        r"
            fn main() {
                scope {
                    fork task = compute();
                    await task;
                }
            }
            fn compute() -> i64 { 42 }
        "
    }

    #[test]
    fn wasm_rejects_supervisor_calls() {
        let output = check_wasm(supervisor_calls_source());
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::PlatformLimitation),
            "supervision operations should be WASM errors; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "Supervision tree"),
            "error message should mention Supervision tree feature; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_supervisor_calls_no_platform_error() {
        let output = check_native(supervisor_calls_source());
        assert!(
            !has_platform_limitation_error(&output),
            "supervision operations should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_supervisor_declaration() {
        let output = check_wasm(
            r"
            actor Worker {
                receive fn ping() {}
            }

            supervisor WorkerPool {
                strategy: one_for_one,
                intensity: 1 within 10s,
                child w1: Worker
            }

            fn main() {}
        ",
        );
        assert!(
            has_platform_limitation_error(&output),
            "supervisor declarations should be a WASM error; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "Supervision tree"),
            "error message should mention Supervision tree feature; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_link_monitor_calls() {
        let output = check_wasm(link_monitor_calls_source());
        assert!(
            has_platform_limitation_error(&output),
            "link/monitor operations should be a WASM error; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "Link/monitor"),
            "error message should mention Link/monitor feature; got: {:?}",
            output.errors
        );
    }

    /// Regression for PR #920: the `"link" | "unlink" | "monitor" | "demonitor"
    /// | "link_remote"` bare-name arm in `reject_if_wasm_incompatible_call`
    /// fired on raw name alone, so an ordinary user function bare-named
    /// `link`/`monitor` (unrelated signature, no actor handle involved) hard-
    /// failed a WASM build with the `LinkMonitor` diagnostic pointed at the
    /// user's own call.
    #[test]
    fn wasm_user_defined_link_and_monitor_do_not_reject() {
        let output = check_wasm(
            "fn link(a: i64, b: i64) -> i64 { a + b } \
             fn monitor(x: i64) -> i64 { x } \
             fn main() { link(1, 2); monitor(3); }",
        );
        assert!(
            !has_platform_limitation_error(&output),
            "user-defined `link`/`monitor` functions must not trigger the LinkMonitor PlatformLimitation error; got errors: {:?}",
            output.errors
        );
    }

    /// Regression for PR #920: same bare-name-match defect for the
    /// `"supervisor_child" | "supervisor_stop"` `SupervisionTrees` arm.
    #[test]
    fn wasm_user_defined_supervisor_child_does_not_reject() {
        let output = check_wasm(
            "fn supervisor_child(n: i64) -> i64 { n } fn main() { supervisor_child(0); }",
        );
        assert!(
            !has_platform_limitation_error(&output),
            "a user-defined `supervisor_child` function must not trigger the SupervisionTrees PlatformLimitation error; got errors: {:?}",
            output.errors
        );
    }

    /// M-8: the `#[on(crash)]` crash-handling surface rides supervision — a
    /// supervisor declaring a crash-hook child is wasm-rejected via the same
    /// Supervision-tree gate, so the crash payload (CrashInfo/CrashAction) can
    /// never reach a wasm crash path that fires. Parity is maintained by the
    /// firing surface (supervisors) being rejected, not the hook code.
    #[test]
    fn wasm_rejects_supervisor_with_crash_hook() {
        let output = check_wasm(
            r"
            import std::failure;

            actor Crasher {
                #[on(crash)]
                fn on_crash(info: CrashInfo) -> CrashAction {
                    CrashAction::Restart
                }
            }

            supervisor App {
                strategy: one_for_one,
                intensity: 1 within 10s,
                child c: Crasher
            }

            fn main() {}
        ",
        );
        assert!(
            has_platform_limitation_error(&output)
                && platform_error_contains(&output, "Supervision tree"),
            "a supervisor with a #[on(crash)] child must be a WASM error \
             (Supervision tree); got: {:?}",
            output.errors
        );
    }

    /// M-8: the `#[on(exit)]` linked-actor exit hook (M-7-R) rides the link
    /// surface — an actor that `link()`s a peer to receive its exit is
    /// wasm-rejected via the Link/monitor gate, so the `CrashNotification` can
    /// never reach a wasm exit path that fires. The `#[on(exit)]` hook itself
    /// compiles on wasm (parity with native), but the delivery surface that
    /// fires it is rejected.
    #[test]
    fn wasm_rejects_link_that_would_fire_exit_hook() {
        let output = check_wasm(
            r"
            import std::failure;

            actor Watcher {
                #[on(exit)]
                fn on_peer_exit(note: CrashNotification) {
                    let _id = note.actor_id;
                }

                receive fn watch(peer: u64) {
                    link(peer);
                }
            }

            fn main() {}
        ",
        );
        assert!(
            has_platform_limitation_error(&output)
                && platform_error_contains(&output, "Link/monitor"),
            "link() (the surface that fires #[on(exit)]) must be a WASM error \
             (Link/monitor); got: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_link_monitor_no_platform_error() {
        let output = check_native(link_monitor_calls_source());
        assert!(
            !has_platform_limitation_error(&output),
            "link/monitor operations should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
        assert!(
            output.errors.is_empty(),
            "link/monitor MonitorRef flow should typecheck cleanly on native target; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn monitor_result_is_not_int() {
        let output = check_native(monitor_result_is_not_int_source());
        assert!(
            output
                .errors
                .iter()
                .any(|e| matches!(e.kind, TypeErrorKind::Mismatch { .. })),
            "monitor() should no longer typecheck as i64; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn remote_monitor_returns_typed_result() {
        let output = check_native(remote_monitor_returns_typed_result_source());
        assert!(
            output.errors.is_empty(),
            "monitor(RemotePid) should return Result<MonitorRef, MonitorError>; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn monitor_ref_use_after_close() {
        let output = check_native(monitor_ref_use_after_close_source());
        assert!(
            output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::UseAfterMove),
            "second close() should surface UseAfterMove; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_structured_concurrency_scope() {
        let output = check_wasm(structured_concurrency_scope_source());
        assert!(
            has_platform_limitation_error(&output),
            "scope expressions should be a WASM error; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "Structured concurrency"),
            "error message should mention Structured concurrency feature; got: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "no cooperative task executor"),
            "error message should name the missing wasm32 task executor; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_scope_no_platform_error() {
        let output = check_native(structured_concurrency_scope_source());
        assert!(
            !has_platform_limitation_error(&output),
            "scope expressions should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_scope_tasks() {
        let output = check_wasm(scope_tasks_source());
        assert!(
            has_platform_limitation_error(&output),
            "scope tasks should be a WASM error; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "Task handles"),
            "error message should mention Task feature; got: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(
                &output,
                "task spawn is thread-based and no cooperative task executor drives forked bodies"
            ),
            "error message should name the missing task spawn/executor path; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_scope_tasks_no_platform_error() {
        let output = check_native(scope_tasks_source());
        assert!(
            !has_platform_limitation_error(&output),
            "scope tasks should not emit PlatformLimitation on native target; got: {:?}",
            output.errors
        );
    }

    // ── Node:: distributed namespace + RemotePid messaging reject ──────────
    //
    // The `Node::*` cluster API (`start`, `connect`, `load_keys`, `register`,
    // `lookup`, …) and `RemotePid<T>::send` / `RemotePid<T>::ask` remote
    // messaging lower to the native mesh transport (`hew_node_api_*` /
    // `hew_remote_pid_send`), which is not compiled for wasm32.  Without a
    // check-time gate the checker admitted these on wasm32 and codegen emitted a
    // module importing an undefined `env::hew_node_api_*` symbol that fails at
    // instantiation (admit-then-abort).  They must fail closed AT CHECK, like
    // every other native-only surface in this file.

    fn remote_pid_send_source() -> &'static str {
        concat!(
            "record Ping { n: i64 }\n",
            "actor Worker { receive fn ping(msg: Ping) {} }\n",
            "impl ActorMsg for Worker { type Msg = Ping; type Reply = (); }\n",
            "fn main() {\n",
            "    let pid: RemotePid<Worker> = RemotePid::from_raw<Worker>(1, 0);\n",
            "    let _ = pid.send(Ping { n: 0 });\n",
            "}\n",
        )
    }

    fn remote_pid_ask_source() -> &'static str {
        concat!(
            "record Ping { n: i64 }\n",
            "actor Worker { receive fn ping(msg: Ping) -> i64 { 0 } }\n",
            "impl ActorMsg for Worker { type Msg = Ping; type Reply = i64; }\n",
            "fn main() {\n",
            "    let pid: RemotePid<Worker> = RemotePid::from_raw<Worker>(1, 0);\n",
            "    let _ = pid.ask(Ping { n: 0 }, 1000);\n",
            "}\n",
        )
    }

    #[test]
    fn wasm_rejects_node_start() {
        let output = check_wasm(r#"fn main() { Node::start("a@127.0.0.1:9000"); }"#);
        assert!(
            has_platform_limitation_error(&output),
            "Node::start should be a compile-time error on WASM; got errors: {:?}",
            output.errors
        );
        assert!(
            platform_error_contains(&output, "Distributed node"),
            "error message should mention the Distributed node feature; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_node_connect() {
        let output = check_wasm(r#"fn main() { Node::connect("b@127.0.0.1:9001"); }"#);
        assert!(
            platform_error_contains(&output, "Distributed node"),
            "Node::connect should be a Distributed-node WASM error; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_node_load_keys() {
        let output = check_wasm(r#"fn main() { Node::load_keys("/keys/node.pem"); }"#);
        assert!(
            platform_error_contains(&output, "Distributed node"),
            "Node::load_keys should be a Distributed-node WASM error; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_node_register_and_lookup() {
        // `Node::register` (LocalPid arg) and `Node::lookup` (RemotePid result)
        // both ride the native registry transport and must fail closed too.
        let source = concat!(
            "actor Worker { receive fn ping() {} }\n",
            "fn main() {\n",
            "    let w = spawn Worker;\n",
            "    Node::register(\"w\", w);\n",
            "    let _ = Node::lookup<Worker>(\"w\");\n",
            "}\n",
        );
        let output = check_wasm(source);
        let count = output
            .errors
            .iter()
            .filter(|e| {
                e.kind == TypeErrorKind::PlatformLimitation
                    && e.message.contains("Distributed node")
            })
            .count();
        assert!(
            count >= 2,
            "Node::register and Node::lookup should each fail closed on WASM; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_node_calls_no_platform_error() {
        let source = concat!(
            "fn main() {\n",
            "    Node::start(\"a@127.0.0.1:9000\");\n",
            "    Node::connect(\"b@127.0.0.1:9001\");\n",
            "    Node::load_keys(\"/keys/node.pem\");\n",
            "}\n",
        );
        let output = check_native(source);
        assert!(
            !has_platform_limitation_error(&output),
            "Node:: calls must not emit PlatformLimitation on native; got: {:?}",
            output.errors
        );
        assert!(
            output.errors.is_empty(),
            "Node:: calls should typecheck cleanly on native; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_remote_pid_send() {
        let output = check_wasm(remote_pid_send_source());
        assert!(
            platform_error_contains(&output, "remote-actor"),
            "RemotePid::send should be a Distributed remote-actor WASM error; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_rejects_remote_pid_ask() {
        let output = check_wasm(remote_pid_ask_source());
        assert!(
            platform_error_contains(&output, "remote-actor"),
            "RemotePid::ask should be a Distributed remote-actor WASM error; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_remote_pid_send_no_platform_error() {
        let output = check_native(remote_pid_send_source());
        assert!(
            !has_platform_limitation_error(&output),
            "RemotePid::send must not emit PlatformLimitation on native; got: {:?}",
            output.errors
        );
        assert!(
            output.errors.is_empty(),
            "RemotePid::send should typecheck cleanly on native; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn native_remote_pid_ask_no_platform_error() {
        let output = check_native(remote_pid_ask_source());
        assert!(
            !has_platform_limitation_error(&output),
            "RemotePid::ask must not emit PlatformLimitation on native; got: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_multi_arm_literal_timed_select_is_not_warning() {
        let output = check_wasm(
            r"
            actor Responder {
                let value: i64;
                receive fn get() -> i64 {
                    value
                }
            }

            fn main() {
                let a = spawn Responder(value: 1);
                let b = spawn Responder(value: 2);
                let result = select {
                    x from a.get() => x,
                    y from b.get() => y,
                    after 1ms => -1,
                };
                println(result);
            }
        ",
        );
        assert!(
            !output
                .warnings
                .iter()
                .any(|w| w.kind == TypeErrorKind::PlatformLimitation),
            "literal timed select should no longer warn on WASM; got warnings: {:?}",
            output.warnings
        );
        assert!(
            !output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::PlatformLimitation),
            "literal timed select should not error on WASM; got errors: {:?}",
            output.errors
        );
    }

    #[test]
    fn wasm_computed_timed_select_no_longer_warns() {
        let output = check_wasm(
            r"
            actor Responder {
                let value: i64;
                receive fn get() -> i64 {
                    value
                }
            }

            fn main() {
                let a = spawn Responder(value: 1);
                let b = spawn Responder(value: 2);
                let timeout = 1ms;
                let result = select {
                    x from a.get() => x,
                    y from b.get() => y,
                    after timeout => -1,
                };
                println(result);
            }
        ",
        );
        assert!(
            !output.warnings.iter().any(
                |w| w.kind == TypeErrorKind::PlatformLimitation && w.message.contains("Select")
            ),
            "computed timed select should not warn on WASM; got warnings: {:?}",
            output.warnings
        );
        assert!(
            !output
                .errors
                .iter()
                .any(|e| e.kind == TypeErrorKind::PlatformLimitation),
            "computed timed select should not error on WASM; got errors: {:?}",
            output.errors
        );
    }

    // ── Ty::Error cascade-suppression regressions ────────────────────────────
    //
    // These tests verify that independent arg diagnostics are NOT suppressed
    // when the receiver/callee already has type Ty::Error.  Prior to the fix,
    // "bad arg" errors were silently dropped at every unknown-method `_` arm
    // and at `check_call_with_type` when called with a Ty::Error callee type.

    fn check_error_cascade(source: &str) -> Vec<TypeErrorKind> {
        let result = hew_parser::parse(source);
        assert!(
            result.errors.is_empty(),
            "parse errors: {:?}",
            result.errors
        );
        let mut checker = Checker::new(ModuleRegistry::new(vec![]));
        let output = checker.check_program(&result.program);
        output.errors.into_iter().map(|e| e.kind).collect()
    }

    #[test]
    fn bad_string_method_with_bad_arg_reports_both_errors() {
        // `s.nonexistent_method(undefined_arg)` must report BOTH:
        //   - "no method `nonexistent_method` on string"
        //   - "undefined variable `undefined_arg`"
        // Before the fix, only the first error was reported.
        let kinds =
            check_error_cascade(r"fn foo(s: string) { s.nonexistent_method(undefined_arg) }");
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedMethod),
            "expected UndefinedMethod; got: {kinds:?}",
        );
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedVariable),
            "expected UndefinedVariable (must not be cascade-suppressed); got: {kinds:?}",
        );
    }

    #[test]
    fn bad_vec_method_with_bad_arg_reports_both_errors() {
        // `v.nonexistent_method(undefined_arg)` on a Vec must report both errors.
        let kinds =
            check_error_cascade(r"fn foo(v: Vec<i64>) { v.nonexistent_method(undefined_arg) }");
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedMethod),
            "expected UndefinedMethod; got: {kinds:?}",
        );
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedVariable),
            "expected UndefinedVariable (must not be cascade-suppressed); got: {kinds:?}",
        );
    }

    #[test]
    fn bad_hashmap_method_with_bad_arg_reports_both_errors() {
        // `m.nonexistent_method(undefined_arg)` on a HashMap must report both errors.
        let kinds = check_error_cascade(
            r"fn foo(m: HashMap<string, i64>) { m.nonexistent_method(undefined_arg) }",
        );
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedMethod),
            "expected UndefinedMethod; got: {kinds:?}",
        );
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedVariable),
            "expected UndefinedVariable (must not be cascade-suppressed); got: {kinds:?}",
        );
    }

    #[test]
    fn error_typed_callable_still_checks_arg_errors() {
        // `let f = undefined_fn(); f(undefined_arg)` — when `f` has type Ty::Error
        // (because `undefined_fn` is unknown), the args to `f(...)` must still be
        // synthesized so `undefined_arg` errors are surfaced.
        let kinds =
            check_error_cascade(r"fn foo() { let f = undefined_fn(); let _ = f(undefined_arg); }");
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedFunction),
            "expected UndefinedFunction for `undefined_fn`; got: {kinds:?}",
        );
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedVariable),
            "expected UndefinedVariable (must not be cascade-suppressed); got: {kinds:?}",
        );
    }

    #[test]
    fn chained_bad_method_with_bad_arg_reports_arg_error() {
        // `s.bad_method().another_method(undefined_arg)` — the (Ty::Error, _) arm
        // already synthesizes args for chained calls, so `undefined_arg` SHOULD be
        // reported.  This test guards that the chained-call path is not regressed.
        let kinds = check_error_cascade(
            r"fn foo(s: string) { let _ = s.bad_method().another_method(undefined_arg); }",
        );
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedMethod),
            "expected UndefinedMethod for `bad_method`; got: {kinds:?}",
        );
        assert!(
            kinds.contains(&TypeErrorKind::UndefinedVariable),
            "expected UndefinedVariable in chained call; got: {kinds:?}",
        );
    }

    #[test]
    fn simple_bad_method_chain_still_suppresses_duplicate_error() {
        // `s.bad_method().to_string()` — the `.to_string()` on the Ty::Error result
        // is correctly suppressed (not a new error). The (Ty::Error, _) arm must
        // NOT emit a duplicate diagnostic for the chained method.
        let kinds =
            check_error_cascade(r"fn foo(s: string) { let _ = s.bad_method().to_string(); }");
        assert_eq!(
            kinds,
            vec![TypeErrorKind::UndefinedMethod],
            "expected exactly [UndefinedMethod] — chain must stay suppressed; got: {kinds:?}",
        );
    }

    // ── Dedup parity: public const ↔ internal rejection table ────────────────

    #[test]
    fn native_only_wasm_modules_const_matches_rejection_table() {
        // Verify that the public API const `crate::NATIVE_ONLY_WASM_MODULES`
        // (consumed by hew-sandbox-wasm's profile gate) and the internal
        // `Checker::NATIVE_ONLY_WASM_MODULE_REJECTIONS` table (consumed by
        // `check_method_call` and `check_field_access`) enumerate the exact same
        // set of module short-names.  Drift between the two means:
        //   - a module added to the public const but missing from the table
        //     → the compiler silently accepts it on wasm32 (fail-open)
        //   - a module in the table but not in the public const
        //     → the sandbox's profile gate silently accepts it (parity gap)
        //
        // To intentionally verify the test catches drift: temporarily comment
        // out one entry from either list and confirm this test fails.
        let mut from_const: Vec<&str> = crate::NATIVE_ONLY_WASM_MODULES.to_vec();
        from_const.sort_unstable();

        let mut from_table: Vec<&str> = Checker::NATIVE_ONLY_WASM_MODULE_REJECTIONS
            .iter()
            .map(|(name, _)| *name)
            .collect();
        from_table.sort_unstable();

        assert_eq!(
            from_const, from_table,
            "NATIVE_ONLY_WASM_MODULES (public API const) and \
             Checker::NATIVE_ONLY_WASM_MODULE_REJECTIONS (internal table) must \
             list the same module short-names; add or remove the entry from BOTH \
             when the native-only module set changes"
        );
    }
}
