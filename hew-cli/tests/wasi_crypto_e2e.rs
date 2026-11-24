mod support;

use std::fs;
use std::process::Command;

use support::{describe_output, repo_root, require_codegen, require_wasi_runner, run_hew_in};

const CRYPTO_E2E_SOURCE: &str = r#"import std.crypto.hash;
import std.crypto.encrypt;
import std.crypto.sign;
import std.encoding.hex;
import std.random;

fn h(text: string) -> bytes {
    hex.decode(text).expect("valid hex")
}

fn aes_key() -> bytes {
    "0123456789abcdef0123456789abcdef".to_bytes()
}

fn tamper_last_byte(ciphertext: bytes) -> bytes {
    let last_index = ciphertext.len() - 1;
    var tampered: bytes = bytes.new();
    var i = 0;
    while i < ciphertext.len() {
        let byte = ciphertext[i];
        if i == last_index {
            if byte == 0 {
                tampered.push(1);
            } else {
                tampered.push(byte - 1);
            }
        } else {
            tampered.push(byte);
        }
        i = i + 1;
    }
    tampered
}

fn main() {
    // 1. std.crypto.hash: SHA-256 / SHA-384 / SHA-512 NIST vectors
    let abc = "abc".to_bytes();
    let d256 = hash.sha256(abc);
    assert(d256 == h("ba7816bf8f01cfea414140de5dae2223b00361a396177a9cb410ff61f20015ad"));
    let d384 = hash.sha384(abc);
    assert(d384 == h("cb00753f45a35e8bb5a03d699ac65007272c32ab0eded1631a8b605a43ff5bed8086072ba1e7cc2358baeca134c825a7"));
    let d512 = hash.sha512(abc);
    assert(d512 == h("ddaf35a193617abacc417349ae20413112e6fa4e89a97ea20a9eeee64b55d39a2192992a274fc1a836ba3c23a3feebbd454d4423643ce80e2a9ac94fa54ca49f"));
    assert(hash.sha256("".to_bytes()) == h("e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855"));

    // HMAC-SHA256 RFC 4231 Test Case 2
    let mac = hash.hmac_sha256("Jefe".to_bytes(), "what do ya want for nothing?".to_bytes());
    assert(mac == h("5bdcc146bf60754e6a042426089575c75a003f089d2739839dec58b964ec3843"));

    // constant_time_eq
    assert(hash.constant_time_eq(mac, mac));
    assert(!hash.constant_time_eq(mac, d256));
    assert(!hash.constant_time_eq("abc".to_bytes(), "ab".to_bytes()));

    // random_bytes + std.random.crypto_u64
    let r1 = hash.random_bytes(32).expect("32 random bytes");
    let r2 = hash.random_bytes(32).expect("32 random bytes");
    assert(r1.len() == 32);
    assert(r2.len() == 32);
    assert(r1 != r2);
    assert(hash.random_bytes(0).expect("0 random bytes").len() == 0);
    match hash.random_bytes(-1) {
        .Ok(_) => panic("negative length unexpectedly succeeded"),
        .Err(RandomError.NegativeLength(n)) => assert(n == -1),
        .Err(other) => panic(f"unexpected random error: {other}"),
    }
    let too_long = hash.max_random_bytes_len() + 1;
    match hash.random_bytes(too_long) {
        .Ok(_) => panic("oversized length unexpectedly succeeded"),
        .Err(RandomError.TooLong { len, max }) => {
            assert(len == too_long);
            assert(max == hash.max_random_bytes_len());
        }
        .Err(other) => panic(f"unexpected random error: {other}"),
    }
    let u1 = random.crypto_u64().expect("crypto_u64");
    let u2 = random.crypto_u64().expect("crypto_u64");
    assert(u1 != u2);

    // 2. std.crypto.encrypt: AES-256-GCM seal / open + tamper + errors
    let sealed = encrypt.seal(r1, "hew wasi secret").expect("seal with random key");
    let opened = encrypt.open(r1, sealed).expect("open with random key");
    assert(opened == "hew wasi secret");

    let empty_sealed = encrypt.seal(aes_key(), "").expect("seal empty plaintext");
    assert(empty_sealed.len() == 28);
    assert(encrypt.open(aes_key(), empty_sealed).expect("open empty plaintext") == "");

    let tampered = tamper_last_byte(sealed);
    match encrypt.open(r1, tampered) {
        .Ok(_) => panic("open accepted tampered ciphertext"),
        .Err(CryptoError.AuthFailed) => {}
        .Err(other) => panic(f"expected AuthFailed, got {other}"),
    }
    match encrypt.open(aes_key(), sealed) {
        .Ok(_) => panic("open accepted wrong key"),
        .Err(CryptoError.AuthFailed) => {}
        .Err(other) => panic(f"expected AuthFailed for wrong key, got {other}"),
    }
    match encrypt.open(r1, bytes [1, 2, 3]) {
        .Ok(_) => panic("open accepted short ciphertext"),
        .Err(CryptoError.ShortCiphertext) => {}
        .Err(other) => panic(f"expected ShortCiphertext, got {other}"),
    }
    match encrypt.seal("short".to_bytes(), "hew") {
        .Ok(_) => panic("seal accepted short key"),
        .Err(CryptoError.InvalidKey) => {}
        .Err(other) => panic(f"expected InvalidKey, got {other}"),
    }

    // 3. std.crypto.sign: Ed25519 generate_seed / from_seed / from_expanded / from_pkcs8 / sign / verify
    let rfc_seed = h("4ccd089b28ff96da9db6c346ec114e0f5b8a319f35aba624da8cf6ed4fb8a6fb");
    let rfc_pub = h("3d4017c3e843895a92b70aa74d1b7ebc9c982ccf2ec4968cc0cd55f12af4660c");
    let rfc_sig = h("92a009a9f0d4cab8720e820b5f642540a2b27b5416503f8fb3762223ebdb69da085ac1e43e15996e458f3613d0f11d8c387b2eaeb4302aeeb00d291612bb0c00");
    let rfc_key = sign.from_seed(rfc_seed).expect("RFC 8032 seed");
    assert(rfc_key.public_key() == rfc_pub);
    let sig = rfc_key.sign(h("72"));
    assert(sig == rfc_sig);
    assert(sign.verify(h("72"), sig, rfc_pub));
    assert(!sign.verify("other".to_bytes(), sig, rfc_pub));

    let expanded_key = sign.from_expanded(rfc_key.expanded()).expect("expanded key");
    assert(expanded_key.public_key() == rfc_pub);
    assert(expanded_key.sign("hello".to_bytes()) == rfc_key.sign("hello".to_bytes()));
    expanded_key.close();
    rfc_key.close();

    // PKCS#8 v1 & v2 (RFC 8410 §10.3)
    let pkcs8_v1 = h("302e020100300506032b657004220420d4ee72dbf913584ad5b6d8f1f769f8ad3afe7c28cbf1d4fbe097a88f44755842");
    let pkcs8_v2 = h("3072020101300506032b657004220420d4ee72dbf913584ad5b6d8f1f769f8ad3afe7c28cbf1d4fbe097a88f44755842a01f301d060a2a864886f70d01090914310f0c0d437572646c652043686169727381210019bf44096984cdfe8541bac167dc3b96c85086aa30b6b6cb0c5c38ad703166e1");
    let v1_key = sign.from_pkcs8(pkcs8_v1).expect("PKCS#8 v1");
    let v2_key = sign.from_pkcs8(pkcs8_v2).expect("PKCS#8 v2");
    assert(v1_key.public_key() == v2_key.public_key());
    let fresh_key = sign.from_seed(sign.generate_seed()).expect("generated seed");
    let fresh_msg = "wasi crypto roundtrip".to_bytes();
    let fresh_sig = fresh_key.sign(fresh_msg);
    assert(sign.verify(fresh_msg, fresh_sig, fresh_key.public_key()));
    assert(!sign.verify(fresh_msg, fresh_sig, v1_key.public_key()));

    // Non-canonical S rejection (S + L)
    let order = h("edd3f55c1a631258d69cf7a2def9de1400000000000000000000000000000010");
    var non_canonical = bytes.new();
    for i in 0..32 {
        non_canonical.push(rfc_sig[i]);
    }
    var carry: i64 = 0;
    for i in 0..32 {
        let sum = (rfc_sig[32 + i] as i64) + (order[i] as i64) + carry;
        non_canonical.push((sum % 256) as u8);
        carry = sum / 256;
    }
    assert(!sign.verify(h("72"), non_canonical, rfc_pub));

    // Small-order identity point rejection (A = O, R = O, S = 0)
    var identity = bytes.new();
    identity.push(1);
    for _ in 1..32 {
        identity.push(0);
    }
    var forged = bytes.new();
    forged.append(identity);
    for _ in 0..32 {
        forged.push(0);
    }
    assert(!sign.verify("hew".to_bytes(), forged, identity));

    fresh_key.close();
    v1_key.close();
    v2_key.close();
    println("wasi crypto parity ok");
}
"#;

#[test]
fn crypto_hash_encrypt_sign_run_on_native_and_wasi() {
    require_codegen();
    require_wasi_runner();

    let dir = support::tempdir();
    let source = dir.path().join("crypto_parity.hew");
    fs::write(&source, CRYPTO_E2E_SOURCE).expect("write crypto parity source");
    let source_str = source.to_str().expect("source path must be UTF-8");

    // Verify `hew run` directly on both native and wasm32-wasi.
    let native_run = run_hew_in(repo_root(), &["run", source_str]);
    assert!(
        native_run.status.success(),
        "native `hew run` failed:\n{}",
        describe_output(&native_run)
    );
    assert_eq!(native_run.stdout, b"wasi crypto parity ok\n");
    assert!(
        native_run.stderr.is_empty(),
        "unexpected native stderr: {}",
        String::from_utf8_lossy(&native_run.stderr)
    );

    let wasi_run = run_hew_in(repo_root(), &["run", source_str, "--target=wasm32-wasi"]);
    assert!(
        wasi_run.status.success(),
        "WASI `hew run --target=wasm32-wasi` failed:\n{}",
        describe_output(&wasi_run)
    );
    assert_eq!(wasi_run.stdout, b"wasi crypto parity ok\n");
    assert!(
        wasi_run.stderr.is_empty(),
        "unexpected WASI stderr: {}",
        String::from_utf8_lossy(&wasi_run.stderr)
    );

    // Also build and execute at O0 and O2 via wasmtime directly.
    for opt in ["0", "2"] {
        let wasm_out = dir.path().join(format!("crypto_O{opt}.wasm"));
        let mut build_cmd = support::hew_command();
        build_cmd.current_dir(repo_root()).args([
            "build",
            source_str,
            "--target=wasm32-wasi",
            "--opt-level",
            opt,
            "-o",
            wasm_out.to_str().expect("output path"),
        ]);
        let build_output =
            support::run_bounded_command(build_cmd, format!("build crypto wasm O{opt}"));
        support::assert_success(&build_output, &format!("build crypto wasm O{opt}"));

        let mut run_cmd = Command::new(support::find_wasmtime().expect("wasmtime"));
        run_cmd.arg("run").arg(&wasm_out);
        let ran = support::run_bounded_command(run_cmd, format!("wasmtime crypto O{opt}"));
        support::assert_success(&ran, &format!("wasmtime crypto O{opt}"));
        assert_eq!(ran.stdout, b"wasi crypto parity ok\n");
    }
}

#[test]
fn generate_still_refuses_crypto_random_and_encrypt_seal() {
    require_codegen();

    for (name, body) in [
        (
            "random_bytes",
            "import std.crypto.hash;\nimport std.fs;\nfn main() { let _ = hash.random_bytes(16); fs.write(\"out/x.hew\", \"pub fn x() -> i64 { 1 }\").expect(\"out\"); }\n",
        ),
        (
            "encrypt_seal",
            "import std.crypto.encrypt;\nimport std.fs;\nfn main() { let _ = encrypt.seal(\"0123456789abcdef0123456789abcdef\".to_bytes(), \"x\"); fs.write(\"out/x.hew\", \"pub fn x() -> i64 { 1 }\").expect(\"out\"); }\n",
        ),
    ] {
        let dir = support::tempdir();
        let root = dir.path();
        fs::create_dir(root.join("tools")).expect("create tools dir");
        fs::write(
            root.join("hew.toml"),
            "[package]\nname = \"gen_crypto_refuse\"\nversion = \"0.1.0\"\n[[generate]]\nname = \"probe\"\nrun = \"tools/gen.hew\"\noutputs = \"out\"\n",
        )
        .expect("write hew.toml");
        fs::write(root.join("tools/gen.hew"), body).expect("write generator");
        fs::write(root.join("main.hew"), "fn main() {}\n").expect("write main.hew");

        let output = run_hew_in(root, &["generate"]);
        assert!(
            !output.status.success(),
            "generator calling {name} must fail closed:\n{}",
            describe_output(&output)
        );
        let stderr = String::from_utf8_lossy(&output.stderr);
        assert!(
            stderr.contains("E_GENERATE_HOST_OPERATION"),
            "expected E_GENERATE_HOST_OPERATION for {name}, got:\n{stderr}"
        );
    }
}
