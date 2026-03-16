// hew-lib: umbrella crate that re-exports hew-runtime + all stdlib crates
// as a single staticlib. Cargo handles deduplication of shared dependencies.
//
// The extern crate declarations force Cargo to link each crate's #[no_mangle]
// FFI symbols into the output archive.

extern crate hew_runtime;
extern crate hew_std_crypto_crypto;
extern crate hew_std_crypto_jwt;
extern crate hew_std_crypto_password;
extern crate hew_std_encoding_base64;
extern crate hew_std_encoding_compress;
extern crate hew_std_encoding_csv;
extern crate hew_std_encoding_json;
extern crate hew_std_encoding_markdown;
extern crate hew_std_encoding_msgpack;
extern crate hew_std_encoding_protobuf;
extern crate hew_std_encoding_toml;
extern crate hew_std_encoding_xml;
extern crate hew_std_encoding_yaml;
extern crate hew_std_math;
extern crate hew_std_misc_log;
extern crate hew_std_misc_uuid;
extern crate hew_std_net_dns;
extern crate hew_std_net_http;
extern crate hew_std_net_ipnet;
extern crate hew_std_net_quic;
extern crate hew_std_net_smtp;
extern crate hew_std_net_tls;
extern crate hew_std_net_url;
extern crate hew_std_net_websocket;
extern crate hew_std_sort;
extern crate hew_std_text_regex;
extern crate hew_std_text_semver;
extern crate hew_std_time_cron;
extern crate hew_std_time_datetime;
