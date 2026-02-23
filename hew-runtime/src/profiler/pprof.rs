//! Pprof-compatible protobuf profile generation.
//!
//! Produces gzip-compressed protobuf files in the
//! [pprof profile.proto](https://github.com/google/pprof/blob/main/proto/profile.proto)
//! format. Each actor becomes a "function" / "location" and its allocation
//! or processing stats become sample values.
//!
//! Also generates a GNU gprof-style flat profile as human-readable text.
//!
//! Usage:
//! ```bash
//! curl -o heap.pb.gz http://localhost:6060/debug/pprof/heap
//! go tool pprof heap.pb.gz
//!
//! curl http://localhost:6060/debug/pprof/profile
//! ```

use std::fmt::Write as _;
use std::io::Write as _;
use std::time::{SystemTime, UNIX_EPOCH};

use flate2::write::GzEncoder;
use flate2::Compression;

use crate::profiler::actor_registry;
use crate::profiler::allocator;

// ── Protobuf wire-format helpers ────────────────────────────────────────

/// Encode a varint into the buffer.
fn encode_varint(buf: &mut Vec<u8>, mut val: u64) {
    loop {
        let byte = (val & 0x7F) as u8;
        val >>= 7;
        if val == 0 {
            buf.push(byte);
            return;
        }
        buf.push(byte | 0x80);
    }
}

/// Encode a tag (field number + wire type).
fn encode_tag(buf: &mut Vec<u8>, field: u32, wire_type: u8) {
    encode_varint(buf, u64::from(field) << 3 | u64::from(wire_type));
}

/// Encode an int64 field (wire type 0 = varint).
fn encode_int64(buf: &mut Vec<u8>, field: u32, val: i64) {
    if val == 0 {
        return;
    }
    encode_tag(buf, field, 0);
    encode_varint(buf, val.cast_unsigned());
}

/// Encode a uint64 field (wire type 0 = varint).
fn encode_uint64(buf: &mut Vec<u8>, field: u32, val: u64) {
    if val == 0 {
        return;
    }
    encode_tag(buf, field, 0);
    encode_varint(buf, val);
}

/// Encode a bool field (wire type 0 = varint).
fn encode_bool(buf: &mut Vec<u8>, field: u32, val: bool) {
    if !val {
        return;
    }
    encode_tag(buf, field, 0);
    buf.push(1);
}

/// Encode a length-delimited field (wire type 2).
fn encode_bytes(buf: &mut Vec<u8>, field: u32, data: &[u8]) {
    encode_tag(buf, field, 2);
    let len = data.len() as u64;
    encode_varint(buf, len);
    buf.extend_from_slice(data);
}

/// Encode a packed repeated int64 field (wire type 2).
fn encode_packed_int64(buf: &mut Vec<u8>, field: u32, vals: &[i64]) {
    if vals.is_empty() {
        return;
    }
    let mut inner = Vec::new();
    for &v in vals {
        encode_varint(&mut inner, v.cast_unsigned());
    }
    encode_bytes(buf, field, &inner);
}

/// Encode a packed repeated uint64 field (wire type 2).
fn encode_packed_uint64(buf: &mut Vec<u8>, field: u32, vals: &[u64]) {
    if vals.is_empty() {
        return;
    }
    let mut inner = Vec::new();
    for &v in vals {
        encode_varint(&mut inner, v);
    }
    encode_bytes(buf, field, &inner);
}

// ── String table ────────────────────────────────────────────────────────

/// Intern strings and return indices into the string table.
struct StringTable {
    entries: Vec<String>,
}

impl StringTable {
    fn new() -> Self {
        // string_table[0] must always be "".
        Self {
            entries: vec![String::new()],
        }
    }

    /// Intern a string, returning its index.
    #[expect(
        clippy::cast_possible_wrap,
        reason = "string table won't exceed i64::MAX entries"
    )]
    fn intern(&mut self, s: &str) -> i64 {
        if s.is_empty() {
            return 0;
        }
        if let Some(idx) = self.entries.iter().position(|e| e == s) {
            return idx as i64;
        }
        let idx = self.entries.len();
        self.entries.push(s.to_owned());
        idx as i64
    }

    /// Encode all entries as repeated string (field 6 of Profile).
    fn encode(&self, buf: &mut Vec<u8>) {
        for s in &self.entries {
            encode_bytes(buf, 6, s.as_bytes());
        }
    }
}

// ── Profile generation ──────────────────────────────────────────────────

/// Generate a pprof-compatible heap profile (.pb.gz).
///
/// Each actor is represented as a function/location. Sample values are:
/// - `alloc_objects` (message count as proxy for allocations)
/// - `alloc_space` (bytes — mailbox high-water mark × estimated message size)
///
/// The global allocator stats are included as a separate "runtime" location.
#[must_use]
pub fn generate_heap_profile() -> Vec<u8> {
    let actors = actor_registry::snapshot_all();
    let alloc = allocator::snapshot();

    let mut strings = StringTable::new();
    let mut buf = Vec::with_capacity(4096);

    // Sample types: [alloc_objects/count, alloc_space/bytes]
    let alloc_objects_idx = strings.intern("alloc_objects");
    let count_idx = strings.intern("count");
    let alloc_space_idx = strings.intern("alloc_space");
    let bytes_idx = strings.intern("bytes");

    // Encode `sample_type` (field 1, repeated `ValueType`).
    encode_value_type(&mut buf, alloc_objects_idx, count_idx);
    encode_value_type(&mut buf, alloc_space_idx, bytes_idx);

    // Function ID and Location ID start at 1 (0 is reserved).
    let runtime_fn_id = 1_u64;
    let mut next_id = 2_u64;

    // Runtime global allocation entry.
    let runtime_name = strings.intern("[runtime]");
    let runtime_file = strings.intern("hew-runtime");
    encode_function(&mut buf, runtime_fn_id, runtime_name, runtime_file);
    encode_location(&mut buf, runtime_fn_id);
    encode_sample(
        &mut buf,
        runtime_fn_id,
        &[
            alloc.alloc_count.cast_signed(),
            alloc.bytes_live.cast_signed(),
        ],
    );

    // Per-actor entries.
    for actor in &actors {
        let fn_id = next_id;
        next_id += 1;

        let name = format!("Actor#{} (pid={})", actor.id, actor.pid);
        let name_idx = strings.intern(&name);
        let file_idx = strings.intern("hew-program");

        encode_function(&mut buf, fn_id, name_idx, file_idx);
        encode_location(&mut buf, fn_id);
        encode_sample(
            &mut buf,
            fn_id,
            &[
                actor.messages_processed.cast_signed(),
                actor.mailbox_hwm * 64, // estimate ~64 bytes per message
            ],
        );
    }

    // Mapping (field 3).
    {
        let hew_name = strings.intern("hew-program");
        let mut mapping = Vec::new();
        encode_uint64(&mut mapping, 1, 1);
        encode_uint64(&mut mapping, 2, 0x0040_0000);
        encode_uint64(&mut mapping, 3, 0x0050_0000);
        encode_int64(&mut mapping, 5, hew_name);
        encode_bool(&mut mapping, 7, true);
        encode_bytes(&mut buf, 3, &mapping);
    }

    // time_nanos (field 9).
    #[expect(
        clippy::cast_possible_truncation,
        reason = "nanos timestamp fits in i64 until year 2262"
    )]
    let time_nanos = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_nanos() as i64)
        .unwrap_or(0);
    encode_int64(&mut buf, 9, time_nanos);

    // period_type (field 11): space/bytes.
    {
        let mut pt = Vec::new();
        encode_int64(&mut pt, 1, alloc_space_idx);
        encode_int64(&mut pt, 2, bytes_idx);
        encode_bytes(&mut buf, 11, &pt);
    }
    encode_int64(&mut buf, 12, 1); // period

    // String table (field 6).
    strings.encode(&mut buf);

    // Gzip compress.
    let mut gz = GzEncoder::new(Vec::new(), Compression::default());
    let _ = gz.write_all(&buf);
    gz.finish().unwrap_or_default()
}

/// Encode a `ValueType` sub-message as `sample_type` (Profile field 1).
fn encode_value_type(buf: &mut Vec<u8>, type_idx: i64, unit_idx: i64) {
    let mut vt = Vec::new();
    encode_int64(&mut vt, 1, type_idx);
    encode_int64(&mut vt, 2, unit_idx);
    encode_bytes(buf, 1, &vt);
}

/// Encode a `Function` sub-message (Profile field 5).
fn encode_function(buf: &mut Vec<u8>, id: u64, name_idx: i64, file_idx: i64) {
    let mut func = Vec::new();
    encode_uint64(&mut func, 1, id);
    encode_int64(&mut func, 2, name_idx);
    encode_int64(&mut func, 3, name_idx); // system_name = name
    encode_int64(&mut func, 4, file_idx);
    encode_bytes(buf, 5, &func);
}

/// Encode a `Location` sub-message (Profile field 4).
fn encode_location(buf: &mut Vec<u8>, id: u64) {
    let mut loc = Vec::new();
    encode_uint64(&mut loc, 1, id);
    let mut line = Vec::new();
    encode_uint64(&mut line, 1, id); // function_id
    encode_bytes(&mut loc, 4, &line);
    encode_bytes(buf, 4, &loc);
}

/// Encode a `Sample` sub-message (Profile field 2).
fn encode_sample(buf: &mut Vec<u8>, location_id: u64, values: &[i64]) {
    let mut sample = Vec::new();
    encode_packed_uint64(&mut sample, 1, &[location_id]);
    encode_packed_int64(&mut sample, 2, values);
    encode_bytes(buf, 2, &sample);
}

// ── gprof-style flat profile ────────────────────────────────────────────

/// Generate a GNU gprof-style flat profile as human-readable text.
///
/// Output format mirrors `gprof` flat profile output:
/// ```text
///   %   cumulative   self              self     total
///  time    time(ms)  time(ms)  calls   us/call  us/call  name
///  45.2      124.0     124.0   3000     41.3     41.3    Actor#1 (pid=42)
/// ```
#[must_use]
#[expect(
    clippy::cast_precision_loss,
    reason = "f64 precision is fine for display"
)]
pub fn generate_flat_profile() -> String {
    let actors = actor_registry::snapshot_all();
    let alloc = allocator::snapshot();

    let total_ns: u64 = actors.iter().map(|a| a.processing_time_ns).sum();
    let total_time_ms = total_ns as f64 / 1_000_000.0;

    let mut out = String::with_capacity(2048);

    // Header.
    out.push_str("Flat profile:\n\n");
    let _ = writeln!(
        out,
        "Total processing time: {total_time_ms:.1} ms across {} actor(s)",
        actors.len()
    );
    let _ = writeln!(
        out,
        "Memory: {} live ({} peak), {} allocations\n",
        format_bytes(alloc.bytes_live),
        format_bytes(alloc.peak_bytes_live),
        alloc.alloc_count,
    );

    // Column headers.
    out.push_str("  %       cumulative   self                self       total\n");
    out.push_str(" time      time(ms)   time(ms)    calls   us/call    us/call    name\n");

    // Sort actors by processing time (descending).
    let mut sorted: Vec<_> = actors.iter().collect();
    sorted.sort_by(|a, b| b.processing_time_ns.cmp(&a.processing_time_ns));

    let mut cumulative_ms = 0.0_f64;

    for actor in &sorted {
        let self_ms = actor.processing_time_ns as f64 / 1_000_000.0;
        cumulative_ms += self_ms;
        let pct = if total_ns > 0 {
            actor.processing_time_ns as f64 / total_ns as f64 * 100.0
        } else {
            0.0
        };
        let self_us_per_call = if actor.messages_processed > 0 {
            actor.processing_time_ns as f64 / actor.messages_processed as f64 / 1000.0
        } else {
            0.0
        };

        let _ = writeln!(
            out,
            "{pct:5.1}    {cumulative_ms:10.1}   {self_ms:8.1}   {msgs:6}   {self_us:7.1}    {total_us:7.1}    Actor#{id} (pid={pid}, state={state})",
            msgs = actor.messages_processed,
            self_us = self_us_per_call,
            total_us = self_us_per_call,
            id = actor.id,
            pid = actor.pid,
            state = actor.state,
        );
    }

    // Mailbox summary.
    if !actors.is_empty() {
        out.push_str("\nMailbox summary:\n");
        out.push_str("   Actor     Depth      HWM\n");
        for actor in &sorted {
            let _ = writeln!(
                out,
                "   #{id:<8} {depth:>5}    {hwm:>5}",
                id = actor.id,
                depth = actor.mailbox_depth,
                hwm = actor.mailbox_hwm,
            );
        }
    }

    out.push('\n');
    out
}

#[expect(
    clippy::cast_precision_loss,
    reason = "f64 precision is fine for display"
)]
fn format_bytes(b: u64) -> String {
    if b < 1024 {
        format!("{b} B")
    } else if b < 1024 * 1024 {
        format!("{:.1} KB", b as f64 / 1024.0)
    } else {
        format!("{:.1} MB", b as f64 / (1024.0 * 1024.0))
    }
}
