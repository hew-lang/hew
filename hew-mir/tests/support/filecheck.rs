/// Thin FileCheck-style directive runner for MIR dump assertions.
///
/// Parses `// CHECK:`, `// CHECK-NEXT:`, and `// CHECK-NOT:` directives from a
/// fixture string and asserts them against a dump string. Returns `Ok(())` on
/// success or `Err(String)` with a diagnostic message on failure.
///
/// # Directives
///
/// - `// CHECK: <pattern>` — the dump must contain `<pattern>` (substring) at
///   or after the position where the previous CHECK matched. Order is enforced:
///   the second CHECK must match later in the dump than the first.
/// - `// CHECK-NEXT: <pattern>` — the very next non-empty line after the
///   previous match position must contain `<pattern>`. Use this to assert
///   consecutive structure without being brittle about exact spacing.
/// - `// CHECK-NOT: <pattern>` — the dump must NOT contain `<pattern>` at any
///   position from the current match cursor to the end of the dump. Runs lazily
///   at the position after the preceding match.
///
/// # Usage
///
/// ```rust
/// use support::filecheck::check_directives;
///
/// let dump = "fn add(i64, i64) -> i64 [conv=default]\n  locals:\n    _0: i64\n  bb0:\n    return\n";
/// let fixture = "// CHECK: fn add\n// CHECK-NEXT: locals:\n// CHECK-NOT: trap\n";
/// check_directives(dump, fixture).unwrap();
/// ```
///
/// # Design
///
/// This is intentionally minimal: no wildcards, no regex, no label syntax,
/// no `CHECK-SAME`. The goal is to express ordering constraints that
/// `str::contains` cannot, not to replicate LLVM `FileCheck`. If more power
/// is needed, graduate to an external `FileCheck` dependency; until then,
/// the 100-line helper below suffices for MIR dump assertions.
pub fn check_directives(dump: &str, fixture: &str) -> Result<(), String> {
    let directives: Vec<Directive> = fixture.lines().filter_map(parse_directive).collect();

    if directives.is_empty() {
        return Ok(());
    }

    let dump_lines: Vec<&str> = dump.lines().collect();
    // `cursor` tracks the line index in `dump_lines` from which the next
    // CHECK/CHECK-NEXT search begins. CHECK-NOT searches from cursor to EOF.
    let mut cursor: usize = 0;

    for directive in &directives {
        match directive {
            Directive::Check(pattern) => {
                let found = dump_lines[cursor..]
                    .iter()
                    .enumerate()
                    .find(|(_, line)| line.contains(pattern.as_str()));
                match found {
                    Some((offset, _)) => cursor += offset + 1,
                    None => {
                        return Err(format!(
                            "CHECK failed: pattern {:?} not found in dump after line {cursor}\n\
                             Dump from cursor:\n{}",
                            pattern,
                            dump_lines[cursor..].join("\n")
                        ));
                    }
                }
            }
            Directive::CheckNext(pattern) => {
                // Find the next non-empty line at or after cursor.
                let next_line = dump_lines[cursor..]
                    .iter()
                    .enumerate()
                    .find(|(_, line)| !line.trim().is_empty());
                match next_line {
                    Some((offset, line)) => {
                        if !line.contains(pattern.as_str()) {
                            return Err(format!(
                                "CHECK-NEXT failed: expected {:?} on next non-empty line\n\
                                 Got: {:?}\n\
                                 Dump from cursor:\n{}",
                                pattern,
                                line,
                                dump_lines[cursor..].join("\n")
                            ));
                        }
                        cursor += offset + 1;
                    }
                    None => {
                        return Err(format!(
                            "CHECK-NEXT failed: no more lines after cursor {cursor}, expected {pattern:?}"
                        ));
                    }
                }
            }
            Directive::CheckNot(pattern) => {
                let found = dump_lines[cursor..]
                    .iter()
                    .enumerate()
                    .find(|(_, line)| line.contains(pattern.as_str()));
                if let Some((offset, line)) = found {
                    return Err(format!(
                        "CHECK-NOT failed: pattern {:?} found at dump line {}\n\
                         Matched line: {:?}",
                        pattern,
                        cursor + offset,
                        line
                    ));
                }
                // CHECK-NOT does not advance cursor — it is a negative window,
                // not a position anchor.
            }
        }
    }

    Ok(())
}

#[derive(Debug)]
enum Directive {
    Check(String),
    CheckNext(String),
    CheckNot(String),
}

fn parse_directive(line: &str) -> Option<Directive> {
    let trimmed = line.trim();
    if let Some(rest) = trimmed
        .strip_prefix("// CHECK-NEXT:")
        .or_else(|| trimmed.strip_prefix("//CHECK-NEXT:"))
    {
        return Some(Directive::CheckNext(rest.trim().to_string()));
    }
    if let Some(rest) = trimmed
        .strip_prefix("// CHECK-NOT:")
        .or_else(|| trimmed.strip_prefix("//CHECK-NOT:"))
    {
        return Some(Directive::CheckNot(rest.trim().to_string()));
    }
    if let Some(rest) = trimmed
        .strip_prefix("// CHECK:")
        .or_else(|| trimmed.strip_prefix("//CHECK:"))
    {
        return Some(Directive::Check(rest.trim().to_string()));
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    /// A CHECK-NOT that matches must fail — proving the harness rejects bad input.
    /// This is the "feature is done when it rejects bad input" gate (plan §6).
    #[test]
    fn check_not_fails_when_pattern_present() {
        let dump = "fn main() -> i64\n  bb0:\n    trap(IntegerOverflow)\n    return\n";
        let fixture = "// CHECK-NOT: trap";
        let result = check_directives(dump, fixture);
        assert!(
            result.is_err(),
            "CHECK-NOT over a dump containing 'trap' must fail"
        );
    }

    /// CHECK-NEXT fails when the next line does not match.
    #[test]
    fn check_next_fails_on_wrong_next_line() {
        let dump = "fn main() -> i64\n  bb0:\n    return\n";
        let fixture = "// CHECK: fn main\n// CHECK-NEXT: trap";
        let result = check_directives(dump, fixture);
        assert!(result.is_err(), "CHECK-NEXT with wrong pattern must fail");
    }

    /// Ordered CHECKs: pattern found before previous match must fail.
    #[test]
    fn check_order_is_enforced() {
        let dump = "fn main() -> i64\n  bb0:\n    return\n";
        // First check anchors at "return", second check looks for "fn" AFTER
        // that position — it won't be found.
        let fixture = "// CHECK: return\n// CHECK: fn main";
        let result = check_directives(dump, fixture);
        assert!(
            result.is_err(),
            "CHECK must not match earlier in the dump than the previous match"
        );
    }

    /// Happy-path: a complete fixture with CHECK, CHECK-NEXT, and CHECK-NOT passes.
    #[test]
    fn full_fixture_passes() {
        let dump =
            "fn add(i64, i64) -> i64 [conv=default]\n  locals:\n    _0: i64\n  bb0:\n    return\n";
        let fixture = "// CHECK: fn add\n// CHECK-NEXT: locals:\n// CHECK-NOT: trap\n";
        check_directives(dump, fixture).expect("full fixture must pass");
    }

    /// An empty fixture (no directives) always passes.
    #[test]
    fn empty_fixture_passes() {
        let dump = "fn main() -> i64\n  bb0:\n    return\n";
        let fixture = "// This is just a comment, no directives.\n";
        check_directives(dump, fixture).expect("empty fixture must pass");
    }
}
