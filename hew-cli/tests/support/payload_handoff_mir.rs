//! Text-MIR helpers shared by the string and bytes payload-handoff oracles.

pub fn function_section<'a>(dump: &'a str, name: &str) -> &'a str {
    let marker = format!("fn {name}");
    let start = dump
        .find(&marker)
        .unwrap_or_else(|| panic!("missing `{marker}` in MIR dump:\n{dump}"));
    let tail = &dump[start..];
    tail.find("\nfn ").map_or(tail, |next| &tail[..next])
}

pub fn unique_drop_locals<'a>(section: &'a str, marker: &str) -> Vec<&'a str> {
    let mut locals = section
        .lines()
        .filter(|line| line.contains(marker))
        .filter_map(|line| line.split_whitespace().nth(1))
        .collect::<Vec<_>>();
    locals.sort_unstable();
    locals.dedup();
    locals
}

/// The payload binder is the destination of the arm's projection move
/// (`_13 = move mvar6.0.0`) — the one local that holds the enum/machine
/// payload for the arm. Reading it off the move, rather than off a
/// neutralize, keeps this helper independent of whether the binder took the
/// payload's release authority or aliases the scrutinee's (#2523).
fn projected_payload_local<'a>(lines: &[&'a str], section: &str) -> &'a str {
    lines
        .iter()
        .find_map(|line| {
            let (dest, source) = line.trim().split_once(" = move ")?;
            (source.starts_with("mvar") || source.starts_with("evar")).then_some(dest)
        })
        .unwrap_or_else(|| panic!("missing projected payload binder:\n{section}"))
}

/// Whether the raw stream demotes the projected payload binder to an alias of
/// the scrutinee's payload slot. The parent's composite drop is then the single
/// release authority for that buffer and the binder carries none; a binder that
/// keeps a release of its own (a `string` payload, whose flag-guarded delayed
/// release the enum-overwrite authority promotes to) is not demoted (#2523).
pub fn projected_payload_binder_is_alias(section: &str) -> bool {
    let lines = section.lines().collect::<Vec<_>>();
    let payload = projected_payload_local(&lines, section);
    let place = format!("place: Local({})", payload.trim_start_matches('_'));
    lines
        .iter()
        .any(|line| line.contains("DemoteToAlias") && line.contains(&place))
}

pub fn retained_payload_locals<'a>(section: &'a str, retain: &str) -> Vec<&'a str> {
    let lines = section.lines().collect::<Vec<_>>();
    let payload = projected_payload_local(&lines, section);
    let mut locals = vec![payload];
    for (index, line) in lines.iter().enumerate() {
        let Some(source) = line.trim().strip_prefix(retain) else {
            continue;
        };
        let move_line = lines
            .get(index + 1)
            .unwrap_or_else(|| panic!("retain must be followed by its owned move:\n{section}"));
        let (destination, moved_source) = move_line
            .trim()
            .split_once(" = move ")
            .unwrap_or_else(|| panic!("retain must be followed by its owned move:\n{section}"));
        assert_eq!(
            moved_source, source,
            "the retained share must move into the next owner:\n{section}"
        );
        locals.push(destination);
    }
    locals
}

/// Count one local's normal and exceptional cleanup appearances, plus the
/// maximum appearances in any single mutually exclusive drop plan.
pub fn drop_plan_counts(section: &str, marker: &str) -> (usize, usize, usize) {
    let mut normal = 0;
    let mut exceptional = 0;
    let mut in_normal = false;
    let mut in_exceptional = false;
    let mut current_plan_count = 0;
    let mut max_per_plan = 0;
    for line in section.lines() {
        let is_header =
            line.starts_with("    ") && !line.starts_with("      ") && line.contains("] ->");
        if is_header {
            max_per_plan = max_per_plan.max(current_plan_count);
            current_plan_count = 0;
            let header = line.trim();
            in_normal = header.starts_with("goto[") || header.starts_with("return[");
            in_exceptional = header.starts_with("unwind[")
                || header.starts_with("cancel[")
                || header.starts_with("panic[");
        } else if line.contains(marker) {
            current_plan_count += 1;
            normal += usize::from(in_normal);
            exceptional += usize::from(in_exceptional);
        }
    }
    max_per_plan = max_per_plan.max(current_plan_count);
    (normal, exceptional, max_per_plan)
}
