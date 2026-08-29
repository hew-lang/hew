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

pub fn retained_payload_locals<'a>(section: &'a str, retain: &str) -> Vec<&'a str> {
    let lines = section.lines().collect::<Vec<_>>();
    let payload = lines
        .iter()
        .find(|line| line.contains("[PayloadBindingTransfer]"))
        .and_then(|line| line.split_once(" -> "))
        .and_then(|(_, tail)| tail.split_whitespace().next())
        .unwrap_or_else(|| panic!("missing payload-transfer owner:\n{section}"));
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
