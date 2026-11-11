//! Interpret llvm-config's C++ flags without splitting its unquoted include path.

pub fn cxx_flags(flags: &str, include_dir: &str) -> Result<Vec<String>, &'static str> {
    let mut remaining = flags.to_owned();
    for candidate in [
        format!("-I{include_dir}"),
        format!("-I\"{include_dir}\""),
        format!("-I'{include_dir}'"),
        format!("-I {include_dir}"),
    ] {
        while let Some(start) = remaining.match_indices(&candidate).find_map(|(start, _)| {
            let end = start + candidate.len();
            let before = remaining[..start].chars().next_back();
            let after = remaining[end..].chars().next();
            (before.is_none_or(char::is_whitespace) && after.is_none_or(char::is_whitespace))
                .then_some(start)
        }) {
            remaining.replace_range(start..start + candidate.len(), "");
        }
    }
    shlex::split(&remaining).ok_or("invalid quoting in llvm-config --cxxflags")
}
