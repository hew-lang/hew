//! Parser-backed authority for stdlib execution-proof imports.

use hew_parser::{ast::Item, parse, Severity};
use std::env;
use std::fs;
use std::path::Path;
use std::process::ExitCode;

fn source_imports_module(source: &str, module: &str) -> Result<bool, Vec<String>> {
    let segments = module.split("::").collect::<Vec<_>>();
    if segments.is_empty() || segments.iter().any(|segment| segment.is_empty()) {
        return Err(vec![format!("invalid module path: {module}")]);
    }

    let parsed = parse(source);
    let fatal_errors = parsed
        .errors
        .iter()
        .filter(|error| error.severity == Severity::Error)
        .map(|error| format!("{} at byte {}", error.message, error.span.start))
        .collect::<Vec<_>>();
    if !fatal_errors.is_empty() {
        return Err(fatal_errors);
    }

    Ok(parsed.program.items.iter().any(|(item, _)| {
        let Item::Import(import) = item else {
            return false;
        };
        import.file_path.is_none()
            && import
                .path
                .iter()
                .map(String::as_str)
                .eq(segments.iter().copied())
    }))
}

fn validate_pairs<F, E>(pairs: &str, mut read_source: F) -> Vec<String>
where
    F: FnMut(&Path) -> Result<String, E>,
    E: std::fmt::Display,
{
    let mut failures = Vec::new();
    for (index, row) in pairs.lines().enumerate() {
        let line = index + 1;
        let Some((module, fixture)) = row.split_once('\t') else {
            failures.push(format!("line {line}: expected module<TAB>fixture"));
            continue;
        };
        if module.is_empty() || fixture.is_empty() || fixture.contains('\t') {
            failures.push(format!(
                "line {line}: expected exactly two non-empty fields"
            ));
            continue;
        }

        let fixture_path = Path::new(fixture);
        let source = match read_source(fixture_path) {
            Ok(source) => source,
            Err(error) => {
                failures.push(format!("{fixture}: {error}"));
                continue;
            }
        };
        match source_imports_module(&source, module) {
            Ok(true) => {}
            Ok(false) => failures.push(format!("{fixture}: does not import {module}")),
            Err(errors) => failures.push(format!(
                "{fixture}: parser rejected source: {}",
                errors.join("; ")
            )),
        }
    }
    failures
}

fn run() -> Result<(), String> {
    let mut args = env::args_os();
    let program = args
        .next()
        .and_then(|arg| Path::new(&arg).file_name().map(ToOwned::to_owned))
        .unwrap_or_else(|| "stdlib_import_authority".into());
    let Some(batch) = args.next() else {
        return Err(format!("usage: {} PAIRS_TSV", program.to_string_lossy()));
    };
    if args.next().is_some() {
        return Err(format!("usage: {} PAIRS_TSV", program.to_string_lossy()));
    }

    let pairs = fs::read_to_string(&batch)
        .map_err(|error| format!("{}: {error}", Path::new(&batch).display()))?;
    let failures = validate_pairs(&pairs, |path| fs::read_to_string(path));
    if failures.is_empty() {
        Ok(())
    } else {
        Err(failures.join("\n"))
    }
}

fn main() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(error) => {
            eprintln!("{error}");
            ExitCode::FAILURE
        }
    }
}

#[cfg(test)]
mod tests {
    use super::{source_imports_module, validate_pairs};
    use hew_parser::{parse, Severity};
    use std::collections::HashMap;
    use std::io;
    use std::path::Path;

    fn assert_evidence(source: &str, module: &str) {
        assert_eq!(source_imports_module(source, module), Ok(true), "{source}");
    }

    fn assert_not_evidence(source: &str, module: &str) {
        assert_ne!(source_imports_module(source, module), Ok(true), "{source}");
    }

    fn assert_invalid(source: &str, module: &str) {
        assert!(source_imports_module(source, module).is_err(), "{source}");
    }

    #[test]
    fn accepts_every_parser_valid_import_tail() {
        for source in [
            "import std::net::dns;",
            "import /* a */ std /* b */ :: net::dns /* c */ ;",
            "import std::net::dns as resolver;",
            "import std::net::dns::*;",
            "import std::net::dns::{Resolver};",
            "import std::net::dns::{Resolver as R, Query,};",
            "import std::net::dns::{};",
            "import std::net::dns; fn main() {}",
        ] {
            assert_evidence(source, "std::net::dns");
        }
        assert_evidence(
            "import std::actor::after as event_source;",
            "std::actor::after",
        );
    }

    #[test]
    fn accepts_exact_import_when_the_parser_reports_only_a_warning() {
        let source = "import std::net::dns; fn main() { if true { 1 } else { 2 }; }";
        let parsed = parse(source);
        assert!(
            parsed
                .errors
                .iter()
                .any(|error| error.severity == Severity::Warning),
            "warning tooth stopped exercising parser warning authority: {:?}",
            parsed.errors
        );
        assert!(
            !parsed
                .errors
                .iter()
                .any(|error| error.severity == Severity::Error),
            "warning tooth unexpectedly contains a fatal diagnostic: {:?}",
            parsed.errors
        );
        assert_evidence(source, "std::net::dns");
    }

    #[test]
    fn ignores_comments_literals_and_nested_text() {
        for source in [
            "// import std::net::dns;\nfn main() {}",
            "/// import std::net::dns;\nfn main() {}",
            "//! import std::net::dns;\nfn main() {}",
            "/* outer /* import std::net::dns; */ decoy */\nfn main() {}",
            r#"fn main() { let value = "import std::net::dns;"; }"#,
            r#"fn main() { let value = r"import std::net::dns;"; }"#,
            r#"fn main() { let value = b"import std::net::dns;"; }"#,
            r#"fn main() { let value = re"import std::net::dns;"; }"#,
            r#"fn main() { let value = f"import std::net::dns;"; }"#,
            "fn main() { import std::net::dns; }",
        ] {
            assert_not_evidence(source, "std::net::dns");
        }
    }

    #[test]
    fn requires_the_exact_maximal_module_path() {
        for source in [
            "import prefix::std::net::dns;",
            "import std::net::dns_suffix;",
            "import std::net::dns::suffix;",
            "import std::net;",
            "import \"std/net/dns.hew\";",
        ] {
            assert_eq!(
                source_imports_module(source, "std::net::dns"),
                Ok(false),
                "{source}"
            );
        }
    }

    #[test]
    fn fails_closed_on_invalid_source() {
        for source in [
            "import /// doc\nstd::net::dns;",
            "import //! doc\nstd::net::dns;",
            "const bogus: i32 = (\nimport std::net::dns;\n);",
            "const bogus: [i32; 1] = [\nimport std::net::dns;\n];",
            "pub import std::net::dns;",
            "import std::net::dns::{Resolver,,};",
            "import std::net::dns::{???};",
            "import std::net::dns::{Resolver} as alias;",
            "import std::net::dns as ;",
            "import std::net::dns; trailing_tokens",
            "import std::net::dns",
            "/* unterminated import std::net::dns;",
            "fn main() { let bogus = 'abc'; }\nimport std::net::dns;",
            "fn main() { let bogus = \"unterminated; }\nimport std::net::dns;",
        ] {
            assert_invalid(source, "std::net::dns");
        }
    }

    #[test]
    fn validates_a_batch_in_one_pass() {
        let sources = HashMap::from([
            ("one.hew", "import std::one;"),
            ("two.hew", "import std::two::{Value};"),
            ("decoy.hew", "// import std::three;\nfn main() {}"),
        ]);
        let failures = validate_pairs(
            "std::one\tone.hew\nstd::two\ttwo.hew\nstd::three\tdecoy.hew\n",
            |path: &Path| {
                sources
                    .get(path.to_str().unwrap_or_default())
                    .map(ToString::to_string)
                    .ok_or_else(|| io::Error::new(io::ErrorKind::NotFound, "missing fixture"))
            },
        );
        assert_eq!(failures.len(), 1, "{failures:?}");
        assert!(failures[0].contains("decoy.hew: does not import std::three"));
    }

    #[test]
    fn rejects_malformed_batch_rows_and_missing_fixtures() {
        let failures = validate_pairs("no-tab\nstd::one\t\nstd::one\tmissing.hew\textra\n", |_| {
            Err(io::Error::new(io::ErrorKind::NotFound, "missing fixture"))
        });
        assert_eq!(failures.len(), 3, "{failures:?}");
    }
}
