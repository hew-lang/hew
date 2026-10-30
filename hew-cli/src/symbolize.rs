//! `hew symbolize`: name the Hew function and source position of addresses
//! in a built binary, from its own debug information or the split debug file,
//! `.dSYM` bundle or `.pdb` beside it.

use std::fmt::Write as _;
use std::path::PathBuf;

use clap::Args;
use hew_symbolize::{BuildId, Frame, Location, Symbolizer};

/// Name the function and source line of addresses in a built binary.
#[derive(Debug, Args)]
pub struct SymbolizeArgs {
    /// The executable, split `.debug` file, `.dSYM` bundle or `.pdb`.
    pub binary: PathBuf,
    /// Addresses to resolve, as 0x-prefixed hex or decimal.
    #[arg(required = true, value_parser = parse_address)]
    pub addresses: Vec<u64>,
    /// The image's run-time base: the load bias on ELF, the `mach_header`
    /// address on Mach-O, the module handle on Windows. Without it the
    /// addresses are offsets from the image's start.
    #[arg(long, value_name = "ADDRESS", value_parser = parse_address, default_value = "0")]
    pub base: u64,
    /// Refuse the binary unless its build identity is this hex string.
    #[arg(long = "build-id", value_name = "HEX", value_parser = parse_build_id)]
    pub build_id: Option<BuildId>,
    /// The addresses are return addresses from a backtrace, so the call that
    /// made each frame is the instruction before it.
    #[arg(long = "return-addresses")]
    pub return_addresses: bool,
}

fn parse_address(value: &str) -> Result<u64, String> {
    let parsed = match value
        .strip_prefix("0x")
        .or_else(|| value.strip_prefix("0X"))
    {
        Some(hex) => u64::from_str_radix(hex, 16),
        None => value.parse(),
    };
    parsed.map_err(|_| format!("`{value}` is not a decimal or 0x-prefixed hex address"))
}

fn parse_build_id(value: &str) -> Result<BuildId, String> {
    BuildId::from_hex(value).ok_or_else(|| format!("`{value}` is not a hex build id"))
}

/// Resolve every address and print one line per location, innermost inlined
/// call first. An address no symbols cover prints `??`.
pub(crate) fn run(args: &SymbolizeArgs) -> Result<String, hew_symbolize::Error> {
    let symbolizer = Symbolizer::open(&args.binary)?;
    let mut out = String::new();
    for &pc in &args.addresses {
        let frame = Frame {
            pc,
            image_base: args.base,
            build_id: args.build_id.clone(),
            return_address: args.return_addresses,
        };
        let locations = symbolizer.symbolize(&frame)?;
        if locations.is_empty() {
            let _ = writeln!(out, "{pc:#x} ??");
        }
        for (depth, location) in locations.iter().enumerate() {
            let lead = if depth == 0 {
                format!("{pc:#x}")
            } else {
                " ".repeat(format!("{pc:#x}").len())
            };
            let inlined = if depth + 1 < locations.len() {
                " (inlined)"
            } else {
                ""
            };
            let _ = writeln!(out, "{lead} {}{inlined}", describe(location));
        }
    }
    Ok(out)
}

/// `function at file:line:col`, leaving out whatever the symbols lack.
fn describe(location: &Location) -> String {
    let mut text = location.function.clone().unwrap_or_else(|| "??".into());
    if let Some(file) = &location.file {
        let _ = write!(text, " at {file}");
        if let Some(line) = location.line {
            let _ = write!(text, ":{line}");
            if let Some(column) = location.column {
                let _ = write!(text, ":{column}");
            }
        }
    }
    text
}

#[cfg(test)]
mod tests {
    use super::{describe, parse_address};
    use hew_symbolize::Location;

    #[test]
    fn addresses_parse_as_hex_or_decimal() {
        assert_eq!(parse_address("0x1f"), Ok(31));
        assert_eq!(parse_address("31"), Ok(31));
        assert!(parse_address("0xzz").is_err());
    }

    #[test]
    fn locations_print_what_the_symbols_know() {
        let full = Location {
            function: Some("level3".into()),
            file: Some("fail.hew".into()),
            line: Some(4),
            column: Some(15),
        };
        assert_eq!(describe(&full), "level3 at fail.hew:4:15");
        let name_only = Location {
            function: Some("hew_fault_raise".into()),
            ..Location::default()
        };
        assert_eq!(describe(&name_only), "hew_fault_raise");
    }
}
