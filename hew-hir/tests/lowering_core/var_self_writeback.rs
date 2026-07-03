use crate::support;

use hew_hir::dump_hir;

#[test]
fn concrete_var_self_next_lowers_to_writeback_call() {
    let src = r"
trait Stepper {
    type Item;
    fn next(var self) -> Option<Self::Item>;
}

type Countdown { n: i64; }

impl Stepper for Countdown {
    type Item = i64;

    fn next(var self) -> Option<i64> {
        if self.n <= 0 {
            None
        } else {
            let cur = self.n;
            self.n = self.n - 1;
            Some(cur)
        }
    }
}

fn main() -> Option<i64> {
    var cd = Countdown { n: 1 };
    cd.next()
}
";

    let output = support::checker_pipeline::lower_through_checker(src);
    assert!(
        output.diagnostics.is_empty(),
        "unexpected HIR diagnostics: {:?}",
        output.diagnostics
    );
    let dump = dump_hir(&output.module);
    assert!(
        dump.contains("var-self-call Countdown::next"),
        "concrete next() must lower through the write-back node; dump:\n{dump}"
    );
    assert!(
        !dump.contains("resolved-impl-call"),
        "user impl next() must not route through ResolvedImplCall; dump:\n{dump}"
    );
}
