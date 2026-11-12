//! Every failure kind in the generated list is a value of the crash counter's
//! `trap_kind` label in the scrape, so a query for one kind works before any
//! actor has crashed with it.

use hew_runtime::failure_kinds::{
    CRASH_LABEL_KEYS, CRASH_SERIES, GIVEUP_SERIES, TRAP_KIND_LABEL_VALUES,
};

#[test]
fn every_generated_kind_appears_in_the_crash_series_labels() {
    let scrape = hew_runtime::observe::scrape_text();
    assert_eq!(
        scrape
            .matches(&format!("# TYPE {CRASH_SERIES} counter\n"))
            .count(),
        1,
        "{scrape}"
    );
    for kind in TRAP_KIND_LABEL_VALUES {
        let label = format!(
            "{CRASH_SERIES}{{{}=\"{kind}\",{}=",
            CRASH_LABEL_KEYS[0], CRASH_LABEL_KEYS[1]
        );
        assert!(scrape.contains(&label), "missing {kind} in:\n{scrape}");
    }
    // The crash counter is only ever labelled; no bare sample shadows it.
    assert!(!scrape.contains(&format!("\n{CRASH_SERIES} ")), "{scrape}");
    assert!(scrape.contains(&format!(
        "# TYPE {GIVEUP_SERIES} counter\n{GIVEUP_SERIES} 0\n"
    )));
}
