use std::{
    fmt::Write as _,
    fs,
    path::{Path, PathBuf},
    process::Command,
};

fn fixtures(root: &Path) -> Vec<PathBuf> {
    let corpus = root.join("examples/v05/checked-mir");
    let mut fixtures = fs::read_dir(&corpus)
        .unwrap_or_else(|error| panic!("read {}: {error}", corpus.display()))
        .map(|entry| entry.expect("read checked-MIR fixture entry").path())
        .filter(|path| path.extension().is_some_and(|extension| extension == "hew"))
        .collect::<Vec<_>>();
    fixtures.sort();
    assert!(!fixtures.is_empty(), "checked-MIR corpus must not be empty");
    fixtures
}

pub(crate) fn checked_mir_corpus(root: &Path, hew: &Path) {
    let mut snapshot = String::new();
    for fixture in fixtures(root) {
        let name = fixture
            .file_stem()
            .expect("checked-MIR fixture must have a stem")
            .to_string_lossy();
        for stage in ["raw", "elab"] {
            let output = Command::new(hew)
                .current_dir(root)
                .args(["compile", "--dump-mir", stage])
                .arg(&fixture)
                .output()
                .unwrap_or_else(|error| panic!("dump {name} ({stage}): {error}"));
            assert!(
                output.status.success(),
                "dump {name} ({stage}) failed with {}:\n{}",
                output.status,
                String::from_utf8_lossy(&output.stderr)
            );
            let output = String::from_utf8(output.stdout)
                .unwrap_or_else(|error| panic!("dump {name} ({stage}) was not UTF-8: {error}"));
            writeln!(snapshot, "===== {name}.{stage}.mir =====")
                .expect("writing to a String cannot fail");
            snapshot.push_str(&output);
            if !output.ends_with('\n') {
                snapshot.push('\n');
            }
        }
    }

    insta::assert_snapshot!("checked_mir_corpus", snapshot);
}
