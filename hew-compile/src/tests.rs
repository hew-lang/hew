use std::fs::{self, File};
use std::io::Write;
use std::path::Path;

mod actors_and_check_program;
mod manifest_and_resolution;
mod semantic_imports;
mod source_analysis_batch;
mod source_and_lowering;
mod stdlib_and_directory_modules;

pub(super) fn write_toml(dir: &Path, content: &str) {
    let mut file = File::create(dir.join("hew.toml")).expect("create hew.toml");
    file.write_all(content.as_bytes()).expect("write hew.toml");
}

pub(super) fn write_lockfile(dir: &Path, content: &str) {
    let mut file = File::create(dir.join("hew.lock")).expect("create hew.lock");
    file.write_all(content.as_bytes()).expect("write hew.lock");
}

/// Make `dir` a package, the anchor directory modules need (spec 3.5.1).
pub(super) fn write_package_manifest(dir: &Path) {
    fs::write(dir.join("hew.toml"), "[package]\nname = \"app\"\n").expect("write manifest");
}

pub(super) fn write_source(dir: &Path, name: &str, content: &str) -> String {
    let path = dir.join(name);
    let mut file = File::create(&path).expect("create source file");
    file.write_all(content.as_bytes())
        .expect("write source file");
    path.display().to_string()
}
