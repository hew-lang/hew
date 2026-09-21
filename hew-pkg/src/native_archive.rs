//! Make a native Cargo crate self-contained in a published Hew package.
//!
//! Cargo workspace inheritance and local Rust dependencies are authoring
//! conveniences. An installed package must not depend on the publisher's
//! checkout. Materialize those inputs in the archive, preserving the lockfile.

use std::collections::BTreeMap;
use std::io;
use std::path::{Component, Path, PathBuf};

use toml::{Table, Value};

use crate::package_fs::{collect_package_snapshot, PackageFile};

const VENDOR: &str = "hew-native-deps";

fn invalid(message: impl Into<String>) -> io::Error {
    io::Error::new(io::ErrorKind::InvalidData, message.into())
}

fn read_manifest(path: &Path) -> io::Result<Table> {
    toml::from_str(&std::fs::read_to_string(path)?)
        .map_err(|error| invalid(format!("{}: {error}", path.display())))
}

fn encode(table: &Table) -> io::Result<Vec<u8>> {
    toml::to_string_pretty(table)
        .map(String::into_bytes)
        .map_err(|error| invalid(error.to_string()))
}

fn inherited(value: &Value) -> bool {
    value.get("workspace").and_then(Value::as_bool) == Some(true)
}

fn inherits_workspace(manifest: &Table) -> bool {
    let dependencies_inherit = |table: &Table| {
        ["dependencies", "build-dependencies", "dev-dependencies"]
            .iter()
            .filter_map(|key| table.get(*key).and_then(Value::as_table))
            .any(|dependencies| dependencies.values().any(inherited))
    };
    manifest
        .get("package")
        .and_then(Value::as_table)
        .is_some_and(|package| package.values().any(inherited))
        || manifest.get("lints").is_some_and(inherited)
        || dependencies_inherit(manifest)
        || manifest
            .get("target")
            .and_then(Value::as_table)
            .is_some_and(|targets| {
                targets
                    .values()
                    .filter_map(Value::as_table)
                    .any(dependencies_inherit)
            })
}

fn workspace(crate_dir: &Path) -> io::Result<(PathBuf, Table)> {
    // Cargo owns workspace discovery, including excluded members and explicit
    // package.workspace paths. This query does not resolve or build dependencies.
    let output = std::process::Command::new("cargo")
        .args([
            "locate-project",
            "--workspace",
            "--message-format",
            "plain",
            "--manifest-path",
        ])
        .arg(crate_dir.join("Cargo.toml"))
        .current_dir(crate_dir)
        .output()?;
    if !output.status.success() {
        return Err(invalid(format!(
            "cannot locate native Cargo workspace: {}",
            String::from_utf8_lossy(&output.stderr).trim()
        )));
    }
    let path = PathBuf::from(
        std::str::from_utf8(&output.stdout)
            .map_err(|error| invalid(error.to_string()))?
            .trim(),
    );
    let root = path
        .parent()
        .ok_or_else(|| invalid("Cargo returned a manifest without a parent directory"))?;
    Ok((root.to_owned(), read_manifest(&path)?))
}

fn effective_resolver(workspace: &Table) -> Value {
    if let Some(resolver) = workspace
        .get("workspace")
        .and_then(|value| value.get("resolver"))
    {
        return resolver.clone();
    }
    let Some(root_package) = workspace.get("package") else {
        // Virtual workspaces have no edition to supply a resolver default.
        return Value::String("1".to_owned());
    };
    if let Some(resolver) = root_package.get("resolver") {
        return resolver.clone();
    }
    let mut edition = root_package.get("edition");
    if edition.is_some_and(inherited) {
        edition = workspace
            .get("workspace")
            .and_then(|value| value.get("package"))
            .and_then(|value| value.get("edition"));
    }
    Value::String(
        match edition.and_then(Value::as_str) {
            Some("2024") => "3",
            Some("2021") => "2",
            _ => "1",
        }
        .to_owned(),
    )
}

fn relative(from: &Path, to: &Path) -> String {
    let from = from
        .components()
        .filter(|part| *part != Component::CurDir)
        .collect::<Vec<_>>();
    let to = to
        .components()
        .filter(|part| *part != Component::CurDir)
        .collect::<Vec<_>>();
    let shared = from.iter().zip(&to).take_while(|(a, b)| a == b).count();
    let mut parts = vec!["..".to_owned(); from.len() - shared];
    parts.extend(
        to[shared..]
            .iter()
            .map(|part| part.as_os_str().to_string_lossy().into_owned()),
    );
    if parts.is_empty() {
        ".".to_owned()
    } else {
        parts.join("/")
    }
}

struct Bundler<'a> {
    files: &'a mut Vec<PackageFile>,
    crates: BTreeMap<PathBuf, PathBuf>,
    native_root: PathBuf,
}

impl Bundler<'_> {
    fn replace_file(&mut self, path: &Path, contents: Vec<u8>) -> io::Result<()> {
        let path = path
            .to_str()
            .ok_or_else(|| invalid("native package path is not UTF-8"))?
            .replace('\\', "/");
        if let Some(file) = self.files.iter_mut().find(|file| file.path == path) {
            file.contents = contents;
        } else {
            self.files.push(PackageFile { path, contents });
        }
        Ok(())
    }

    fn dependency(&mut self, source: &Path) -> io::Result<PathBuf> {
        let source = source.canonicalize()?;
        if let Some(destination) = self.crates.get(&source) {
            return Ok(destination.clone());
        }
        let destination = PathBuf::from(VENDOR).join(format!("crate-{}", self.crates.len()));
        self.crates.insert(source.clone(), destination.clone());
        for mut file in collect_package_snapshot(&source)? {
            file.path = format!(
                "{}/{path}",
                destination.to_string_lossy().replace('\\', "/"),
                path = file.path
            );
            self.files.push(file);
        }
        self.crate_manifest(&source, &destination, false)?;
        Ok(destination)
    }

    fn dependencies(
        &mut self,
        table: &mut Table,
        source: &Path,
        destination: &Path,
        workspace_root: &Path,
        workspace_deps: &Table,
    ) -> io::Result<()> {
        for (name, dependency) in table.iter_mut() {
            let mut base = source;
            if inherited(dependency) {
                let local = dependency
                    .as_table()
                    .expect("workspace dependency is a table")
                    .clone();
                *dependency = workspace_deps
                    .get(name)
                    .ok_or_else(|| invalid(format!("missing workspace dependency {name}")))?
                    .clone();
                if dependency.is_str() {
                    *dependency = Value::Table(Table::from_iter([(
                        "version".to_owned(),
                        dependency.clone(),
                    )]));
                }
                let resolved = dependency
                    .as_table_mut()
                    .ok_or_else(|| invalid("invalid workspace dependency"))?;
                for (key, value) in local {
                    if key == "workspace" {
                        continue;
                    }
                    if key == "features" {
                        let features = resolved
                            .entry(key)
                            .or_insert_with(|| Value::Array(Vec::new()))
                            .as_array_mut()
                            .ok_or_else(|| invalid("dependency features must be an array"))?;
                        for feature in value
                            .as_array()
                            .ok_or_else(|| invalid("dependency features must be an array"))?
                        {
                            if !features.contains(feature) {
                                features.push(feature.clone());
                            }
                        }
                    } else if key == "default-features"
                        && value.as_bool() == Some(false)
                        && resolved.get(&key).and_then(Value::as_bool) != Some(false)
                    {
                        // An inherited dependency cannot disable defaults that
                        // its workspace enables (Cargo's pre-2024 behaviour).
                    } else if matches!(key.as_str(), "optional" | "default-features") {
                        resolved.insert(key, value);
                    } else {
                        return Err(invalid(format!(
                            "unsupported workspace dependency override {name}.{key}"
                        )));
                    }
                }
                base = workspace_root;
            }
            if let Some(spec) = dependency.as_table_mut() {
                if let Some(path) = spec.get("path").and_then(Value::as_str) {
                    let bundled = self.dependency(&base.join(path))?;
                    spec.insert(
                        "path".to_owned(),
                        Value::String(relative(destination, &bundled)),
                    );
                }
            }
        }
        Ok(())
    }

    fn crate_manifest(&mut self, source: &Path, destination: &Path, root: bool) -> io::Result<()> {
        let mut manifest = read_manifest(&source.join("Cargo.toml"))?;
        if !manifest.contains_key("package") {
            return Err(invalid(
                "native crate must point to a Cargo package, not a virtual workspace",
            ));
        }
        // A patched dependency can live beneath a workspace without being a
        // member. Cargo accepts it as a dependency, but locate-project rejects
        // it as a workspace entry point. Only resolve its authoring workspace
        // when it actually inherits settings; the archive root owns resolver,
        // profiles and patches for the installed dependency graph.
        let (workspace_root, workspace_manifest) = if root || inherits_workspace(&manifest) {
            workspace(source)?
        } else {
            (source.to_owned(), manifest.clone())
        };
        let empty = Table::new();
        let workspace = workspace_manifest
            .get("workspace")
            .and_then(Value::as_table)
            .unwrap_or(&empty);
        let package_defaults = workspace
            .get("package")
            .and_then(Value::as_table)
            .unwrap_or(&empty);
        if let Some(package) = manifest.get_mut("package").and_then(Value::as_table_mut) {
            package.remove("workspace");
            for (key, value) in package.iter_mut() {
                if !inherited(value) {
                    continue;
                }
                *value = package_defaults
                    .get(key)
                    .ok_or_else(|| invalid(format!("missing workspace package.{key}")))?
                    .clone();
                if matches!(key.as_str(), "license-file" | "readme") {
                    if let Some(path) = value.as_str() {
                        let contents = std::fs::read(workspace_root.join(path))?;
                        let target = PathBuf::from(VENDOR)
                            .join(format!("workspace-{}-{key}", self.crates.len()));
                        self.replace_file(&target, contents)?;
                        *value = Value::String(relative(destination, &target));
                    }
                }
            }
        }
        if manifest.get("lints").is_some_and(inherited) {
            let lints = workspace
                .get("lints")
                .ok_or_else(|| invalid("missing workspace lints"))?
                .clone();
            manifest.insert("lints".to_owned(), lints);
        }
        let workspace_deps = workspace
            .get("dependencies")
            .and_then(Value::as_table)
            .unwrap_or(&empty);
        for key in ["dependencies", "build-dependencies", "dev-dependencies"] {
            if let Some(deps) = manifest.get_mut(key).and_then(Value::as_table_mut) {
                self.dependencies(deps, source, destination, &workspace_root, workspace_deps)?;
            }
        }
        if let Some(targets) = manifest.get_mut("target").and_then(Value::as_table_mut) {
            for target in targets
                .iter_mut()
                .filter_map(|(_, value)| value.as_table_mut())
            {
                for key in ["dependencies", "build-dependencies", "dev-dependencies"] {
                    if let Some(deps) = target.get_mut(key).and_then(Value::as_table_mut) {
                        self.dependencies(
                            deps,
                            source,
                            destination,
                            &workspace_root,
                            workspace_deps,
                        )?;
                    }
                }
            }
        }
        manifest.remove("workspace");
        if root {
            self.root_workspace(
                &mut manifest,
                destination,
                &workspace_root,
                &workspace_manifest,
            )?;
        }
        self.replace_file(&destination.join("Cargo.toml"), encode(&manifest)?)
    }
    fn root_workspace(
        &mut self,
        manifest: &mut Table,
        destination: &Path,
        workspace_root: &Path,
        workspace_manifest: &Table,
    ) -> io::Result<()> {
        let empty = Table::new();
        // Keep this crate independent of any Cargo workspace surrounding the
        // consumer's Hew package cache. Bundled path dependencies join it.
        let resolver = effective_resolver(workspace_manifest);
        if let Some(package) = manifest.get_mut("package").and_then(Value::as_table_mut) {
            package.remove("resolver");
        }
        let isolated = Table::from_iter([("resolver".to_owned(), resolver)]);
        manifest.insert("workspace".to_owned(), Value::Table(isolated));
        for key in ["profile", "patch", "replace"] {
            if let Some(value) = workspace_manifest.get(key) {
                manifest.insert(key.to_owned(), value.clone());
            }
        }
        if let Some(patches) = manifest.get_mut("patch").and_then(Value::as_table_mut) {
            for patch in patches
                .iter_mut()
                .filter_map(|(_, value)| value.as_table_mut())
            {
                self.dependencies(patch, workspace_root, destination, workspace_root, &empty)?;
            }
        }
        if let Some(replacements) = manifest.get_mut("replace").and_then(Value::as_table_mut) {
            self.dependencies(
                replacements,
                workspace_root,
                destination,
                workspace_root,
                &empty,
            )?;
        }
        let lock = workspace_root.join("Cargo.lock");
        if lock.is_file() {
            self.replace_file(&self.native_root.join("Cargo.lock"), std::fs::read(lock)?)?;
        }
        Ok(())
    }
}

pub(crate) fn prepare(dir: &Path, files: &mut Vec<PackageFile>) -> io::Result<()> {
    let Some(hew) = files.iter().find(|file| file.path == "hew.toml") else {
        return Ok(());
    };
    let manifest: Table = toml::from_str(
        std::str::from_utf8(&hew.contents).map_err(|error| invalid(error.to_string()))?,
    )
    .map_err(|error| invalid(error.to_string()))?;
    let Some(native) = manifest.get("native") else {
        return Ok(());
    };
    let path = native.get("crate").and_then(Value::as_str).unwrap_or(".");
    let native_root = PathBuf::from(path)
        .components()
        .filter(|part| *part != Component::CurDir)
        .collect::<PathBuf>();
    if native_root.is_absolute()
        || native_root
            .components()
            .any(|part| matches!(part, Component::ParentDir | Component::Prefix(_)))
    {
        return Err(invalid("native crate must be inside the Hew package"));
    }
    let cargo_path = native_root
        .join("Cargo.toml")
        .to_string_lossy()
        .replace('\\', "/");
    if !files.iter().any(|file| file.path == cargo_path) {
        return Err(invalid("native Cargo.toml was excluded from the package"));
    }
    if files
        .iter()
        .any(|file| file.path == VENDOR || file.path.starts_with(&format!("{VENDOR}/")))
    {
        return Err(invalid(format!(
            "{VENDOR} is reserved for bundled native dependencies"
        )));
    }
    let source = dir.join(&native_root).canonicalize()?;
    let mut bundler = Bundler {
        files,
        crates: BTreeMap::from([(source.clone(), native_root.clone())]),
        native_root: native_root.clone(),
    };
    bundler.crate_manifest(&source, &native_root, true)?;
    bundler
        .files
        .sort_by(|left, right| left.path.cmp(&right.path));
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    fn write(root: &Path, path: &str, text: &str) {
        let path = root.join(path);
        std::fs::create_dir_all(path.parent().unwrap()).unwrap();
        std::fs::write(path, text).unwrap();
    }

    fn native_workspace() -> tempfile::TempDir {
        let source = tempfile::tempdir().unwrap();
        write(
            source.path(),
            "Cargo.toml",
            r#"
[workspace]
members = ["client", "sql"]
resolver = "2"
[workspace.package]
version = "1.2.3"
edition = "2021"
license = "MIT"
readme = "README.md"
[workspace.dependencies]
sql = { path = "sql", features = ["shared"] }
[workspace.lints.rust]
unsafe_op_in_unsafe_fn = "deny"
[profile.release]
panic = "abort"
[patch.crates-io]
archive-helper = { path = "vendor/helper" }
"#,
        );
        write(source.path(), "README.md", "Shared SQL client\n");
        write(
            source.path(),
            "client/hew.toml",
            r#"
[package]
name = "test.client"
version = "1.0.0"
[native]
crate = "."
lib = "client"
kind = "staticlib"
"#,
        );
        write(
            source.path(),
            "client/Cargo.toml",
            r#"
[package]
name = "client"
version.workspace = true
edition.workspace = true
license.workspace = true
readme.workspace = true
[lib]
crate-type = ["staticlib"]
[dependencies]
sql = { workspace = true, features = ["local"] }
archive-helper = "1.0.0"
[lints]
workspace = true
"#,
        );
        write(
            source.path(),
            "client/src/lib.rs",
            "pub use sql::Param;\npub use archive_helper::Value;\n",
        );
        write(
            source.path(),
            "vendor/helper/Cargo.toml",
            "[package]\nname = 'archive-helper'\nversion = '1.0.0'\nedition = '2021'\n",
        );
        write(
            source.path(),
            "vendor/helper/src/lib.rs",
            "pub struct Value;\n",
        );
        write(
            source.path(),
            "sql/Cargo.toml",
            r#"
[package]
name = "sql"
version.workspace = true
edition.workspace = true
[features]
shared = []
local = []
"#,
        );
        write(
            source.path(),
            "sql/src/lib.rs",
            "#[cfg(all(feature = \"shared\", feature = \"local\"))]\npub enum Param { Null }\n",
        );
        source
    }

    #[test]
    fn installed_native_archive_resolves_without_the_authoring_workspace() {
        let source = native_workspace();
        let packed = crate::tarball::pack(&source.path().join("client"), &[], &[]).unwrap();
        assert_eq!(
            packed.data,
            crate::tarball::pack(&source.path().join("client"), &[], &[])
                .unwrap()
                .data
        );
        let installed = tempfile::tempdir().unwrap();
        crate::tarball::unpack(&packed.data, installed.path()).unwrap();
        source.close().unwrap();
        let output = hew_testutil::cargo_build_isolated(
            &installed.path().join("Cargo.toml"),
            &installed.path().join("target"),
            &["--offline"],
        )
        .unwrap();
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );
        let manifest = read_manifest(&installed.path().join("Cargo.toml")).unwrap();
        let readme = manifest["package"]["readme"].as_str().unwrap();
        assert_eq!(
            std::fs::read_to_string(installed.path().join(readme)).unwrap(),
            "Shared SQL client\n"
        );
        assert_eq!(
            manifest["lints"]["rust"]["unsafe_op_in_unsafe_fn"].as_str(),
            Some("deny")
        );
        assert_eq!(
            manifest["profile"]["release"]["panic"].as_str(),
            Some("abort")
        );
    }

    #[test]
    fn virtual_workspace_default_preserves_build_dependency_features() {
        let source = native_workspace();
        let root_path = source.path().join("Cargo.toml");
        let mut root = read_manifest(&root_path).unwrap();
        root["workspace"].as_table_mut().unwrap().remove("resolver");
        std::fs::write(root_path, encode(&root).unwrap()).unwrap();
        let client_path = source.path().join("client/Cargo.toml");
        let mut client = std::fs::read_to_string(&client_path).unwrap();
        client.push_str(
            "\n[build-dependencies]\nsql = { workspace = true, features = [\"build-only\"] }\n",
        );
        std::fs::write(client_path, client).unwrap();
        write(source.path(), "client/build.rs", "fn main() {}\n");
        let sql_path = source.path().join("sql/Cargo.toml");
        let sql = std::fs::read_to_string(&sql_path).unwrap() + "build-only = []\n";
        std::fs::write(sql_path, sql).unwrap();
        write(
            source.path(),
            "sql/src/lib.rs",
            "#[cfg(feature = \"build-only\")]\npub enum Param { Null }\n",
        );
        let packed = crate::tarball::pack(&source.path().join("client"), &[], &[]).unwrap();
        let installed = tempfile::tempdir().unwrap();
        crate::tarball::unpack(&packed.data, installed.path()).unwrap();
        source.close().unwrap();
        let manifest_path = installed.path().join("Cargo.toml");
        let target = installed.path().join("target");
        let output =
            hew_testutil::cargo_build_isolated(&manifest_path, &target, &["--offline"]).unwrap();
        assert!(
            output.status.success(),
            "{}",
            String::from_utf8_lossy(&output.stderr)
        );

        // Resolver 2 intentionally separates the build dependency's features:
        // the same client then fails because its ordinary dependency lacks Param.
        let mut manifest = read_manifest(&manifest_path).unwrap();
        manifest["workspace"]["resolver"] = Value::String("2".to_owned());
        std::fs::write(&manifest_path, encode(&manifest).unwrap()).unwrap();
        let counterfactual =
            hew_testutil::cargo_build_isolated(&manifest_path, &target, &["--offline"]).unwrap();
        assert!(!counterfactual.status.success());
        assert!(String::from_utf8_lossy(&counterfactual.stderr).contains("sql::Param"));
    }

    #[test]
    fn native_archive_does_not_bypass_excluded_inputs() {
        let source = tempfile::tempdir().unwrap();
        write(source.path(), "hew.toml", "[native]\ncrate = '.'\n");
        write(
            source.path(),
            "Cargo.toml",
            "[package]\nname='client'\nversion='1.0.0'\n",
        );
        let error =
            crate::tarball::pack(source.path(), &["Cargo.toml".to_owned()], &[]).unwrap_err();
        assert!(error.to_string().contains("excluded"));
    }
}
