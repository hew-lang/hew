use std::cell::RefCell;
use std::path::{Path, PathBuf};

/// Presentation prefixes. Source and declaration identities retain their real paths.
#[derive(Debug, Clone, Default)]
pub struct SourcePathPolicy {
    prefixes: Vec<(PathBuf, PathBuf)>,
}

impl SourcePathPolicy {
    pub fn insert(&mut self, source: &Path, display: &Path) {
        // Missing inputs cannot be canonicalized, so diagnostics can retain the
        // invocation spelling even when its parent resolves through a symlink.
        for source in [lexical_absolute_path(source), absolute_path(source)] {
            self.prefixes.retain(|(existing, _)| *existing != source);
            self.prefixes.push((source, display.to_path_buf()));
        }
        self.prefixes
            .sort_by_key(|(source, _)| std::cmp::Reverse(source.components().count()));
    }

    #[must_use]
    pub fn display(&self, path: &Path) -> PathBuf {
        if self.prefixes.is_empty() || path.as_os_str().is_empty() {
            return path.to_path_buf();
        }
        let absolute = absolute_path(path);
        self.prefixes
            .iter()
            .find_map(|(source, display)| {
                absolute
                    .strip_prefix(source)
                    .ok()
                    .map(|relative| display.join(relative))
            })
            .unwrap_or_else(|| path.to_path_buf())
    }

    #[must_use]
    pub fn prefixes(&self) -> impl DoubleEndedIterator<Item = (&Path, &Path)> {
        self.prefixes
            .iter()
            .map(|(source, display)| (source.as_path(), display.as_path()))
    }

    /// Render path-bearing messages without changing their attached source text.
    #[must_use]
    pub fn display_text(&self, text: &str) -> String {
        let mut text = text.to_string();
        for (source, display) in &self.prefixes {
            let source = source.to_string_lossy();
            let mut rendered = String::new();
            let mut end = 0;
            for (start, matched) in text.match_indices(source.as_ref()) {
                let next = start + matched.len();
                if text[next..].chars().next().is_some_and(|character| {
                    character.is_alphanumeric() || matches!(character, '_' | '-' | '.')
                }) {
                    continue;
                }
                rendered.push_str(&text[end..start]);
                rendered.push_str(&display.to_string_lossy());
                end = next;
            }
            rendered.push_str(&text[end..]);
            text = rendered;
        }
        text
    }

    /// Install presentation rules for this command, restoring its caller on return.
    #[must_use]
    pub fn enter(self) -> SourcePathScope {
        SourcePathScope {
            previous: ACTIVE.with(|active| active.replace(self)),
            thread: std::marker::PhantomData,
        }
    }
}

fn absolute_path(path: &Path) -> PathBuf {
    path.canonicalize().map_or_else(
        |_| lexical_absolute_path(path),
        |absolute| PathBuf::from(super::display_path(&absolute)),
    )
}

fn lexical_absolute_path(path: &Path) -> PathBuf {
    let absolute =
        std::env::current_dir().map_or_else(|_| path.to_path_buf(), |cwd| cwd.join(path));
    PathBuf::from(super::display_path(&absolute))
}

thread_local! {
    static ACTIVE: RefCell<SourcePathPolicy> = RefCell::new(SourcePathPolicy::default());
}

#[derive(Debug)]
pub struct SourcePathScope {
    previous: SourcePathPolicy,
    thread: std::marker::PhantomData<std::rc::Rc<()>>,
}

impl Drop for SourcePathScope {
    fn drop(&mut self) {
        ACTIVE.with(|active| *active.borrow_mut() = std::mem::take(&mut self.previous));
    }
}

#[must_use]
pub fn source_path_policy() -> SourcePathPolicy {
    ACTIVE.with(|active| active.borrow().clone())
}

/// Add a resolved package's presentation prefix to the current command.
pub fn register_source_path(source: &Path, display: &Path) {
    ACTIVE.with(|active| active.borrow_mut().insert(source, display));
}

#[must_use]
pub fn display_source_path(path: &Path) -> PathBuf {
    ACTIVE.with(|active| active.borrow().display(path))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn nested_prefixes_preserve_unrelated_paths_and_message_boundaries() {
        let dir = tempfile::tempdir().unwrap();
        let root = dir.path().canonicalize().unwrap();
        let nested = root.join("dependency");
        let mut policy = SourcePathPolicy::default();
        policy.insert(&root, Path::new("."));
        policy.insert(&nested, Path::new("packages/dependency"));
        assert_eq!(
            policy.display(&nested.join("missing.hew")),
            Path::new("packages/dependency/missing.hew")
        );
        let unrelated = root.with_file_name("unrelated").join("main.hew");
        assert_eq!(policy.display(&unrelated), unrelated);
        let similar = format!("{}-other/main.hew", root.display());
        assert_eq!(policy.display_text(&similar), similar);
    }

    #[cfg(unix)]
    #[test]
    fn symlink_prefix_trims_missing_paths_and_messages_in_both_spellings() {
        let dir = tempfile::tempdir().unwrap();
        let real = dir.path().join("real");
        let alias = dir.path().join("alias");
        std::fs::create_dir(&real).unwrap();
        std::os::unix::fs::symlink(&real, &alias).unwrap();
        let mut policy = SourcePathPolicy::default();
        policy.insert(&alias, Path::new("."));
        for root in [&alias, &real.canonicalize().unwrap()] {
            let missing = root.join("missing.hew");
            assert_eq!(policy.display(&missing), Path::new("./missing.hew"));
            assert_eq!(
                policy.display_text(&format!("cannot read {}: missing", missing.display())),
                "cannot read ./missing.hew: missing"
            );
        }
    }
}
