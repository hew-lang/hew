use std::borrow::Cow;
use std::path::{Path, PathBuf};
use std::str::FromStr;

use tower_lsp_server::ls_types::Uri;

/// Converts between LSP URIs and filesystem paths without interpolating paths
/// into URI strings. Conversion remains fallible for paths or URIs that cannot
/// be represented on the current platform.
pub(super) trait FileUriExt: Sized {
    fn from_checked_file_path(path: impl AsRef<Path>) -> Option<Self>;

    fn to_checked_file_path(&self) -> Option<Cow<'_, Path>>;
}

impl FileUriExt for Uri {
    fn from_checked_file_path(path: impl AsRef<Path>) -> Option<Self> {
        let file_url = url::Url::from_file_path(path).ok()?;
        Self::from_str(file_url.as_str()).ok()
    }

    fn to_checked_file_path(&self) -> Option<Cow<'_, Path>> {
        let file_url = url::Url::parse(self.as_str()).ok()?;
        if file_url.scheme() != "file" {
            return None;
        }
        file_url.to_file_path().ok().map(Cow::Owned)
    }
}

/// Resolve existing parents even when an editor has opened a new unsaved file.
/// Appending an unresolved leaf to a lexical symlink path would misstate its
/// workspace ownership. Resolve components before interpreting a later `..`.
pub(super) fn resolved_physical_path(path: &Path) -> std::io::Result<PathBuf> {
    use std::path::Component;
    if let Ok(path) = std::fs::canonicalize(path) {
        return Ok(path);
    }
    let absolute = if path.is_absolute() {
        path.to_path_buf()
    } else {
        std::env::current_dir()?.join(path)
    };
    let mut resolved = PathBuf::new();
    for component in absolute.components() {
        match component {
            Component::CurDir => {}
            Component::ParentDir => {
                resolved.pop();
            }
            Component::Prefix(_) | Component::RootDir => resolved.push(component.as_os_str()),
            Component::Normal(_) => {
                resolved.push(component.as_os_str());
                match std::fs::canonicalize(&resolved) {
                    Ok(path) => resolved = path,
                    Err(error) if error.kind() == std::io::ErrorKind::NotFound => {
                        if std::fs::symlink_metadata(&resolved)
                            .is_ok_and(|metadata| metadata.file_type().is_symlink())
                        {
                            return Err(error);
                        }
                    }
                    Err(error) => return Err(error),
                }
            }
        }
    }
    Ok(resolved)
}

/// Use a canonical file URI for dependency keys, retaining non-file URIs and
/// paths that do not yet exist so unsaved documents can still be indexed.
pub(super) fn source_file_key(uri: &Uri) -> Uri {
    uri.to_checked_file_path()
        .and_then(|path| std::fs::canonicalize(path).ok())
        .and_then(Uri::from_checked_file_path)
        .unwrap_or_else(|| uri.clone())
}

/// Compare editor URIs using the frontend's source-file identity rule.
pub(super) fn same_source_file(left: &Uri, right: &Uri) -> bool {
    left == right
        || left
            .to_checked_file_path()
            .zip(right.to_checked_file_path())
            .is_some_and(|(left, right)| hew_compile::paths_name_same_file(&left, &right))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn absolute_paths_round_trip_with_uri_encoding() {
        let plain_path = std::env::temp_dir().join("hew-lsp-uri.hew");
        let encoded_path = std::env::temp_dir().join("naïve Hew source.hew");

        for path in [&plain_path, &encoded_path] {
            let uri =
                Uri::from_checked_file_path(path).expect("absolute path should become a file URI");
            assert_eq!(uri.to_checked_file_path().as_deref(), Some(path.as_path()));
        }

        let encoded_uri =
            Uri::from_checked_file_path(&encoded_path).expect("absolute path should convert");
        assert!(encoded_uri.as_str().contains("%20"));
        assert!(Uri::from_checked_file_path(PathBuf::from("relative.hew")).is_none());
    }

    #[cfg(windows)]
    #[test]
    fn windows_drive_and_unc_paths_round_trip() {
        for path in [
            PathBuf::from(r"C:\Hew source\naïve.hew"),
            PathBuf::from(r"\\server\Hew share\naïve.hew"),
        ] {
            let uri =
                Uri::from_checked_file_path(&path).expect("absolute Windows path should convert");
            assert_eq!(uri.to_checked_file_path().as_deref(), Some(path.as_path()));
        }
    }
}
