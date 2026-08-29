use std::borrow::Cow;
use std::path::Path;
use std::str::FromStr;

use tower_lsp_server::lsp_types::Uri;

/// Converts between LSP URIs and filesystem paths without interpolating paths
/// into URI strings. Conversion remains fallible for paths or URIs that cannot
/// be represented on the current platform.
pub(super) trait FileUriExt: Sized {
    fn from_file_path(path: impl AsRef<Path>) -> Option<Self>;

    fn to_file_path(&self) -> Option<Cow<'_, Path>>;
}

impl FileUriExt for Uri {
    fn from_file_path(path: impl AsRef<Path>) -> Option<Self> {
        let file_url = url::Url::from_file_path(path).ok()?;
        Self::from_str(file_url.as_str()).ok()
    }

    fn to_file_path(&self) -> Option<Cow<'_, Path>> {
        let file_url = url::Url::parse(self.as_str()).ok()?;
        if file_url.scheme() != "file" {
            return None;
        }
        file_url.to_file_path().ok().map(Cow::Owned)
    }
}

#[cfg(test)]
mod tests {
    use std::path::PathBuf;

    use super::*;

    #[test]
    fn absolute_paths_round_trip_with_uri_encoding() {
        let plain_path = std::env::temp_dir().join("hew-lsp-uri.hew");
        let encoded_path = std::env::temp_dir().join("naïve Hew source.hew");

        for path in [&plain_path, &encoded_path] {
            let uri = Uri::from_file_path(path).expect("absolute path should become a file URI");
            assert_eq!(uri.to_file_path().as_deref(), Some(path.as_path()));
        }

        let encoded_uri = Uri::from_file_path(&encoded_path).expect("absolute path should convert");
        assert!(encoded_uri.as_str().contains("%20"));
        assert!(Uri::from_file_path(PathBuf::from("relative.hew")).is_none());
    }

    #[cfg(windows)]
    #[test]
    fn windows_drive_and_unc_paths_round_trip() {
        for path in [
            PathBuf::from(r"C:\Hew source\naïve.hew"),
            PathBuf::from(r"\\server\Hew share\naïve.hew"),
        ] {
            let uri = Uri::from_file_path(&path).expect("absolute Windows path should convert");
            assert_eq!(uri.to_file_path().as_deref(), Some(path.as_path()));
        }
    }
}
