use super::*;

#[cfg(unix)]
// Restores file/directory permissions on drop, used by permission-change tests.
struct RestoreOnDrop(PathBuf, u32);

#[cfg(unix)]
impl Drop for RestoreOnDrop {
    fn drop(&mut self) {
        use std::os::unix::fs::PermissionsExt;
        let _ = std::fs::set_permissions(&self.0, std::fs::Permissions::from_mode(self.1));
    }
}

// ── plan_workspace_rename: aliased-importer guard (Fix 1) ──────────

#[test]
fn plan_workspace_rename_skips_aliased_top_level_importer() {
    // util.hew defines `foo`; main.hew imports it as `bar` (aliased).
    // Renaming `foo` → `bar` from util.hew must NOT be rejected because
    // main.hew's `bar` is the alias — it will be rewritten as part of the
    // rename, not be left as a colliding name.
    let util_source = "pub fn foo() -> i32 { 1 }";
    let main_source = "import util.{ foo as bar };\nfn m() -> i32 { bar() }";

    let util_uri = make_test_uri("/project/util.hew");
    let main_uri = make_test_uri("/project/main.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(util_uri.clone(), make_doc(util_source));
    documents.insert(main_uri.clone(), make_doc(main_source));

    let util_doc = documents.get(&util_uri).unwrap();
    let offset = util_source.find("fn foo").unwrap() + 3;
    // This must succeed — the alias `bar` in main.hew is NOT a conflict with
    // the new name `bar` because the import will be rewritten as `{ bar }`.
    let result = plan_workspace_rename(&util_uri, &util_doc, offset, "bar", &documents);
    match result {
        Ok(_) => {} // correct — no conflict
        Err(hew_analysis::RenameError::Conflicts { ref conflicts }) => {
            panic!("aliased importer should not produce a conflict, got: {conflicts:?}");
        }
        Err(other) => panic!("unexpected error: {other:?}"),
    }
}

// ── plan_workspace_rename: definition-file local shadow (Fix 2) ────

#[test]
fn plan_workspace_rename_importer_side_detects_definition_file_local_shadow() {
    // Regression: importer-originated path must also detect local shadows
    // in the definition file.
    //
    // util.hew defines `pub fn foo()` at the top level AND has an internal
    // helper with `let bar = 0; foo()` — so renaming `foo` → `bar` from the
    // definition side is correctly refused.  The gap (PR #1255 rev5) was
    // that the SAME rename initiated from the import token in main.hew was
    // NOT refused, because `collect_cross_file_conflict` does not walk
    // local scopes of the definition file.
    let util_source = "pub fn foo() -> i32 { 1 }\nfn helper() -> i32 { let bar = 0; foo() }";
    let main_source = "import util.{ foo };\nfn main() -> i32 { foo() }";

    let util_uri = make_test_uri("/project/util.hew");
    let main_uri = make_test_uri("/project/main.hew");

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(util_uri.clone(), make_doc(util_source));
    documents.insert(main_uri.clone(), make_doc(main_source));

    // Cursor on `foo` import token in main.hew — importer-originated path.
    let main_doc = documents.get(&main_uri).unwrap();
    let offset = main_source.find("foo").unwrap();
    let err = plan_workspace_rename(&main_uri, &main_doc, offset, "bar", &documents)
        .expect_err("ShadowsLocal in definition file must be detected from importer side");
    match err {
        hew_analysis::RenameError::Conflicts { conflicts } => {
            assert!(
                conflicts
                    .iter()
                    .any(|c| c.kind == hew_analysis::RenameConflictKind::ShadowsLocal),
                "expected a ShadowsLocal conflict, got {conflicts:?}"
            );
        }
        other => panic!("expected Conflicts, got {other:?}"),
    }
}

#[test]
fn plan_workspace_rename_detects_definition_file_local_shadow() {
    // util.hew defines top-level `foo` AND a helper function that
    // binds `let bar = 0` in scope and then calls `foo()`.
    // Renaming `foo` → `bar` from util.hew must surface a ShadowsLocal
    // conflict because the rename site `foo()` is in scope of `let bar`.
    let util_source = "pub fn foo() -> i32 { 1 }\nfn helper() -> i32 { let bar = 0; foo() }";

    let util_uri = make_test_uri("/project/util.hew");
    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(util_uri.clone(), make_doc(util_source));

    let util_doc = documents.get(&util_uri).unwrap();
    let offset = util_source.find("fn foo").unwrap() + 3;
    let err = plan_workspace_rename(&util_uri, &util_doc, offset, "bar", &documents)
        .expect_err("definition-file local shadow must be detected");
    match err {
        hew_analysis::RenameError::Conflicts { conflicts } => {
            assert!(
                conflicts
                    .iter()
                    .any(|c| c.kind == hew_analysis::RenameConflictKind::ShadowsLocal),
                "expected a ShadowsLocal conflict, got {conflicts:?}"
            );
        }
        other => panic!("expected Conflicts, got {other:?}"),
    }
}

#[test]
fn plan_workspace_rename_scans_unopened_disk_importer_for_conflicts() {
    // Regression test for issue #1285: the conflict walk must include files
    // that are not open in the LSP documents map, reading them from disk.
    //
    // Layout:
    //   <test_dir>/std/          ← presence triggers workspace-root detection
    //   <test_dir>/util.hew      ← defines `pub fn foo`; OPEN in documents
    //   <test_dir>/importer.hew  ← imports `foo` and defines `bar`; NOT open
    //
    // Rename `foo` → `bar`.  `importer.hew` has a top-level `bar`, so the
    // scan must surface a ShadowsTopLevel conflict even though the file is
    // not open.

    let test_dir = TestDir::new("disk-importer-conflict");
    let project_root = test_dir.path();

    // Create the std/ stub so find_workspace_root can locate the project root.
    std::fs::create_dir_all(project_root.join("std")).unwrap();

    let util_path = project_root.join("util.hew");
    let importer_path = project_root.join("importer.hew");

    let util_source = "pub fn foo() -> i64 { 0 }";
    let importer_source = "import util.{ foo };\npub fn bar() -> i64 { foo() }";

    std::fs::write(&util_path, util_source).unwrap();
    std::fs::write(&importer_path, importer_source).unwrap();

    let util_uri = Url::from_checked_file_path(&util_path).unwrap();

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(util_uri.clone(), make_doc(util_source));
    // importer.hew is intentionally NOT inserted — only on disk.

    let util_doc = documents.get(&util_uri).unwrap();
    let offset = util_source.find("fn foo").unwrap() + 3;

    let result = plan_workspace_rename(&util_uri, &util_doc, offset, "bar", &documents);
    match result {
        Err(hew_analysis::RenameError::Conflicts { conflicts }) => {
            assert!(
                conflicts
                    .iter()
                    .any(|c| c.kind == hew_analysis::RenameConflictKind::ShadowsTopLevel),
                "expected ShadowsTopLevel conflict from unopened importer.hew, got {conflicts:?}"
            );
        }
        Ok(_) => panic!(
            "expected rename to be rejected due to conflict in unopened importer.hew, \
                 but it succeeded — disk scan did not run"
        ),
        Err(other) => panic!("expected Conflicts, got {other:?}"),
    }
}

#[test]
fn plan_workspace_rename_unopened_definition_file_checked_from_disk() {
    // Regression for issue #1285: when a rename targets a definition file,
    // the conflict walk must check that file even if it's not open, by reading
    // it from disk and checking for top-level conflicts.
    //
    // This scenario: a definition file is INITIALLY open (so import resolution
    // works), then CLOSED (simulating a user action). The rename must still check
    // the definition file for top-level symbol conflicts.
    //
    // Layout:
    //   <test_dir>/std/          ← workspace root marker
    //   <test_dir>/util.hew      ← defines `pub fn foo` AND `pub fn bar`; WAS open
    //   <test_dir>/importer.hew  ← imports `foo`; OPEN (cursor here)
    //
    // After both files are created, util.hew is closed (removed from documents).
    // Rename `foo -> bar` from importer.hew's import binding.
    // util.hew already has a top-level `bar`, so this is a ShadowsTopLevel conflict.

    let test_dir = TestDir::new("unopened-def-file-conflict");
    let project_root = test_dir.path();
    std::fs::create_dir_all(project_root.join("std")).unwrap();

    let util_path = project_root.join("util.hew");
    let importer_path = project_root.join("importer.hew");

    // util.hew defines both foo and bar.
    let util_source = "pub fn foo() -> i64 { 0 }\npub fn bar() -> i64 { 1 }";
    let importer_source = "import util.{ foo };\nfn main() { foo() }";

    std::fs::write(&util_path, util_source).unwrap();
    std::fs::write(&importer_path, importer_source).unwrap();

    let util_uri = Url::from_checked_file_path(&util_path).unwrap();
    let importer_uri = Url::from_checked_file_path(&importer_path).unwrap();

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    // Both files are initially open for import resolution to work.
    documents.insert(util_uri.clone(), make_doc(util_source));
    documents.insert(importer_uri.clone(), make_doc(importer_source));

    // Now close util.hew (user closed the file) by removing it from documents.
    documents.remove(&util_uri);

    let importer_doc = documents.get(&importer_uri).unwrap();
    // Place cursor on the `foo` binding in `import util::{ foo }`.
    let offset = importer_source.find("{ foo }").unwrap() + 2;

    let result = plan_workspace_rename(&importer_uri, &importer_doc, offset, "bar", &documents);
    match result {
        Err(hew_analysis::RenameError::Conflicts { conflicts }) => {
            assert!(
                conflicts
                    .iter()
                    .any(|c| c.kind == hew_analysis::RenameConflictKind::ShadowsTopLevel),
                "expected ShadowsTopLevel conflict from unopened definition file util.hew, got {conflicts:?}"
            );
        }
        Ok(_) => panic!(
            "expected rename to be rejected due to conflict in unopened definition file, \
                 but it succeeded"
        ),
        Err(other) => panic!("expected Conflicts, got {other:?}"),
    }
}

#[test]
fn plan_workspace_rename_disk_scan_skips_aliased_importer() {
    // Regression for issue #1285: an unopened file that imports `foo as baz`
    // must NOT block renaming `foo -> bar`, because the visible binding in
    // that file remains `baz`, not `bar`.
    //
    // Layout:
    //   <test_dir>/std/           ← workspace root marker
    //   <test_dir>/util.hew       ← defines `pub fn foo`; OPEN
    //   <test_dir>/aliased.hew    ← `import util::{ foo as baz }; fn uses() { baz() }`; NOT open
    //                               Also defines `bar` — but since it imports via alias,
    //                               there is no `bar` conflict introduced by the rename.

    let test_dir = TestDir::new("disk-aliased-importer");
    let project_root = test_dir.path();
    std::fs::create_dir_all(project_root.join("std")).unwrap();

    let util_path = project_root.join("util.hew");
    let aliased_path = project_root.join("aliased.hew");

    let util_source = "pub fn foo() -> i64 { 0 }";
    // aliased.hew imports foo with an alias and has a top-level `bar`.
    // If the disk scan incorrectly treats this as a conflict, the rename
    // `foo -> bar` would be rejected; but it should succeed.
    let aliased_source = "import util.{ foo as baz };\npub fn bar() -> i64 { baz() }";

    std::fs::write(&util_path, util_source).unwrap();
    std::fs::write(&aliased_path, aliased_source).unwrap();

    let util_uri = Url::from_checked_file_path(&util_path).unwrap();

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(util_uri.clone(), make_doc(util_source));
    // aliased.hew is intentionally NOT inserted — only on disk.

    let util_doc = documents.get(&util_uri).unwrap();
    let offset = util_source.find("fn foo").unwrap() + 3;

    // `aliased.hew` imports `foo as baz`, so it is an aliased importer.
    // The disk scan must skip it, and the rename must succeed.
    let result = plan_workspace_rename(&util_uri, &util_doc, offset, "bar", &documents);
    assert!(
        result.is_ok(),
        "rename foo->bar should succeed when only importer is aliased (foo as baz), got {result:?}"
    );
}

#[test]
fn plan_workspace_rename_from_importer_cursor_catches_unopened_sibling_conflict() {
    // Regression for issue #1283: when the rename is initiated from an
    // *importer* cursor (cursor on the import-binding token in importer.hew),
    // the disk scan must search for files that import the *definition* file
    // (util.hew), not files that import importer.hew.
    //
    // Layout:
    //   <test_dir>/std/              ← workspace root marker
    //   <test_dir>/util.hew          ← defines `pub fn foo`; OPEN
    //   <test_dir>/importer.hew      ← `import util::{ foo }`; OPEN (cursor here)
    //   <test_dir>/sibling.hew       ← also imports `foo` from util + defines `bar`; NOT open
    //
    // Rename `foo -> bar` from importer.hew's import binding.
    // sibling.hew would have a conflict (it imports foo and has `bar`).

    let test_dir = TestDir::new("disk-sibling-conflict-from-importer");
    let project_root = test_dir.path();
    std::fs::create_dir_all(project_root.join("std")).unwrap();

    let util_path = project_root.join("util.hew");
    let importer_path = project_root.join("importer.hew");
    let sibling_path = project_root.join("sibling.hew");

    let util_source = "pub fn foo() -> i64 { 0 }";
    let importer_source = "import util.{ foo };\nfn use_foo() -> i64 { foo() }";
    // sibling.hew imports foo and defines bar — conflict when renaming foo->bar.
    let sibling_source = "import util.{ foo };\npub fn bar() -> i64 { foo() }";

    std::fs::write(&util_path, util_source).unwrap();
    std::fs::write(&importer_path, importer_source).unwrap();
    std::fs::write(&sibling_path, sibling_source).unwrap();

    let util_uri = Url::from_checked_file_path(&util_path).unwrap();
    let importer_uri = Url::from_checked_file_path(&importer_path).unwrap();

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(util_uri.clone(), make_doc(util_source));
    documents.insert(importer_uri.clone(), make_doc(importer_source));
    // sibling.hew is intentionally NOT inserted.

    let importer_doc = documents.get(&importer_uri).unwrap();
    // Place cursor on the `foo` binding in `import util::{ foo }`.
    let offset = importer_source.find("{ foo }").unwrap() + 2;

    let result = plan_workspace_rename(&importer_uri, &importer_doc, offset, "bar", &documents);
    match result {
        Err(hew_analysis::RenameError::Conflicts { conflicts }) => {
            assert!(
                conflicts
                    .iter()
                    .any(|c| c.kind == hew_analysis::RenameConflictKind::ShadowsTopLevel),
                "expected ShadowsTopLevel conflict from unopened sibling.hew, got {conflicts:?}"
            );
        }
        Ok(_) => panic!(
            "expected rename to be rejected due to conflict in unopened sibling.hew, \
                 but it succeeded — disk scan searched wrong URI"
        ),
        Err(other) => panic!("expected Conflicts, got {other:?}"),
    }
}

#[test]
fn plan_workspace_rename_disk_scan_skips_worktrees_dir() {
    // Regression for issue #1285: the disk scan must not descend into
    // `worktrees/` directories (matching workspace.rs skip policy).
    //
    // Layout:
    //   <test_dir>/std/
    //   <test_dir>/util.hew       ← defines `pub fn foo`; OPEN
    //   <test_dir>/worktrees/wt/importer.hew  ← imports `foo` + defines `bar`; NOT open
    //
    // Without the fix, the scan descends into `worktrees/` and finds a conflict.
    // With the fix, `worktrees/` is skipped and the rename succeeds.

    let test_dir = TestDir::new("disk-worktrees-skip");
    let project_root = test_dir.path();
    std::fs::create_dir_all(project_root.join("std")).unwrap();

    let util_path = project_root.join("util.hew");
    let wt_dir = project_root.join("worktrees").join("wt");
    std::fs::create_dir_all(&wt_dir).unwrap();
    let wt_importer_path = wt_dir.join("importer.hew");

    let util_source = "pub fn foo() -> i64 { 0 }";
    // This importer is under worktrees/ — it must be skipped.
    let wt_importer_source = "import util.{ foo };\npub fn bar() -> i64 { foo() }";

    std::fs::write(&util_path, util_source).unwrap();
    std::fs::write(&wt_importer_path, wt_importer_source).unwrap();

    let util_uri = Url::from_checked_file_path(&util_path).unwrap();

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(util_uri.clone(), make_doc(util_source));

    let util_doc = documents.get(&util_uri).unwrap();
    let offset = util_source.find("fn foo").unwrap() + 3;

    // The worktrees/wt/importer.hew has `bar` — if it were scanned, the rename
    // `foo -> bar` would be rejected. Skipping worktrees/ means it succeeds.
    let result = plan_workspace_rename(&util_uri, &util_doc, offset, "bar", &documents);
    assert!(
        result.is_ok(),
        "rename foo->bar should succeed when the only importer is under worktrees/ \
             (which should be skipped), got {result:?}"
    );
}

// ── #1290 Symlink-cycle regression ──────────────────────────────────────

// Windows requires elevated rights for symlinks, so this is unix-only.
#[cfg(unix)]
#[test]
fn plan_workspace_rename_disk_scan_skips_symlinked_directory() {
    // Regression for issue #1290: a symlink pointing to an ancestor directory
    // under the workspace root must not cause unbounded recursion or a
    // stack overflow.
    //
    // Layout:
    //   <test_dir>/std/         ← workspace root marker
    //   <test_dir>/util.hew     ← defines `pub fn foo`; OPEN
    //   <test_dir>/loop         → symlink to <test_dir> itself
    //
    // Without the fix (is_dir() == true through a symlink) the scan would
    // recurse endlessly.  With symlink_metadata the symlink is identified
    // and skipped.

    let test_dir = TestDir::new("disk-symlink-cycle");
    let project_root = test_dir.path();
    std::fs::create_dir_all(project_root.join("std")).unwrap();

    let util_path = project_root.join("util.hew");
    let util_source = "pub fn foo() -> i64 { 0 }";
    std::fs::write(&util_path, util_source).unwrap();

    // Create a directory symlink that points back at the workspace root.
    let loop_path = project_root.join("loop");
    std::os::unix::fs::symlink(project_root, &loop_path).unwrap();

    let util_uri = Url::from_checked_file_path(&util_path).unwrap();
    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(util_uri.clone(), make_doc(util_source));

    let util_doc = documents.get(&util_uri).unwrap();
    let offset = util_source.find("fn foo").unwrap() + 3;

    // This must complete without stack overflow or infinite loop.
    // No conflict exists — the rename should succeed.
    let result = plan_workspace_rename(&util_uri, &util_doc, offset, "bar", &documents);
    assert!(
        result.is_ok(),
        "rename foo->bar should succeed; symlink cycle must be skipped, got {result:?}"
    );
}

// ── #1288 Unreadable-file / unreadable-directory regressions ────────────

#[test]
#[cfg(unix)]
fn plan_workspace_rename_disk_scan_surfaces_error_for_unreadable_file() {
    // Regression for issue #1288: an unreadable .hew file under the workspace
    // must surface a RenameError::Io rather than being silently skipped and
    // producing a potentially-incomplete conflict check.
    //
    // Skipped when running as root because the kernel ignores mode bits for root,
    // so the permission change would be a no-op.  The guard below verifies this
    // at runtime rather than relying on an env-var convention.
    use std::os::unix::fs::PermissionsExt;

    let test_dir = TestDir::new("disk-unreadable-file");
    let project_root = test_dir.path();
    std::fs::create_dir_all(project_root.join("std")).unwrap();

    let util_path = project_root.join("util.hew");
    let secret_path = project_root.join("secret.hew");

    let util_source = "pub fn foo() -> i64 { 0 }";
    // secret.hew imports foo — if readable it would trigger the scan.
    let secret_source = "import util.{ foo };\npub fn uses_foo() -> i64 { foo() }";

    std::fs::write(&util_path, util_source).unwrap();
    std::fs::write(&secret_path, secret_source).unwrap();

    // Remove read permission from secret.hew.
    std::fs::set_permissions(&secret_path, std::fs::Permissions::from_mode(0o000)).unwrap();

    // Verify that the permission change actually took effect (no-op under root).
    if std::fs::read_to_string(&secret_path).is_ok() {
        return; // running as root — permission enforcement is bypassed; skip
    }

    // Restore permissions on drop so TestDir cleanup succeeds.
    let _restore = RestoreOnDrop(secret_path.clone(), 0o644);

    let util_uri = Url::from_checked_file_path(&util_path).unwrap();
    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(util_uri.clone(), make_doc(util_source));

    let util_doc = documents.get(&util_uri).unwrap();
    let offset = util_source.find("fn foo").unwrap() + 3;

    let result = plan_workspace_rename(&util_uri, &util_doc, offset, "bar", &documents);
    // Assert both the variant and that the path field is non-empty and names
    // the unreadable file, so "rename failed: : permission denied" is impossible.
    match &result {
        Err(hew_analysis::RenameError::Io { path, .. }) => {
            assert!(
                !path.is_empty(),
                "RenameError::Io path must be non-empty; got empty string"
            );
            assert!(
                path.contains("secret.hew"),
                "RenameError::Io path should name the unreadable file; got {path:?}"
            );
        }
        other => panic!("expected RenameError::Io for unreadable file, got {other:?}"),
    }
}

#[test]
#[cfg(unix)]
fn plan_workspace_rename_disk_scan_surfaces_error_for_unreadable_directory() {
    // Regression for issue #1288: an unreadable subdirectory under the workspace
    // must surface a RenameError::Io rather than being silently skipped.
    use std::os::unix::fs::PermissionsExt;

    let test_dir = TestDir::new("disk-unreadable-dir");
    let project_root = test_dir.path();
    std::fs::create_dir_all(project_root.join("std")).unwrap();

    let util_path = project_root.join("util.hew");
    let locked_dir = project_root.join("locked");
    std::fs::create_dir_all(&locked_dir).unwrap();
    let inner_path = locked_dir.join("importer.hew");

    let util_source = "pub fn foo() -> i64 { 0 }";
    // importer.hew inside locked/ imports foo.
    let inner_source = "import util.{ foo };\npub fn uses_foo() -> i64 { foo() }";

    std::fs::write(&util_path, util_source).unwrap();
    std::fs::write(&inner_path, inner_source).unwrap();

    // Remove read+exec from the directory so read_dir will fail.
    std::fs::set_permissions(&locked_dir, std::fs::Permissions::from_mode(0o000)).unwrap();

    // Verify the permission took effect.
    if std::fs::read_dir(&locked_dir).is_ok() {
        return; // running as root
    }

    let _restore = RestoreOnDrop(locked_dir.clone(), 0o755);

    let util_uri = Url::from_checked_file_path(&util_path).unwrap();
    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(util_uri.clone(), make_doc(util_source));

    let util_doc = documents.get(&util_uri).unwrap();
    let offset = util_source.find("fn foo").unwrap() + 3;

    let result = plan_workspace_rename(&util_uri, &util_doc, offset, "bar", &documents);
    // Assert both the variant and that the path field names the locked directory,
    // so "rename failed: : permission denied" is impossible.
    match &result {
        Err(hew_analysis::RenameError::Io { path, .. }) => {
            assert!(
                !path.is_empty(),
                "RenameError::Io path must be non-empty; got empty string"
            );
            assert!(
                path.contains("locked"),
                "RenameError::Io path should name the locked directory; got {path:?}"
            );
        }
        other => panic!("expected RenameError::Io for unreadable directory, got {other:?}"),
    }
}

#[test]
fn importer_originated_rename_includes_unopened_definition_file_edits() {
    // Regression test: when an importer cursor renames a symbol, the workspace
    // edit must include edits for the definition file even if it's not open.
    //
    // Layout:
    //   <test_dir>/std/          ← workspace root marker
    //   <test_dir>/util.hew      ← defines `pub fn foo`; WAS open, then closed
    //   <test_dir>/importer.hew  ← imports `foo` and uses it; OPEN (cursor here)
    //
    // Rename `foo -> bar` from the import binding in importer.hew.
    // The resulting WorkspaceEdit must include edits for BOTH:
    // 1. The importer (the binding token in `import util::{ foo }`)
    // 2. The unopened definition file (the definition `pub fn foo`)
    //
    // This verifies that build_workspace_edit loads unopened definition files
    // from disk and includes their edits in the returned WorkspaceEdit.

    let test_dir = TestDir::new("unopened-def-file-edits");
    let project_root = test_dir.path();
    std::fs::create_dir_all(project_root.join("std")).unwrap();

    let util_path = project_root.join("util.hew");
    let importer_path = project_root.join("importer.hew");

    let util_source = "pub fn foo() -> i64 { 0 }";
    let importer_source = "import util.{ foo };\nfn main() { foo() }";

    std::fs::write(&util_path, util_source).unwrap();
    std::fs::write(&importer_path, importer_source).unwrap();

    let util_uri = Url::from_checked_file_path(&util_path).unwrap();
    let importer_uri = Url::from_checked_file_path(&importer_path).unwrap();

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    // Both files are initially open for import resolution to work.
    documents.insert(util_uri.clone(), make_doc(util_source));
    documents.insert(importer_uri.clone(), make_doc(importer_source));

    // Now close util.hew (user closed the file) by removing it from documents.
    documents.remove(&util_uri);

    let importer_doc = documents.get(&importer_uri).unwrap();
    // Place cursor on the `foo` binding in `import util::{ foo }`.
    let offset = importer_source.find("{ foo }").unwrap() + 2;

    let edit = super::super::navigation::build_workspace_edit(
        &importer_uri,
        &importer_doc,
        offset,
        "bar",
        &documents,
    );
    let edit = edit
        .expect("workspace edit should not error")
        .expect("workspace edit should be generated");

    let changes = edit.changes.expect("changes must be present");

    // Verify edits exist for both URIs.
    assert!(
        changes.contains_key(&importer_uri),
        "workspace edit must include changes for importer URI: {importer_uri:?}"
    );
    assert!(
        changes.contains_key(&util_uri),
        "workspace edit must include changes for unopened definition URI: {util_uri:?}"
    );

    // Verify the definition file's edit covers the `foo` definition.
    let util_edits = &changes[&util_uri];
    assert!(
        !util_edits.is_empty(),
        "unopened definition file must have at least one edit"
    );
    // The edit should rename `foo` to `bar` in the definition.
    let util_has_bar_edit = util_edits.iter().any(|e| {
        e.new_text == "bar"
            && util_source[util_source.find("pub fn foo").unwrap()..].starts_with("pub fn foo")
    });
    assert!(
        util_has_bar_edit || util_edits.iter().any(|e| e.new_text == "bar"),
        "unopened definition file edits must include renaming foo to bar, got {util_edits:?}"
    );

    // Verify the importer file's edit covers the import binding and usages.
    let importer_edits = &changes[&importer_uri];
    assert!(
        !importer_edits.is_empty(),
        "importer file must have at least one edit"
    );
    let renames_to_bar = importer_edits
        .iter()
        .filter(|e| e.new_text == "bar")
        .count();
    // Should have at least 2 edits: one for the binding, one for the usage.
    assert!(
        renames_to_bar >= 2,
        "importer file should rename at least 2 occurrences of foo (binding + usage), got {renames_to_bar} edits to bar"
    );
}

#[test]
fn importer_originated_rename_includes_unopened_sibling_importer_edits() {
    // Regression test: when an importer cursor renames a symbol, the workspace
    // edit must include edits for unopened sibling importers that also import
    // the symbol non-aliased.
    //
    // Layout:
    //   <test_dir>/std/          ← workspace root marker
    //   <test_dir>/util.hew      ← defines `pub fn foo`
    //   <test_dir>/importer1.hew ← imports `foo` and uses it; OPEN (cursor here)
    //   <test_dir>/importer2.hew ← imports `foo` and uses it; CLOSED (unopened)
    //
    // Rename `foo -> bar` from the import binding in importer1.hew.
    // The resulting WorkspaceEdit must include edits for ALL THREE:
    // 1. The current importer (importer1.hew)
    // 2. The unopened sibling importer (importer2.hew)
    // 3. The definition file (util.hew)
    //
    // This verifies that build_workspace_edit includes edits for unopened
    // sibling importers when the cursor is on an import binding.

    let test_dir = TestDir::new("unopened-sibling-importer-edits");
    let project_root = test_dir.path();
    std::fs::create_dir_all(project_root.join("std")).unwrap();

    let util_path = project_root.join("util.hew");
    let importer1_path = project_root.join("importer1.hew");
    let importer2_path = project_root.join("importer2.hew");

    let util_source = "pub fn foo() -> i64 { 0 }";
    let importer1_source = "import util.{ foo };\nfn main() { foo() }";
    let importer2_source = "import util.{ foo };\nfn helper() { foo() }";

    std::fs::write(&util_path, util_source).unwrap();
    std::fs::write(&importer1_path, importer1_source).unwrap();
    std::fs::write(&importer2_path, importer2_source).unwrap();

    let util_uri = Url::from_checked_file_path(&util_path).unwrap();
    let importer1_uri = Url::from_checked_file_path(&importer1_path).unwrap();
    let importer2_uri = Url::from_checked_file_path(&importer2_path).unwrap();

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    // Only importer1 is open; importer2 is closed.
    documents.insert(util_uri.clone(), make_doc(util_source));
    documents.insert(importer1_uri.clone(), make_doc(importer1_source));

    let importer1_doc = documents.get(&importer1_uri).unwrap();
    // Place cursor on the `foo` binding in `import util::{ foo }`.
    let offset = importer1_source.find("{ foo }").unwrap() + 2;

    let edit = super::super::navigation::build_workspace_edit(
        &importer1_uri,
        &importer1_doc,
        offset,
        "bar",
        &documents,
    );
    let edit = edit
        .expect("workspace edit should not error")
        .expect("workspace edit should be generated");

    let changes = edit.changes.expect("changes must be present");

    // Verify edits exist for all three URIs.
    assert!(
        changes.contains_key(&importer1_uri),
        "workspace edit must include changes for current importer URI: {importer1_uri:?}"
    );
    assert!(
        changes.contains_key(&util_uri),
        "workspace edit must include changes for definition URI: {util_uri:?}"
    );
    assert!(
        changes.contains_key(&importer2_uri),
        "workspace edit must include changes for unopened sibling importer URI: {importer2_uri:?}"
    );

    // Verify importer1's edits (binding + usage).
    let importer1_edits = &changes[&importer1_uri];
    assert!(
        !importer1_edits.is_empty(),
        "current importer file must have at least one edit"
    );
    let renames_to_bar_1 = importer1_edits
        .iter()
        .filter(|e| e.new_text == "bar")
        .count();
    assert!(
        renames_to_bar_1 >= 2,
        "current importer should rename at least 2 occurrences of foo (binding + usage), got {renames_to_bar_1}"
    );

    // Verify importer2's edits (binding + usage).
    let importer2_edits = &changes[&importer2_uri];
    assert!(
        !importer2_edits.is_empty(),
        "unopened sibling importer file must have at least one edit"
    );
    let renames_to_bar_2 = importer2_edits
        .iter()
        .filter(|e| e.new_text == "bar")
        .count();
    assert!(
        renames_to_bar_2 >= 2,
        "unopened sibling importer should rename at least 2 occurrences of foo (binding + usage), got {renames_to_bar_2}"
    );

    // Verify the definition file's edit (just the definition).
    let util_edits = &changes[&util_uri];
    assert!(
        !util_edits.is_empty(),
        "definition file must have at least one edit"
    );
    let util_has_bar_edit = util_edits.iter().any(|e| e.new_text == "bar");
    assert!(
        util_has_bar_edit,
        "definition file edits must include renaming foo to bar, got {util_edits:?}"
    );
}

#[test]
fn unopened_aliased_importer_rename_rewrites_import_name() {
    // Regression test: unopened files with aliased imports (`import foo as baz`)
    // must have the imported name (`foo`) rewritten when the symbol is renamed,
    // but the alias (`baz`) must remain unchanged.
    //
    // Layout:
    //   <test_dir>/std/          ← workspace root marker
    //   <test_dir>/util.hew      ← defines `pub fn foo`
    //   <test_dir>/importer.hew  ← imports `foo as baz` and uses `baz`; CLOSED
    //
    // Rename `foo -> bar` from the definition file cursor.
    // The WorkspaceEdit must include an edit in importer.hew that rewrites
    // only the imported name token from `foo` to `bar`, leaving `baz` intact.

    let test_dir = TestDir::new("unopened-aliased-importer-rename");
    let project_root = test_dir.path();
    std::fs::create_dir_all(project_root.join("std")).unwrap();

    let util_path = project_root.join("util.hew");
    let importer_path = project_root.join("importer.hew");

    let util_source = "pub fn foo() -> i64 { 0 }";
    let importer_source = "import util.{ foo as baz };\nfn main() { baz() }";

    std::fs::write(&util_path, util_source).unwrap();
    std::fs::write(&importer_path, importer_source).unwrap();

    let util_uri = Url::from_checked_file_path(&util_path).unwrap();
    let importer_uri = Url::from_checked_file_path(&importer_path).unwrap();

    let documents: DashMap<Url, DocumentState> = DashMap::new();
    // Only util is open; importer is closed.
    documents.insert(util_uri.clone(), make_doc(util_source));

    let util_doc = documents.get(&util_uri).unwrap();
    // Place cursor on the `foo` definition.
    let offset = util_source.find("fn foo").unwrap() + 3;

    let edit = super::super::navigation::build_workspace_edit(
        &util_uri, &util_doc, offset, "bar", &documents,
    );
    let edit = edit
        .expect("workspace edit should not error")
        .expect("workspace edit should be generated");

    let changes = edit.changes.expect("changes must be present");

    // Verify edits exist for both the util and the unopened importer.
    assert!(
        changes.contains_key(&util_uri),
        "workspace edit must include changes for definition URI: {util_uri:?}"
    );
    assert!(
        changes.contains_key(&importer_uri),
        "workspace edit must include changes for unopened aliased importer URI: {importer_uri:?}"
    );

    // Verify importer's edit: should rewrite `foo` to `bar` in the import,
    // leaving `baz` unchanged. The importer uses `baz()`, which should NOT
    // be renamed (only the imported name in the import statement changes).
    let importer_edits = &changes[&importer_uri];
    assert!(
        !importer_edits.is_empty(),
        "unopened aliased importer file must have at least one edit"
    );

    // Expect exactly 1 edit: the import-name token from foo to bar.
    // The alias `baz` and the usage `baz()` should NOT be edited.
    assert_eq!(
        importer_edits.len(),
        1,
        "unopened aliased importer should have exactly 1 edit (the import-name), got {}: {:?}",
        importer_edits.len(),
        importer_edits
    );

    let edit = &importer_edits[0];
    assert_eq!(
        edit.new_text, "bar",
        "edit should rename to bar, got {}",
        edit.new_text
    );
}

#[test]
fn rename_error_to_jsonrpc_uses_request_failed_code() {
    // LSP 3.17 §3.16.3: semantic refusals of well-formed rename requests
    // must use RequestFailed (-32803), not InvalidParams (-32602).
    use tower_lsp_server::jsonrpc::ErrorCode;

    let invalid_id_err = hew_analysis::RenameError::InvalidIdentifier {
        name: "123bad".to_string(),
        message: "not a valid identifier".to_string(),
    };
    let builtin_err = hew_analysis::RenameError::Builtin {
        name: "print".to_string(),
        message: "cannot rename to a built-in".to_string(),
    };
    let conflicts_err = hew_analysis::RenameError::Conflicts {
        conflicts: vec![hew_analysis::RenameConflict {
            kind: hew_analysis::RenameConflictKind::ShadowsLocal,
            message: "shadows local binding".to_string(),
            existing_span: hew_analysis::OffsetSpan { start: 0, end: 0 },
            offending_span: hew_analysis::OffsetSpan { start: 0, end: 0 },
        }],
    };

    for err in [&invalid_id_err, &builtin_err, &conflicts_err] {
        let jsonrpc_err = rename_error_to_jsonrpc(err);
        assert_eq!(
            jsonrpc_err.code,
            ErrorCode::ServerError(-32803),
            "expected RequestFailed (-32803) for {err:?}, got {:?}",
            jsonrpc_err.code,
        );
    }
}

#[test]
#[cfg(unix)]
fn build_workspace_edit_propagates_io_error_for_unreadable_closed_definition_file() {
    // Regression for issue #1299: when the definition file is closed and unreadable,
    // build_workspace_edit should propagate the I/O error via RenameError::Io,
    // not silently skip edits and return a partial rename.
    use std::os::unix::fs::PermissionsExt;

    let test_dir = TestDir::new("build-workspace-edit-unreadable-definition");
    let project_root = test_dir.path();
    std::fs::create_dir_all(project_root.join("std")).unwrap();

    let util_path = project_root.join("util.hew");
    let importer_path = project_root.join("importer.hew");

    let util_source = "pub fn foo() -> i64 { 0 }";
    let importer_source = "import util.{ foo };\npub fn uses_foo() -> i64 { foo() }";

    std::fs::write(&util_path, util_source).unwrap();
    std::fs::write(&importer_path, importer_source).unwrap();

    // Remove read permission from the definition file.
    std::fs::set_permissions(&util_path, std::fs::Permissions::from_mode(0o000)).unwrap();

    // Verify the permission took effect.
    if std::fs::read(util_path.clone()).is_ok() {
        return; // running as root
    }

    let _restore = RestoreOnDrop(util_path.clone(), 0o644);

    let _util_uri = Url::from_checked_file_path(&util_path).unwrap();
    let importer_uri = Url::from_checked_file_path(&importer_path).unwrap();
    let documents: DashMap<Url, DocumentState> = DashMap::new();
    documents.insert(importer_uri.clone(), make_doc(importer_source));

    let importer_doc = documents.get(&importer_uri).unwrap();
    // Rename the import binding reference (offset inside `uses_foo`).
    let offset = importer_source.find("uses_foo").unwrap() + 5;

    let result = build_workspace_edit(&importer_uri, &importer_doc, offset, "bar", &documents);

    match result {
        Err(hew_analysis::RenameError::Io { path, .. }) => {
            assert!(
                path.contains("util.hew"),
                "RenameError::Io should name the unreadable definition file; got {path:?}"
            );
        }
        other => {
            panic!("expected RenameError::Io for unreadable closed definition file, got {other:?}")
        }
    }
}

#[test]
#[cfg(unix)]
fn for_each_hew_file_allows_symlinked_workspace_root() {
    // Regression for issue #1299: the workspace root itself may be a symlink,
    // and for_each_hew_file should traverse it. Only intra-tree symlinks are skipped.
    let test_dir = TestDir::new("for-each-hew-file-symlink-root");
    let real_path = test_dir.path();
    std::fs::create_dir_all(real_path.join("std")).unwrap();

    // Create a .hew file in the real directory.
    let real_file = real_path.join("lib.hew");
    std::fs::write(&real_file, "pub fn bar() {}").unwrap();

    // Create a symlink to the directory.
    let symlink_parent = real_path.parent().unwrap();
    let symlink_path = symlink_parent.join("symlink-root");
    let _ = std::fs::remove_dir_all(&symlink_path); // Clean up any leftover from previous runs
    std::os::unix::fs::symlink(real_path, &symlink_path).unwrap();

    let mut visited_files = Vec::new();

    let result: std::result::Result<(), (std::path::PathBuf, std::io::Error)> =
        super::super::workspace::for_each_hew_file(&symlink_path, |path| {
            visited_files.push(path.to_path_buf());
            Ok(())
        });

    assert!(
        result.is_ok(),
        "for_each_hew_file should traverse a symlinked root, got {result:?}"
    );
    assert!(
        !visited_files.is_empty(),
        "for_each_hew_file should have visited at least one .hew file under the symlinked root"
    );
    assert!(
        visited_files.iter().any(|p| p.ends_with("lib.hew")),
        "for_each_hew_file should have visited lib.hew"
    );
}

#[test]
#[cfg(unix)]
fn for_each_hew_file_visits_real_dir_despite_symlink_sibling() {
    // Regression for issue #1299: when a workspace contains both a real directory
    // and a symlink to it, both paths must visit the real directory's files.
    // Previously, the symlink would claim the canonical path in visited, and the
    // real directory would be skipped as already-visited.
    let test_dir = TestDir::new("for-each-hew-file-symlink-and-real");
    let root = test_dir.path();
    std::fs::create_dir_all(root.join("std")).unwrap();

    // Create a real subdirectory with a .hew file. Name it so it sorts AFTER
    // the symlink — the walker's children.sort() + reverse-push + pop yields
    // ascending order, so the symlink must come first in this test to actually
    // exercise the "symlink visited first poisons the visited set" bug.
    let real_subdir = root.join("z_real");
    std::fs::create_dir_all(&real_subdir).unwrap();
    let real_file = real_subdir.join("def.hew");
    std::fs::write(&real_file, "pub fn foo() {}").unwrap();

    // Create a symlink to the real directory, named to sort BEFORE it.
    let symlink_path = root.join("a_link");
    let _ = std::fs::remove_dir_all(&symlink_path); // Clean up any leftover
    std::os::unix::fs::symlink(&real_subdir, &symlink_path).unwrap();

    // Walk from the root. Pop order is a_link, then z_real. The symlink is
    // skipped on encounter; the real directory must still be visited.
    let mut visited_files = Vec::new();
    let result: std::result::Result<(), (std::path::PathBuf, std::io::Error)> =
        super::super::workspace::for_each_hew_file(root, |path| {
            visited_files.push(path.to_path_buf());
            Ok(())
        });

    assert!(
        result.is_ok(),
        "for_each_hew_file should succeed when workspace has symlink and real dir, got {result:?}"
    );
    assert!(
        visited_files.iter().any(|p| p.ends_with("def.hew")),
        "for_each_hew_file should have visited def.hew in the real directory despite the symlink sibling"
    );
}
