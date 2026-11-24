//! CLI test invocation, event streaming, and test diagnostic publication.

use std::collections::HashSet;
use std::path::{Path, PathBuf};
use std::process::Stdio;

use dashmap::DashMap;
use serde_json::{json, Value};
use tokio::io::{AsyncBufReadExt, AsyncRead, BufReader};
use tower_lsp_server::jsonrpc::Result;
use tower_lsp_server::ls_types::{Diagnostic, MessageType, Uri as Url};
use tower_lsp_server::Client;

use super::analysis::{collect_published_diagnostics, open_document_uri};
use super::capabilities::RUN_TEST_COMMAND;
use super::testing::TestFailureBatch;
use super::uri::FileUriExt;
use super::{internal_error, HewLanguageServer};

async fn stream_command_output<R>(
    client: Client,
    reader: R,
    level: MessageType,
) -> std::io::Result<()>
where
    R: AsyncRead + Unpin,
{
    let mut lines = BufReader::new(reader).lines();
    while let Some(line) = lines.next_line().await? {
        if !line.trim().is_empty() {
            client.show_message(level, line).await;
        }
    }
    Ok(())
}

async fn read_test_events<R>(reader: R) -> std::io::Result<Vec<Value>>
where
    R: AsyncRead + Unpin,
{
    let mut lines = BufReader::new(reader).lines();
    let mut events = Vec::new();
    while let Some(line) = lines.next_line().await? {
        if let Ok(event) = serde_json::from_str::<Value>(&line) {
            if event.get("event").is_some() {
                events.push(event);
            }
        }
    }
    Ok(events)
}

pub(super) fn with_test_diagnostics(
    mut published: Vec<(Url, Vec<Diagnostic>)>,
    tests: &DashMap<Url, Vec<Diagnostic>>,
) -> Vec<(Url, Vec<Diagnostic>)> {
    for (uri, diagnostics) in &mut published {
        if let Some(failures) = tests.get(uri) {
            diagnostics.extend(failures.iter().cloned());
        }
    }
    published
}

async fn wait_for_output_task(
    task: Option<tokio::task::JoinHandle<std::io::Result<()>>>,
    stream_name: &str,
) -> Result<()> {
    if let Some(task) = task {
        let read_result = task.await.map_err(|error| {
            internal_error(format!("failed to join {stream_name} stream task: {error}"))
        })?;
        read_result.map_err(|error| {
            internal_error(format!("failed to read {stream_name} stream: {error}"))
        })?;
    }
    Ok(())
}

pub(super) fn hew_cli_executable() -> PathBuf {
    if let Some(path) = option_env!("CARGO_BIN_EXE_hew") {
        return PathBuf::from(path);
    }
    if let Ok(path) = std::env::var("CARGO_BIN_EXE_hew") {
        return PathBuf::from(path);
    }
    if let Ok(current_exe) = std::env::current_exe() {
        let candidate = current_exe.with_file_name(format!("hew{}", std::env::consts::EXE_SUFFIX));
        if candidate.exists() {
            return candidate;
        }
    }
    PathBuf::from(format!("hew{}", std::env::consts::EXE_SUFFIX))
}

pub(super) fn build_run_test_invocation(selector: &str) -> (PathBuf, Vec<String>) {
    (
        hew_cli_executable(),
        vec![
            "test".to_string(),
            selector.to_string(),
            "--format".to_string(),
            "json".to_string(),
        ],
    )
}

fn build_seeded_test_invocation(selector: &str, seed: Option<&str>) -> (PathBuf, Vec<String>) {
    let (program, mut args) = build_run_test_invocation(selector);
    if let Some(seed) = seed {
        args.extend(["--seed".to_string(), seed.to_string()]);
    }
    (program, args)
}

struct TestRunState {
    affected: HashSet<Url>,
    target_uri: Option<Url>,
    run_version: Option<u64>,
    edit_version: Option<u64>,
}

impl HewLanguageServer {
    async fn publish_test_diagnostics_for(&self, uris: &HashSet<Url>) {
        let published = collect_published_diagnostics(&self.documents, uris.clone());
        for (uri, diagnostics) in with_test_diagnostics(published, &self.test_diagnostics) {
            if uris.contains(&uri) {
                self.client
                    .publish_diagnostics(uri, diagnostics, None)
                    .await;
            }
        }
    }

    async fn begin_test_run(&self, test_name: &str) -> TestRunState {
        let analysis = self.analysis_jobs.lock().await;
        let file = test_name
            .rsplit_once("::")
            .map_or(test_name, |(file, _)| file);
        let target_uri = Url::from_checked_file_path(Path::new(file))
            .map(|uri| open_document_uri(&uri, &self.documents));
        let mut affected = HashSet::new();
        let run_version = target_uri.as_ref().map(|uri| {
            let mut version = self.test_run_versions.entry(uri.clone()).or_insert(0);
            *version += 1;
            *version
        });
        let edit_version = target_uri
            .as_ref()
            .and_then(|uri| self.analysis_versions.get(uri).map(|version| *version));
        if let Some(uri) = &target_uri {
            affected.insert(uri.clone());
            if let Some(mut diagnostics) = self.test_diagnostics.get_mut(uri) {
                if test_name.contains("::") {
                    diagnostics.retain(|diagnostic| {
                        diagnostic
                            .data
                            .as_ref()
                            .and_then(|data| data.get("selector"))
                            .and_then(Value::as_str)
                            != Some(test_name)
                    });
                } else {
                    diagnostics.clear();
                }
            }
        }
        if test_name.contains("::") {
            self.test_seeds.remove(test_name);
        } else {
            let prefix = format!("{test_name}::");
            self.test_seeds
                .retain(|selector, _| !selector.starts_with(&prefix));
        }
        self.publish_test_diagnostics_for(&affected).await;
        drop(analysis);
        self.refresh_pull_diagnostics().await;
        TestRunState {
            affected,
            target_uri,
            run_version,
            edit_version,
        }
    }

    fn record_test_failures(&self, events: &[Value], affected: &mut HashSet<Url>) {
        let mut batch = TestFailureBatch::default();
        for event in events {
            if let Some(failure) = batch.failure_from_event(event, &self.documents) {
                affected.insert(failure.uri.clone());
                self.test_diagnostics
                    .entry(failure.uri)
                    .or_default()
                    .push(failure.diagnostic);
                if let Some(seed) = failure.seed {
                    self.test_seeds.insert(failure.selector, seed);
                }
            }
        }
    }

    pub(super) async fn run_test_command(
        &self,
        test_name: &str,
        seed: Option<&str>,
    ) -> Result<Option<Value>> {
        let Some(workspace_root) = self.workspace_root() else {
            self.client
                .show_message(
                    MessageType::ERROR,
                    "Cannot run Hew test: no workspace root is available.",
                )
                .await;
            return Ok(None);
        };

        let TestRunState {
            mut affected,
            target_uri,
            run_version,
            edit_version,
        } = self.begin_test_run(test_name).await;

        let (program, args) = build_seeded_test_invocation(test_name, seed);
        self.client
            .show_message(
                MessageType::INFO,
                format!("Running Hew test `{test_name}`..."),
            )
            .await;

        match tokio::process::Command::new(&program)
            .args(&args)
            .current_dir(&workspace_root)
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()
        {
            Ok(mut child) => {
                let stdout_task = child
                    .stdout
                    .take()
                    .map(|stdout| tokio::spawn(read_test_events(stdout)));
                let stderr_task = child.stderr.take().map(|stderr| {
                    let client = self.client.clone();
                    tokio::spawn(stream_command_output(client, stderr, MessageType::ERROR))
                });
                let status = child.wait().await.map_err(|error| {
                    internal_error(format!("failed to wait for test process: {error}"))
                })?;
                let events = if let Some(task) = stdout_task {
                    task.await
                        .map_err(|error| {
                            internal_error(format!("failed to join test stream: {error}"))
                        })?
                        .map_err(|error| {
                            internal_error(format!("failed to read test stream: {error}"))
                        })?
                } else {
                    Vec::new()
                };
                wait_for_output_task(stderr_task, "stderr").await?;
                let analysis = self.analysis_jobs.lock().await;
                if let (Some(uri), Some(run_version)) = (&target_uri, run_version) {
                    if self.test_run_versions.get(uri).map(|version| *version) != Some(run_version)
                        || self.analysis_versions.get(uri).map(|version| *version) != edit_version
                    {
                        return Ok(None);
                    }
                }
                self.record_test_failures(&events, &mut affected);
                self.publish_test_diagnostics_for(&affected).await;
                drop(analysis);
                self.refresh_pull_diagnostics().await;
                let client = self.client.clone();
                tokio::spawn(async move {
                    let _ = client.code_lens_refresh().await;
                });
                let level = if status.success() {
                    MessageType::INFO
                } else {
                    MessageType::ERROR
                };
                self.client
                    .show_message(level, format!("Hew test `{test_name}` finished."))
                    .await;
                Ok(Some(json!({
                    "command": RUN_TEST_COMMAND,
                    "success": status.success(),
                    "test": test_name,
                })))
            }
            Err(error) => {
                self.client
                    .show_message(
                        MessageType::ERROR,
                        format!(
                            "Failed to start `{}` for test `{test_name}`: {error}",
                            program.display()
                        ),
                    )
                    .await;
                Ok(None)
            }
        }
    }
}
