# Diagnostic only: reuse a published SDK and the original two-process harness.
$ErrorActionPreference = 'Stop'
$PSNativeCommandUseErrorActionPreference = $false
$repo = (Get-Location).Path
$evidence = Join-Path $env:RUNNER_TEMP 'grant-evidence'
$sdkDir = Join-Path $env:RUNNER_TEMP 'grant-sdk'
$runner = Join-Path $repo '.diagnostic-grant-runner'
$target = Join-Path $env:RUNNER_TEMP 'grant-harness-target'
$archive = Join-Path $env:RUNNER_TEMP 'grant-sdk-download/hew-v0.6.2-windows-x86_64.zip'
Expand-Archive -Path $archive -DestinationPath $sdkDir
$sdk = Join-Path $sdkDir 'hew-v0.6.2-windows-x86_64'
$compiler = Join-Path $sdk 'bin/hew.exe'
$runtime = Join-Path $sdk 'lib/hew.lib'
$targetRuntime = Join-Path $sdk 'lib/x86_64-pc-windows-msvc/hew.lib'
foreach ($file in @($compiler, $runtime, $targetRuntime)) {
    if (-not (Test-Path $file -PathType Leaf)) { throw "Missing SDK file: $file" }
}
if ((Get-FileHash $runtime).Hash -ne (Get-FileHash $targetRuntime).Hash) {
    throw 'SDK runtime archive copies differ'
}

# An immediate child preserves support::repo_root() without copying the harness.
New-Item -ItemType Directory -Path $runner | Out-Null
New-Item -ItemType Directory -Path (Join-Path $target 'debug') -Force | Out-Null
Copy-Item $runtime (Join-Path $target 'debug/hew.lib')
@'
[package]
name = "hew-windows-grant-diagnostic"
version = "0.0.0"
edition = "2024"

[workspace]

[dependencies]
hew-testutil = { path = "../hew-testutil" }
tempfile = "=3.27.0"

[[test]]
name = "distributed_two_process_e2e"
path = "../hew-cli/tests/distributed_two_process_e2e.rs"
'@ | Set-Content (Join-Path $runner 'Cargo.toml')
# Start with the source revision's dependency versions; Cargo prunes unused crates.
Copy-Item (Join-Path $repo 'Cargo.lock') (Join-Path $runner 'Cargo.lock')
@'
.PHONY: diagnostic
diagnostic:
	cargo test --manifest-path .diagnostic-grant-runner/Cargo.toml --test distributed_two_process_e2e authenticated_service_grants_supply_and_revoke_exact_interface -- --exact --nocapture --test-threads=1
'@ | Set-Content (Join-Path $runner 'Makefile')
$env:CARGO_TARGET_DIR = $target
$env:CARGO_BIN_EXE_hew = $compiler
$env:HEW_DIST_E2E_HEW_BIN = $compiler
$env:HEW_TEST_NO_BUILD = '1'

$source = git rev-parse HEAD
if ($LASTEXITCODE -ne 0) { throw 'Could not identify diagnostic source' }
$files = @($archive, $compiler, $runtime, $targetRuntime,
    (Join-Path $repo 'hew-cli/tests/distributed_two_process_e2e.rs'),
    (Join-Path $repo 'hew-cli/tests/fixtures/distributed/dist_node.hew'))
$provenance = [ordered]@{
    purpose = 'Diagnostic only; not release qualification'
    source_sha = $source
    sdk_source_sha = '24720d0d7d28b56fb53854309682f35796751606'
    sdk_run_id = '39412485562'
    sdk_artifact_id = '12261137490'
    runtime_modified = $false
    rustc = (& rustc --version | Out-String).Trim()
    clang = (& clang --version | Out-String).Trim()
    compiler_version = (& $compiler --version | Out-String).Trim()
    files = @($files | ForEach-Object {
        @{ path = $_; sha256 = (Get-FileHash $_ -Algorithm SHA256).Hash.ToLowerInvariant() }
    })
}
$provenance | ConvertTo-Json -Depth 5 | Set-Content (Join-Path $evidence 'provenance.json')
Copy-Item (Join-Path $runner 'Cargo.toml') $evidence
Copy-Item (Join-Path $runner 'Makefile') $evidence

# Preserve the actual failure status and full test output, including failure metrics.
& make -f .diagnostic-grant-runner/Makefile diagnostic 2>&1 |
    Tee-Object -FilePath (Join-Path $evidence 'service-grant.log')
$result = $LASTEXITCODE
Copy-Item (Join-Path $runner 'Cargo.lock') $evidence
@{ exit_code = $result; release_qualification = $false } |
    ConvertTo-Json | Set-Content (Join-Path $evidence 'result.json')
exit $result
