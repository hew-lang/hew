# Diagnostic only: reuse a published SDK and the original two-process harness.
param([ValidateSet('grants', 'process', 'grant-load')] [string] $Mode = 'grants')

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

if ($Mode -eq 'grant-load') {
    $env:HEW_GRANT_LOAD_SDK = $sdk
    $env:HEW_GRANT_LOAD_EVIDENCE = $evidence
    $loadMakefile = Join-Path $env:RUNNER_TEMP 'grant-load-diagnostic.mk'
    @'
.PHONY: grant-load-diagnostic
grant-load-diagnostic:
	python scripts/windows-grant-coload.py
'@ | Set-Content $loadMakefile
    & make SHELL=bash -f $loadMakefile grant-load-diagnostic 2>&1 |
        Tee-Object -FilePath (Join-Path $evidence 'grant-load.log')
    exit $LASTEXITCODE
}

if ($Mode -eq 'process') {
    $env:HEW_PROCESS_DIAGNOSTIC_COMPILER = $compiler
    $env:HEW_PROCESS_DIAGNOSTIC_EVIDENCE = $evidence
    $processMakefile = Join-Path $env:RUNNER_TEMP 'process-diagnostic.mk'
    @'
.PHONY: process-diagnostic
process-diagnostic:
	python scripts/windows-process-diagnostic.py
'@ | Set-Content $processMakefile
    & make SHELL=bash -f $processMakefile process-diagnostic 2>&1 |
        Tee-Object -FilePath (Join-Path $evidence 'process-diagnostic.log')
    exit $LASTEXITCODE
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
.PHONY: diagnostic runtime
runtime:
	cargo build --locked -p hew-lib
diagnostic:
	cargo test --manifest-path .diagnostic-grant-runner/Cargo.toml --test distributed_two_process_e2e authenticated_service_grants_supply_and_revoke_exact_interface -- --exact --nocapture --test-threads=1
'@ | Set-Content (Join-Path $runner 'Makefile')
$env:CARGO_TARGET_DIR = $target
$env:CARGO_BIN_EXE_hew = $compiler
$env:HEW_DIST_E2E_HEW_BIN = $compiler
$env:HEW_TEST_NO_BUILD = '1'

$source = git rev-parse HEAD
if ($LASTEXITCODE -ne 0) { throw 'Could not identify diagnostic source' }
$frozenCompilerHash = (Get-FileHash $compiler).Hash
$frozenRuntimeHash = (Get-FileHash $runtime).Hash
$files = @($archive, $compiler, $runtime, $targetRuntime,
    (Join-Path $repo 'hew-cli/tests/distributed_two_process_e2e.rs'),
    (Join-Path $repo 'hew-cli/tests/fixtures/distributed/dist_node.hew'))
$provenance = [ordered]@{
    purpose = 'Diagnostic only; not release qualification'
    source_sha = $source
    sdk_source_sha = '24720d0d7d28b56fb53854309682f35796751606'
    sdk_run_id = '39412485562'
    sdk_artifact_id = '12261137490'
    control_runtime_modified = $false
    experiment_runtime = 'debug profile; original source plus saved observation-only delta'
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

# Record the complete runtime delta against the SDK's original source revision.
$base = '24720d0d7d28b56fb53854309682f35796751606'
git fetch --no-tags --depth=1 origin $base
if ($LASTEXITCODE -ne 0) { throw 'Could not fetch the SDK source revision for comparison' }
$allowedRuntimeChanges = @(
    'hew-runtime/src/lib.rs',
    'hew-runtime/src/routing.rs',
    'hew-runtime/src/hew_node/remote_call.rs',
    'hew-runtime/src/hew_node/monitor_link.rs',
    'hew-runtime/src/hew_node/mod.rs',
    'hew-runtime/src/hew_node/inbound.rs'
)
$allowedChanges = $allowedRuntimeChanges + @(
    '.github/workflows/release-gate.yml',
    'scripts/windows-grant-diagnostic.ps1',
    'hew-cli/tests/fixtures/distributed/dist_node.hew',
    'scripts/windows-process-diagnostic.py',
    'scripts/windows-grant-coload.py',
    'tests/core-acceptance/cases/process-bounded-output.hew',
    'tests/core-acceptance/cases/process-run-suspends.hew',
    'tests/core-acceptance/cases/process-supervised-reap.hew',
    'tests/core-acceptance/fixtures/process-probe.hew'
)
$sourceChanges = @(git diff --name-only $base HEAD)
if ($LASTEXITCODE -ne 0 -or @($sourceChanges | Where-Object { $_ -notin $allowedChanges }).Count -ne 0) {
    throw 'Runtime source differs from the selected base outside diagnostic observations'
}
git diff $base HEAD -- hew-runtime | Set-Content (Join-Path $evidence 'runtime-observations.patch')
if ($LASTEXITCODE -ne 0) { throw 'Could not preserve runtime source delta' }

function Run-GrantPhase([string] $name, [string] $selectedSdk, [bool] $observedRuntime) {
    $phase = Join-Path $evidence $name
    $fixtureTemp = Join-Path $env:RUNNER_TEMP "grant-fixtures/$name"
    New-Item -ItemType Directory -Path $phase, $fixtureTemp -Force | Out-Null
    $env:TEMP = $fixtureTemp
    $env:TMP = $fixtureTemp
    $env:HEW_GRANT_DIAGNOSTIC_DIR = $phase
    $env:HEW_DIST_E2E_HEW_BIN = Join-Path $selectedSdk 'bin/hew.exe'
    $env:CARGO_TARGET_DIR = $target
    Copy-Item (Join-Path $selectedSdk 'lib/hew.lib') (Join-Path $target 'debug/hew.lib') -Force
    # CARGO_BIN_EXE_hew stays fixed; the harness's existing runtime override selects the SDK.
    & make SHELL=bash -f .diagnostic-grant-runner/Makefile diagnostic 2>&1 |
        Tee-Object -FilePath (Join-Path $phase 'test.log') | Out-Host
    $actualExit = $LASTEXITCODE
    $events = @(Get-ChildItem $phase -Filter '*-*.log' | Where-Object {
        Select-String -Path $_.FullName -Pattern 'GRANT_RUNTIME_TRACE' -Quiet
    })
    $binaries = @(Get-ChildItem $fixtureTemp -Recurse -Filter dist_node.exe)
    $phaseResult = [ordered]@{
        phase = $name
        actual_test_exit = $actualExit
        observed_runtime = $observedRuntime
        compiler_sha256 = (Get-FileHash $env:HEW_DIST_E2E_HEW_BIN).Hash.ToLowerInvariant()
        runtime_sha256 = (Get-FileHash (Join-Path $selectedSdk 'lib/hew.lib')).Hash.ToLowerInvariant()
        runtime_event_files = @($events.Name)
        debug_runtime_selection_proved = $observedRuntime -and $events.Count -gt 0
        binaries = @($binaries | ForEach-Object {
            @{ path = $_.FullName; sha256 = (Get-FileHash $_.FullName).Hash.ToLowerInvariant() }
        })
    }
    $phaseResult | ConvertTo-Json -Depth 5 | Set-Content (Join-Path $phase 'result.json')
    return [pscustomobject] $phaseResult
}

$phases = @()
$phases += Run-GrantPhase 'release-control' $sdk $false
$phases | ConvertTo-Json -Depth 5 | Set-Content (Join-Path $evidence 'phases.json')

# Build only the combined runtime/stdlib archive, never the compiler or workspace tests.
$env:TEMP = $env:RUNNER_TEMP
$env:TMP = $env:RUNNER_TEMP
$runtimeTarget = Join-Path $env:RUNNER_TEMP 'grant-runtime-target'
$env:CARGO_TARGET_DIR = $runtimeTarget
& make SHELL=bash -f .diagnostic-grant-runner/Makefile runtime 2>&1 |
    Tee-Object -FilePath (Join-Path $evidence 'debug-runtime-build.log')
$buildExit = $LASTEXITCODE
@{ actual_build_exit = $buildExit; source_sha = $source; base_sha = $base; profile = 'dev'; package = 'hew-lib' } |
    ConvertTo-Json | Set-Content (Join-Path $evidence 'debug-runtime-build.json')
if ($buildExit -ne 0) { exit $buildExit }

$debugArchive = Join-Path $runtimeTarget 'debug/hew.lib'
$debugSdk = Join-Path $env:RUNNER_TEMP 'grant-sdk-debug'
Copy-Item $sdk $debugSdk -Recurse
$debugCopies = @((Join-Path $debugSdk 'lib/hew.lib'), (Join-Path $debugSdk 'lib/x86_64-pc-windows-msvc/hew.lib'))
foreach ($copy in $debugCopies) { Copy-Item $debugArchive $copy -Force }
$debugHash = (Get-FileHash $debugArchive).Hash
if ($debugHash -eq $frozenRuntimeHash -or
    (Get-FileHash (Join-Path $debugSdk 'bin/hew.exe')).Hash -ne $frozenCompilerHash -or
    @($debugCopies | Where-Object { (Get-FileHash $_).Hash -ne $debugHash }).Count -ne 0) {
    throw 'Debug SDK substitution or compiler identity verification failed'
}
@{
    compiler_sha256 = $frozenCompilerHash.ToLowerInvariant()
    built_debug_archive_sha256 = $debugHash.ToLowerInvariant()
    substituted_archives = @($debugCopies | ForEach-Object {
        @{ path = $_; sha256 = (Get-FileHash $_).Hash.ToLowerInvariant() }
    })
    selection_proof = 'Identical archive copies plus runtime-only diagnostic events from the compiled fixture'
} | ConvertTo-Json -Depth 5 | Set-Content (Join-Path $evidence 'debug-sdk.json')

foreach ($attempt in 1..2) {
    $phase = Run-GrantPhase "debug-$attempt" $debugSdk $true
    $phases += $phase
    $phases | ConvertTo-Json -Depth 5 | Set-Content (Join-Path $evidence 'phases.json')
    if ($phase.actual_test_exit -ne 0 -or -not $phase.debug_runtime_selection_proved) { break }
}
Copy-Item (Join-Path $runner 'Cargo.lock') $evidence
$frozenUnchanged = (Get-FileHash $compiler).Hash -eq $frozenCompilerHash -and
    (Get-FileHash $runtime).Hash -eq $frozenRuntimeHash -and
    (Get-FileHash $targetRuntime).Hash -eq $frozenRuntimeHash
$failed = @($phases | Where-Object {
    $_.actual_test_exit -ne 0 -or ($_.observed_runtime -and -not $_.debug_runtime_selection_proved)
})
@{
    release_qualification = $false
    frozen_sdk_unchanged = $frozenUnchanged
    phases = $phases
    source_delta = 'runtime-observations.patch'
    interpretation = 'Release versus observed debug runtime with fixed release compiler; not original debug-compiler or workspace co-load reproduction'
} | ConvertTo-Json -Depth 6 | Set-Content (Join-Path $evidence 'result.json')
if (-not $frozenUnchanged -or $failed.Count -gt 0) { exit 1 }
exit 0
