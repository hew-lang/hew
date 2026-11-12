$ErrorActionPreference = "Stop"
Set-StrictMode -Version Latest

$installerPath = Join-Path $PSScriptRoot "install.ps1"
. $installerPath -LoadFunctionsOnly

function Assert-Equal {
    param($Expected, $Actual, [string]$Message)
    if ($Expected -ne $Actual) {
        throw "${Message}: expected '${Expected}', got '${Actual}'"
    }
}

function Assert-ThrowsLike {
    param([scriptblock]$Action, [string]$Pattern, [string]$Message)
    try {
        & $Action
    }
    catch {
        if ($_.Exception.Message -like $Pattern) {
            return
        }
        throw "${Message}: unexpected error: $($_.Exception.Message)"
    }
    throw "${Message}: expected an error matching ${Pattern}"
}

function New-ToolchainTree {
    param([string]$Root, [string]$Prefix, [switch]$MissingCompiler, [switch]$CoreOnly, [switch]$MissingLib)
    $bin = Join-Path $Root "bin"
    New-Item -ItemType Directory -Path $bin -Force | Out-Null
    if (-not $MissingCompiler) {
        Set-Content -LiteralPath (Join-Path $bin "hew.exe") -Value "${Prefix}-hew" -NoNewline
    }
    if (-not $CoreOnly) {
        Set-Content -LiteralPath (Join-Path $bin "hew-lsp.exe") -Value "${Prefix}-lsp" -NoNewline
        Set-Content -LiteralPath (Join-Path $bin "hew-observe.exe") -Value "${Prefix}-observe" -NoNewline
    }
    if (-not $MissingLib) {
        $lib = Join-Path $Root "lib"
        New-Item -ItemType Directory -Path $lib -Force | Out-Null
        Set-Content -LiteralPath (Join-Path $lib "hew.lib") -Value "${Prefix}-lib" -NoNewline
    }
    $std = Join-Path $Root "std"
    $completions = Join-Path $Root "completions"
    $include = Join-Path $Root "include"
    $share = Join-Path $Root "share/hew"
    New-Item -ItemType Directory -Path $std -Force | Out-Null
    New-Item -ItemType Directory -Path $completions -Force | Out-Null
    New-Item -ItemType Directory -Path $include -Force | Out-Null
    New-Item -ItemType Directory -Path $share -Force | Out-Null
    Set-Content -LiteralPath (Join-Path $std "prelude.hew") -Value "${Prefix}-std" -NoNewline
    Set-Content -LiteralPath (Join-Path $completions "hew.powershell") -Value "${Prefix}-completion" -NoNewline
    Set-Content -LiteralPath (Join-Path $completions "hew.ps1") -Value "${Prefix}-completion" -NoNewline
    Set-Content -LiteralPath (Join-Path $include "hew.h") -Value "${Prefix}-header" -NoNewline
    Set-Content -LiteralPath (Join-Path $include "hew_host.h") -Value "${Prefix}-host-header" -NoNewline
    Set-Content -LiteralPath (Join-Path $share "hew-gdb.py") -Value "${Prefix}-gdb" -NoNewline
    Set-Content -LiteralPath (Join-Path $share "hew_lldb.py") -Value "${Prefix}-lldb" -NoNewline
    Set-Content -LiteralPath (Join-Path $Root "README.md") -Value "${Prefix}-readme" -NoNewline
}

function New-TransactionFixture {
    param([switch]$Fresh, [switch]$MissingCompiler, [switch]$Bracketed)
    $label = if ($Bracketed) { '[fixture]' } else { 'fixture' }
    $root = Join-Path ([System.IO.Path]::GetTempPath()) "hew-installer-test-${label}-$([System.Guid]::NewGuid().ToString('N'))"
    $install = Join-Path $root "install"
    $source = Join-Path $root "source"
    $stage = Join-Path $root "stage"
    New-Item -ItemType Directory -Path $install -Force | Out-Null
    if (-not $Fresh) {
        New-ToolchainTree -Root $install -Prefix "old" -MissingCompiler:$MissingCompiler
        Set-Content -LiteralPath (Join-Path $install ".hew-version") -Value "0.1.0" -NoNewline
    }
    New-ToolchainTree -Root $source -Prefix "new"
    # Shipped archives contain hew.powershell, not the legacy alias.
    Remove-Item -LiteralPath (Join-Path $source "completions/hew.ps1")
    Stage-ReleaseToolchain -InnerDir $source -StageBin (Join-Path $stage "bin") -StageLib (Join-Path $stage "lib")
    return @{
        Root = $root
        Install = $install
        Source = $source
        StageBin = Join-Path $stage "bin"
        StageLib = Join-Path $stage "lib"
        VersionFile = Join-Path $install ".hew-version"
    }
}

function Get-ToolchainHashes {
    param([string]$Root)
    $hashes = @{}
    foreach ($relative in @("bin/hew.exe", "bin/hew-lsp.exe", "bin/hew-observe.exe", "lib/hew.lib")) {
        $path = Join-Path $Root $relative
        if (Test-Path -LiteralPath $path) {
            $hashes[$relative] = (Get-FileHash -LiteralPath $path -Algorithm SHA256).Hash
        }
    }
    return $hashes
}

function Assert-HashesEqual {
    param($Expected, [string]$Root, [string]$Message)
    $actual = Get-ToolchainHashes -Root $Root
    Assert-Equal $Expected.Count $actual.Count "${Message} file count"
    foreach ($relative in $Expected.Keys) {
        Assert-Equal $Expected[$relative] $actual[$relative] "${Message} ${relative}"
    }
}

function Assert-Version {
    param([string]$VersionFile, [string]$Expected, [string]$Message)
    Assert-Equal $Expected (Get-Content -LiteralPath $VersionFile -Raw) $Message
}

function Assert-Payload {
    param([string]$Root, [string]$Prefix, [string]$Message)
    foreach ($entry in @(
        @("include/hew.h", "${Prefix}-header"),
        @("include/hew_host.h", "${Prefix}-host-header"),
        @("share/hew/hew-gdb.py", "${Prefix}-gdb"),
        @("share/hew/hew_lldb.py", "${Prefix}-lldb"),
        @("completions/hew.powershell", "${Prefix}-completion"),
        @("completions/hew.ps1", "${Prefix}-completion"),
        @("std/prelude.hew", "${Prefix}-std"),
        @("README.md", "${Prefix}-readme")
    )) {
        Assert-Equal $entry[1] (Get-Content -LiteralPath (Join-Path $Root $entry[0]) -Raw) "${Message} $($entry[0])"
    }
}

function Invoke-TestInstall {
    param(
        $Fixture,
        [string]$Version,
        [scriptblock]$MovePath,
        [scriptblock]$CreateDirectory,
        [scriptblock]$CopyPath
    )
    $arguments = @{
        InnerDir = $Fixture.Source
        InstallDir = $Fixture.Install
        StageBin = $Fixture.StageBin
        StageLib = $Fixture.StageLib
        VersionFile = $Fixture.VersionFile
        Version = $Version
    }
    if ($MovePath) { $arguments.MovePath = $MovePath }
    if ($CreateDirectory) { $arguments.CreateDirectory = $CreateDirectory }
    if ($CopyPath) { $arguments.CopyPath = $CopyPath }
    Install-ReleaseToolchain @arguments
}

function Test-ForwardFailureRollback {
    param([int]$FailAt)
    $fixture = New-TransactionFixture
    try {
        $originalHashes = Get-ToolchainHashes -Root $fixture.Install
        $script:moveCount = 0
        $script:failureInjected = $false
        $movePath = {
            param($Source, $Destination)
            $script:moveCount += 1
            if (-not $script:failureInjected -and $script:moveCount -eq $FailAt) {
                $script:failureInjected = $true
                throw "injected move failure ${FailAt}"
            }
            Move-Item -LiteralPath $Source -Destination $Destination -ErrorAction Stop
        }
        Assert-ThrowsLike { Invoke-TestInstall -Fixture $fixture -Version "0.2.0" -MovePath $movePath } "*injected move failure ${FailAt}*" "move ${FailAt}"
        Assert-HashesEqual $originalHashes $fixture.Install "move ${FailAt} rollback"
        Assert-Version $fixture.VersionFile "0.1.0" "move ${FailAt} version"
    }
    finally {
        Remove-Item -LiteralPath $fixture.Root -Recurse -Force -ErrorAction SilentlyContinue
    }
}

# Backup-directory creation must fail before any original is moved.
$createFailure = New-TransactionFixture
try {
    $originalHashes = Get-ToolchainHashes -Root $createFailure.Install
    $createDirectory = { param($Path) throw "injected backup creation failure" }
    Assert-ThrowsLike { Invoke-TestInstall -Fixture $createFailure -Version "0.2.0" -CreateDirectory $createDirectory } "*injected backup creation failure*" "backup creation"
    Assert-HashesEqual $originalHashes $createFailure.Install "backup creation rollback"
    Assert-Version $createFailure.VersionFile "0.1.0" "backup creation version"
}
finally {
    Remove-Item -LiteralPath $createFailure.Root -Recurse -Force -ErrorAction SilentlyContinue
}

# Forward moves: backup bin, backup lib, install bin, install lib.
foreach ($failAt in 1..4) {
    Test-ForwardFailureRollback -FailAt $failAt
}

# A failed rollback restore must retain both recoverable backups and the marker.
$rollbackFailure = New-TransactionFixture
try {
    $originalHashes = Get-ToolchainHashes -Root $rollbackFailure.Install
    $script:moveCount = 0
    $movePath = {
        param($Source, $Destination)
        $script:moveCount += 1
        if ($script:moveCount -eq 4) { throw "injected install-lib failure" }
        if ($script:moveCount -eq 5) { throw "injected restore-lib failure" }
        Move-Item -LiteralPath $Source -Destination $Destination -ErrorAction Stop
    }
    Assert-ThrowsLike { Invoke-TestInstall -Fixture $rollbackFailure -Version "0.2.0" -MovePath $movePath } "*injected restore-lib failure*" "rollback restore"
    Assert-Version $rollbackFailure.VersionFile "0.1.0" "rollback restore version"
    $backup = Get-ChildItem -LiteralPath $rollbackFailure.Install -Directory -Force -Filter ".hew-backup-*" | Select-Object -First 1
    if (-not $backup) { throw "rollback restore: recoverable backup directory missing" }
    Assert-HashesEqual $originalHashes $backup.FullName "recoverable backup"
}
finally {
    Remove-Item -LiteralPath $rollbackFailure.Root -Recurse -Force -ErrorAction SilentlyContinue
}

# Fresh install.
$fresh = New-TransactionFixture -Fresh
try {
    $expectedHashes = Get-ToolchainHashes -Root $fresh.Source
    Invoke-TestInstall -Fixture $fresh -Version "0.2.0"
    Assert-HashesEqual $expectedHashes $fresh.Install "fresh install inventory"
    Assert-Payload $fresh.Install "new" "fresh install payload"
    Assert-Version $fresh.VersionFile "0.2.0" "fresh install version"
}
finally { Remove-Item -LiteralPath $fresh.Root -Recurse -Force -ErrorAction SilentlyContinue }

# Brackets in a user-selected prefix and temporary source are literal path characters.
$bracketed = New-TransactionFixture -Fresh -Bracketed
try {
    Invoke-TestInstall -Fixture $bracketed -Version "0.2.0"
    Assert-HashesEqual (Get-ToolchainHashes -Root $bracketed.Source) $bracketed.Install "bracketed path inventory"
    Assert-Payload $bracketed.Install "new" "bracketed path payload"
    Assert-Version $bracketed.VersionFile "0.2.0" "bracketed path version"
}
finally { Remove-Item -LiteralPath $bracketed.Root -Recurse -Force -ErrorAction SilentlyContinue }

# Upgrade replaces the complete pair and advances the marker.
$upgrade = New-TransactionFixture
try {
    $oldHashes = Get-ToolchainHashes -Root $upgrade.Install
    $expectedHashes = Get-ToolchainHashes -Root $upgrade.Source
    Invoke-TestInstall -Fixture $upgrade -Version "0.2.0"
    $newHashes = Get-ToolchainHashes -Root $upgrade.Install
    if ($oldHashes["bin/hew.exe"] -eq $newHashes["bin/hew.exe"]) { throw "upgrade did not replace binaries" }
    Assert-HashesEqual $expectedHashes $upgrade.Install "upgrade inventory"
    Assert-Payload $upgrade.Install "new" "upgrade payload"
    Assert-Version $upgrade.VersionFile "0.2.0" "upgrade version"
}
finally { Remove-Item -LiteralPath $upgrade.Root -Recurse -Force -ErrorAction SilentlyContinue }

# Same-version missing-compiler repair must be detected and restore the toolchain.
$repair = New-TransactionFixture -MissingCompiler
try {
    $expectedHashes = Get-ToolchainHashes -Root $repair.Source
    Assert-Equal "hew.exe" (Get-MissingRequiredBinary -InstallDir $repair.Install) "repair detection"
    Invoke-TestInstall -Fixture $repair -Version "0.1.0"
    Assert-Equal $null (Get-MissingRequiredBinary -InstallDir $repair.Install) "repair completion"
    Assert-HashesEqual $expectedHashes $repair.Install "repair inventory"
    Assert-Payload $repair.Install "new" "repair payload"
    Assert-Version $repair.VersionFile "0.1.0" "repair version"
}
finally { Remove-Item -LiteralPath $repair.Root -Recurse -Force -ErrorAction SilentlyContinue }

# A staging copy failure leaves the existing installation untouched; retry repairs it.
$payloadRetry = New-TransactionFixture -MissingCompiler
try {
    $oldPayloadHash = (Get-FileHash -LiteralPath (Join-Path $payloadRetry.Install "std/prelude.hew") -Algorithm SHA256).Hash
    $newPayloadHash = (Get-FileHash -LiteralPath (Join-Path $payloadRetry.Source "std/prelude.hew") -Algorithm SHA256).Hash

    $copyFailure = { param($Source, $Destination, $Recurse) throw "injected payload copy failure" }
    Assert-ThrowsLike {
        Invoke-TestInstall -Fixture $payloadRetry -Version "0.1.0" -CopyPath $copyFailure
    } "*injected payload copy failure*" "payload copy"
    Assert-Equal $false (Test-ToolchainInstallationComplete -InstallDir $payloadRetry.Install -Version "0.1.0") "payload failure must not skip retry"
    Assert-Equal $oldPayloadHash (Get-FileHash -LiteralPath (Join-Path $payloadRetry.Install "std/prelude.hew") -Algorithm SHA256).Hash "failed payload remains old"

    # Model the next installer run: restage, swap, copy payload, then complete.
    Stage-ReleaseToolchain -InnerDir $payloadRetry.Source -StageBin $payloadRetry.StageBin -StageLib $payloadRetry.StageLib
    Invoke-TestInstall -Fixture $payloadRetry -Version "0.1.0"
    Assert-Equal $true (Test-ToolchainInstallationComplete -InstallDir $payloadRetry.Install -Version "0.1.0") "payload retry completion"
    Assert-Equal $newPayloadHash (Get-FileHash -LiteralPath (Join-Path $payloadRetry.Install "std/prelude.hew") -Algorithm SHA256).Hash "payload retry repair"
}
finally { Remove-Item -LiteralPath $payloadRetry.Root -Recurse -Force -ErrorAction SilentlyContinue }

# A failure during the payload swap restores all prior payload and toolchain files.
$payloadRollback = New-TransactionFixture
try {
    $oldHashes = Get-ToolchainHashes -Root $payloadRollback.Install
    $failShareInstall = {
        param($Source, $Destination)
        if ($Source -like '*/payload/share' -or $Source -like '*\payload\share') {
            throw "injected share install failure"
        }
        Move-Item -LiteralPath $Source -Destination $Destination -ErrorAction Stop
    }
    Assert-ThrowsLike {
        Invoke-TestInstall -Fixture $payloadRollback -Version "0.2.0" -MovePath $failShareInstall
    } "*injected share install failure*" "payload rollback"
    Assert-HashesEqual $oldHashes $payloadRollback.Install "payload rollback toolchain"
    Assert-Payload $payloadRollback.Install "old" "payload rollback"
    Assert-Version $payloadRollback.VersionFile "0.1.0" "payload rollback version"
}
finally { Remove-Item -LiteralPath $payloadRollback.Root -Recurse -Force -ErrorAction SilentlyContinue }

# The v0.9.2 release inventory contains only hew; optional tools may be absent.
$coreOnly = New-TransactionFixture
try {
    $coreSource = Join-Path $coreOnly.Root "core-only"
    New-ToolchainTree -Root $coreSource -Prefix "core" -CoreOnly
    $coreStageBin = Join-Path $coreOnly.Root "core-stage/bin"
    $coreStageLib = Join-Path $coreOnly.Root "core-stage/lib"
    Stage-ReleaseToolchain -InnerDir $coreSource -StageBin $coreStageBin -StageLib $coreStageLib
    Assert-Equal $true (Test-Path -LiteralPath (Join-Path $coreStageBin "hew.exe")) "core-only compiler staged"
    Assert-Equal $false (Test-Path -LiteralPath (Join-Path $coreStageBin "hew-lsp.exe")) "core-only LSP absent"
    Assert-Equal $false (Test-Path -LiteralPath (Join-Path $coreStageBin "hew-observe.exe")) "core-only observer absent"
}
finally { Remove-Item -LiteralPath $coreOnly.Root -Recurse -Force -ErrorAction SilentlyContinue }

# Missing required staged inventory fails before the live installation is touched.
foreach ($missing in @("compiler", "lib")) {
    $fixture = New-TransactionFixture
    try {
        $originalHashes = Get-ToolchainHashes -Root $fixture.Install
        $badSource = Join-Path $fixture.Root "bad-${missing}"
        New-ToolchainTree -Root $badSource -Prefix "bad" -MissingCompiler:($missing -eq "compiler") -MissingLib:($missing -eq "lib")
        Assert-ThrowsLike {
            Stage-ReleaseToolchain -InnerDir $badSource -StageBin (Join-Path $fixture.Root "bad-stage-${missing}/bin") -StageLib (Join-Path $fixture.Root "bad-stage-${missing}/lib")
        } "*missing required*" "missing staged ${missing}"
        Assert-HashesEqual $originalHashes $fixture.Install "missing staged ${missing}"
        Assert-Version $fixture.VersionFile "0.1.0" "missing staged ${missing} version"
    }
    finally { Remove-Item -LiteralPath $fixture.Root -Recurse -Force -ErrorAction SilentlyContinue }
}

# Checksum download, entry lookup, and mismatch all fail closed.
$checksumRoot = Join-Path ([System.IO.Path]::GetTempPath()) "hew-checksum-test-$([System.Guid]::NewGuid().ToString('N'))"
New-Item -ItemType Directory -Path $checksumRoot -Force | Out-Null
$checksumInstall = New-TransactionFixture
try {
    $originalHashes = Get-ToolchainHashes -Root $checksumInstall.Install
    $archiveName = "hew-test.zip"
    $archivePath = Join-Path $checksumRoot $archiveName
    $checksumsPath = Join-Path $checksumRoot "checksums.txt"
    Set-Content -LiteralPath $archivePath -Value "archive bytes" -NoNewline
    $hash = (Get-FileHash -LiteralPath $archivePath -Algorithm SHA256).Hash.ToLower()

    $downloadFailure = { param($Url, $Destination) throw "injected checksum download failure" }
    Assert-ThrowsLike {
        Get-AndVerifyArchiveChecksum -ChecksumsUrl "https://invalid/checksums" -ChecksumsPath $checksumsPath -ArchivePath $archivePath -ArchiveName $archiveName -DownloadFile $downloadFailure
    } "*injected checksum download failure*" "checksum download"
    Assert-HashesEqual $originalHashes $checksumInstall.Install "checksum download"
    Assert-Version $checksumInstall.VersionFile "0.1.0" "checksum download version"

    Set-Content -LiteralPath $checksumsPath -Value "${hash}  another.zip" -NoNewline
    Assert-ThrowsLike {
        Confirm-ArchiveChecksum -ChecksumsPath $checksumsPath -ArchivePath $archivePath -ArchiveName $archiveName
    } "*${archiveName}*" "checksum missing entry"
    Assert-HashesEqual $originalHashes $checksumInstall.Install "checksum missing entry"
    Assert-Version $checksumInstall.VersionFile "0.1.0" "checksum missing-entry version"

    Set-Content -LiteralPath $checksumsPath -Value "$('0' * 64)  ${archiveName}" -NoNewline
    Assert-ThrowsLike {
        Confirm-ArchiveChecksum -ChecksumsPath $checksumsPath -ArchivePath $archivePath -ArchiveName $archiveName
    } "*${hash}*" "checksum mismatch"
    Assert-HashesEqual $originalHashes $checksumInstall.Install "checksum mismatch"
    Assert-Version $checksumInstall.VersionFile "0.1.0" "checksum mismatch version"

    Set-Content -LiteralPath $checksumsPath -Value "${hash}  ${archiveName}" -NoNewline
    Confirm-ArchiveChecksum -ChecksumsPath $checksumsPath -ArchivePath $archivePath -ArchiveName $archiveName
}
finally {
    Remove-Item -LiteralPath $checksumRoot -Recurse -Force -ErrorAction SilentlyContinue
    Remove-Item -LiteralPath $checksumInstall.Root -Recurse -Force -ErrorAction SilentlyContinue
}

# Exercise release selection and dry-run through the actual installer entry point.
# The child process provides a deterministic GitHub response and performs no download.
$selectionRoot = Join-Path ([System.IO.Path]::GetTempPath()) "hew-selection-test-$([System.Guid]::NewGuid().ToString('N'))"
New-Item -ItemType Directory -Path $selectionRoot -Force | Out-Null
try {
    $harness = Join-Path $selectionRoot "selection.ps1"
    @'
param([string]$Installer, [string]$Prefix, [string]$Mode)
function Invoke-RestMethod {
    param($Uri, $Headers, [switch]$UseBasicParsing)
    @(
        [pscustomobject]@{ tag_name = 'v0.9.9' },
        [pscustomobject]@{ tag_name = 'v0.11.0-rc1' },
        [pscustomobject]@{ tag_name = 'v0.10.0' },
        [pscustomobject]@{ tag_name = 'v0.11.0-rc2' }
    )
}
if ($Mode -eq 'stable') { . $Installer -Stable -DryRun -Prefix $Prefix }
else { . $Installer -DryRun -Prefix $Prefix }
'@ | Set-Content -LiteralPath $harness
    $shell = (Get-Process -Id $PID).Path
    $helpOutput = & $shell -NoProfile -File $installerPath -Help 2>&1 | Out-String
    Assert-Equal 0 $LASTEXITCODE 'help exit'
    foreach ($expected in @('-Stable', '-DryRun', 'https://hew.sh/install.ps1')) {
        if (-not $helpOutput.Contains($expected)) { throw "Help is missing ${expected}: $helpOutput" }
    }
    foreach ($case in @(@('default', 'v0.11.0-rc2'), @('stable', 'v0.10.0'))) {
        $prefix = Join-Path $selectionRoot $case[0]
        $output = & $shell -NoProfile -File $harness -Installer $installerPath -Prefix $prefix -Mode $case[0] 2>&1 | Out-String
        Assert-Equal 0 $LASTEXITCODE "$($case[0]) dry-run exit"
        if ($output -notmatch [regex]::Escape($case[1])) { throw "Unexpected $($case[0]) dry-run output: $output" }
        Assert-Equal $false (Test-Path -LiteralPath $prefix) "$($case[0]) dry-run created an installation"
    }
    $prefix = Join-Path $selectionRoot 'conflict'
    $output = & $shell -NoProfile -File $installerPath -Stable -Version '0.10.0' -DryRun -Prefix $prefix 2>&1 | Out-String
    if ($LASTEXITCODE -eq 0 -or $output -notmatch 'mutually exclusive') { throw "Conflicting options were accepted: $output" }
    Assert-Equal $false (Test-Path -LiteralPath $prefix) 'conflicting options created an installation'
}
finally {
    Remove-Item -LiteralPath $selectionRoot -Recurse -Force -ErrorAction SilentlyContinue
}

Write-Host "Windows installer transaction and release-selection tests passed"
# The final negative test intentionally leaves a nonzero native exit status.
exit 0
