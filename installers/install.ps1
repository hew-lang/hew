#!/usr/bin/env pwsh
#Requires -Version 5.1

<#
.SYNOPSIS
    Installs Hew from GitHub releases.

.DESCRIPTION
    Downloads and installs the Hew programming language compiler and runtime
    from GitHub releases. Verifies SHA-256 checksums and adds the install
    directory to your PATH.

    Supports both .zip and .tar.gz archive formats. Prefers .zip on Windows
    but falls back to .tar.gz when the .zip asset is not available.

    Works on Windows PowerShell 5.1+ and PowerShell Core 7+.

    Usage via web:
      irm https://hew.sh/install.ps1 | iex

.PARAMETER Version
    Specific version to install (e.g. "0.1.0"). Default: newest release
    including release candidates.

.PARAMETER Stable
    Install the newest final (non-rc) release instead of the newest release
    candidate.

.PARAMETER Prefix
    Installation directory. Defaults to $env:HEW_HOME or $env:USERPROFILE\.hew

.PARAMETER DryRun
    Resolve the version and print what would be installed, without
    downloading or installing anything.

.PARAMETER Help
    Show this help message.

.EXAMPLE
    irm https://hew.sh/install.ps1 | iex

.EXAMPLE
    .\install.ps1 -Version 0.1.0

.EXAMPLE
    .\install.ps1 -Stable

.EXAMPLE
    .\install.ps1 -Prefix C:\tools\hew
#>

# By default install the newest release, including release candidates.
# Pass -Stable to select the newest final release.

param(
    [string]$Version,
    [switch]$Stable,
    [string]$Prefix,
    [switch]$DryRun,
    [switch]$Help,
    [switch]$LoadFunctionsOnly
)

if ($Version -and $Stable) {
    Write-Host ""
    Write-Host "  error: -Version and -Stable are mutually exclusive" -ForegroundColor Red
    Write-Host ""
    exit 1
}

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$GITHUB_ORG = "hew-lang"
$GITHUB_REPO = "hew"

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

function Write-Banner {
    $banner = @"

    _   _
   | | | | _____      __
   | |_| |/ _ \ \ /\ / /
   |  _  |  __/\ V  V /
   |_| |_|\___| \_/\_/

"@
    Write-Host $banner -ForegroundColor Cyan
}

function Write-Step {
    param(
        [string]$Label,
        [string]$Status = "",
        [switch]$NoNewline
    )
    $padding = 18 - $Label.Length - 3  # match bash: %-18s with "..."
    if ($padding -lt 1) { $padding = 1 }
    $spaces = " " * $padding
    if ($NoNewline) {
        Write-Host "  ${Label}...${spaces}" -NoNewline
    }
    elseif ($Status) {
        Write-Host "  ${Label}...${spaces}" -NoNewline
        Write-Host $Status -ForegroundColor Green
    }
}

function Write-ErrorAndExit {
    param([string]$Message)
    Write-Host ""
    Write-Host "  error: $Message" -ForegroundColor Red
    Write-Host ""
    exit 1
}

function Show-Help {
    Write-Banner
    Write-Host "  Hew installer" -ForegroundColor White
    Write-Host ""
    Write-Host "  USAGE:" -ForegroundColor White
    Write-Host "    install.ps1 [OPTIONS]"
    Write-Host ""
    Write-Host "  OPTIONS:" -ForegroundColor White
    Write-Host "    -Version <ver>   Install a specific version (e.g. 0.9.2). Default: latest"
    Write-Host "    -Prefix  <dir>   Installation directory. Default: `$env:HEW_HOME or `$env:USERPROFILE\.hew"
    Write-Host "    -Help            Show this help message"
    Write-Host ""
    Write-Host "  EXAMPLES:" -ForegroundColor White
    Write-Host "    irm https://install.hew.sh/install.ps1 | iex"
    Write-Host "    .\install.ps1 -Version 0.9.2"
    Write-Host "    .\install.ps1 -Prefix C:\tools\hew"
    Write-Host ""
}

# ---------------------------------------------------------------------------
# Architecture detection (compatible with PS 5.1 and PS 7+)
# ---------------------------------------------------------------------------

function Get-Platform {
    $detectedArch = $null

    # Try .NET RuntimeInformation first (PS 6+)
    try {
        $osArch = [System.Runtime.InteropServices.RuntimeInformation]::OSArchitecture
        if ($osArch -eq [System.Runtime.InteropServices.Architecture]::X64) {
            $detectedArch = "x86_64"
        }
        elseif ($osArch -eq [System.Runtime.InteropServices.Architecture]::Arm64) {
            $detectedArch = "aarch64"
        }
    }
    catch {
        # RuntimeInformation not available — fall back to environment variable
    }

    # Fallback for Windows PowerShell 5.1
    if (-not $detectedArch) {
        $envArch = $env:PROCESSOR_ARCHITECTURE
        switch ($envArch) {
            "AMD64" { $detectedArch = "x86_64" }
            "ARM64" { $detectedArch = "aarch64" }
            default { Write-ErrorAndExit "Unsupported architecture: $envArch" }
        }
    }

    return "windows-$detectedArch"
}

# ---------------------------------------------------------------------------
# Archive extraction (handles both .zip and .tar.gz)
# ---------------------------------------------------------------------------

function Expand-TarGz {
    param(
        [string]$Path,
        [string]$DestinationPath
    )
    New-Item -ItemType Directory -Path $DestinationPath -Force | Out-Null

    if ($PSVersionTable.PSVersion.Major -ge 7) {
        # PowerShell 7+ ships with tar
        tar -xzf $Path -C $DestinationPath
        if ($LASTEXITCODE -ne 0) {
            throw "tar extraction failed with exit code $LASTEXITCODE"
        }
    }
    elseif (Get-Command tar -ErrorAction SilentlyContinue) {
        # Windows 10 1803+ includes tar.exe
        tar -xzf $Path -C $DestinationPath
        if ($LASTEXITCODE -ne 0) {
            throw "tar extraction failed with exit code $LASTEXITCODE"
        }
    }
    else {
        throw ".tar.gz extraction requires tar (available on Windows 10 1803+) or PowerShell 7+"
    }
}

function Get-MissingRequiredBinary {
    param([Parameter(Mandatory = $true)][string]$InstallDir)

    $binary = "hew.exe"
    if (-not (Test-Path -LiteralPath (Join-Path $InstallDir "bin/$binary"))) {
        return $binary
    }
    return $null
}

function Test-ToolchainInstallationComplete {
    param(
        [Parameter(Mandatory = $true)][string]$InstallDir,
        [Parameter(Mandatory = $true)][string]$Version
    )

    $versionFile = Join-Path $InstallDir ".hew-version"
    if (-not (Test-Path -LiteralPath $versionFile)) {
        return $false
    }
    $installedVersion = (Get-Content -LiteralPath $versionFile -Raw).Trim()
    return $installedVersion -eq $Version -and -not (Get-MissingRequiredBinary -InstallDir $InstallDir)
}

function Confirm-ArchiveChecksum {
    param(
        [Parameter(Mandatory = $true)][string]$ChecksumsPath,
        [Parameter(Mandatory = $true)][string]$ArchivePath,
        [Parameter(Mandatory = $true)][string]$ArchiveName
    )

    $expectedHash = $null
    foreach ($line in Get-Content -LiteralPath $ChecksumsPath) {
        if ($line -match "^([0-9a-fA-F]{64})\s+(.+)$") {
            $hash = $Matches[1]
            $file = $Matches[2].Trim()
            if ($file -eq $ArchiveName) {
                $expectedHash = $hash.ToLower()
                break
            }
        }
    }
    if (-not $expectedHash) {
        throw "Checksum for ${ArchiveName} not found in $(Split-Path -Leaf $ChecksumsPath)"
    }

    $actualHash = (Get-FileHash -LiteralPath $ArchivePath -Algorithm SHA256).Hash.ToLower()
    if ($actualHash -ne $expectedHash) {
        throw "SHA-256 mismatch!`n  Expected: ${expectedHash}`n  Got:      ${actualHash}"
    }
}

function Get-AndVerifyArchiveChecksum {
    param(
        [Parameter(Mandatory = $true)][string]$ChecksumsUrl,
        [Parameter(Mandatory = $true)][string]$ChecksumsPath,
        [Parameter(Mandatory = $true)][string]$ArchivePath,
        [Parameter(Mandatory = $true)][string]$ArchiveName,
        [scriptblock]$DownloadFile = {
            param($Url, $Destination)
            Invoke-WebRequest -Uri $Url -OutFile $Destination -UseBasicParsing
        }
    )

    & $DownloadFile $ChecksumsUrl $ChecksumsPath
    Confirm-ArchiveChecksum `
        -ChecksumsPath $ChecksumsPath `
        -ArchivePath $ArchivePath `
        -ArchiveName $ArchiveName
}

function Stage-ReleaseToolchain {
    param(
        [Parameter(Mandatory = $true)][string]$InnerDir,
        [Parameter(Mandatory = $true)][string]$StageBin,
        [Parameter(Mandatory = $true)][string]$StageLib
    )

    New-Item -ItemType Directory -Path $StageBin -Force | Out-Null
    New-Item -ItemType Directory -Path $StageLib -Force | Out-Null

    foreach ($binary in @("hew", "hew-lsp", "hew-observe")) {
        $source = Join-Path $InnerDir "bin/${binary}.exe"
        $destinationName = "${binary}.exe"
        if (-not (Test-Path -LiteralPath $source)) {
            $source = Join-Path $InnerDir "bin/${binary}"
            $destinationName = $binary
        }
        if (-not (Test-Path -LiteralPath $source)) {
            if ($binary -eq "hew") {
                throw "Release archive is missing required binary: ${binary}"
            }
            continue
        }
        Copy-Item -LiteralPath $source -Destination (Join-Path $StageBin $destinationName) -Force
    }

    $libSource = Join-Path $InnerDir "lib"
    if (-not (Test-Path -LiteralPath $libSource)) {
        throw "Release archive is missing required lib directory."
    }
    Copy-Item -Path (Join-Path $libSource "*") -Destination $StageLib -Recurse -Force
}

function Install-ReleasePayload {
    param(
        [Parameter(Mandatory = $true)][string]$InnerDir,
        [Parameter(Mandatory = $true)][string]$InstallDir,
        [scriptblock]$CopyPath = {
            param($Source, $Destination, $Recurse)
            if ($Recurse) {
                Copy-Item -Path $Source -Destination $Destination -Recurse -Force -ErrorAction Stop
            }
            else {
                Copy-Item -LiteralPath $Source -Destination $Destination -Force -ErrorAction Stop
            }
        }
    )

    foreach ($directory in @("std", "completions")) {
        New-Item -ItemType Directory -Path (Join-Path $InstallDir $directory) -Force | Out-Null
        $source = Join-Path $InnerDir $directory
        if (Test-Path -LiteralPath $source) {
            & $CopyPath (Join-Path $source "*") (Join-Path $InstallDir $directory) $true
        }
    }

    foreach ($file in @("LICENSE-MIT", "LICENSE-APACHE", "NOTICE", "README.md")) {
        $source = Join-Path $InnerDir $file
        if (Test-Path -LiteralPath $source) {
            & $CopyPath $source (Join-Path $InstallDir $file) $false
        }
    }
}

function Install-StagedToolchain {
    param(
        [Parameter(Mandatory = $true)]
        [string]$InstallDir,
        [Parameter(Mandatory = $true)]
        [string]$StageBin,
        [Parameter(Mandatory = $true)]
        [string]$StageLib,
        [Parameter(Mandatory = $true)]
        [string]$VersionFile,
        [scriptblock]$CreateDirectory = {
            param($Path)
            New-Item -ItemType Directory -Path $Path -Force -ErrorAction Stop | Out-Null
        },
        [scriptblock]$MovePath = {
            param($Source, $Destination)
            Move-Item -LiteralPath $Source -Destination $Destination -ErrorAction Stop
        },
        [scriptblock]$RemovePath = {
            param($Path)
            Remove-Item -LiteralPath $Path -Recurse -Force -ErrorAction Stop
        }
    )

    $binDir = Join-Path $InstallDir "bin"
    $libDir = Join-Path $InstallDir "lib"
    $transactionId = [System.Guid]::NewGuid().ToString('N').Substring(0,8)
    $backupRoot = Join-Path $InstallDir ".hew-backup-${transactionId}"
    $backupBin = Join-Path $backupRoot "bin"
    $backupLib = Join-Path $backupRoot "lib"

    # Track each completed transition. Rollback must never remove a destination
    # that still contains an untouched original directory.
    $binBackedUp = $false
    $libBackedUp = $false
    $binInstalled = $false
    $libInstalled = $false
    $versionFileExisted = Test-Path -LiteralPath $VersionFile
    $previousVersion = if ($versionFileExisted) {
        Get-Content -LiteralPath $VersionFile -Raw
    }
    else {
        $null
    }

    & $CreateDirectory $backupRoot
    try {
        if (Test-Path -LiteralPath $binDir) {
            & $MovePath $binDir $backupBin
            $binBackedUp = $true
        }
        if (Test-Path -LiteralPath $libDir) {
            & $MovePath $libDir $backupLib
            $libBackedUp = $true
        }

        & $MovePath $StageBin $binDir
        $binInstalled = $true
        & $MovePath $StageLib $libDir
        $libInstalled = $true
        # A completed bin/lib swap is not a completed installation. Invalidate
        # the marker until the remaining payload has copied successfully so a
        # failed payload copy cannot make the next run exit early.
        Set-Content -LiteralPath $VersionFile -Value "incomplete-${transactionId}" -NoNewline
    }
    catch {
        $installError = $_
        try {
            if ($libInstalled -and (Test-Path -LiteralPath $libDir)) {
                & $RemovePath $libDir
            }
            if ($binInstalled -and (Test-Path -LiteralPath $binDir)) {
                & $RemovePath $binDir
            }
            if ($libBackedUp -and (Test-Path -LiteralPath $backupLib)) {
                & $MovePath $backupLib $libDir
            }
            if ($binBackedUp -and (Test-Path -LiteralPath $backupBin)) {
                & $MovePath $backupBin $binDir
            }
            if ($versionFileExisted) {
                Set-Content -LiteralPath $VersionFile -Value $previousVersion -NoNewline
            }
            elseif (Test-Path -LiteralPath $VersionFile) {
                Remove-Item -LiteralPath $VersionFile -Force -ErrorAction Stop
            }
        }
        catch {
            throw "Install failed and rollback also failed. Previous files remain under ${backupRoot}. Install error: ${installError}; rollback error: $_"
        }

        if (Test-Path -LiteralPath $backupRoot) {
            Remove-Item -LiteralPath $backupRoot -Recurse -Force -ErrorAction SilentlyContinue
        }
        throw $installError
    }

    if (Test-Path -LiteralPath $backupRoot) {
        Remove-Item -LiteralPath $backupRoot -Recurse -Force
    }
}

function Install-ReleaseToolchain {
    param(
        [Parameter(Mandatory = $true)][string]$InnerDir,
        [Parameter(Mandatory = $true)][string]$InstallDir,
        [Parameter(Mandatory = $true)][string]$StageBin,
        [Parameter(Mandatory = $true)][string]$StageLib,
        [Parameter(Mandatory = $true)][string]$VersionFile,
        [Parameter(Mandatory = $true)][string]$Version,
        [scriptblock]$CreateDirectory,
        [scriptblock]$MovePath,
        [scriptblock]$RemovePath,
        [scriptblock]$CopyPath
    )

    $toolchainArguments = @{
        InstallDir = $InstallDir
        StageBin = $StageBin
        StageLib = $StageLib
        VersionFile = $VersionFile
    }
    if ($CreateDirectory) { $toolchainArguments.CreateDirectory = $CreateDirectory }
    if ($MovePath) { $toolchainArguments.MovePath = $MovePath }
    if ($RemovePath) { $toolchainArguments.RemovePath = $RemovePath }
    Install-StagedToolchain @toolchainArguments

    $payloadArguments = @{
        InnerDir = $InnerDir
        InstallDir = $InstallDir
    }
    if ($CopyPath) { $payloadArguments.CopyPath = $CopyPath }
    Install-ReleasePayload @payloadArguments

    # This is the completion marker: write it only after every required
    # toolchain transition and all available payload copies succeed.
    Set-Content -LiteralPath $VersionFile -Value $Version -NoNewline
}

function Select-NewestTag {
    param([string[]]$Tags)
    # $Tags: 'X.Y.Z' or 'X.Y.Z-rcN' strings, no leading 'v'.
    $best = $null
    $bestKey = $null
    foreach ($tag in $Tags) {
        if ($tag -notmatch '^(\d+)\.(\d+)\.(\d+)(-rc(\d+))?$') { continue }
        $major = [int]$Matches[1]
        $minor = [int]$Matches[2]
        $patch = [int]$Matches[3]
        # A final release outranks any rc of the same major.minor.patch.
        $rc = if ($Matches[4]) { [int]$Matches[5] } else { 999999 }
        $key = "{0:D6}.{1:D6}.{2:D6}.{3:D6}" -f $major, $minor, $patch, $rc
        if (-not $bestKey -or $key -gt $bestKey) {
            $bestKey = $key
            $best = $tag
        }
    }
    return $best
}


# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

if ($LoadFunctionsOnly) {
    return
}

if ($Help) {
    Show-Help
    exit 0
}

Write-Banner

# Detect platform
$platform = Get-Platform

# Resolve install directory: -Prefix > $env:HEW_HOME > $env:USERPROFILE\.hew
if ($Prefix) {
    $InstallDir = $Prefix
}
elseif ($env:HEW_HOME) {
    $InstallDir = $env:HEW_HOME
}
else {
    $InstallDir = Join-Path $env:USERPROFILE ".hew"
}

$BinDir = Join-Path $InstallDir "bin"

# ---------------------------------------------------------------------------
# Resolve version
#
# Picks the newest tag by semver (major.minor.patch[-rcN]), not by GitHub's
# release-creation order, since a hotfix tagged later could otherwise shadow
# a newer version. Both the default (rc-inclusive) and -Stable pick from the
# same /releases listing so there is one authority for "what tags exist";
# -Stable just drops any tag containing '-', mirroring how the release
# workflow itself decides final vs. pre-release, and how install.sh's
# --stable behaves.
#
# SHORTCUT: the semver comparison below only understands this repo's actual
# tag scheme, `vX.Y.Z` or `vX.Y.Z-rcN` — WHY: that is the only scheme the
# release workflow emits (release.yml sets `prerelease` from `contains(tag,
# '-rc')`); WHEN this breaks: a tag with any other prerelease label (e.g.
# `-beta1`) would sort incorrectly; WHAT the real fix looks like: a full
# SemVer 2.0.0 §11 precedence comparison if the tag scheme ever grows more
# prerelease kinds.
# ---------------------------------------------------------------------------

if (-not $Version) {
    Write-Step -Label "Fetching version" -NoNewline
    try {
        # per_page=100 is a SHORTCUT: WHY — the repo has well under 100
        # release tags today, so one unpaginated page covers every
        # candidate; WHEN this breaks — release count crosses 100; WHAT the
        # real fix looks like — follow the API's Link: rel="next" header
        # across pages.
        $releasesUrl = "https://api.github.com/repos/${GITHUB_ORG}/${GITHUB_REPO}/releases?per_page=100"
        $headers = @{ "Accept" = "application/vnd.github.v3+json" }
        if ($env:GITHUB_TOKEN) {
            $headers["Authorization"] = "token $env:GITHUB_TOKEN"
        }
        $releases = Invoke-RestMethod -Uri $releasesUrl -Headers $headers -UseBasicParsing
        $tags = $releases | ForEach-Object { $_.tag_name -replace '^v', '' } | Where-Object { $_ -match '^\d' }

        if ($Stable) {
            # Final releases only, matching the workflow's own final-vs-rc test.
            $tags = $tags | Where-Object { $_ -notmatch '-' }
        }

        $Version = Select-NewestTag -Tags $tags
        if (-not $Version) {
            Write-Host "failed" -ForegroundColor Red
            Write-ErrorAndExit "Could not determine a version to install from GitHub releases"
        }
        Write-Host "done" -ForegroundColor Green
    }
    catch {
        Write-Host "failed" -ForegroundColor Red
        Write-ErrorAndExit "Failed to fetch releases from GitHub: $_"
    }
}
else {
    # Strip leading 'v' if provided
    $Version = $Version -replace '^v', ''
}

if ($DryRun) {
    $dryBaseUrl = "https://github.com/${GITHUB_ORG}/${GITHUB_REPO}/releases/download/v${Version}"
    Write-Host ""
    Write-Host "  tag:      v${Version}"
    Write-Host "  platform: ${platform}"
    Write-Host "  archive:  ${dryBaseUrl}/hew-v${Version}-${platform}.zip (or .tar.gz)"
    Write-Host "  prefix:   ${InstallDir}"
    exit 0
}

Write-Host "  Installing Hew v${Version} (${platform})" -ForegroundColor White
Write-Host ""

# ---------------------------------------------------------------------------
# Check if same version is already installed
# ---------------------------------------------------------------------------

$versionFile = Join-Path $InstallDir ".hew-version"
if (Test-ToolchainInstallationComplete -InstallDir $InstallDir -Version $Version) {
    Write-Host "  Hew v${Version} is already installed at ${InstallDir}" -ForegroundColor Yellow
    Write-Host ""
    Write-Host "  To reinstall, remove $versionFile and run again."
    Write-Host ""
    exit 0
}
if (Test-Path -LiteralPath $versionFile) {
    $installedVersion = (Get-Content $versionFile -Raw).Trim()
    if ($installedVersion -eq $Version) {
        $missingBinary = Get-MissingRequiredBinary -InstallDir $InstallDir
        Write-Host "  Hew v${Version} is missing ${missingBinary}; restoring the release toolchain." -ForegroundColor Yellow
        Write-Host ""
    }
}

# ---------------------------------------------------------------------------
# Build download URLs — prefer .zip, fall back to .tar.gz
# ---------------------------------------------------------------------------

$baseUrl = "https://github.com/${GITHUB_ORG}/${GITHUB_REPO}/releases/download/v${Version}"
$archiveZip = "hew-v${Version}-${platform}.zip"
$archiveTarGz = "hew-v${Version}-${platform}.tar.gz"
$checksumsName = "hew-v${Version}-checksums.txt"

# Create temp directory
$tempDir = Join-Path ([System.IO.Path]::GetTempPath()) "hew-install-$([System.Guid]::NewGuid().ToString('N').Substring(0,8))"
New-Item -ItemType Directory -Path $tempDir -Force | Out-Null

try {
    # ------------------------------------------------------------------
    # Download archive (.zip preferred, .tar.gz fallback)
    # ------------------------------------------------------------------
    Write-Step -Label "Downloading" -NoNewline
    $archiveName = $null
    $archivePath = $null
    $savedProgressPreference = $ProgressPreference
    try {
        $ProgressPreference = 'SilentlyContinue'

        # Try .zip first
        $archiveName = $archiveZip
        $archivePath = Join-Path $tempDir $archiveName
        try {
            Invoke-WebRequest -Uri "${baseUrl}/${archiveName}" -OutFile $archivePath -UseBasicParsing
        }
        catch {
            # Fall back to .tar.gz
            $archiveName = $archiveTarGz
            $archivePath = Join-Path $tempDir $archiveName
            Invoke-WebRequest -Uri "${baseUrl}/${archiveName}" -OutFile $archivePath -UseBasicParsing
        }

        Write-Host "done" -ForegroundColor Green
    }
    catch {
        Write-Host "failed" -ForegroundColor Red
        Write-ErrorAndExit "Failed to download Hew v${Version} for ${platform}: $_"
    }
    finally {
        $ProgressPreference = $savedProgressPreference
    }

    # ------------------------------------------------------------------
    # Download and verify SHA-256 checksum
    # ------------------------------------------------------------------
    Write-Step -Label "Verifying" -NoNewline
    $checksumsPath = Join-Path $tempDir $checksumsName
    try {
        $ProgressPreference = 'SilentlyContinue'
        Get-AndVerifyArchiveChecksum `
            -ChecksumsUrl "${baseUrl}/${checksumsName}" `
            -ChecksumsPath $checksumsPath `
            -ArchivePath $archivePath `
            -ArchiveName $archiveName

        Write-Host "done" -ForegroundColor Green
    }
    catch {
        Write-Host "failed" -ForegroundColor Red
        Write-ErrorAndExit "Failed to download or verify ${checksumsName}: $_"
    }
    finally {
        $ProgressPreference = $savedProgressPreference
    }

    # ------------------------------------------------------------------
    # Extract archive
    # ------------------------------------------------------------------
    Write-Step -Label "Extracting" -NoNewline
    try {
        $extractDir = Join-Path $tempDir "extracted"

        if ($archiveName.EndsWith(".zip")) {
            Expand-Archive -Path $archivePath -DestinationPath $extractDir -Force
        }
        elseif ($archiveName.EndsWith(".tar.gz")) {
            Expand-TarGz -Path $archivePath -DestinationPath $extractDir
        }
        else {
            throw "Unknown archive format: $archiveName"
        }

        # Find the inner directory (e.g. hew-v0.9.2-windows-x86_64/)
        $innerDir = Get-ChildItem -Path $extractDir -Directory | Select-Object -First 1
        if (-not $innerDir) {
            Write-Host "failed" -ForegroundColor Red
            Write-ErrorAndExit "Archive does not contain expected directory structure."
        }

        # Stage the required toolchain before touching the current installation.
        # Keeping the staging and backup directories under InstallDir also keeps
        # the directory renames on the same volume.
        New-Item -ItemType Directory -Path $InstallDir -Force | Out-Null
        $transactionId = [System.Guid]::NewGuid().ToString('N').Substring(0,8)
        $stageRoot = Join-Path $InstallDir ".hew-install-${transactionId}"
        $stageBin = Join-Path $stageRoot "bin"
        $stageLib = Join-Path $stageRoot "lib"
        Stage-ReleaseToolchain `
            -InnerDir $innerDir.FullName `
            -StageBin $stageBin `
            -StageLib $stageLib

        # Swap bin/ and lib/ as one rollback-protected transaction. A failure
        # restores only directories whose backup move completed, preserving any
        # original destination that was never moved.
        try {
            Install-ReleaseToolchain `
                -InnerDir $innerDir.FullName `
                -InstallDir $InstallDir `
                -StageBin $stageBin `
                -StageLib $stageLib `
                -VersionFile $versionFile `
                -Version $Version
        }
        finally {
            if (Test-Path -LiteralPath $stageRoot) {
                Remove-Item -LiteralPath $stageRoot -Recurse -Force -ErrorAction SilentlyContinue
            }
        }

        Write-Host "done" -ForegroundColor Green
    }
    catch {
        Write-Host "failed" -ForegroundColor Red
        Write-ErrorAndExit "Failed to extract archive: $_"
    }
}
finally {
    # Clean up temp directory
    if (Test-Path $tempDir) {
        Remove-Item -Path $tempDir -Recurse -Force -ErrorAction SilentlyContinue
    }
}

# ---------------------------------------------------------------------------
# Environment variables — add to user PATH and set HEW_HOME / HEW_STD
# ---------------------------------------------------------------------------

$pathUpdated = $false
$userPath = [System.Environment]::GetEnvironmentVariable("Path", [System.EnvironmentVariableTarget]::User)
if (-not ($userPath -split ";" | Where-Object { $_ -eq $BinDir })) {
    $newPath = if ($userPath) { "${userPath};${BinDir}" } else { $BinDir }
    [System.Environment]::SetEnvironmentVariable("Path", $newPath, [System.EnvironmentVariableTarget]::User)
    $pathUpdated = $true
}

# Persist HEW_HOME so other tools can find the installation
$currentHewHome = [System.Environment]::GetEnvironmentVariable("HEW_HOME", [System.EnvironmentVariableTarget]::User)
if ($currentHewHome -ne $InstallDir) {
    [System.Environment]::SetEnvironmentVariable("HEW_HOME", $InstallDir, [System.EnvironmentVariableTarget]::User)
}

# Persist HEW_STD for the standard library
$StdDir = Join-Path $InstallDir "std"
$currentHewStd = [System.Environment]::GetEnvironmentVariable("HEW_STD", [System.EnvironmentVariableTarget]::User)
if ($currentHewStd -ne $StdDir) {
    [System.Environment]::SetEnvironmentVariable("HEW_STD", $StdDir, [System.EnvironmentVariableTarget]::User)
}

# ---------------------------------------------------------------------------
# Success message
# ---------------------------------------------------------------------------

Write-Host ""
Write-Host "  Hew was installed to " -NoNewline
Write-Host $InstallDir -ForegroundColor Green
Write-Host ""

if ($pathUpdated) {
    Write-Host "  Added " -NoNewline
    Write-Host $BinDir -ForegroundColor Cyan -NoNewline
    Write-Host " to your PATH."
    Write-Host ""
    Write-Host "  Restart your terminal, then run " -NoNewline
    Write-Host "hew version" -ForegroundColor Cyan -NoNewline
    Write-Host " to verify."
}
else {
    Write-Host "  Run " -NoNewline
    Write-Host "hew version" -ForegroundColor Cyan -NoNewline
    Write-Host " to verify the installation."
}

Write-Host ""
