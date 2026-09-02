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

# Version policy: until v0.6.0 ships as a final release, the default install
# picks the newest published release *including release candidates*, so
# `irm | iex` tracks the pre-release ladder during the v0.6.0 stabilization
# window. Pass -Stable to install the newest tagged final release instead —
# matches install.sh's --stable (#3214) and the `hew@stable` Homebrew
# formula. Once a final v0.6.0 ships, the two policies agree until the next
# pre-release window.

param(
    [string]$Version,
    [switch]$Stable,
    [string]$Prefix,
    [switch]$DryRun,
    [switch]$Help
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
    Write-Host "    -Version <ver>   Install a specific version (e.g. 0.1.0). Default: newest"
    Write-Host "                     release including release candidates"
    Write-Host "    -Stable          Install the newest final (non-rc) release instead of"
    Write-Host "                     the newest release candidate"
    Write-Host "    -Prefix  <dir>   Installation directory. Default: `$env:HEW_HOME or `$env:USERPROFILE\.hew"
    Write-Host "    -DryRun          Resolve the version and print what would be installed,"
    Write-Host "                     without installing it"
    Write-Host "    -Help            Show this help message"
    Write-Host ""
    Write-Host "  EXAMPLES:" -ForegroundColor White
    Write-Host "    irm https://hew.sh/install.ps1 | iex"
    Write-Host "    .\install.ps1 -Version 0.1.0"
    Write-Host "    .\install.ps1 -Stable"
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

# ---------------------------------------------------------------------------
# Main
# ---------------------------------------------------------------------------

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
if (Test-Path $versionFile) {
    $installedVersion = (Get-Content $versionFile -Raw).Trim()
    if ($installedVersion -eq $Version) {
        Write-Host "  Hew v${Version} is already installed at ${InstallDir}" -ForegroundColor Yellow
        Write-Host ""
        Write-Host "  To reinstall, remove $versionFile and run again."
        Write-Host ""
        exit 0
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
        Invoke-WebRequest -Uri "${baseUrl}/${checksumsName}" -OutFile $checksumsPath -UseBasicParsing
        $ProgressPreference = $savedProgressPreference

        $expectedHash = $null
        foreach ($line in Get-Content $checksumsPath) {
            if ($line -match "^([0-9a-fA-F]{64})\s+(.+)$") {
                $hash = $Matches[1]
                $file = $Matches[2].Trim()
                if ($file -eq $archiveName) {
                    $expectedHash = $hash.ToLower()
                    break
                }
            }
        }

        if (-not $expectedHash) {
            Write-Host "failed" -ForegroundColor Red
            Write-ErrorAndExit "Checksum for ${archiveName} not found in ${checksumsName}"
        }

        $actualHash = (Get-FileHash -Path $archivePath -Algorithm SHA256).Hash.ToLower()
        if ($actualHash -ne $expectedHash) {
            Write-Host "failed" -ForegroundColor Red
            Write-ErrorAndExit "SHA-256 mismatch!`n  Expected: ${expectedHash}`n  Got:      ${actualHash}"
        }

        Write-Host "done" -ForegroundColor Green
    }
    catch [System.Net.WebException] {
        $ProgressPreference = $savedProgressPreference
        Write-Host "skipped" -ForegroundColor DarkGray
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

        # Find the inner directory (e.g. hew-v0.1.0-windows-x86_64/)
        $innerDir = Get-ChildItem -Path $extractDir -Directory | Select-Object -First 1
        if (-not $innerDir) {
            Write-Host "failed" -ForegroundColor Red
            Write-ErrorAndExit "Archive does not contain expected directory structure."
        }

        # Prepare install directory — remove old bin/ and lib/ but preserve user data
        if (Test-Path $InstallDir) {
            foreach ($subdir in @("bin", "lib")) {
                $sub = Join-Path $InstallDir $subdir
                if (Test-Path $sub) {
                    Remove-Item -Path $sub -Recurse -Force
                }
            }
        }

        # Create standard directories
        foreach ($dir in @("bin", "lib", "std", "completions")) {
            New-Item -ItemType Directory -Path (Join-Path $InstallDir $dir) -Force | Out-Null
        }

        # Copy binaries
        foreach ($bin in @("hew", "hew-lsp", "hew-observe")) {
            # On Windows, binaries have .exe extension
            foreach ($ext in @(".exe", "")) {
                $src = Join-Path $innerDir.FullName "bin/${bin}${ext}"
                if (Test-Path $src) {
                    Copy-Item -Path $src -Destination (Join-Path $InstallDir "bin/${bin}${ext}") -Force
                }
            }
        }

        # Copy all of lib/ — preserves libhew.a at both the flat path and any
        # triple-specific subdirectory (e.g. lib/x86_64-pc-windows-msvc/hew.lib)
        $libSrc = Join-Path $innerDir.FullName "lib"
        if (Test-Path $libSrc) {
            Copy-Item -Path (Join-Path $libSrc "*") -Destination (Join-Path $InstallDir "lib") -Recurse -Force
        }

        # Standard library (best-effort — may not be in older releases)
        $stdSrc = Join-Path $innerDir.FullName "std"
        if (Test-Path $stdSrc) {
            Copy-Item -Path (Join-Path $stdSrc "*") -Destination (Join-Path $InstallDir "std") -Recurse -Force
        }

        # Generate shell completions from installed binaries
        $hewBin = Join-Path $InstallDir "bin/hew.exe"
        if (-not (Test-Path $hewBin)) { $hewBin = Join-Path $InstallDir "bin/hew" }
        $compDir = Join-Path $InstallDir "completions"
        foreach ($sh in @("bash", "zsh", "fish")) {
            try { & $hewBin  completions $sh | Out-File -Encoding utf8NoBOM (Join-Path $compDir "hew.$sh")  } catch {}
        }
        try { & $hewBin  completions powershell | Out-File -Encoding UTF8 (Join-Path $compDir "hew.ps1")  } catch {}

        # Copy license and doc files
        foreach ($f in @("LICENSE-MIT", "LICENSE-APACHE", "NOTICE", "README.md")) {
            $src = Join-Path $innerDir.FullName $f
            if (Test-Path $src) {
                Copy-Item -Path $src -Destination (Join-Path $InstallDir $f) -Force
            }
        }

        # Write version marker
        Set-Content -Path $versionFile -Value $Version -NoNewline

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
Write-Host "  To activate PowerShell completions, add this to your " -NoNewline
Write-Host '$PROFILE' -ForegroundColor Cyan -NoNewline
Write-Host ":"
Write-Host ""
Write-Host "    . `"$(Join-Path $InstallDir 'completions\hew.ps1')`"" -ForegroundColor DarkGray
Write-Host ""
