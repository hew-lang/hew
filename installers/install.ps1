#!/usr/bin/env pwsh
#Requires -Version 5.1

<#
.SYNOPSIS
    Installs Hew from GitHub releases.

.DESCRIPTION
    Downloads and installs the Hew programming language compiler and runtime
    from GitHub releases. Verifies SHA256 checksums and adds the install
    directory to your PATH.

    Usage via web:
      irm https://install.hew.sh/install.ps1 | iex

.PARAMETER Version
    Specific version to install (e.g. "0.1.0"). Defaults to latest.

.PARAMETER Prefix
    Installation directory. Defaults to $env:USERPROFILE\.hew

.PARAMETER Help
    Show this help message.

.EXAMPLE
    irm https://install.hew.sh/install.ps1 | iex

.EXAMPLE
    .\install.ps1 -Version 0.1.0

.EXAMPLE
    .\install.ps1 -Prefix C:\tools\hew
#>

param(
    [string]$Version,
    [string]$Prefix,
    [switch]$Help
)

Set-StrictMode -Version Latest
$ErrorActionPreference = "Stop"

$GITHUB_ORG = "hew-lang"
$GITHUB_REPO = "hew"

# --- Helpers ----------------------------------------------------------------

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
    $padding = 16 - $Label.Length
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

function Write-Error-And-Exit {
    param([string]$Message)
    Write-Host ""
    Write-Host "  Error: $Message" -ForegroundColor Red
    Write-Host ""
    exit 1
}

function Show-Help {
    Write-Banner
    Write-Host "  Usage: install.ps1 [options]" -ForegroundColor White
    Write-Host ""
    Write-Host "  Options:" -ForegroundColor White
    Write-Host "    -Version <version>   Install a specific version (e.g. 0.1.0)"
    Write-Host "    -Prefix <path>       Install directory (default: `$env:USERPROFILE\.hew)"
    Write-Host "    -Help                Show this help message"
    Write-Host ""
    Write-Host "  Examples:" -ForegroundColor White
    Write-Host "    irm https://install.hew.sh/install.ps1 | iex"
    Write-Host "    .\install.ps1 -Version 0.1.0"
    Write-Host "    .\install.ps1 -Prefix C:\tools\hew"
    Write-Host ""
}

# --- Main -------------------------------------------------------------------

if ($Help) {
    Show-Help
    exit 0
}

Write-Banner

# Detect architecture
$arch = [System.Runtime.InteropServices.RuntimeInformation]::OSArchitecture
if ($arch -ne [System.Runtime.InteropServices.Architecture]::X64) {
    # Fallback for older PowerShell without RuntimeInformation
    $envArch = $env:PROCESSOR_ARCHITECTURE
    if ($envArch -ne "AMD64") {
        Write-Error-And-Exit "Unsupported architecture: $envArch. Hew currently supports x86_64 (AMD64) only."
    }
}

$platform = "windows-x86_64"

# Set install directory
if (-not $Prefix) {
    $InstallDir = Join-Path $env:USERPROFILE ".hew"
}
else {
    $InstallDir = $Prefix
}

$BinDir = Join-Path $InstallDir "bin"

# Fetch version
if (-not $Version) {
    Write-Step -Label "Fetching version" -NoNewline
    try {
        $releaseUrl = "https://api.github.com/repos/${GITHUB_ORG}/${GITHUB_REPO}/releases/latest"
        $headers = @{ "Accept" = "application/vnd.github.v3+json" }
        if ($env:GITHUB_TOKEN) {
            $headers["Authorization"] = "token $env:GITHUB_TOKEN"
        }
        $release = Invoke-RestMethod -Uri $releaseUrl -Headers $headers -UseBasicParsing
        $Version = $release.tag_name -replace '^v', ''
        Write-Host "done" -ForegroundColor Green
    }
    catch {
        Write-Host "failed" -ForegroundColor Red
        Write-Error-And-Exit "Failed to fetch latest version from GitHub: $_"
    }
}

Write-Host "  Installing Hew v${Version} (${platform})" -ForegroundColor White
Write-Host ""

# Check if same version is already installed
$versionFile = Join-Path $InstallDir ".hew-version"
if (Test-Path $versionFile) {
    $installedVersion = (Get-Content $versionFile -Raw).Trim()
    if ($installedVersion -eq $Version) {
        Write-Host "  Hew v${Version} is already installed at ${InstallDir}" -ForegroundColor Yellow
        Write-Host ""
        exit 0
    }
}

# Build download URLs
$archiveName = "hew-v${Version}-${platform}.zip"
$downloadUrl = "https://github.com/${GITHUB_ORG}/${GITHUB_REPO}/releases/download/v${Version}/${archiveName}"
$checksumsName = "hew-v${Version}-checksums.txt"
$checksumsUrl  = "https://github.com/${GITHUB_ORG}/${GITHUB_REPO}/releases/download/v${Version}/${checksumsName}"

# Create temp directory
$tempDir = Join-Path ([System.IO.Path]::GetTempPath()) "hew-install-$([System.Guid]::NewGuid().ToString('N').Substring(0,8))"
New-Item -ItemType Directory -Path $tempDir -Force | Out-Null

try {
    $archivePath = Join-Path $tempDir $archiveName
    $checksumsPath = Join-Path $tempDir $checksumsName

    # Download archive
    Write-Step -Label "Downloading" -NoNewline
    try {
        $progressPreference = 'SilentlyContinue'
        Invoke-WebRequest -Uri $downloadUrl -OutFile $archivePath -UseBasicParsing
        Write-Host "done" -ForegroundColor Green
    }
    catch {
        Write-Host "failed" -ForegroundColor Red
        Write-Error-And-Exit "Failed to download ${downloadUrl}: $_"
    }

    # Download checksums and verify
    Write-Step -Label "Verifying" -NoNewline
    try {
        Invoke-WebRequest -Uri $checksumsUrl -OutFile $checksumsPath -UseBasicParsing

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
            Write-Error-And-Exit "Checksum for ${archiveName} not found in ${checksumsName}"
        }

        $actualHash = (Get-FileHash -Path $archivePath -Algorithm SHA256).Hash.ToLower()
        if ($actualHash -ne $expectedHash) {
            Write-Host "failed" -ForegroundColor Red
            Write-Error-And-Exit "SHA256 mismatch!`n  Expected: ${expectedHash}`n  Got:      ${actualHash}"
        }

        Write-Host "done" -ForegroundColor Green
    }
    catch [System.Net.WebException] {
        Write-Host "skipped (no $checksumsName)" -ForegroundColor Yellow
    }

    # Extract archive
    Write-Step -Label "Extracting" -NoNewline
    try {
        $extractDir = Join-Path $tempDir "extracted"
        Expand-Archive -Path $archivePath -DestinationPath $extractDir -Force

        # Find the inner directory (hew-v{version}-windows-x86_64/)
        $innerDir = Get-ChildItem -Path $extractDir -Directory | Select-Object -First 1
        if (-not $innerDir) {
            Write-Host "failed" -ForegroundColor Red
            Write-Error-And-Exit "Archive does not contain expected directory structure."
        }

        # Create install directory
        if (Test-Path $InstallDir) {
            # Remove old bin/ and lib/ but preserve user data
            foreach ($subdir in @("bin", "lib")) {
                $sub = Join-Path $InstallDir $subdir
                if (Test-Path $sub) {
                    Remove-Item -Path $sub -Recurse -Force
                }
            }
        }
        New-Item -ItemType Directory -Path $InstallDir -Force | Out-Null

        # Copy files from extracted archive to install directory
        Get-ChildItem -Path $innerDir.FullName | ForEach-Object {
            Copy-Item -Path $_.FullName -Destination $InstallDir -Recurse -Force
        }

        # std/ and completions/ (best-effort)
        $stdSrc = Join-Path $innerDir.FullName "std"
        if (Test-Path $stdSrc) {
            Copy-Item -Path $stdSrc -Destination $InstallDir -Recurse -Force
        }
        $compSrc = Join-Path $innerDir.FullName "completions"
        if (Test-Path $compSrc) {
            Copy-Item -Path $compSrc -Destination $InstallDir -Recurse -Force
        }

        # Write version file
        Set-Content -Path $versionFile -Value $Version -NoNewline

        Write-Host "done" -ForegroundColor Green
    }
    catch {
        Write-Host "failed" -ForegroundColor Red
        Write-Error-And-Exit "Failed to extract archive: $_"
    }
}
finally {
    # Clean up temp files
    if (Test-Path $tempDir) {
        Remove-Item -Path $tempDir -Recurse -Force -ErrorAction SilentlyContinue
    }
}

# Add to PATH
$pathUpdated = $false
$userPath = [System.Environment]::GetEnvironmentVariable("Path", [System.EnvironmentVariableTarget]::User)
if ($userPath -split ";" | Where-Object { $_ -eq $BinDir }) {
    # Already in PATH
}
else {
    $newPath = if ($userPath) { "${userPath};${BinDir}" } else { $BinDir }
    [System.Environment]::SetEnvironmentVariable("Path", $newPath, [System.EnvironmentVariableTarget]::User)
    $pathUpdated = $true
}

# Set HEW_STD environment variable
$StdDir = Join-Path $InstallDir "std"
$currentHewStd = [System.Environment]::GetEnvironmentVariable("HEW_STD", [System.EnvironmentVariableTarget]::User)
if ($currentHewStd -ne $StdDir) {
    [System.Environment]::SetEnvironmentVariable("HEW_STD", $StdDir, [System.EnvironmentVariableTarget]::User)
}

# Success message
Write-Host ""
Write-Host "  Hew was installed to " -NoNewline
Write-Host $InstallDir -ForegroundColor Green
Write-Host ""

if ($pathUpdated) {
    Write-Host "  Added " -NoNewline
    Write-Host $BinDir -ForegroundColor Cyan -NoNewline
    Write-Host " to your PATH."
    Write-Host "  Restart your terminal, then run " -NoNewline
    Write-Host "hew version" -ForegroundColor Cyan -NoNewline
    Write-Host " and " -NoNewline
    Write-Host "adze --version" -ForegroundColor Cyan -NoNewline
    Write-Host " to verify."
}
else {
    Write-Host "  $BinDir is already in your PATH."
}

Write-Host ""
