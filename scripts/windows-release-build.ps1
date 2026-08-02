<#
Build and prove a staged Hew release candidate on Windows.

pre-release-validate.sh transfers the candidate archive to an isolated
directory and invokes this file from that directory. Keeping the build body in
the candidate avoids sending a multi-kilobyte -EncodedCommand through Windows
OpenSSH/cmd.exe, whose command-line limit is substantially smaller after the
Visual Studio environment is imported.
#>

$ErrorActionPreference = 'Stop'

if (-not [Environment]::Is64BitOperatingSystem -or $env:PROCESSOR_ARCHITECTURE -ne 'AMD64') {
    throw "Windows x86_64 validator requires an AMD64 host, got $env:PROCESSOR_ARCHITECTURE"
}

function Assert-NativeSuccess([string]$Label) {
    if ($LASTEXITCODE -ne 0) {
        throw "${Label} failed with exit code $LASTEXITCODE"
    }
}

foreach ($Required in 'HEW_WINDOWS_LLVM_CONFIG', 'HEW_WINDOWS_LLVM_PREFIX', 'HEW_WINDOWS_CC', 'HEW_WINDOWS_CXX') {
    if ([string]::IsNullOrWhiteSpace([Environment]::GetEnvironmentVariable($Required))) {
        throw "Missing required staged-build environment variable: $Required"
    }
}

$LlvmConfig = $env:HEW_WINDOWS_LLVM_CONFIG
$LlvmPrefix = $env:HEW_WINDOWS_LLVM_PREFIX

if (-not (Test-Path $LlvmConfig)) {
    throw "Missing $LlvmConfig. Bootstrap LLVM 22 at C:\llvm-22 (see docs/cross-platform-build-guide.md) or set HEW_WINDOWS_LLVM_PREFIX / HEW_WINDOWS_LLVM_CONFIG before running pre-release validation."
}
$LlvmConfigExe = Join-Path $LlvmPrefix 'bin\llvm-config.exe'
if (-not (Test-Path $LlvmConfigExe -PathType Leaf)) {
    throw "Missing $LlvmConfigExe. HEW_WINDOWS_LLVM_PREFIX must name an LLVM 22 installation."
}
$LlvmVersion = & $LlvmConfigExe --version
Assert-NativeSuccess 'llvm-config.exe --version'
if ([string]::IsNullOrWhiteSpace([string]$LlvmVersion) -or $LlvmVersion -notmatch '^22\.1\.0\s*$') {
    throw "Expected release-toolchain LLVM 22.1.0 from $LlvmConfigExe, got: $LlvmVersion"
}
Write-Host "Using LLVM $LlvmVersion from $LlvmPrefix"

# $PSScriptRoot is <candidate>/scripts. Build only the staged candidate, never
# a checkout that happens to exist on the host.
Set-Location (Split-Path -Parent $PSScriptRoot)

# Import the MSVC/Windows SDK environment into this non-interactive SSH
# PowerShell. Native C dependencies require the SDK/CRT INCLUDE and LIB paths
# normally installed by VsDevCmd.
$VsWhere = Join-Path ([Environment]::GetEnvironmentVariable('ProgramFiles(x86)')) 'Microsoft Visual Studio\Installer\vswhere.exe'
if (-not (Test-Path $VsWhere)) {
    throw 'Visual Studio vswhere.exe is missing; install the C++ Build Tools workload.'
}
$VsInstall = & $VsWhere -latest -products '*' -requires Microsoft.VisualStudio.Component.VC.Tools.x86.x64 -property installationPath
if ($LASTEXITCODE -ne 0 -or [string]::IsNullOrWhiteSpace($VsInstall)) {
    throw 'No Visual Studio installation with the x64 C++ tools was found.'
}
$VsDevCmd = Join-Path $VsInstall 'Common7\Tools\VsDevCmd.bat'
if (-not (Test-Path $VsDevCmd)) {
    throw "Missing Visual Studio developer environment bootstrap: $VsDevCmd"
}
$DevCommand = '"' + $VsDevCmd + '" -no_logo -arch=x64 -host_arch=x64 >nul && set'
$DevEnvironment = & cmd.exe /d /s /c $DevCommand
if ($LASTEXITCODE -ne 0) {
    throw "Visual Studio developer environment bootstrap failed with exit code $LASTEXITCODE"
}
foreach ($Line in $DevEnvironment) {
    if ($Line -match '^([^=]+)=(.*)$') {
        [Environment]::SetEnvironmentVariable($Matches[1], $Matches[2], 'Process')
    }
}
if ([string]::IsNullOrWhiteSpace($env:WindowsSdkDir) -or [string]::IsNullOrWhiteSpace($env:LIB)) {
    throw 'Visual Studio developer environment did not publish WindowsSdkDir and LIB.'
}
Write-Host "Using Visual Studio developer environment: $VsDevCmd"

$env:LLVM_PREFIX = $LlvmPrefix
# llvm-sys 221 reads this versioned variable before falling back to PATH. Set
# it explicitly so a stale host setting cannot redirect the staged build.
$env:LLVM_SYS_221_PREFIX = $LlvmPrefix
$env:Path = "$LlvmPrefix\bin;" + $env:Path
$env:CC = $env:HEW_WINDOWS_CC
$env:CXX = $env:HEW_WINDOWS_CXX
# Match the hosted release lanes: aws-lc's pregenerated Windows assembly avoids
# a NASM/toolchain-dependent rebuild, and bounded parallelism keeps LTO release
# builds within the validation VM's memory budget.
$env:AWS_LC_SYS_PREBUILT_NASM = '1'
if ([string]::IsNullOrWhiteSpace($env:CARGO_BUILD_JOBS)) {
    $env:CARGO_BUILD_JOBS = '2'
}

# Cargo's JSON compiler-artifact messages are the authority for every path
# below.  Do not reconstruct target/release from the filesystem: CARGO_TARGET_DIR,
# build.target-dir, build.target, and target paths containing spaces all move the
# actual output location.  Capturing these messages from THIS build also makes a
# stale executable/archive elsewhere on disk unusable as release evidence.
$ReleaseBuildMessages = @(
    & cargo build -p hew-cli -p adze-cli -p hew-lsp -p hew-observe --release --message-format=json
)
Assert-NativeSuccess 'cargo build release binaries'

$ReleaseLibBuildMessages = @(
    & cargo build -p hew-lib --profile release-lib --message-format=json
)
Assert-NativeSuccess 'cargo build hew-lib'

function Get-CargoCompilerArtifacts([object[]]$Messages, [string]$BuildLabel) {
    $Artifacts = @()
    foreach ($Line in $Messages) {
        if ([string]::IsNullOrWhiteSpace([string]$Line)) {
            continue
        }
        try {
            $Message = $Line | ConvertFrom-Json -ErrorAction Stop
        }
        catch {
            throw "Cargo $BuildLabel emitted non-JSON output while artifact paths were required: $Line"
        }
        if ($Message.reason -eq 'compiler-artifact') {
            $Artifacts += $Message
        }
    }
    if ($Artifacts.Count -eq 0) {
        throw "Cargo $BuildLabel emitted no compiler-artifact messages"
    }
    return @($Artifacts)
}

function Resolve-UniqueCargoArtifact(
    [object[]]$Artifacts,
    [string]$LeafName,
    [string]$BuildLabel,
    [switch]$Executable
) {
    $Matches = @()
    foreach ($Artifact in $Artifacts) {
        if ($Executable) {
            if (-not [string]::IsNullOrWhiteSpace([string]$Artifact.executable) -and
                [System.IO.Path]::GetFileName([string]$Artifact.executable) -ieq $LeafName) {
                $Matches += [string]$Artifact.executable
            }
            continue
        }
        foreach ($Filename in @($Artifact.filenames)) {
            if (-not [string]::IsNullOrWhiteSpace([string]$Filename) -and
                [System.IO.Path]::GetFileName([string]$Filename) -ieq $LeafName) {
                $Matches += [string]$Filename
            }
        }
    }

    $Matches = @($Matches | Select-Object -Unique)
    if ($Matches.Count -ne 1) {
        $Rendered = if ($Matches.Count -eq 0) { '<none>' } else { $Matches -join '; ' }
        throw "Cargo $BuildLabel did not emit exactly one $LeafName artifact (found $($Matches.Count): $Rendered)"
    }
    if (-not (Test-Path -LiteralPath $Matches[0] -PathType Leaf)) {
        throw "Cargo $BuildLabel reported $LeafName at $($Matches[0]), but that exact artifact is missing"
    }
    return $Matches[0]
}

$ReleaseArtifacts = Get-CargoCompilerArtifacts $ReleaseBuildMessages 'release binary build'
$ReleaseLibArtifacts = Get-CargoCompilerArtifacts $ReleaseLibBuildMessages 'release-lib build'
$Hew = Resolve-UniqueCargoArtifact $ReleaseArtifacts 'hew.exe' 'release binary build' -Executable
$Adze = Resolve-UniqueCargoArtifact $ReleaseArtifacts 'adze.exe' 'release binary build' -Executable
$HewLsp = Resolve-UniqueCargoArtifact $ReleaseArtifacts 'hew-lsp.exe' 'release binary build' -Executable
$HewObserve = Resolve-UniqueCargoArtifact $ReleaseArtifacts 'hew-observe.exe' 'release binary build' -Executable
$ReleaseLib = Resolve-UniqueCargoArtifact $ReleaseLibArtifacts 'hew.lib' 'release-lib build'
$ReleaseDir = Split-Path -Parent $Hew
$ReleaseLibDir = Split-Path -Parent $ReleaseLib
[System.IO.File]::WriteAllText(
    (Join-Path (Get-Location) '.hew-release-dir'),
    $ReleaseDir,
    [System.Text.UTF8Encoding]::new($false)
)

& .\scripts\test-release-lib-link.ps1 -Hew $Hew -Archive $ReleaseLib
Assert-NativeSuccess 'release library consumer proof'

& $Hew --version
Assert-NativeSuccess 'hew.exe --version'

& $Adze --version
Assert-NativeSuccess 'adze.exe --version'

& $HewLsp --version
Assert-NativeSuccess 'hew-lsp.exe --version'

& $HewObserve --version
Assert-NativeSuccess 'hew-observe.exe --version'

# Compile and execute a minimal program in this same process so it inherits the
# candidate-local TEMP/TMP, Cargo target, LLVM, and Visual Studio environment
# established by the staged launcher and this script.
$SmokeSource = Join-Path (Get-Location) '_smoke.hew'
$SmokeOutput = Join-Path (Get-Location) '_smoke.exe'
Remove-Item -LiteralPath $SmokeSource, $SmokeOutput -Force -ErrorAction SilentlyContinue
try {
    [System.IO.File]::WriteAllText(
        $SmokeSource,
        'fn main() { println("smoke-ok") }',
        [System.Text.UTF8Encoding]::new($false)
    )

    & $Hew build $SmokeSource -o $SmokeOutput
    Assert-NativeSuccess 'hew.exe smoke build'
    if (-not (Test-Path -LiteralPath $SmokeOutput -PathType Leaf)) {
        throw "Smoke build did not produce $SmokeOutput"
    }

    $SmokeResult = & $SmokeOutput
    Assert-NativeSuccess '_smoke.exe run'
    if ($SmokeResult -notmatch 'smoke-ok') {
        throw "Smoke test failed: expected smoke-ok, got $SmokeResult"
    }
    Write-Host 'Smoke test passed'
} finally {
    Remove-Item -LiteralPath $SmokeSource, $SmokeOutput -Force -ErrorAction SilentlyContinue
}
