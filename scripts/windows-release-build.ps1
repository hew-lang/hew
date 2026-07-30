<#
Build and prove a staged Hew release candidate on Windows.

pre-release-validate.sh transfers the candidate archive to an isolated
directory and invokes this file from that directory. Keeping the build body in
the candidate avoids sending a multi-kilobyte -EncodedCommand through Windows
OpenSSH/cmd.exe, whose command-line limit is substantially smaller after the
Visual Studio environment is imported.
#>

$ErrorActionPreference = 'Stop'

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

cargo build -p hew-cli -p adze-cli -p hew-lsp -p hew-observe --release
Assert-NativeSuccess 'cargo build release binaries'

cargo build -p hew-lib --profile release-lib
Assert-NativeSuccess 'cargo build hew-lib'

if (-not (Test-Path '.\target\release-lib\hew.lib')) {
    throw 'target/release-lib/hew.lib missing after cargo build --profile release-lib'
}
& .\scripts\test-release-lib-link.ps1 -Hew .\target\release\hew.exe -Archive .\target\release-lib\hew.lib
Assert-NativeSuccess 'release library consumer proof'

& .\target\release\hew.exe --version
Assert-NativeSuccess 'hew.exe --version'

& .\target\release\adze.exe --version
Assert-NativeSuccess 'adze.exe --version'

& .\target\release\hew-lsp.exe --version
Assert-NativeSuccess 'hew-lsp.exe --version'

& .\target\release\hew-observe.exe --version
Assert-NativeSuccess 'hew-observe.exe --version'
