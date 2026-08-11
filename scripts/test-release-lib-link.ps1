param(
    [Parameter(Mandatory = $true)]
    [string]$Hew,
    [Parameter(Mandatory = $true)]
    [string]$Archive
)

$ErrorActionPreference = 'Stop'

function Assert-NativeSuccess([string]$Label) {
    if ($LASTEXITCODE -ne 0) {
        throw "${Label} failed with exit code ${LASTEXITCODE}"
    }
}

$Hew = (Resolve-Path -LiteralPath $Hew).Path
$Archive = (Resolve-Path -LiteralPath $Archive).Path
if (-not (Test-Path -LiteralPath $Hew -PathType Leaf)) {
    throw "release hew executable is missing: ${Hew}"
}
if (-not (Test-Path -LiteralPath $Archive -PathType Leaf)) {
    throw "release hew library is missing: ${Archive}"
}
if (-not (Get-Command rustc -ErrorAction SilentlyContinue)) {
    throw 'rustc is required for native-package link validation'
}

$WorkDir = Join-Path ([System.IO.Path]::GetTempPath()) ("hew-release-link-" + [guid]::NewGuid())
New-Item -ItemType Directory -Path $WorkDir | Out-Null
try {
    $ReleaseBin = Join-Path $WorkDir 'release/bin'
    $ReleaseLib = Join-Path $WorkDir 'release/lib'
    New-Item -ItemType Directory -Path $ReleaseBin, $ReleaseLib | Out-Null
    $StagedHew = Join-Path $ReleaseBin 'hew.exe'
    Copy-Item -LiteralPath $Hew -Destination $StagedHew
    Copy-Item -LiteralPath $Archive -Destination (Join-Path $ReleaseLib 'hew.lib')

    $Native = Join-Path $WorkDir 'native.rs'
    @'
#[no_mangle]
pub extern "C" fn release_link_probe() -> i64 {
    String::from("release-link-ok").len() as i64
}
'@ | Set-Content -LiteralPath $Native -NoNewline

    $Foreign = Join-Path $WorkDir 'release_link_probe.lib'
    & rustc --crate-type staticlib --crate-name hew_release_link_probe --edition 2021 `
        -C panic=abort -C codegen-units=1 -o $Foreign $Native
    Assert-NativeSuccess 'rustc release-link consumer'

    $Source = Join-Path $WorkDir 'main.hew'
    @'
extern "C" { fn release_link_probe() -> i64; }

fn main() {
    let result: i64 = unsafe { release_link_probe() };
    if result != 15 {
        panic("native release link probe returned an unexpected value");
    }
    println("release-native-link-ok");
}
'@ | Set-Content -LiteralPath $Source -NoNewline

    $Output = Join-Path $WorkDir 'release-link-probe.exe'
    & $StagedHew build $Source --link-lib $Foreign -o $Output
    Assert-NativeSuccess 'hew build --link-lib release-link consumer'
    $ProbeOutput = & $Output
    Assert-NativeSuccess 'release-link consumer run'
    if ($ProbeOutput -notmatch 'release-native-link-ok') {
        throw "release-link consumer produced unexpected output: ${ProbeOutput}"
    }
} finally {
    Remove-Item -LiteralPath $WorkDir -Recurse -Force -ErrorAction SilentlyContinue
}

Write-Host 'PASS: release hew.lib linked and ran a Rust native staticlib consumer'
