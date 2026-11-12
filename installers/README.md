# Hew Installers

Installation options for [Hew](https://hew.sh).

## Quick Install (Linux / macOS)

```sh
curl -fsSL https://hew.sh/install | sh
```

This installs the newest published release, including release candidates. Pass `--stable` to install the
newest final release instead:

```sh
curl -fsSL https://hew.sh/install | sh -s -- --stable
```

Other `install.sh` flags: `--version <ver>` (pin an exact version),
`--prefix <dir>` (installation directory), `--dry-run` (print what would be
installed without installing it). `sh install.sh --help` lists all of them.

## Native build prerequisites

Hew includes LLVM and its runtime. Building or running a native Hew program also
requires a system C compiler driver and C development files for the final link.
The Linux `.deb`, `.rpm` and Arch packages install these dependencies through the
package manager. When using the shell installer or extracting a tarball, install
them first:

| System          | Command                              |
| --------------- | ------------------------------------ |
| Debian / Ubuntu | `sudo apt-get install gcc libc6-dev` |
| Fedora / RHEL   | `sudo dnf install gcc glibc-devel`   |
| Arch Linux      | `sudo pacman -S gcc glibc`           |
| macOS           | `xcode-select --install`             |

On FreeBSD, the base system provides Clang and the C development files. On
Windows, put LLVM 22.1.6 `clang.exe` and `lld-link.exe` on `PATH`, and install
Visual Studio Build Tools with the Desktop development with C++ workload and
Windows SDK. The linker discovers the MSVC and Windows SDK libraries; a developer
shell is not required. Prebuilt SDK users do not need Rust or LLVM development
headers and libraries. The shipped Linux archives target glibc;
they do not run on an unmodified musl-based Alpine system.

## Quick Install (FreeBSD)

Prebuilt tools require FreeBSD 15.1 or newer. FreeBSD base includes `fetch(1)`:

```sh
fetch -o - https://hew.sh/install | sh
```

Download `hew-v<ver>-freebsd-x86_64.tar.gz` from the
[releases page](https://github.com/hew-lang/hew/releases). FreeBSD ARM64 has
build and link checks; an ARM64 SDK archive is not currently shipped.

## Quick Install (Windows)

```powershell
irm https://hew.sh/install.ps1 | iex
```

This installs the newest published release, including release candidates. Pass `-Stable` to install the newest final release instead:

```powershell
& ([scriptblock]::Create((irm https://hew.sh/install.ps1))) -Stable
```

Or download the script and run it with flags directly:

```powershell
.\install.ps1 -Stable
```

Other `install.ps1` flags: `-Version <ver>` (pin an exact version),
`-Prefix <dir>` (installation directory), `-DryRun` (print what would be
installed without installing it). `.\install.ps1 -Help` lists all of them.

## Package Managers

| Platform                 | Command                                                                                             |
| ------------------------ | --------------------------------------------------------------------------------------------------- |
| macOS / Linux (Homebrew) | `brew install hew-lang/hew/hew` (newest release, rc included) or `brew install hew-lang/hew/hew-stable` (final releases only) |
| FreeBSD (script)         | `fetch -o - https://hew.sh/install \| sh`                                                           |
| Arch Linux (AUR)         | `yay -S hew-bin`                                                                                    |
| Debian / Ubuntu          | `.deb` packages on the [releases page](https://github.com/hew-lang/hew/releases)                    |
| Fedora / RHEL / openSUSE | See `rpm/hew.spec`                                                                                  |
| Alpine Linux             | See `alpine/APKBUILD`                                                                               |
| Docker                   | `docker run --rm -v $(pwd):/work ghcr.io/hew-lang/hew build /work/main.hew`                         |

## Docker Usage

```sh
# Compile a Hew program to a native binary
docker run --rm -v $(pwd):/work ghcr.io/hew-lang/hew build /work/main.hew -o /work/out

# Run a Hew program directly
docker run --rm -v $(pwd):/work ghcr.io/hew-lang/hew run /work/main.hew

# Interactive shell with hew available
docker run --rm -it --entrypoint sh -v $(pwd):/work ghcr.io/hew-lang/hew
```

To build a local image from a Linux release archive in `dist/`:

```sh
installers/build-packages.sh --version <version> --arch <arch> --skip-build --only docker
```

The image is tagged `hew:local-<version>` and uses the release Dockerfile.

## Files in This Directory

| File / Directory            | Description                                                                                     |
| --------------------------- | ----------------------------------------------------------------------------------------------- |
| `install.sh`                | Unix shell installer — POSIX `/bin/sh`, Linux / macOS / FreeBSD (`curl \| sh` or `fetch \| sh`) |
| `install.ps1`               | Windows PowerShell installer                                                                    |
| `arch/PKGBUILD`             | Arch Linux AUR binary package                                                                   |
| `debian/`                   | Debian/Ubuntu `.deb` packaging                                                                  |
| `rpm/hew.spec`              | Fedora/RHEL/openSUSE RPM spec                                                                   |
| `alpine/APKBUILD`           | Alpine Linux apk package template                                                               |
| `docker/Dockerfile.release` | Docker release image and clean-room test                                                        |

The Homebrew formula lives in the `homebrew-hew` tap repo, not here — the
release workflow's `homebrew` job triggers that repo's own
`update-formula.yml`, which updates `Formula/hew.rb` and
`Formula/hew-stable.rb`.

## Package templates

`build-packages.sh` fills template versions and builds from local SDK archives.
It computes Alpine checksums during packaging. The Arch and Alpine files are
templates, not standalone packages.
