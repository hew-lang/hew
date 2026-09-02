# Hew Installers

Installation options for [Hew](https://hew.sh).

## Quick Install (Linux / macOS)

```sh
curl -fsSL https://hew.sh/install | sh
```

## Quick Install (FreeBSD)

FreeBSD base includes `fetch(1)` — no extra packages needed:

```sh
fetch -o - https://hew.sh/install | sh
```

Prebuilt release tarballs: `hew-v<ver>-freebsd-x86_64.tar.gz` and
`hew-v<ver>-freebsd-aarch64.tar.gz` on the
[releases page](https://github.com/hew-lang/hew/releases).

## Quick Install (Windows)

```powershell
irm https://hew.sh/install.ps1 | iex
```

Until v0.6.0 ships as a final release, this installs the newest published
release _including release candidates_ — matching `install.sh`'s policy
(#3214). Pass `-Stable` to install the newest final release instead:

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

| Platform                 | Command                                                                          |
| ------------------------ | -------------------------------------------------------------------------------- |
| macOS / Linux (Homebrew) | `brew install hew-lang/tap/hew`                                                  |
| FreeBSD (script)         | `fetch -o - https://hew.sh/install \| sh`                                        |
| Arch Linux (AUR)         | `yay -S hew-bin`                                                                 |
| Debian / Ubuntu          | `.deb` packages on the [releases page](https://github.com/hew-lang/hew/releases) |
| Fedora / RHEL / openSUSE | See `rpm/hew.spec`                                                               |
| Alpine Linux             | See `alpine/APKBUILD`                                                            |
| Nix / NixOS              | See `nix/default.nix`                                                            |
| Docker                   | `docker run --rm -v $(pwd):/work r.hew.sh/hew build /work/main.hew`              |

## Docker Usage

```sh
# Compile a Hew program to a native binary
docker run --rm -v $(pwd):/work r.hew.sh/hew build /work/main.hew -o /work/out

# Run a Hew program directly
docker run --rm -v $(pwd):/work r.hew.sh/hew run /work/main.hew

# Interactive shell with hew available
docker run --rm -it --entrypoint sh -v $(pwd):/work r.hew.sh/hew
```

## Files in This Directory

| File / Directory    | Description                                                                                     |
| ------------------- | ----------------------------------------------------------------------------------------------- |
| `install.sh`        | Unix shell installer — POSIX `/bin/sh`, Linux / macOS / FreeBSD (`curl \| sh` or `fetch \| sh`) |
| `install.ps1`       | Windows PowerShell installer                                                                    |
| `arch/PKGBUILD`     | Arch Linux AUR binary package                                                                   |
| `debian/`           | Debian/Ubuntu `.deb` packaging                                                                  |
| `rpm/hew.spec`      | Fedora/RHEL/openSUSE RPM spec                                                                   |
| `nix/default.nix`   | Nix derivation (binary fetch)                                                                   |
| `alpine/APKBUILD`   | Alpine Linux apk package                                                                        |
| `docker/Dockerfile` | Alpine-based container image                                                                    |

The Homebrew formula lives in the `homebrew-hew` tap repo, not here — the
release workflow's `homebrew` job triggers that repo's own
`update-formula.yml`, which updates `Formula/hew.rb` and
`Formula/hew@stable.rb`.

## Updating Checksums at Release Time

Package files contain `__SHA256_*__` or `SKIP` placeholder checksums.
After cutting a release, update them:

```sh
VERSION=0.1.0
for target in linux-x86_64 linux-aarch64 darwin-x86_64 darwin-aarch64; do
  curl -fsSL -O \
    "https://github.com/hew-lang/hew/releases/download/v${VERSION}/hew-v${VERSION}-${target}.tar.gz"
done

sha256sum hew-v${VERSION}-*.tar.gz
```

Files to update with the resulting hashes:

| File              | Placeholder                                  |
| ----------------- | -------------------------------------------- |
| `nix/default.nix` | `__SHA256_{DARWIN,LINUX}_{X86_64,AARCH64}__` |
| `arch/PKGBUILD`   | `SKIP` → actual sums                         |
| `alpine/APKBUILD` | run `abuild checksum`                        |

(The Homebrew formula's checksums are updated automatically by the release
workflow's `homebrew` job — see above.)
