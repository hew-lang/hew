# Hew Installers

Installation options for [Hew](https://hew.sh) and [Adze](https://hew.sh/adze).

## Quick Install (Linux / macOS)

```sh
curl -fsSL https://install.hew.sh | bash
```

## Quick Install (Windows)

```powershell
irm https://install.hew.sh/install.ps1 | iex
```

## Package Managers

| Platform                 | Command                                                                          |
| ------------------------ | -------------------------------------------------------------------------------- |
| macOS / Linux (Homebrew) | `brew install hew-lang/tap/hew`                                                  |
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

# Interactive shell with hew and adze available
docker run --rm -it --entrypoint sh -v $(pwd):/work r.hew.sh/hew
```

## Files in This Directory

| File / Directory    | Description                         |
| ------------------- | ----------------------------------- |
| `install.sh`        | Unix shell installer (curl \| bash) |
| `install.ps1`       | Windows PowerShell installer        |
| `arch/PKGBUILD`     | Arch Linux AUR binary package       |
| `debian/`           | Debian/Ubuntu `.deb` packaging      |
| `rpm/hew.spec`      | Fedora/RHEL/openSUSE RPM spec       |
| `homebrew/hew.rb`   | Homebrew formula (macOS/Linux)      |
| `nix/default.nix`   | Nix derivation (binary fetch)       |
| `alpine/APKBUILD`   | Alpine Linux apk package            |
| `docker/Dockerfile` | Alpine-based container image        |

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
| `homebrew/hew.rb` | `__SHA256_{DARWIN,LINUX}_{X86_64,AARCH64}__` |
| `nix/default.nix` | `__SHA256_{DARWIN,LINUX}_{X86_64,AARCH64}__` |
| `arch/PKGBUILD`   | `SKIP` → actual sums                         |
| `alpine/APKBUILD` | run `abuild checksum`                        |
