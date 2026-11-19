# Linking Hew programs

`hew build` produces a native executable from Hew source in two steps. The
compiler, which embeds LLVM, turns the program into an object file. The
system C compiler driver (`clang` or `cc`) then links that object with the
Hew runtime and the platform's C library. This page covers what each step
needs on your machine, the two ways the runtime can be linked, and how
dynamically linked programs find their runtime.

## What you need installed

| Task                                 | Needs                                                                 |
| ------------------------------------ | --------------------------------------------------------------------- |
| `hew check`, `hew fmt`, `hew doc`    | Only `hew`. LLVM is built into the compiler.                          |
| `hew build`, `hew run`, `hew test`   | A C compiler driver and the C library development files.              |
| Running a statically linked program  | Nothing from Hew; only the system C library (glibc on Linux).         |
| Running a dynamically linked program | The Hew runtime of the release that built it (`libhew.so.<version>`). |

The C compiler driver comes from the system package manager:

| System          | Command                                 |
| --------------- | --------------------------------------- |
| Debian / Ubuntu | `sudo apt install gcc libc6-dev`        |
| Fedora / RHEL   | `sudo dnf install gcc glibc-devel`      |
| openSUSE        | `sudo zypper install gcc glibc-devel`   |
| Arch Linux      | `sudo pacman -S gcc`                    |
| macOS           | `xcode-select --install`                |
| FreeBSD         | Nothing: the base system provides `cc`. |

The Linux `.deb`, `.rpm` and Arch packages install these as dependencies.

On Windows, Hew links with LLVM's `clang.exe` and `lld-link.exe` against
the MSVC C runtime. Install LLVM (`winget install LLVM.LLVM`) and put its
`bin` directory on `PATH`. Then install Visual Studio Build Tools with the
"Desktop development with C++" workload and a Windows SDK. `lld-link` finds
the MSVC and SDK libraries itself, so no developer command prompt is needed.

Cross-target builds (`--target` naming another architecture or OS) need
`clang`, because GCC's `cc` cannot take a target triple. To pick a specific
driver, set `HEW_CC=<path>`. If a driver is missing, `hew` reports the
install command for the machine it is running on.

## Static and dynamic runtime linking

`--link` chooses how a program links the Hew runtime and standard library:

```sh
hew build main.hew                  # --link=static, the default
hew build --link=dynamic main.hew
```

|                     | `--link=static` (default)               | `--link=dynamic`                                           |
| ------------------- | --------------------------------------- | ---------------------------------------------------------- |
| Runtime             | Copied into the program from `libhew.a` | Loaded at start-up from `libhew.so.<version>`              |
| Program size        | About 1 MB for hello-world              | A few KB, plus one shared runtime per release              |
| Runs where          | Any system with a compatible C library  | Systems with that release's Hew runtime                    |
| Hew upgrade/removal | Never affects the program               | Keeps working while that release's runtime stays installed |
| Platforms           | All                                     | Linux (glibc), FreeBSD, macOS                              |

In both modes the C library, `libm`, `libpthread` and the macOS system
frameworks are linked dynamically, as each platform expects. Static linking
is the default because a Hew program is then one self-contained file that no
later Hew install, upgrade or removal can break.

Dynamic linking suits machines that run many Hew programs from one Hew
installation, and container images or packages that ship several Hew
programs side by side. It is not available on Windows yet, for musl targets
(those programs link everything statically so they run without any Hew
files), or for wasm.

`hew run --link=dynamic` builds and runs the same way. `--link` and
`--rpath` do not apply to `--emit-obj`, which stops before the link.

### Releases and the runtime name

The runtime's ABI is not stable from one Hew release to the next. A
dynamically linked program therefore records the **exact release** in the
runtime it loads:

| Platform       | Runtime file          | Name recorded in the program       |
| -------------- | --------------------- | ---------------------------------- |
| Linux, FreeBSD | `libhew.so.0.11.1`    | `NEEDED libhew.so.0.11.1` (SONAME) |
| macOS          | `libhew.0.11.1.dylib` | `@rpath/libhew.0.11.1.dylib`       |

Each release's runtime has its own file name, so several releases install
side by side. Upgrading Hew adds the new runtime without replacing the old
one, and a program keeps loading the runtime it was built against. A
program never runs against a runtime from another release:

- If its runtime is not installed, the system loader refuses to start it
  and names the missing file, for example `libhew.so.0.11.1: cannot open
shared object file`. Install that Hew release, ship the runtime with the
  program (below), or rebuild the program with the Hew you have.
- If a runtime from another release is substituted (a renamed file, an
  `LD_LIBRARY_PATH` override), the program stops at start-up with exit status
  70 before running any Hew code:

  ```
  hew: this program was compiled by Hew 0.11.1 but loaded the Hew 0.12.0 runtime from /opt/x/libhew.so.0.11.1. Install the Hew 0.11.1 runtime, or rebuild the program with this Hew release.
  ```

Every program, static or dynamic, starts by registering itself with the
runtime. The program hands over its source and symbol tables (the tables
behind fault locations and `-g` backtraces) and the release that compiled
it. A dynamically linked program therefore reports faults exactly like its
static twin.

### How a program finds its runtime

A dynamically linked program searches these directories, in order:

1. its own directory (`$ORIGIN`, or `@executable_path` on macOS);
2. `../lib/` relative to the executable (for example, `app/lib/` for a program in `app/bin/`);
3. the directory of the runtime it was linked against, unless the system
   loader already searches that directory, or the build trims paths.

The first two let you ship a program with its runtime: put
`libhew.so.<version>` next to the binary, or lay out `bin/app` with
`lib/libhew.so.<version>`. The third makes `hew build --link=dynamic &&
./app` and `hew run --link=dynamic` work in place against an installed Hew.
A runtime installed by a distribution package sits in the system library
directory, so the program records no install path for it.

A build that trims paths leaves the third entry out. That includes
`--release` builds by default, and any build with `--trim-paths`. Such a
build names no directory on the build machine, so it links the same
wherever it is built. It finds its runtime once you ship that runtime
beside it, or once the runtime is installed in the system library
directory; until then the loader refuses to start it and names the
missing `libhew.so.<version>`. To run such a build in place against
`~/.hew`, add `--no-trim-paths`.

On Linux and FreeBSD the search path is a `DT_RUNPATH`, so
`LD_LIBRARY_PATH` still takes precedence for testing.

`--rpath DIR` (repeatable) replaces the whole default list. For example,
`--rpath '$ORIGIN/../lib'` searches only `../lib/` relative to the executable. Quote `$ORIGIN` so the shell passes it through.

### Where the runtime is installed

| Install method          | Runtime location                                                      |
| ----------------------- | --------------------------------------------------------------------- |
| `install.sh` / tarball  | `$HEW_HOME/lib/libhew.so.<version>` (default `~/.hew/lib`)            |
| `.deb` (Debian, Ubuntu) | `/usr/lib/<multiarch>/libhew.so.<version>`, package `libhew<version>` |
| `.rpm` (Fedora, RHEL)   | `/usr/lib64/libhew.so.<version>`, package `libhew<version>`           |
| Arch package            | `/usr/lib/libhew.so.<version>`                                        |
| Docker image            | `/usr/local/lib/libhew.so.<version>`                                  |
| macOS tarball           | `$HEW_HOME/lib/libhew.<version>.dylib`                                |

Upgrades never delete a previous release's runtime from an `install.sh`
installation. Re-installing a release replaces its runtime by renaming the
new file into place, so programs already running keep their mapped copy.

The Debian and RPM packages put each release's runtime in its own package
(`libhew0.11.1`), which the `hew` package depends on. Upgrading `hew`
installs the new release's runtime and leaves the old one installed, so
existing dynamically linked programs keep running. Because no package
depends on the old runtime, `apt autoremove` offers to remove it. Keep it
with `sudo apt-mark manual libhew0.11.1`, or rebuild the programs that
use it. Arch replaces the runtime on upgrade, as is usual there, so
rebuild dynamically linked programs after upgrading Hew. Removing a Hew
installation removes its runtime, and the programs that load it stop
starting; their statically linked builds are unaffected.

### Native packages and C libraries

Packages with `[native]` code link the same way in both modes. Their
libraries leave the runtime's `hew_*` symbols undefined, and those symbols
resolve against `libhew.a` or against the shared runtime. A library that
defines a `hew_*` symbol of its own is refused in both modes: with a shared
runtime it would silently take over the runtime's own copy.

Objects built with `--emit-obj --export-c` are linked by you, so you choose
the runtime. Link with `libhew.a` for a self-contained library, or with
`libhew.so.<version>` to share one runtime among several Hew-built
libraries in a process.

## Design notes

These notes are for Hew maintainers.

- **Exact-release soname.** The runtime ABI covers more than function
  signatures. Codegen also bakes in exported data layouts
  (`hew_layout_key_*`, `hew_layout_val_*`, `hew_last_fault_record`), the
  debug table formats (`hew-debug-abi`) and runtime semantics. Before 1.0
  none of these is frozen, and no fingerprint can prove they are unchanged,
  so a series soname (`libhew.so.0.11`) would promise compatibility nothing
  checks. Moving to a series soname is a 1.0 decision, and it should wait
  until CI runs the previous release's binaries against the new runtime.
- **Registration, not symbol interposition.** Statically linked programs
  replace weak default tables in the runtime (`hew-runtime/src/debug_tables.c`)
  at link time. That cannot cross a shared library boundary: rustc exports
  only `#[no_mangle]` symbols from a cdylib, and macOS binds with a two-level
  namespace. Every native entry therefore calls `hew_image_register(version,
files, sites, symtab, has_debug_info)` first. The call also carries the
  release check. ELF symbol versioning was rejected: rustc already passes an
  anonymous version script for cdylibs, and the mechanism does not exist on
  Mach-O.
- **Windows** has no dynamic mode. MSVC-mode `lld-link` has no auto-import,
  so the runtime's exported data would need `dllimport` in codegen. The
  `/alternatename` defaults in `debug_tables.c` do not cross a DLL boundary.
  And without an rpath, every program would need its own copy of the DLL.
  `hew.dll` remains an internal artifact of the experimental live sessions.
- **Data symbols.** Codegen emits position-independent code (`RelocMode::PIC`)
  and the drivers link position-independent executables, so references to
  the runtime's exported data go through the GOT and create no copy
  relocations.
- **Build output.** `hew-lib/build.rs` gives the cdylib its versioned
  SONAME or install name and links the versioned name to Cargo's
  `libhew.so`/`libhew.dylib`, so in-tree tests resolve what programs record.
  `scripts/stage-release-package.sh --shared-runtime auto` ships that
  library as `lib/libhew.so.<version>`.
