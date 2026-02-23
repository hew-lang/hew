{ lib
, stdenv
, fetchurl
, autoPatchelfHook
}:

let
  version = "0.1.0";

  # NOTE: Replace __SHA256_*__ with actual SHA256 hashes at release time.
  # Run: nix-prefetch-url --unpack <url>  or  sha256sum <file>
  platformSources = {
    "x86_64-linux" = {
      url = "https://github.com/hew-lang/hew/releases/download/v${version}/hew-v${version}-linux-x86_64.tar.gz";
      sha256 = "__SHA256_LINUX_X86_64__";
    };
    "aarch64-linux" = {
      url = "https://github.com/hew-lang/hew/releases/download/v${version}/hew-v${version}-linux-aarch64.tar.gz";
      sha256 = "__SHA256_LINUX_AARCH64__";
    };
    "x86_64-darwin" = {
      url = "https://github.com/hew-lang/hew/releases/download/v${version}/hew-v${version}-darwin-x86_64.tar.gz";
      sha256 = "__SHA256_DARWIN_X86_64__";
    };
    "aarch64-darwin" = {
      url = "https://github.com/hew-lang/hew/releases/download/v${version}/hew-v${version}-darwin-aarch64.tar.gz";
      sha256 = "__SHA256_DARWIN_AARCH64__";
    };
  };

  src = fetchurl (platformSources.${stdenv.system}
    or (throw "Unsupported platform: ${stdenv.system}"));

in stdenv.mkDerivation rec {
  pname = "hew";
  inherit version src;

  # autoPatchelfHook fixes ELF interpreter/RPATH on pre-built Linux binaries
  # so they run under NixOS's non-FHS /nix/store layout.
  nativeBuildInputs = lib.optionals stdenv.isLinux [ autoPatchelfHook ];

  buildInputs = [ ];

  dontConfigure = true;
  dontBuild = true;

  installPhase = ''
    runHook preInstall

    install -Dm755 bin/hew          $out/bin/hew
    install -Dm755 bin/adze         $out/bin/adze
    install -Dm755 bin/hew-codegen  $out/bin/hew-codegen
    install -Dm644 lib/libhew_runtime.a $out/lib/hew/libhew_runtime.a

    if [ -d std ]; then
      mkdir -p $out/share/hew/std
      cp -r std/. $out/share/hew/std/
    fi

    if [ -f completions/hew.bash ]; then
      install -Dm644 completions/hew.bash \
        $out/share/bash-completion/completions/hew
    fi
    if [ -f completions/hew.zsh ]; then
      install -Dm644 completions/hew.zsh \
        $out/share/zsh/site-functions/_hew
    fi
    if [ -f completions/hew.fish ]; then
      install -Dm644 completions/hew.fish \
        $out/share/fish/vendor_completions.d/hew.fish
    fi
    if [ -f completions/adze.bash ]; then
      install -Dm644 completions/adze.bash \
        $out/share/bash-completion/completions/adze
    fi
    if [ -f completions/adze.zsh ]; then
      install -Dm644 completions/adze.zsh \
        $out/share/zsh/site-functions/_adze
    fi
    if [ -f completions/adze.fish ]; then
      install -Dm644 completions/adze.fish \
        $out/share/fish/vendor_completions.d/adze.fish
    fi

    install -Dm644 LICENSE-MIT    $out/share/licenses/hew/LICENSE-MIT
    install -Dm644 LICENSE-APACHE $out/share/licenses/hew/LICENSE-APACHE
    install -Dm644 NOTICE         $out/share/licenses/hew/NOTICE

    runHook postInstall
  '';

  meta = with lib; {
    description = "Statically-typed, actor-oriented programming language";
    homepage = "https://hew.sh";
    license = with licenses; [ mit asl20 ];
    maintainers = [
      { name = "Stephen Olesen"; email = "slepp@slepp.ca"; github = "slepp"; }
      { name = "The Hew Project Developers"; email = "hello@hew.sh"; github = "hew-lang"; }
    ];
    platforms = [ "x86_64-linux" "aarch64-linux" "x86_64-darwin" "aarch64-darwin" ];
    sourceProvenance = with sourceTypes; [ binaryNativeCode ];
  };
}
