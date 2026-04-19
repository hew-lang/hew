class Hew < Formula
  desc "Statically-typed, actor-oriented programming language"
  homepage "https://hew.sh"
  version "0.1.0"
  license any_of: ["MIT", "Apache-2.0"]

  # NOTE: Replace __SHA256_*__ with actual SHA256 hashes at release time.
  # Run: sha256sum hew-v#{version}-{darwin,linux}-{x86_64,aarch64}.tar.gz

  on_macos do
    if Hardware::CPU.arm?
      url "https://github.com/hew-lang/hew/releases/download/v#{version}/hew-v#{version}-darwin-aarch64.tar.gz"
      sha256 "__SHA256_DARWIN_AARCH64__"
    else
      url "https://github.com/hew-lang/hew/releases/download/v#{version}/hew-v#{version}-darwin-x86_64.tar.gz"
      sha256 "__SHA256_DARWIN_X86_64__"
    end
  end

  on_linux do
    if Hardware::CPU.intel?
      url "https://github.com/hew-lang/hew/releases/download/v#{version}/hew-v#{version}-linux-x86_64.tar.gz"
      sha256 "__SHA256_LINUX_X86_64__"
    else
      url "https://github.com/hew-lang/hew/releases/download/v#{version}/hew-v#{version}-linux-aarch64.tar.gz"
      sha256 "__SHA256_LINUX_AARCH64__"
    end
  end

  def install
    bin.install "bin/hew"
    bin.install "bin/adze"
    bin.install "bin/hew-lsp"
    lib.install "lib/libhew.a"

    # Install target-specific lib subtree so find_hew_lib() can probe
    # lib/<triple>/libhew.a before falling back to the flat path.
    triple = if OS.mac?
      Hardware::CPU.arm? ? "aarch64-apple-darwin" : "x86_64-apple-darwin"
    else
      Hardware::CPU.arm? ? "aarch64-unknown-linux-gnu" : "x86_64-unknown-linux-gnu"
    end
    if (buildpath/"lib"/triple/"libhew.a").exist?
      (lib/triple).mkpath
      (lib/triple).install "lib/#{triple}/libhew.a"
    end

    (share/"hew/std").mkpath
    (share/"hew/std").install Dir["std/*"] if (buildpath/"std").exist?

    # Generate shell completions from the installed binaries
    output = Utils.safe_popen_read(bin/"hew", "completions", "bash")
    (bash_completion/"hew").write output
    output = Utils.safe_popen_read(bin/"hew", "completions", "zsh")
    (zsh_completion/"_hew").write output
    output = Utils.safe_popen_read(bin/"hew", "completions", "fish")
    (fish_completion/"hew.fish").write output
    output = Utils.safe_popen_read(bin/"adze", "completions", "bash")
    (bash_completion/"adze").write output
    output = Utils.safe_popen_read(bin/"adze", "completions", "zsh")
    (zsh_completion/"_adze").write output
    output = Utils.safe_popen_read(bin/"adze", "completions", "fish")
    (fish_completion/"adze.fish").write output
  end

  def caveats
    <<~EOS
      The Hew standard library is installed to:
        #{HOMEBREW_PREFIX}/share/hew/std/

      To use the standard library, set:
        export HEW_STD="#{HOMEBREW_PREFIX}/share/hew/std"
    EOS
  end

  test do
    system "#{bin}/hew", "version"
    system "#{bin}/adze", "--version"
    system "#{bin}/hew-lsp", "--version"
  end
end
