# Reference scaffold for the Homebrew formula.
# The live tap formula (hew-lang/homebrew-hew/Formula/hew.rb) is updated
# automatically by the release workflow via update-formula.yml.
# This file is NOT read by release.yml — it is a human-readable reference
# for the next release author. SHA256 values below reflect v0.4.0; recompute
# from the release tarballs at the next release.
class Hew < Formula
  desc "Statically-typed, actor-oriented programming language"
  homepage "https://hew.sh"
  version "0.4.0"
  license any_of: ["MIT", "Apache-2.0"]

  on_macos do
    if Hardware::CPU.arm?
      url "https://github.com/hew-lang/hew/releases/download/v#{version}/hew-v#{version}-darwin-aarch64.tar.gz"
      sha256 "d0c8fcb38ec4c6114f4a3d90d762ec92b2a4371a0c426dd07db0fcb427956350"
    else
      url "https://github.com/hew-lang/hew/releases/download/v#{version}/hew-v#{version}-darwin-x86_64.tar.gz"
      sha256 "fc73f71bb93e7d24a621aee7fd82a4f2be1668ccbf30244b78ead3f61c760c0f"
    end
  end

  on_linux do
    if Hardware::CPU.intel?
      url "https://github.com/hew-lang/hew/releases/download/v#{version}/hew-v#{version}-linux-x86_64.tar.gz"
      sha256 "a0af9de33c36a199760fbb07314b273103efa30ea5838c59de74b4e2a977115b"
    else
      url "https://github.com/hew-lang/hew/releases/download/v#{version}/hew-v#{version}-linux-aarch64.tar.gz"
      sha256 "a51b8acf5a8a685d09676c050964c9519146ae50c6210a244bdd5efc6a918ec3"
    end
  end

  def install
    bin.install "bin/hew"
    bin.install "bin/adze"
    bin.install "bin/hew-lsp"
    bin.install "bin/hew-observe"
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
    system "#{bin}/hew-observe", "--version"
  end
end
