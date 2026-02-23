Name:           hew
Version:        0.1.0
Release:        1%{?dist}
Summary:        The Hew programming language compiler and package manager

License:        MIT OR Apache-2.0
URL:            https://hew.sh
ExclusiveArch:  x86_64 aarch64

%ifarch x86_64
%define hew_arch x86_64
%endif
%ifarch aarch64
%define hew_arch aarch64
%endif

Source0:        https://github.com/hew-lang/hew/releases/download/v%{version}/hew-v%{version}-linux-%{hew_arch}.tar.gz

# Pre-built binaries — no compilation needed.
%global debug_package %{nil}

%description
Hew is a statically-typed, actor-oriented programming language targeting
concurrent and distributed systems, featuring Erlang-inspired supervision
trees and first-class async/await.

This package provides:
  - hew          the compiler driver
  - adze         the package manager
  - hew-codegen  the MLIR code generator
  - libhew_runtime.a  the actor runtime static library

%prep
%autosetup -n hew-v%{version}-linux-%{hew_arch}

%install
install -Dm755 bin/hew          %{buildroot}%{_bindir}/hew
install -Dm755 bin/adze         %{buildroot}%{_bindir}/adze
install -Dm755 bin/hew-codegen  %{buildroot}%{_bindir}/hew-codegen

install -Dm644 lib/libhew_runtime.a %{buildroot}%{_libdir}/hew/libhew_runtime.a

# Standard library
if [ -d std ]; then
  install -dm755 %{buildroot}%{_datadir}/hew/std
  cp -r std/. %{buildroot}%{_datadir}/hew/std/
fi

# Shell completions
[ -f completions/hew.bash ] && \
  install -Dm644 completions/hew.bash \
    %{buildroot}%{_datadir}/bash-completion/completions/hew
[ -f completions/hew.zsh ] && \
  install -Dm644 completions/hew.zsh \
    %{buildroot}%{_datadir}/zsh/site-functions/_hew
[ -f completions/hew.fish ] && \
  install -Dm644 completions/hew.fish \
    %{buildroot}%{_datadir}/fish/vendor_completions.d/hew.fish
[ -f completions/adze.bash ] && \
  install -Dm644 completions/adze.bash \
    %{buildroot}%{_datadir}/bash-completion/completions/adze
[ -f completions/adze.zsh ] && \
  install -Dm644 completions/adze.zsh \
    %{buildroot}%{_datadir}/zsh/site-functions/_adze
[ -f completions/adze.fish ] && \
  install -Dm644 completions/adze.fish \
    %{buildroot}%{_datadir}/fish/vendor_completions.d/adze.fish

# Licenses
install -Dm644 LICENSE-MIT    %{buildroot}%{_licensedir}/%{name}/LICENSE-MIT
install -Dm644 LICENSE-APACHE %{buildroot}%{_licensedir}/%{name}/LICENSE-APACHE
install -Dm644 NOTICE         %{buildroot}%{_licensedir}/%{name}/NOTICE

%files
%license LICENSE-MIT LICENSE-APACHE NOTICE
%{_bindir}/hew
%{_bindir}/adze
%{_bindir}/hew-codegen
%{_libdir}/hew/
%{_datadir}/hew/
%{_datadir}/bash-completion/completions/hew
%{_datadir}/zsh/site-functions/_hew
%{_datadir}/fish/vendor_completions.d/hew.fish
%{_datadir}/bash-completion/completions/adze
%{_datadir}/zsh/site-functions/_adze
%{_datadir}/fish/vendor_completions.d/adze.fish

%changelog
* Thu Feb 19 2026 The Hew Project Developers <hello@hew.sh> - 0.1.0-1
- Initial RPM packaging
