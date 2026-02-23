# syntax=docker/dockerfile:1
# Hew compiler image — usage:
#   docker run --rm -v $(pwd):/work r.hew.sh/hew build /work/main.hew
#   docker run --rm -v $(pwd):/work r.hew.sh/hew run  /work/main.hew
#
# Build:
#   docker build --build-arg HEW_VERSION=0.1.0 -t r.hew.sh/hew installers/docker/
#   docker build --build-arg HEW_VERSION=0.1.0 --build-arg HEW_ARCH=aarch64 -t r.hew.sh/hew installers/docker/

ARG HEW_VERSION=0.1.0
ARG HEW_ARCH=x86_64

# ── Stage 1: download ────────────────────────────────────────────────────────
FROM alpine:3.21 AS fetch

ARG HEW_VERSION
ARG HEW_ARCH

RUN apk add --no-cache curl

WORKDIR /tmp/hew-install

RUN curl -fsSL \
  "https://github.com/hew-lang/hew/releases/download/v${HEW_VERSION}/hew-v${HEW_VERSION}-linux-${HEW_ARCH}.tar.gz" \
  -o hew.tar.gz \
  && tar -xzf hew.tar.gz \
  && mv "hew-v${HEW_VERSION}-linux-${HEW_ARCH}" hew

# ── Stage 2: runtime image ───────────────────────────────────────────────────
FROM alpine:3.21

# gcompat: glibc ABI shim for Rust binaries (linked glibc) on Alpine musl.
# libgcc + libstdc++: needed by hew-codegen (C++/LLVM).
RUN apk add --no-cache \
      gcompat \
      libgcc \
      libstdc++ \
      ca-certificates

COPY --from=fetch /tmp/hew-install/hew/bin/hew           /usr/local/bin/hew
COPY --from=fetch /tmp/hew-install/hew/bin/adze          /usr/local/bin/adze
COPY --from=fetch /tmp/hew-install/hew/bin/hew-codegen   /usr/local/bin/hew-codegen
COPY --from=fetch /tmp/hew-install/hew/lib               /usr/local/lib/hew/
COPY --from=fetch /tmp/hew-install/hew/std               /usr/local/share/hew/std/

# HEW_STD tells the compiler where the standard library lives.
ENV HEW_STD=/usr/local/share/hew/std

# Users mount their source here: docker run -v $(pwd):/work r.hew.sh/hew ...
WORKDIR /work

LABEL org.opencontainers.image.title="Hew" \
      org.opencontainers.image.description="Statically-typed, actor-oriented programming language" \
      org.opencontainers.image.url="https://hew.sh" \
      org.opencontainers.image.source="https://github.com/hew-lang/hew" \
      org.opencontainers.image.licenses="MIT OR Apache-2.0" \
      org.opencontainers.image.authors="Stephen Olesen <slepp@slepp.ca>"

ENTRYPOINT ["/usr/local/bin/hew"]
CMD ["--help"]
