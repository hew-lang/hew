#!/usr/bin/env bash
# Compile one accepted vertical-slice fixture with the exact side-artifact
# policy its oracle requires. Keep this Bash 3.2 compatible: expanding an empty
# array under `set -u` aborts before the compiler runs on macOS's system Bash.
set -euo pipefail

if [[ $# -ne 4 ]]; then
  echo "usage: $0 HEW ROOT FIXTURE OUTPUT" >&2
  exit 2
fi

hew="$1"
root="$2"
fixture="$3"
output="$4"
source_path="${root}/tests/vertical-slice/accept/${fixture}.hew"

status=0
case "${fixture}" in
  link_monitor_value_monitor_in_actor|actor_channel_shadow_sender_codec)
    if "${hew}" compile --emit-llvm "${source_path}" >"${output}" 2>&1; then
      status=0
    else
      status=$?
    fi
    ;;
  *)
    if "${hew}" compile "${source_path}" >"${output}" 2>&1; then
      status=0
    else
      status=$?
    fi
    ;;
esac

if [[ "${status}" -ne 0 ]]; then
  echo "expected ${fixture} to compile cleanly, got exit ${status}" >&2
  cat "${output}" >&2
  exit 1
fi
