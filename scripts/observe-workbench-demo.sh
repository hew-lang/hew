#!/usr/bin/env bash
# Run and privately record the deterministic Observe recovery workbench.

set -euo pipefail

repo_root="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
workload="${repo_root}/examples/observe/recovery_workbench.hew"
mode="${1:-run}"
listen_addr="${HEW_OBSERVE_DEMO_ADDR:-127.0.0.1:6067}"
endpoint="http://${listen_addr}"
artifact_dir="${HEW_OBSERVE_ARTIFACT_DIR:-/tmp/hew-observe-workbench}"
demo_seconds="${HEW_OBSERVE_DEMO_SECONDS:-18}"
workload_pid=""
trace_stream_id=""
trace_after=""

usage() {
    cat <<'USAGE'
Usage: scripts/observe-workbench-demo.sh [run|snapshot|record-tui|replay]

  run         Run the real Hew workload with hot profiling on 127.0.0.1:6067.
  snapshot    Run it and save one-second API snapshots plus program output.
  record-tui  Record a real hew-observe terminal session. Uses asciinema when
              installed, otherwise portable util-linux script timing files.
  replay      Replay the latest util-linux script recording.

Environment:
  HEW_OBSERVE_DEMO_ADDR       profiler address (default 127.0.0.1:6067)
  HEW_OBSERVE_ARTIFACT_DIR    private output directory
                              (default /tmp/hew-observe-workbench)
  HEW_OBSERVE_DEMO_SECONDS    terminal recording limit (default 18)
  HEW_OBSERVE_BIN             hew-observe executable (default PATH lookup)
USAGE
}

cleanup() {
    if [[ -n "${workload_pid}" ]] && kill -0 "${workload_pid}" 2>/dev/null; then
        kill "${workload_pid}" 2>/dev/null || true
        wait "${workload_pid}" 2>/dev/null || true
    fi
}
trap cleanup EXIT INT TERM

run_workload() {
    HEW_PPROF="${listen_addr}" HEW_OBSERVE=hot \
        hew run --profile "${workload}"
}

start_workload() {
    mkdir -p "${artifact_dir}"
    run_workload >"${artifact_dir}/workload.log" 2>&1 &
    workload_pid=$!
}

wait_for_profiler() {
    local attempts=0
    while (( attempts < 100 )); do
        if curl --fail --silent --output /dev/null "${endpoint}/api/metrics"; then
            return 0
        fi
        if ! kill -0 "${workload_pid}" 2>/dev/null; then
            echo "workload exited before profiler became ready; see ${artifact_dir}/workload.log" >&2
            return 1
        fi
        sleep 0.1
        attempts=$((attempts + 1))
    done
    echo "profiler did not become ready at ${endpoint}" >&2
    return 1
}

resolve_observer() {
    if [[ -n "${HEW_OBSERVE_BIN:-}" ]]; then
        printf '%s\n' "${HEW_OBSERVE_BIN}"
    elif command -v hew-observe >/dev/null 2>&1; then
        command -v hew-observe
    elif [[ -x "${repo_root}/target/debug/hew-observe" ]]; then
        printf '%s\n' "${repo_root}/target/debug/hew-observe"
    else
        echo "hew-observe was not found; build it with: cargo build -p hew-observe" >&2
        return 1
    fi
}

capture_trace_pages() {
    local sample_name="$1"
    local page=0
    local output
    local url
    local status

    while :; do
        output="${artifact_dir}/${sample_name}-traces-$(printf '%02d' "${page}").json"
        url="${endpoint}/api/traces"
        if [[ -n "${trace_stream_id}" ]]; then
            url="${url}?stream_id=${trace_stream_id}&after=${trace_after}&limit=256"
        fi
        status="$(curl --silent --show-error --output "${output}" \
            --write-out '%{http_code}' "${url}" || true)"
        if [[ "${status}" != "200" ]]; then
            return 0
        fi

        # The cursor batch is the only shape we advance. Keeping the fallback
        # file makes this harness useful against older builds without claiming
        # that their destructive trace endpoint is a trustworthy recording.
        if ! jq --exit-status '.data | type == "object" and has("stream_id") and has("next_cursor")' \
            "${output}" >/dev/null; then
            return 0
        fi
        trace_stream_id="$(jq --raw-output '.data.stream_id' "${output}")"
        trace_after="$(jq --raw-output '.data.next_cursor' "${output}")"
        if [[ "$(jq --raw-output '.data.has_more' "${output}")" != "true" ]]; then
            return 0
        fi
        page=$((page + 1))
    done
}

case "${mode}" in
    run)
        run_workload
        ;;
    snapshot)
        if ! command -v jq >/dev/null 2>&1; then
            echo "snapshot mode requires jq for lossless trace cursor advancement" >&2
            exit 1
        fi
        start_workload
        wait_for_profiler
        sample=0
        while kill -0 "${workload_pid}" 2>/dev/null; do
            sample_name="$(printf '%03d' "${sample}")"
            for path in metrics actors memory supervisors crashes; do
                curl --fail --silent --show-error \
                    "${endpoint}/api/${path}" \
                    --output "${artifact_dir}/${sample_name}-${path}.json" || true
            done
            capture_trace_pages "${sample_name}"
            sleep 1
            sample=$((sample + 1))
        done
        wait "${workload_pid}"
        workload_pid=""
        printf 'snapshots: %s\n' "${artifact_dir}"
        ;;
    record-tui)
        observer="$(resolve_observer)"
        if [[ ! "${demo_seconds}" =~ ^[1-9][0-9]*$ ]]; then
            echo "HEW_OBSERVE_DEMO_SECONDS must be a positive integer" >&2
            exit 2
        fi
        printf -v observer_command '%q --addr %q --refresh-ms 250' \
            "${observer}" "${listen_addr}"
        record_command="timeout --signal=INT ${demo_seconds}s ${observer_command}"
        start_workload
        wait_for_profiler
        record_status=0
        if command -v asciinema >/dev/null 2>&1; then
            asciinema rec --overwrite --idle-time-limit 2 \
                --command "${record_command}" \
                "${artifact_dir}/observe-workbench.cast" || record_status=$?
            printf 'cast: %s\n' "${artifact_dir}/observe-workbench.cast"
        elif command -v script >/dev/null 2>&1; then
            script --quiet --return --flush \
                --log-out "${artifact_dir}/observe-workbench.typescript" \
                --log-timing "${artifact_dir}/observe-workbench.timing" \
                --command "${record_command}" || record_status=$?
            printf 'recording: %s\n' "${artifact_dir}/observe-workbench.typescript"
            printf 'timing:    %s\n' "${artifact_dir}/observe-workbench.timing"
            printf 'replay:    %s replay\n' "$0"
        else
            echo "neither asciinema nor util-linux script is installed" >&2
            exit 1
        fi
        if [[ "${record_status}" != "0" && "${record_status}" != "124" && "${record_status}" != "130" ]]; then
            exit "${record_status}"
        fi
        ;;
    replay)
        if ! command -v scriptreplay >/dev/null 2>&1; then
            echo "scriptreplay is not installed" >&2
            exit 1
        fi
        scriptreplay \
            --timing "${artifact_dir}/observe-workbench.timing" \
            "${artifact_dir}/observe-workbench.typescript"
        ;;
    -h|--help|help)
        usage
        ;;
    *)
        usage >&2
        exit 2
        ;;
esac
