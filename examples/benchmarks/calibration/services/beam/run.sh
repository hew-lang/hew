#!/bin/sh
set -eu

workers=${BENCH_WORKERS:-2}
case "$workers" in
*[!0-9]*)
    printf '%s\n' 'BENCH_WORKERS must be a positive integer' >&2
    exit 2
    ;;
esac
if [ "$workers" -lt 1 ]; then
    printf '%s\n' 'BENCH_WORKERS must be a positive integer' >&2
    exit 2
fi
exec erl +S "$workers" -pa _build/default/lib/*/ebin -noshell -s calibration_service start
