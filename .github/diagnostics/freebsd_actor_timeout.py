#!/usr/bin/env python3
"""Measure the exact actor ownership fixture on a FreeBSD guest."""
import os
import subprocess
import time
from pathlib import Path

root = Path.cwd()
out = root / 'diagnostic-output'
out.mkdir(exist_ok=True)
source = root / 'tests/core-acceptance/fixtures/ownership-balance/actor_state_vec_seat.hew'
text = source.read_text()
needle = '        k = k + 1;\n'
assert text.count(needle) == 1
progress = out / 'actor_progress.hew'
progress.write_text(text.replace(needle, needle + '        if k % 10000 == 0 { println(k); }\n'))
for name, src in [('original', source), ('progress', progress)]:
    dest = out / name
    dest.mkdir(exist_ok=True)
    subprocess.run(['target/debug/hew', 'compile', '--emit-dir', str(dest), '--opt-level', '0', str(src)], check=True)

def measure(name, binary, count, expected):
    print(f'BEGIN {name}: {count} concurrent processes', flush=True)
    start = time.monotonic()
    logs = [open(out / f'{name}-{i}.out', 'w') for i in range(count)]
    procs = [subprocess.Popen([str(binary)], stdout=logs[i], stderr=subprocess.STDOUT) for i in range(count)]
    pending = set(range(count))
    next_sample = 0
    while pending:
        now = time.monotonic() - start
        for i in list(pending):
            status = procs[i].poll()
            if status is not None:
                logs[i].flush()
                data = (out / f'{name}-{i}.out').read_text()
                print(f'EXIT {name} index={i} elapsed={now:.2f}s status={status} output={data!r}', flush=True)
                if status != 0 or not data.endswith(expected):
                    raise SystemExit(f'{name} index={i} failed or produced incorrect output')
                pending.remove(i)
        if not pending:
            break
        if now >= next_sample:
            for i in pending:
                pid = procs[i].pid
                print(f'SAMPLE {name} index={i} elapsed={now:.2f}s pid={pid}', flush=True)
                subprocess.run(['ps', '-o', 'pid,etime,time,pcpu,state', '-p', str(pid)], check=False)
                subprocess.run(['procstat', '-t', str(pid)], check=False)
                logs[i].flush()
                print('OUTPUT', (out / f'{name}-{i}.out').read_text()[-300:], flush=True)
                if now >= 100:
                    subprocess.run(['procstat', '-kk', str(pid)], check=False)
                    try:
                        subprocess.run(['gdb', '-q', '-batch', '-ex', 'thread apply all bt', '-p', str(pid)], check=False, timeout=15)
                    except subprocess.TimeoutExpired:
                        print('gdb timed out', flush=True)
            next_sample += 20
        if now >= 150:
            for i in pending:
                procs[i].kill()
            raise SystemExit(f'{name} exceeded 150 seconds; samples retained above')
        time.sleep(1)
    for log in logs:
        log.close()

measure('original-single-1', out / 'original/actor_state_vec_seat', 1, '1800004\n')
measure('original-single-2', out / 'original/actor_state_vec_seat', 1, '1800004\n')
measure('progress-four', out / 'progress/actor_progress', 4, '1800004\n')
print('Diagnostic completed; this is not the full platform gate.', flush=True)
