"""Failure controls for the measurement boundary, rather than timing assertions."""

import json
import sys
import unittest

from run import execute, validate


class MeasurementBoundary(unittest.TestCase):
    def record(self, **changes):
        value = dict(
            workload="bytes_cow", iterations=10, size=8, checksum=68, elapsed_ns=1000
        )
        value.update(changes)
        return json.dumps(value)

    def test_accepts_checked_output(self):
        validate(self.record(), "bytes_cow", 10, 8)

    def test_wrong_result_is_not_a_measurement(self):
        with self.assertRaisesRegex(ValueError, "checksum"):
            validate(self.record(checksum=29), "bytes_cow", 10, 8)

    def test_truncated_roundtrips_are_rejected(self):
        output = self.record(workload="actor_roundtrip", checksum=55, latencies_ns=[1])
        with self.assertRaisesRegex(ValueError, "latency sample"):
            validate(output, "actor_roundtrip", 10, 8)

    def test_failed_process_is_not_a_measurement(self):
        with self.assertRaisesRegex(RuntimeError, "exit 3"):
            execute([sys.executable, "-c", "raise SystemExit(3)"], timeout=5)

    def test_timeout_is_not_a_measurement(self):
        with self.assertRaisesRegex(RuntimeError, "timeout"):
            execute([sys.executable, "-c", "import time; time.sleep(30)"], timeout=0.05)


if __name__ == "__main__":
    unittest.main()
