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

    def test_stream_sequence_checksum_and_metadata_are_checked(self):
        output = self.record(
            workload="stream_pipeline",
            iterations=4,
            size=2,
            checksum=28,
            latencies_ns=[10],
        )
        validate(output, "stream_pipeline", 4, 2)
        wrong_checksum = json.loads(output)
        wrong_checksum["checksum"] = 27
        with self.assertRaisesRegex(ValueError, "checksum"):
            validate(json.dumps(wrong_checksum), "stream_pipeline", 4, 2)
        with self.assertRaisesRegex(ValueError, "iterations"):
            validate(output, "stream_pipeline", 5, 2)

    def test_stream_latency_samples_are_capped_and_clock_free_means_empty(self):
        output = self.record(
            workload="stream_roundtrip",
            iterations=640001,
            size=1,
            checksum=640001 * 640000 // 2,
            latencies_ns=[1] * 10000,
        )
        validate(output, "stream_roundtrip", 640001, 1)
        too_few_value = json.loads(output)
        too_few_value["latencies_ns"].pop()
        with self.assertRaisesRegex(ValueError, "64 completed stream operations"):
            validate(json.dumps(too_few_value), "stream_roundtrip", 640001, 1)
        too_many_value = json.loads(output)
        too_many_value["latencies_ns"].append(1)
        with self.assertRaisesRegex(ValueError, "64 completed stream operations"):
            validate(json.dumps(too_many_value), "stream_roundtrip", 640001, 1)

        clock_free = self.record(
            workload="stream_pipeline_alloc",
            iterations=4,
            size=2,
            checksum=28,
            latencies_ns=[],
        )
        validate(clock_free, "stream_pipeline_alloc", 4, 2)
        missing_samples = json.loads(clock_free)
        del missing_samples["latencies_ns"]
        with self.assertRaisesRegex(ValueError, "must report latencies_ns"):
            validate(json.dumps(missing_samples), "stream_pipeline_alloc", 4, 2)
        clocked_value = json.loads(clock_free)
        clocked_value["latencies_ns"] = [1]
        with self.assertRaisesRegex(ValueError, "must not report"):
            validate(json.dumps(clocked_value), "stream_pipeline_alloc", 4, 2)

    def test_boolean_duration_and_latency_are_not_integers(self):
        with self.assertRaisesRegex(ValueError, "elapsed_ns"):
            validate(self.record(elapsed_ns=True), "bytes_cow", 10, 8)
        with self.assertRaisesRegex(ValueError, "latency samples"):
            validate(
                self.record(
                    workload="stream_roundtrip",
                    iterations=1,
                    size=1,
                    checksum=0,
                    latencies_ns=[True],
                ),
                "stream_roundtrip",
                1,
                1,
            )

    def test_failed_process_is_not_a_measurement(self):
        with self.assertRaisesRegex(RuntimeError, "exit 3"):
            execute([sys.executable, "-c", "raise SystemExit(3)"], timeout=5)

    def test_timeout_is_not_a_measurement(self):
        with self.assertRaisesRegex(RuntimeError, "timeout"):
            execute([sys.executable, "-c", "import time; time.sleep(30)"], timeout=0.05)


if __name__ == "__main__":
    unittest.main()
