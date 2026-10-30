#!/usr/bin/env python3
"""Exercise raw replay's process failure and containment boundaries."""

import importlib.util
import os
from pathlib import Path
import signal
import sys
import tempfile
import time
import unittest

spec = importlib.util.spec_from_file_location(
    "run_oracle", Path(__file__).with_name("run-oracle.py")
)
oracle = importlib.util.module_from_spec(spec)
sys.modules[spec.name] = oracle
spec.loader.exec_module(oracle)


@unittest.skipUnless(os.name == "posix", "raw replay uses POSIX process groups")
class ProcessBoundaries(unittest.TestCase):
    def run_script(self, source, timeout=2, output_cap=4096):
        with tempfile.TemporaryDirectory() as directory:
            binary = Path(directory) / "candidate"
            binary.write_text(f"#!{sys.executable}\n{source}\n")
            binary.chmod(0o755)
            return oracle._run_binary(binary, timeout, output_cap, None)

    def test_runtime_signal_is_not_an_expected_source_rejection(self):
        result = self.run_script(
            "import os, signal\nos.kill(os.getpid(), signal.SIGKILL)"
        )
        self.assertEqual(result[0], -signal.SIGKILL)
        self.assertEqual(oracle._classify_run(*result, None, None)[0], "runtime-crash")

    def test_output_flood_is_capped(self):
        result = self.run_script("import os\nwhile True: os.write(1, b'x' * 8192)")
        self.assertTrue(result[4])
        self.assertEqual(oracle._classify_run(*result, None, None)[0], "output-cap")

    def test_timeout_kills_descendants(self):
        for parent_wait in ["time.sleep(10)", "pass"]:
            with (
                self.subTest(parent_wait=parent_wait),
                tempfile.TemporaryDirectory() as directory,
            ):
                marker = Path(directory) / "survived"
                child = f"import time; from pathlib import Path; time.sleep(.4); Path({str(marker)!r}).touch()"
                source = f"import subprocess, sys, time\nsubprocess.Popen([sys.executable, '-c', {child!r}])\n{parent_wait}"
                result = self.run_script(source, timeout=0.1)
                self.assertTrue(result[3])
                time.sleep(0.5)
                self.assertFalse(
                    marker.exists(), "timed-out candidate's descendant kept running"
                )


class BuildClassification(unittest.TestCase):
    def test_source_without_main_is_not_a_program(self):
        output = (
            "E_NO_MAIN: candidate.hew has no `fn main`, so it cannot be built"
            " or run as a program\n"
        )
        verdict = oracle._classify_build_failure(output)
        self.assertEqual(verdict, "not-a-program")
        self.assertIn(verdict, oracle._ACCEPTED)

    def test_unexplained_build_failure_stays_a_failure(self):
        verdict = oracle._classify_build_failure("error: linking failed\n")
        self.assertEqual(verdict, "build-ice")
        self.assertNotIn(verdict, oracle._ACCEPTED)


if __name__ == "__main__":
    unittest.main()
