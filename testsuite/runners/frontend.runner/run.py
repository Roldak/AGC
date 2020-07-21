import os
import tempfile
import subprocess
import sys


def run(src_file):
    with tempfile.TemporaryDirectory() as d:
        # Run AGC
        agc_out = subprocess.run(
            ["agc", src_file, "--output-dir", d],
            capture_output=True,
            text=True
        )
        if agc_out.returncode != 0:
            print(agc_out.stderr)
            return

        # Run gnatpp
        subprocess.run(["gnatpp", "--pipe", os.path.join(d, src_file)])


if __name__ == "__main__":
    run("test.adb")
