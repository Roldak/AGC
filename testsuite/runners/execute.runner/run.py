import os
import subprocess
import sys

sys.path.append(os.path.join(
    os.path.dirname(os.path.realpath(__file__)),
    ".."
))

import support


def run(src_file):
    with support.agc_build(src_file) as main:
        exec_out = subprocess.run(
            main,
            capture_output=True,
            text=True
        )
        if exec_out.stderr:
            print(exec_out.stderr)
        else:
            print(exec_out.stdout)


if __name__ == "__main__":
    run("test.adb")
