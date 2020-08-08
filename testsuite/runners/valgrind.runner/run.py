import os
import subprocess
import sys

sys.path.append(os.path.join(
    os.path.dirname(os.path.realpath(__file__)),
    ".."
))

import support


def format_valgrind_output(output):
    lines = output.splitlines()[6:]
    lines = [l[l.rindex("=") + 1:] for l in lines]
    return "\n".join(lines)


def run(src_file):
    with support.agc_build(src_file) as main:
        # Run valgrind on resulting binary
        valgrind_out = subprocess.run(
            ["valgrind", main],
            capture_output=True,
            text=True
        )
        print(format_valgrind_output(valgrind_out.stderr))


if __name__ == "__main__":
    run("test.adb")
