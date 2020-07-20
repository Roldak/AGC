import os
import tempfile
import subprocess
import sys


def run(src_file):
    out = subprocess.run(["agc", src_file], capture_output=True)
    with tempfile.NamedTemporaryFile() as temp:
        temp.write(out.stdout)
        temp.flush()
        subprocess.run(["gnatpp", "--pipe", temp.name])


if __name__ == "__main__":
    run("test.adb")
