import os
import tempfile
import subprocess
import sys


gpr_template = """
with "agc_runtime";
project Test is
    for Main use ("{}");
end Test;
"""


def format_valgrind_output(output):
    lines = output.splitlines()[6:]
    lines = [l[l.rindex("=") + 1:] for l in lines]
    return "\n".join(lines)


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

        # Create GPR project file
        gpr_path = os.path.join(d, "test.gpr")
        with open(gpr_path, "wb") as gpr:
            gpr_content = gpr_template.format(src_file).encode()
            gpr.write(gpr_content)
            gpr.flush()

        # Build generated project
        gprbuild_out = subprocess.run(
            ["gprbuild", "-p", "-P", gpr_path],
            capture_output=True,
            text=True
        )
        if gprbuild_out.returncode != 0:
            print(gprbuild_out.stderr)
            return

        # Run valgrind on resulting binary
        valgrind_out = subprocess.run(
            ["valgrind",
             os.path.join(d, src_file[:-4])],
            capture_output=True,
            text=True
        )
        print(format_valgrind_output(valgrind_out.stderr))


if __name__ == "__main__":
    run("test.adb")
