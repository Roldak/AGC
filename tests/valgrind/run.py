import os
import tempfile
import subprocess
import sys


gpr_template = """
with "agc_runtime";
project Test is
    for Main use ("{}");
    for Source_Dirs use ("{}");
    for Object_Dir use "{}";
    for Exec_Dir use "{}";
end Test;
"""


def run(src_file):
    out = subprocess.run(["agc", "test.adb"], capture_output=True)
    with tempfile.TemporaryDirectory() as d:
        with tempfile.NamedTemporaryFile(suffix=".adb", dir=d) as src:
            src.write(out.stdout)
            src.flush()
            with tempfile.NamedTemporaryFile(suffix=".gpr") as gpr:
                gpr_content = gpr_template.format(
                    src.name,
                    d,
                    d,
                    d,
                    src.name
                ).encode()
                gpr.write(gpr_content)
                gpr.flush()
                subprocess.run(
                    ["gprbuild", "-P", gpr.name],
                    capture_output=True
                )
                valgrind_out = subprocess.run(
                    ["valgrind", os.path.join(d, src.name[:-4])],
                    capture_output=True,
                    text=True
                )
                print(valgrind_out.stderr)


if __name__ == "__main__":
    run("test.adb")
