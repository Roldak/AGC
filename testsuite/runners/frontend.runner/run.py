import os
import tempfile
import subprocess
import sys

from glob import glob


def run(arg):
    with tempfile.TemporaryDirectory() as d:
        # Run AGC
        agc_out = subprocess.run(
            ["agc", arg, "--output-dir", d],
            capture_output=True,
            text=True
        )
        if agc_out.returncode != 0:
            raise RuntimeError(agc_out.stderr)

        gpr_path = os.path.join(d, "prj.gpr")
        with open(gpr_path, "w") as gpr:
            gpr.write("project Prj is end Prj;")
            gpr.flush()

            # Run gnatpp
            subprocess.run(
                ["gnatpp", "--pipe", "-q", "-P", "prj.gpr"],
                cwd=d
            )


if __name__ == "__main__":
    gprs = glob("*.gpr")
    if len(gprs) == 0:
        run("test.adb")
    elif len(gprs) == 1:
        run("-P{}".format(gprs[0]))
    else:
        print("Multiple gpr files found: {}".format(", ".join(gprs)))
