import os
import tempfile
import subprocess
import sys


def run(arg, opt_level="none"):
    with tempfile.TemporaryDirectory() as d:
        # Run AGC
        agc_out = subprocess.run(
            ["agc", arg, "--output-dir", d, "--no-hash",
             "--optimize", opt_level],
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

