from contextlib import contextmanager

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

@contextmanager
def agc_build(src_file):
    with tempfile.TemporaryDirectory() as d:
        # Run AGC
        agc_out = subprocess.run(
            ["agc", src_file, "--output-dir", d],
            capture_output=True,
            text=True
        )
        if agc_out.returncode != 0:
            raise RuntimeError(agc_out.stderr)

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
            raise RuntimeError(gprbuild_out.stderr)

        yield os.path.join(d, src_file[:-4])

