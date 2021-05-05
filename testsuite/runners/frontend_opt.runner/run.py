import os
import sys

sys.path.append(os.path.join(
    os.path.dirname(os.path.realpath(__file__)),
    "..",
    "frontend.runner"
))

import frontend_support

from glob import glob


if __name__ == "__main__":
    gprs = glob("*.gpr")
    if len(gprs) == 0:
        frontend_support.run("test.adb", opt_level="full")
    elif len(gprs) == 1:
        frontend_support.run("-P{}".format(gprs[0]), opt_level="full")
    else:
        print("Multiple gpr files found: {}".format(", ".join(gprs)))
