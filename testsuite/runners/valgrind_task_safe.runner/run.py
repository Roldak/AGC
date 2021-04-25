import os
import sys

sys.path.append(os.path.join(
    os.path.dirname(os.path.realpath(__file__)),
    "..",
    "valgrind.runner"
))

import valgrind_support

if __name__ == "__main__":
    valgrind_support.run("test.adb", "agc_task_safe_runtime")
