from glob import iglob
from os import path

import argparse
import re
import subprocess
import sys


arg_parser = argparse.ArgumentParser()
arg_parser.add_argument("--rewrite", action="store_true")
arg_parser.add_argument("patterns", nargs="*", type=str)


base_dir = path.dirname(__file__)


def discover_runners():
    for runner in iglob(path.join(base_dir, "**", "run.py")):
        yield runner


def discover_tests(patterns):
    regexps = [re.compile(p) for p in patterns]
    for test_folder in iglob(path.join(base_dir, "**", "*.test")):
        if len(regexps) == 0 or any(r.match(test_folder) for r in regexps):
            yield test_folder


def runners_for_test(test, runners):
    for runner in runners:
        if test.startswith(path.dirname(runner)):
            yield runner


def run_test(runner, test, rewrite):
    try:
        runner = path.abspath(runner)
        out = subprocess.run(
            args=[sys.executable, runner],
            capture_output=True,
            text=True,
            cwd=test
        ).stdout

        out_file_path = path.join(test, "test.out")

        with open(out_file_path, "r") as f:
            expected_output = f.read()

        if out == expected_output:
            status = "PASSED"
        else:
            status = "FAILED"
            if rewrite:
                with open(out_file_path, "w") as f:
                    f.write(out)
    except subprocess.CalledProcessError:
        status = "CRASH"

    print("{}: {}".format(test[:-5], status))


def run(test_patterns, rewrite):
    runners = list(discover_runners())
    tests = list(discover_tests(test_patterns))

    for test in tests:
        for runner in runners_for_test(test, runners):
            run_test(runner, test, rewrite)


if __name__ == "__main__":
    args = arg_parser.parse_args()
    run(args.patterns, args.rewrite)
