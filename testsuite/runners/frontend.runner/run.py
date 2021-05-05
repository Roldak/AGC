import frontend_support

from glob import glob


if __name__ == "__main__":
    gprs = glob("*.gpr")
    if len(gprs) == 0:
        frontend_support.run("test.adb")
    elif len(gprs) == 1:
        frontend_support.run("-P{}".format(gprs[0]))
    else:
        print("Multiple gpr files found: {}".format(", ".join(gprs)))
