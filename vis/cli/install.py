#!/usr/bin/python3

from subprocess import Popen, PIPE
from sys import argv


def elm_install(pkg):
    cmd = Popen(
        ["elm", "install", pkg],
        stderr=PIPE,
        stdout=PIPE
    )
    _, stderr = cmd.communicate()

    if cmd.returncode != 0:
        print(f"[{pkg}]\tfailed")
        print(f"\t{stderr}")
    else:
        print(f"[{pkg}]\tsucceeded")


def main():
    [elm_install(pkg) for pkg in argv[1:]]


main()
