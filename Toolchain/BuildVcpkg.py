#!/usr/bin/env python3

import os
import subprocess
import pathlib
import sys
import shutil


def main() -> int:
    script_dir = pathlib.Path(__file__).parent.resolve()

    git_repo = "https://github.com/microsoft/vcpkg.git"
    git_rev = "369cb5c76ab32408b87fa3f7557e0660b375e582"  # 2024.10.14

    tarball_dir = script_dir / "Tarballs"
    tarball_dir.mkdir(parents=True, exist_ok=True)
    vcpkg_checkout = tarball_dir / "vcpkg"

    if not vcpkg_checkout.is_dir():
        subprocess.check_call(args=["git", "clone", git_repo], cwd=tarball_dir)
    else:
        bootstrapped_vcpkg_version = subprocess.check_output(
            ["git", "-C", vcpkg_checkout, "rev-parse", "HEAD"]).strip().decode()

        if bootstrapped_vcpkg_version == git_rev:
            return 0

    print(f"Building vcpkg@{git_rev}")

    subprocess.check_call(args=["git", "fetch", "origin"], cwd=vcpkg_checkout)
    subprocess.check_call(args=["git", "checkout", git_rev], cwd=vcpkg_checkout)

    bootstrap_script = "bootstrap-vcpkg.bat" if os.name == 'nt' else "bootstrap-vcpkg.sh"
    subprocess.check_call(args=[vcpkg_checkout / bootstrap_script, "-disableMetrics"], cwd=vcpkg_checkout, shell=True)

    install_dir = script_dir / "Local" / "vcpkg" / "bin"
    install_dir.mkdir(parents=True, exist_ok=True)

    vcpkg_name = "vcpkg.exe" if os.name == 'nt' else "vcpkg"
    shutil.copy(vcpkg_checkout / vcpkg_name, install_dir / vcpkg_name)

    return 0


if __name__ == "__main__":
    sys.exit(main())
