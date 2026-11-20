#!/usr/bin/env bash

# Build the shared Qt SDK used to package Ladybird on Ubuntu 24.04.

set -euo pipefail

if [[ $# -ne 1 ]]; then
    echo "Usage: $0 BUILD_DIRECTORY" >&2
    exit 1
fi

qt_version=6.11.3
qt_archive_url="https://download.qt.io/archive/qt/${qt_version%.*}/${qt_version}/submodules"
build_directory="$(realpath -m "$1")"
downloads="${build_directory}/downloads"
prefix="${build_directory}/install"
licenses_directory="${prefix}/share/LadybirdQt"
mkdir -p "$downloads"

build_module() {
    local module="$1" sha256="$2"
    local archive="${module}-everywhere-src-${qt_version}.tar.xz"
    local source_directory="${build_directory}/${module}-everywhere-src-${qt_version}"
    local module_build_directory="${build_directory}/${module}-build"
    local archive_path="${downloads}/${archive}"

    if ! printf '%s  %s\n' "$sha256" "$archive_path" | sha256sum --check --status; then
        curl --fail --silent --show-error --location --retry 5 --retry-all-errors \
            --output "${archive_path}.tmp" "${qt_archive_url}/${archive}"
        printf '%s  %s\n' "$sha256" "${archive_path}.tmp" | sha256sum --check
        mv "${archive_path}.tmp" "$archive_path"
    fi
    if [[ ! -d "$source_directory" ]]; then
        (
            unpack_directory="$(mktemp -d "${build_directory}/unpack.XXXXXX")"
            trap 'rm -rf "$unpack_directory"' EXIT
            tar -xf "$archive_path" -C "$unpack_directory"
            mv "${unpack_directory}/${source_directory##*/}" "$source_directory"
        )
    fi
    mkdir -p "$module_build_directory"
    (
        cd "$module_build_directory"
        if [[ "$module" == qtbase ]]; then
            "$source_directory/configure" -prefix "$prefix" -release -shared -nomake examples -nomake tests \
                -xcb -feature-wayland -feature-accessibility-atspi-bridge -openssl-runtime -sbom
        else
            "$prefix/bin/qt-configure-module" "$source_directory" -- \
                -DQT_BUILD_TESTS=OFF -DQT_BUILD_EXAMPLES=OFF -DQT_GENERATE_SBOM=ON
        fi
        cmake --build .
        cmake --install .
    )
    install -d "${licenses_directory}/${module}"
    find "${source_directory}/LICENSES" -maxdepth 1 -name '*.txt' ! -name LicenseRef-Qt-Commercial.txt \
        -exec install -m 0644 -t "${licenses_directory}/${module}" {} +
}

build_module qtbase 46de0710b594945a5d446ae44354daa386141c6903c53781b2ca547bc62f8f8f
build_module qtserialport b09bfb0eace070ce1b980f7f2c9ba9fb885c2cc29a4cd0416061496426601722
build_module qtpositioning a38695bc412b7261476e8e96178460ec9319e61c34ecc6a14eb2e76e5c73eca1
build_module qtsvg 9bdf6fa4b5a2d91c3eaa2edd12263fdff0ae19d0ac25d3c99946cd4189a07123

qtbase_licenses="${build_directory}/qtbase-everywhere-src-${qt_version}/LICENSES"
{
    printf 'This build of Ladybird bundles Qt %s under the GNU Lesser General Public License, version 3.\n\n' "$qt_version"
    printf 'Copyright (C) The Qt Company Ltd. and other contributors.\n\n'
    printf 'Qt source archives: %s/\n' "$qt_archive_url"
    printf 'Qt and third-party attributions: share/doc/ladybird/qt/\n\n'
    cat "${qtbase_licenses}/LGPL-3.0-only.txt" "${qtbase_licenses}/GPL-3.0-only.txt"
} > "${licenses_directory}/LICENSES.txt"
