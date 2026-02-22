#!/bin/zsh
set -e

ROOT_DIR=$(realpath .)

THIRD_PARTY_DIR="$ROOT_DIR/third_party"
IAPWS_DIR="$THIRD_PARTY_DIR/IAPWS"
INSTALL_PREFIX="$THIRD_PARTY_DIR/.local"

if [ ! -d "$THIRD_PARTY_DIR" ]; then
    mkdir -p "$THIRD_PARTY_DIR"
fi
cd "$THIRD_PARTY_DIR"

if [ -d "$IAPWS_DIR" ]; then
    echo "Removing existing IAPWS directory..."
    rm -rf "$IAPWS_DIR"
fi

echo "Cloning IAPWS repository..."
git clone https://github.com/ysy307/IAPWS.git "$IAPWS_DIR"
cd "$IAPWS_DIR"

cmake --preset=intel-release \
      -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX" \
      -DCMAKE_VERBOSE_MAKEFILE=On
cmake --build --preset=build-intel-release
cmake --install build/intel-release --prefix "$INSTALL_PREFIX"

