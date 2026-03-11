#!/bin/zsh

set -e

# Parse arguments (default: intel)
COMPILER=${1:-intel}

# Set preset names based on compiler selection
case "$COMPILER" in
  intel)
    CONFIG_PRESET="intel-release"
    BUILD_PRESET="build-intel-release"
    ;;
  gcc)
    CONFIG_PRESET="gcc-release"
    BUILD_PRESET="build-gcc-release"
    ;;
  nvidia)
    CONFIG_PRESET="nvidia-release"
    BUILD_PRESET="build-nvidia-release"
    ;;
  *)
    echo "Error: Unknown compiler '$COMPILER'. Use intel, gcc, or nvidia."
    exit 1
    ;;
esac

ROOT_DIR=$(realpath .)

THIRD_PARTY_DIR="$ROOT_DIR/third_party"
IAPWS_DIR="$THIRD_PARTY_DIR/IAPWS"
INSTALL_PREFIX="$THIRD_PARTY_DIR/.local/$COMPILER"

if [ ! -d "$THIRD_PARTY_DIR" ]; then
    mkdir -p "$THIRD_PARTY_DIR"
fi
cd "$THIRD_PARTY_DIR"

if [ -d "$IAPWS_DIR" ]; then
    rm -rf "$IAPWS_DIR"
fi

git clone https://github.com/ysy307/IAPWS.git "$IAPWS_DIR"
cd "$IAPWS_DIR"

# Configure using preset
cmake --preset="$CONFIG_PRESET" \
      -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX" \
      -DCMAKE_VERBOSE_MAKEFILE=On

# Build using preset
cmake --build --preset="$BUILD_PRESET"

# Install from preset build directory
cmake --install "build/$CONFIG_PRESET" --prefix "$INSTALL_PREFIX"