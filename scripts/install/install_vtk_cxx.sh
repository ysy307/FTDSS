#!/bin/bash

set -e
set -o pipefail

# Parse arguments (default: intel)
COMPILER=${1:-intel}

# Set compiler configurations
case "$COMPILER" in
  intel)
    FC_COMP="ifx"
    CC_COMP="icx"
    CXX_COMP="icpx"
    export FFLAGS="-O3 -xCORE-AVX2"
    ;;
  gnu)
    FC_COMP="gfortran"
    CC_COMP="gcc"
    CXX_COMP="g++"
    export FFLAGS="-O3 -march=native"
    ;;
  nvidia)
    FC_COMP="nvfortran"
    CC_COMP="nvc"
    CXX_COMP="nvc++"
    export FFLAGS="-O3 -fast"
    ;;
  *)
    echo "Error: Unknown compiler '$COMPILER'. Use intel, gnu, or nvidia."
    exit 1
    ;;
esac

# Directory configurations
ROOT_DIR=$(realpath .)
THIRD_PARTY_DIR="$ROOT_DIR/third_party"
VTK_SRC_DIR="$THIRD_PARTY_DIR/vtk"
VTK_BUILD_DIR="$VTK_SRC_DIR/build"
VTK_INSTALL_DIR="$THIRD_PARTY_DIR/.local/$COMPILER"

# Check dependencies
command -v cmake >/dev/null || { echo "Error: You must install CMake"; exit 1; }
command -v ninja >/dev/null || { echo "Error: You must install ninja"; exit 1; }

apt update
apt install -y \
  build-essential cmake g++ git \
  libgl1-mesa-dev libxt-dev \
  libxrender-dev libxext-dev \
  qtbase5-dev libeigen3-dev \
  zlib1g-dev libjpeg-dev libpng-dev \
  libtiff-dev libtheora-dev libogg-dev \
  libdouble-conversion-dev libblas-dev liblapack-dev

VTK_VERSION="9.5.0"
VTK_MAJOR_MINOR=$(echo "$VTK_VERSION" | cut -d. -f1,2)
VTK_TAR="VTK-$VTK_VERSION.tar.gz"
VTK_URL="https://www.vtk.org/files/release/$VTK_MAJOR_MINOR/$VTK_TAR"

cd "$THIRD_PARTY_DIR"
mkdir -p "$VTK_SRC_DIR"

if [ ! -f "$VTK_TAR" ]; then
  echo "Downloading $VTK_TAR from $VTK_URL"
  wget -O "$VTK_TAR" "$VTK_URL"
fi

rm -rf "$VTK_SRC_DIR"
tar xf "$VTK_TAR"
mv "VTK-$VTK_VERSION" "$VTK_SRC_DIR"

# Build
mkdir -p "$VTK_BUILD_DIR"
cd "$VTK_BUILD_DIR"

cmake .. \
  -DCMAKE_Fortran_COMPILER=$FC_COMP \
  -DCMAKE_C_COMPILER=$CC_COMP \
  -DCMAKE_CXX_COMPILER=$CXX_COMP \
  -DCMAKE_BUILD_TYPE=Release \
  -DCMAKE_INSTALL_PREFIX="$VTK_INSTALL_DIR" \
  -DBUILD_SHARED_LIBS=OFF \
  -DVTK_BUILD_TESTING=OFF \
  -DVTK_GROUP_ENABLE_Rendering=YES \
  -DVTK_GROUP_ENABLE_StandAlone=YES \
  -G Ninja

ninja
ninja install

echo "VTK Installed to: $VTK_INSTALL_DIR"