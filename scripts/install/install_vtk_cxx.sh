#!/bin/zsh

set -e
set -o pipefail

# Use provided arguments or default to 'intel'
COMPILERS=("$@")
if [ ${#COMPILERS[@]} -eq 0 ]; then
  COMPILERS=("intel")
fi

# Check dependencies (Execute once)
command -v cmake >/dev/null || { echo "Error: You must install CMake"; exit 1; }
command -v ninja >/dev/null || { echo "Error: You must install ninja"; exit 1; }

# Removed GUI/Rendering related dependencies
sudo apt update
sudo apt install -y \
  build-essential cmake g++ git ninja-build \
  zlib1g-dev libjpeg-dev libpng-dev libtiff-dev \
  libdouble-conversion-dev libblas-dev liblapack-dev

# Directory configurations
ROOT_DIR=$(realpath .)
THIRD_PARTY_DIR="$ROOT_DIR/third_party"
VTK_SRC_DIR="$THIRD_PARTY_DIR/vtk"

mkdir -p "$THIRD_PARTY_DIR"
cd "$THIRD_PARTY_DIR"

# Download and extract VTK only once
VTK_VERSION="9.5.0"
VTK_MAJOR_MINOR=$(echo "$VTK_VERSION" | cut -d. -f1,2)
VTK_TAR="VTK-$VTK_VERSION.tar.gz"
VTK_URL="https://www.vtk.org/files/release/$VTK_MAJOR_MINOR/$VTK_TAR"

if [ ! -d "$VTK_SRC_DIR" ]; then
  if [ ! -f "$VTK_TAR" ]; then
    wget -O "$VTK_TAR" "$VTK_URL"
  fi
  tar xf "$VTK_TAR"
  mv "VTK-$VTK_VERSION" "$VTK_SRC_DIR"
fi

# Loop through each specified compiler
for COMPILER in "${COMPILERS[@]}"; do
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
      continue
      ;;
  esac

  VTK_BUILD_DIR="$VTK_SRC_DIR/build-$COMPILER"
  VTK_INSTALL_DIR="$THIRD_PARTY_DIR/.local/$COMPILER"

  # Reset only the compiler-specific build directory
  if [ -d "$VTK_BUILD_DIR" ]; then
    rm -rf "$VTK_BUILD_DIR"
  fi
  mkdir -p "$VTK_BUILD_DIR"
  cd "$VTK_BUILD_DIR"

  # CMake configuration: Build ONLY necessary modules
  cmake .. \
    -DCMAKE_Fortran_COMPILER=$FC_COMP \
    -DCMAKE_C_COMPILER=$CC_COMP \
    -DCMAKE_CXX_COMPILER=$CXX_COMP \
    -DCMAKE_BUILD_TYPE=Release \
    -DCMAKE_INSTALL_PREFIX="$VTK_INSTALL_DIR" \
    -DBUILD_SHARED_LIBS=OFF \
    -DVTK_BUILD_TESTING=OFF \
    -DVTK_GROUP_ENABLE_Rendering=DONT_WANT \
    -DVTK_GROUP_ENABLE_StandAlone=DONT_WANT \
    -DVTK_MODULE_ENABLE_VTK_CommonCore=YES \
    -DVTK_MODULE_ENABLE_VTK_CommonDataModel=YES \
    -DVTK_MODULE_ENABLE_VTK_IOLegacy=YES \
    -DVTK_MODULE_ENABLE_VTK_IOXML=YES \
    -G Ninja

  # Build and Install
  ninja
  ninja install
done