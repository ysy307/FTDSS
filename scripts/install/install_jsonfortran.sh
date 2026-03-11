#!/bin/zsh

set -e

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

# Get absolute path of current directory
ROOT_DIR=$(realpath .)

# Directory for external dependencies
THIRD_PARTY_DIR="$ROOT_DIR/third_party"
JSONFORTRAN_DIR="$THIRD_PARTY_DIR/json-fortran"
INSTALL_PREFIX="$THIRD_PARTY_DIR/.local/$COMPILER"

# Create third_party directory if it does not exist
if [ ! -d "$THIRD_PARTY_DIR" ]; then
    mkdir -p "$THIRD_PARTY_DIR"
fi
cd "$THIRD_PARTY_DIR"

# Remove existing json-fortran directory if it exists
if [ -d "$JSONFORTRAN_DIR" ]; then
    rm -rf "$JSONFORTRAN_DIR"
fi

# Clone json-fortran from GitHub
git clone https://github.com/jacobwilliams/json-fortran.git
cd "$JSONFORTRAN_DIR"

# CMake configuration
cmake -B build \
      -G Ninja \
      -DCMAKE_Fortran_COMPILER=$FC_COMP \
      -DCMAKE_C_COMPILER=$CC_COMP \
      -DCMAKE_CXX_COMPILER=$CXX_COMP \
      -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX" \
      -DCMAKE_VERBOSE_MAKEFILE=On \
      -DCMAKE_BUILD_TYPE=Release \
      -DCMAKE_Fortran_MODULE_DIRECTORY="$JSONFORTRAN_DIR/build/include"

# Build
cmake --build build

# Install
cmake --install build --prefix "$INSTALL_PREFIX"