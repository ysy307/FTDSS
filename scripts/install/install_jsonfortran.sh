#!/bin/zsh

set -e

# Use provided arguments or default to 'intel'
COMPILERS=("$@")
if [ ${#COMPILERS[@]} -eq 0 ]; then
    COMPILERS=("intel")
fi

ROOT_DIR=$(realpath .)
THIRD_PARTY_DIR="$ROOT_DIR/third_party"
JSONFORTRAN_DIR="$THIRD_PARTY_DIR/json-fortran"

mkdir -p "$THIRD_PARTY_DIR"
cd "$THIRD_PARTY_DIR"

# Clone json-fortran only once if it does not exist
if [ ! -d "$JSONFORTRAN_DIR" ]; then
    git clone https://github.com/jacobwilliams/json-fortran.git
fi
cd "$JSONFORTRAN_DIR"

# Loop through each specified compiler
for COMPILER in "${COMPILERS[@]}"; do
    echo "--- Building json-fortran for $COMPILER ---"

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
        echo "Error: Unknown compiler '$COMPILER'. Skipping."
        continue
        ;;
    esac

    INSTALL_PREFIX="$THIRD_PARTY_DIR/.local/$COMPILER"
    BUILD_DIR="build-$COMPILER"

    # Reset only the compiler-specific build directory
    if [ -d "$BUILD_DIR" ]; then
        rm -rf "$BUILD_DIR"
    fi

    # CMake configuration
    cmake -B "$BUILD_DIR" \
          -G Ninja \
          -DCMAKE_Fortran_COMPILER=$FC_COMP \
          -DCMAKE_C_COMPILER=$CC_COMP \
          -DCMAKE_CXX_COMPILER=$CXX_COMP \
          -DCMAKE_INSTALL_PREFIX="$INSTALL_PREFIX" \
          -DCMAKE_VERBOSE_MAKEFILE=On \
          -DCMAKE_BUILD_TYPE=Release \
          -DCMAKE_Fortran_MODULE_DIRECTORY="$JSONFORTRAN_DIR/$BUILD_DIR/include"

    # Build and Install
    cmake --build "$BUILD_DIR"
    cmake --install "$BUILD_DIR" --prefix "$INSTALL_PREFIX"

    echo "--- Completed json-fortran for $COMPILER ---"
done