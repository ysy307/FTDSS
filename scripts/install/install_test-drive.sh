#!/bin/bash

set -e

# Use provided arguments or default to 'intel'
COMPILERS=("$@")
if [ ${#COMPILERS[@]} -eq 0 ]; then
    COMPILERS=("intel")
fi

ROOT_DIR=$(realpath .)
THIRD_PARTY_DIR="$ROOT_DIR/third_party"
JSONFORTRAN_DIR="$THIRD_PARTY_DIR/test-drive"

mkdir -p "$THIRD_PARTY_DIR"
cd "$THIRD_PARTY_DIR"

# Clone test-drive only once if it does not exist
if [ ! -d "$JSONFORTRAN_DIR" ]; then
    git clone https://github.com/fortran-lang/test-drive.git
fi
cd "$JSONFORTRAN_DIR"

# Loop through each specified compiler
for COMPILER in "${COMPILERS[@]}"; do
    echo "--- Building test-drive for $COMPILER ---"

    case "$COMPILER" in
        intel)
            FC_COMP="ifx"
            CC_COMP="icx"
            CXX_COMP="icpx"
            # BuildSettings.cmake の Intel Debug 設定
            export FFLAGS="-O0 -g -traceback -stand f18 -fpp -fpscomp logicals -extend-source -check all -init=snan -init=arrays -warn all -warn errors -implicitnone -fstack-protector-all"
            ;;
        gnu)
            FC_COMP="gfortran"
            CC_COMP="gcc"
            CXX_COMP="g++"
            # BuildSettings.cmake の GNU Debug 設定
            export FFLAGS="-O0 -g -fbacktrace -std=f2018 -cpp -fcheck=all -ffpe-trap=invalid,zero,overflow -finit-real=snan -finit-integer=-9999999 -Wall -Wextra -Wno-maybe-uninitialized -Wno-uninitialized -Wno-c-binding-type -Wno-surprising -Wno-unused-dummy-argument -Wno-compare-reals -Wno-unused-function -Wno-unused-value"
            ;;
        nvidia)
            FC_COMP="nvfortran"
            CC_COMP="nvc"
            CXX_COMP="nvc++"
            # BuildSettings.cmake の NVHPC Debug 設定
            export FFLAGS="-O0 -g -Mbounds -Mchkptr -traceback -Ktrap=fp -Mpreprocess"
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

    echo "--- Completed test-drive for $COMPILER ---"
done
