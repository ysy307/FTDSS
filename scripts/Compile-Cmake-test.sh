#!/bin/zsh
set -e
set -o pipefail

cmake -S . -B CMakeBuild \
    -DBUILD_APP=test \
    -DCMAKE_BUILD_TYPE=Debug \
    -DCMAKE_Fortran_COMPILER=ifx \
    -DCMAKE_C_COMPILER=icx \
    -DCMAKE_CXX_COMPILER=icpx \
    -G "Ninja"

# cmake --build CMakeBuild
cmake --build CMakeBuild --parallel