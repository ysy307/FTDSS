#!/bin/zsh
set -e
set -o pipefail

# スクリプト終了時に必ず実行
trap '/usr/bin/python3 /workspaces/FTDSS/scripts/check.py' EXIT

cmake -S . -B CMakeBuild \
    -DBUILD_APP=test \
    -DTEST_NAME=core \
    -DCMAKE_BUILD_TYPE=Debug \
    -DCMAKE_Fortran_COMPILER=mpiifx \
    -DCMAKE_C_COMPILER=mpiicx \
    -DCMAKE_CXX_COMPILER=mpiicpx \
    -DMKL_SYCL_LINK=OFF \
    -G "Ninja"

cmake --build CMakeBuild --parallel --verbose
