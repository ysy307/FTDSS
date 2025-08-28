#!/bin/zsh
set -e
set -o pipefail

# スクリプト終了時に必ず実行
trap '/usr/bin/python3 /workspaces/FTDSS/scripts/check.py' EXIT

cmake -S . -B CMakeBuild \
    -DBUILD_APP=test \
    -DCMAKE_BUILD_TYPE=Debug \
    -DCMAKE_Fortran_COMPILER=ifx \
    -DCMAKE_C_COMPILER=icx \
    -DCMAKE_CXX_COMPILER=icpx \
    -G "Ninja"

cmake --build CMakeBuild --parallel
