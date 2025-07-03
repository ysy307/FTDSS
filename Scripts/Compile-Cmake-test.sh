#!/bin/zsh
set -e  # エラーで停止
set -o pipefail
# cmake -S . -B CMakeBuild -DBUILD_APP=test -DCMAKE_BUILD_TYPE=Debug -G "Ninja" -DCOMPILER=intel
cmake -S . -B CMakeBuild -DBUILD_APP=test -DCMAKE_BUILD_TYPE=Release -G "Ninja" -DCOMPILER=intel
cmake --build CMakeBuild --parallel
./bin/test