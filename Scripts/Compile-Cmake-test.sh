#!/bin/zsh
cmake -S . -B CMakeBuild -DBUILD_APP=test -G "Ninja" -DCMAKE_VERBOSE_MAKEFILE=ON
# cmake -S . -B CMakeBuild -DBUILD_APP=test
cmake --build CMakeBuild
cmake --build CMakeBuild --target run_test