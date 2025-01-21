#!/bin/zsh
cmake -S . -B CMakeBuild -DBUILD_APP=test -DCMAKE_Fortran_COMPILER=ifx -G "Ninja"
cmake --build CMakeBuild
cmake --build CMakeBuild --target run_test