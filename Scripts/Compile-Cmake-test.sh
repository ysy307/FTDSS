#!/bin/zsh
cmake -S . -B CMakeBuild -DBUILD_APP=test -G "Unix Makefiles"
# cmake -S . -B CMakeBuild -DBUILD_APP=test -DCMAKE_Fortran_COMPILER=ifx -DCMAKE_"-fpp -traceback -standard-semantics" -G "Ninja"
cmake --build CMakeBuild
cmake --build CMakeBuild --target run_test