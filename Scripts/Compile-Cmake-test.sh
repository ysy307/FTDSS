#!/bin/zsh
cmake -S . -B CMakeBuild -DBUILD_APP=test -DCMAKE_BUILD_TYPE=Release -G "Ninja"
cmake --build CMakeBuild --parallel