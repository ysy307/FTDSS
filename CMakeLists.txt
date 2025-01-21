cmake_minimum_required(VERSION 3.30)

# プロジェクト名と言語
project(YSY_FC LANGUAGES Fortran)

# オプション
option(ENABLE_OPTIMIZE "Enable optimization flags" ON)
option(ENABLE_OPENMP "Enable OpenMP support" ON)
option(ENABLE_MKL "Enable MKL support" ON)
option(ENABLE_MPI "Enable MPI support" OFF)
option(ENABLE_DEBUG "Enable debug flags" OFF)

# ビルドするアプリケーションの選択（デフォルトは空）
set(BUILD_APP "" CACHE STRING "Specify the application to build: ysy_fc or test")

# 出力ディレクトリ設定
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/.mod)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib)
set(CMAKE_INCLUDE_PATH ${PROJECT_SOURCE_DIR}/include)

# コンパイラ設定
if(NOT DEFINED CMAKE_Fortran_COMPILER)
    set(CMAKE_Fortran_COMPILER ifx)
endif()

# ソースファイルとオブジェクトファイル
file(GLOB_RECURSE ALL_SRC_FILES ${PROJECT_SOURCE_DIR}/src/**/*.f90)

# 共通のコンパイルフラグ
set(COMPILE_Fortran_FLAGS "-fpp -traceback -standard-semantics")
if(ENABLE_DEBUG)
    set(COMPILE_Fortran_FLAGS "${COMPILE_Fortran_FLAGS} -O0 -g -check all")
elseif(ENABLE_OPTIMIZE)
    set(COMPILE_Fortran_FLAGS "${COMPILE_Fortran_FLAGS} -O3 -flto -xHost")
endif()

# libraries settings
file(GLOB_RECURSE MY_ALL_LIBRARIES ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/*.a)
set(LINK_LIBRARIES ${MY_ALL_LIBRARIES})
# set(LINK_LIBRARIES ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/libjsonfortran-static.a)
# set(LINK_LIBRARIES ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/libtomlf.a ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/libvtkfortran.a ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/libjsonfortran-static.a)

if(ENABLE_OPENMP)
    find_package(OpenMP REQUIRED)
    list(APPEND LINK_LIBRARIES OpenMP::OpenMP_Fortran)
endif()

if(ENABLE_MKL)
    list(APPEND LINK_LIBRARIES
        -Wl,--start-group
        $ENV{MKLROOT}/lib/intel64/libmkl_intel_lp64.a
        $ENV{MKLROOT}/lib/intel64/libmkl_intel_thread.a
        $ENV{MKLROOT}/lib/intel64/libmkl_core.a
        -Wl,--end-group
        -liomp5
        -lpthread
        -lm
        -ldl
    )
endif()

if(ENABLE_MPI)
    find_package(MPI REQUIRED)
    list(APPEND LINK_LIBRARIES MPI::MPI_Fortran)
endif()

# Include path settings
set(My_INCLUDE_PATH ${CMAKE_INCLUDE_PATH}/tomlf ${CMAKE_INCLUDE_PATH}/VTKFortran ${CMAKE_INCLUDE_PATH}/Json-Fortran)

# Select build target
if(BUILD_APP STREQUAL "ysy_fc")
    add_executable(ysy_fc ${PROJECT_SOURCE_DIR}/app/ysy_fc.f90 ${ALL_SRC_FILES})
    target_compile_options(ysy_fc PRIVATE ${COMPILE_Fortran_FLAGS})
    target_link_libraries(ysy_fc PRIVATE ${LINK_LIBRARIES})
    target_include_directories(ysy_fc PUBLIC ${My_INCLUDE_PATH})
    add_custom_target(run_ysy_fc
        COMMAND OMP_NUM_THREADS=4 ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/ysy_fc
        DEPENDS ysy_fc
        WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
    )
elseif(BUILD_APP STREQUAL "test")
    add_executable(test ${PROJECT_SOURCE_DIR}/test/test.f90 ${ALL_SRC_FILES})
    target_compile_options(test PRIVATE ${COMPILE_Fortran_FLAGS})
    target_link_libraries(test PRIVATE ${LINK_LIBRARIES})
    target_include_directories(test PUBLIC ${My_INCLUDE_PATH})
    add_custom_target(run_test
        COMMAND OMP_NUM_THREADS=4 ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test
        DEPENDS test
        WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
    )
endif()
