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

# コンパイラ設定
set(CMAKE_Fortran_COMPILER ifx)

# ソースファイルとオブジェクトファイル
file(GLOB_RECURSE ALL_SRC_FILES ${PROJECT_SOURCE_DIR}/src/**/*.f90)

# 除外フォルダのファイルを除外
list(FILTER ALL_SRC_FILES EXCLUDE REGEX ".*/archive/.*")

# 共通のコンパイルフラグ
set(COMPILE_Fortran_FLAGS "-fpp -module -traceback -standard-semantics")
if(ENABLE_DEBUG)
    set(COMPILE_Fortran_FLAGS "${COMPILE_Fortran_FLAGS} -O0 -g -check all")
elseif(ENABLE_OPTIMIZE)
    set(COMPILE_Fortran_FLAGS "${COMPILE_Fortran_FLAGS} -O3 -flto -xHost")
endif()

# ライブラリ設定
set(LINK_LIBRARIES "")
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

# ビルドするターゲットを選択
if(BUILD_APP STREQUAL "ysy_fc")
    add_executable(CMAKE_ysy_fc ${PROJECT_SOURCE_DIR}/app/ysy_fc.f90 ${ALL_SRC_FILES})
    target_compile_options(CMAKE_ysy_fc PRIVATE ${COMPILE_Fortran_FLAGS})
    target_link_libraries(CMAKE_ysy_fc PRIVATE ${LINK_LIBRARIES})
    add_custom_target(run_ysy_fc
        COMMAND OMP_NUM_THREADS=4 ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/CMAKE_ysy_fc
        DEPENDS CMAKE_ysy_fc
        WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
    )
elseif(BUILD_APP STREQUAL "test")
    add_executable(CMAKE_test ${PROJECT_SOURCE_DIR}/app/test.f90 ${ALL_SRC_FILES})
    target_compile_options(CMAKE_test PRIVATE ${COMPILE_Fortran_FLAGS})
    target_link_libraries(CMAKE_test PRIVATE ${LINK_LIBRARIES})
    add_custom_target(run_test
        COMMAND OMP_NUM_THREADS=4 ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/CMAKE_test
        DEPENDS CMAKE_test
        WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
    )
else()
    message(FATAL_ERROR "Please specify a valid application to build with -DBUILD_APP=ysy_fc or -DBUILD_APP=test")
endif()
