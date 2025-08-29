# CMake Minimum Version
cmake_minimum_required(VERSION 3.20)

# Project Name and Language
project(YSY_FC LANGUAGES Fortran C CXX)

# ====================================================================
# 1. Compiler and Build Type Setup
# ====================================================================
# (このセクションはあなたの設定が素晴らしいので、そのまま使います)
if(NOT CMAKE_Fortran_COMPILER)
    message(STATUS "Fortran compiler not specified, defaulting to 'ifx'.")
    set(CMAKE_Fortran_COMPILER ifx)
endif()
if(NOT CMAKE_CXX_COMPILER)
    message(STATUS "CXX compiler not specified, defaulting to 'icpx'.")
    set(CMAKE_CXX_COMPILER icpx)
endif()
if(NOT CMAKE_C_COMPILER)
    message(STATUS "C compiler not specified, defaulting to 'icx'.")
    set(CMAKE_C_COMPILER icx)
endif()

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build: Debug or Release" FORCE)
endif()
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")

# Options
option(ENABLE_OPENMP "Enable OpenMP support" ON)
option(ENABLE_MKL "Enable MKL support" ON)
option(ENABLE_MPI "Enable MPI support" OFF)
option(ENABLE_DEBUG "Enable debug flags" ON)

# Select the application to build
set(BUILD_APP "ysy_fc" CACHE STRING "Specify the application to build: ysy_fc or test")

# Output Directories
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/.mod)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib)

# Include custom CMake modules
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
include(Toolchain)


# ====================================================================
# 2. Source Tree Definition
# ====================================================================
# 各モジュールのトップディレクトリを一度だけ呼び出すことで、構造をクリーンに保つ
add_subdirectory(src/core)
add_subdirectory(src/inout)
add_subdirectory(src/linalg)
add_subdirectory(src/control)
add_subdirectory(src/domain)
add_subdirectory(src/physics)
add_subdirectory(src/solver)
add_subdirectory(src/field)
add_subdirectory(src/conditions)
add_subdirectory(src/main)


# ====================================================================
# 3. Build Targets
# ====================================================================

# 実行ファイルのソースを指定
set(MAIN_APP_SOURCE "app/ysy_fc.F90")
set(TEST_APP_SOURCE "test/test.F90")

# Build Targets
if(BUILD_APP STREQUAL "ysy_fc")
    add_executable(ysy_fc ${MAIN_APP_SOURCE})

    # FTDSSアプリケーションは、Solver::FTDSSにリンクする
    # これにより、必要な全てのライブラリが自動的にリンクされる
    target_link_libraries(ysy_fc PRIVATE Solver::FTDSS)

    # カスタム実行ターゲット
    add_custom_target(run_ysy_fc
        COMMAND OMP_NUM_THREADS=4 ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/ysy_fc
        DEPENDS ysy_fc
        WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
    )

elseif(BUILD_APP STREQUAL "test")
    add_executable(test ${TEST_APP_SOURCE})

    # テストの目的に応じて、リンクするライブラリを選択する
    # 例1: Coreライブラリの単体テストを行う場合
    # target_link_libraries(test PRIVATE Solver::Core)

    # 例2: ソルバー全体の結合テストを行う場合
    target_link_libraries(test PRIVATE Solver::FTDSS)
endif()


# Final Messages
if(BUILD_APP STREQUAL "")
    message(WARNING "No application selected to build. Set BUILD_APP to 'ysy_fc' or 'test'.")
endif()