# CMake Minimum Version
cmake_minimum_required(VERSION 3.20)

# ====================================================================
# 1. Options and Toolchain Setup
# ====================================================================

# プロジェクトの挙動を決定するオプションを先に定義
option(ENABLE_OPENMP "Enable OpenMP support" ON)
option(ENABLE_MKL    "Enable MKL support"    ON)
option(ENABLE_MPI    "Enable MPI support"    ON)
option(ENABLE_DEBUG  "Enable debug flags"    OFF)

# ENABLE_MPIオプションに応じてコンパイラを自動で設定
# NOTE: project()コマンドの前にコンパイラを設定するのがベストプラクティスです
if(ENABLE_MPI)
    message(STATUS "MPI support is ON. Setting compiler to MPI wrappers.")
    set(CMAKE_Fortran_COMPILER mpiifx CACHE STRING "MPI Fortran Compiler" FORCE)
    set(CMAKE_CXX_COMPILER   mpiicpx CACHE STRING "MPI CXX Compiler" FORCE)
    set(CMAKE_C_COMPILER     mpiicx  CACHE STRING "MPI C Compiler" FORCE)
else()
    message(STATUS "MPI support is OFF. Setting compiler to standard Intel compilers.")
    set(CMAKE_Fortran_COMPILER ifx CACHE STRING "Fortran Compiler" FORCE)
    set(CMAKE_CXX_COMPILER   icpx CACHE STRING "CXX Compiler" FORCE)
    set(CMAKE_C_COMPILER     icx  CACHE STRING "C Compiler" FORCE)
endif()

# Project Name and Language
project(YSY_FC LANGUAGES Fortran C CXX)

# Build Type
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build: Debug or Release" FORCE)
endif()
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")

# Select the application to build
set(BUILD_APP "ysy_fc" CACHE STRING "Specify the application to build: ysy_fc or test")

# Output Directories
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/.mod)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib)

# Include custom CMake modules (Toolchain.cmakeがMPI/MKLを探す)
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
include(Toolchain)


# ====================================================================
# 2. Source Tree Definition
# ====================================================================
add_subdirectory(src/core)
add_subdirectory(src/inout)
add_subdirectory(src/linalg)
add_subdirectory(src/control)
add_subdirectory(src/physics)
add_subdirectory(src/domain)
# add_subdirectory(src/solver)
# add_subdirectory(src/field)
# add_subdirectory(src/conditions)
# add_subdirectory(src/main)


# ====================================================================
# 3. Build Targets
# ====================================================================

# 実行ファイルのソースを指定
set(MAIN_APP_SOURCE "app/ysy_fc.F90")
set(TEST_APP_SOURCE "test/test_domain.F90")

# Build Targets
if(BUILD_APP STREQUAL "ysy_fc")
    add_executable(ysy_fc ${MAIN_APP_SOURCE})
    set(APP_TARGET ysy_fc)

elseif(BUILD_APP STREQUAL "test")
    enable_language(Fortran)
    add_executable(test ${TEST_APP_SOURCE})
    set_target_properties(test PROPERTIES LINKER_LANGUAGE Fortran)
    set(APP_TARGET test)
    enable_build_flags(test)
    enable_thirdparty(test)
endif()

# ライブラリのリンク
if(TARGET ${APP_TARGET})
    # まず、アプリケーションのコアとなるライブラリをリンク
    target_link_libraries(${APP_TARGET} PRIVATE Solver::Input Solver::Core Solver::Control Solver::Physics Solver::Domain Solver::Linalg)

    # MPIが有効な場合、MPIライブラリを追加でリンク
    if(ENABLE_MPI AND TARGET MPI::MPI_Fortran)
        message(STATUS "Linking ${APP_TARGET} with MPI libraries.")
        target_link_libraries(${APP_TARGET} PRIVATE MPI::MPI_Fortran)
    endif()
endif()

# カスタム実行ターゲット (MPIの有無で実行コマンドを切り替え)
if(BUILD_APP STREQUAL "ysy_fc")
    if(ENABLE_MPI)
        add_custom_target(run_ysy_fc
            COMMAND mpirun -np 4 ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/ysy_fc
            DEPENDS ysy_fc
            WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
        )
    else()
        add_custom_target(run_ysy_fc
            COMMAND OMP_NUM_THREADS=4 ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/ysy_fc
            DEPENDS ysy_fc
            WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
        )
    endif()
endif()


# Final Messages
if(BUILD_APP STREQUAL "")
    message(WARNING "No application selected to build. Set BUILD_APP to 'ysy_fc' or 'test'.")
endif()