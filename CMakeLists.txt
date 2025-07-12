# CMake Minimum Version
cmake_minimum_required(VERSION 3.20)

# Project Name and Language
project(YSY_FC LANGUAGES Fortran C CXX)
enable_language(Fortran C CXX)

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
  set(CMAKE_BUILD_TYPE Release CACHE STRING
      "Choose the type of build: Debug or Release" FORCE)
endif()
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")

# Options
option(ENABLE_OPTIMIZE "Enable optimization flags" OFF)
option(ENABLE_OPENMP   "Enable OpenMP support"     ON)
option(ENABLE_MKL      "Enable MKL support"        ON)
option(ENABLE_MPI      "Enable MPI support"        OFF)
option(ENABLE_DEBUG    "Enable debug flags"        ON)

# Select the application to build
set(BUILD_APP "" CACHE STRING "Specify the application to build: ysy_fc or test")

# Output Directories
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/.mod)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib)
set(CMAKE_INCLUDE_PATH             ${PROJECT_SOURCE_DIR}/include)

# 共通スクリプト読み込み
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
include(Toolchain)

# Source Files
add_subdirectory(src/core)
add_subdirectory(src/inout/Input)
add_subdirectory(src/matrix)
add_subdirectory(src/domain)
add_subdirectory(src/calculate/BLAS)
add_subdirectory(src/calculate/WRF)
add_subdirectory(src/calculate/HCF)
add_subdirectory(src/calculate/GCC)
add_subdirectory(src/calculate/ThermalConductivity)
add_subdirectory(src/calculate/density)
add_subdirectory(src/calculate/SpecificHeat)
add_subdirectory(src/calculate/HeatCapacity)
add_subdirectory(src/properties)
add_subdirectory(src/conditions/Boundary)
add_subdirectory(src/conditions/Initial)
add_subdirectory(src/solver)
add_subdirectory(src/control)
add_subdirectory(src/main/Thermal)
add_subdirectory(src/inout/Output)
add_subdirectory(src/main/FTDSS)

# =========================================================================
# ★改善点1: 全ての共通ライブラリをグループ化するINTERFACEライブラリを定義
# =========================================================================
set(COMMON_LIBS
    Core
    Input
    Matrix 
    Domain 
    Calc_WRF
    Calc_HCF
    Calc_GCC
    Calc_THC
    Calc_DEN
    Calc_SPH
    Calc_VHC
    Properties
    Calc_BLAS
    Boundary
    Initial
    Solver
    control
    Thermal
    Output
    FTDSS
)

# Debug Information
message(STATUS "Fortran Compiler ID: ${CMAKE_Fortran_COMPILER_ID}")

set(MAIN_FILE "${PROJECT_SOURCE_DIR}/app/ysy_fc.F90")
set(TEST_FILE "${PROJECT_SOURCE_DIR}/test/test.F90")

# Build Targets
if(BUILD_APP STREQUAL "ysy_fc")
    # ysy_fc Target
    add_executable(ysy_fc ${MAIN_FILE})
    target_link_libraries(ysy_fc PRIVATE ${COMMON_LIBS})
    enable_build_flags(ysy_fc)
    enable_thirdparty (ysy_fc)
    set_target_properties(ysy_fc PROPERTIES LINKER_LANGUAGE Fortran)


    # Custom Run Target
    add_custom_target(run_ysy_fc
        COMMAND OMP_NUM_THREADS=4 ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/ysy_fc
        DEPENDS ysy_fc
        WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
    )

elseif(BUILD_APP STREQUAL "test")
    add_executable(test ${TEST_FILE})
    target_link_libraries(test PRIVATE ${COMMON_LIBS})
    enable_build_flags(test)
    enable_thirdparty (test)
    set_target_properties(test PROPERTIES LINKER_LANGUAGE Fortran)
endif()


# Final Messages
if(BUILD_APP STREQUAL "")
    message(WARNING "No application selected to build. Set BUILD_APP to 'ysy_fc' or 'test'.")
endif()
