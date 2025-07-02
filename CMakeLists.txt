# CMake Minimum Version
cmake_minimum_required(VERSION 3.30)

# コンパイラ指定
if(DEFINED COMPILER)
    if(COMPILER STREQUAL "intel")
        set(CMAKE_Fortran_COMPILER ifx CACHE STRING "" FORCE)
        set(CMAKE_C_COMPILER icx CACHE STRING "" FORCE)
    elseif(COMPILER STREQUAL "gnu")
        set(CMAKE_Fortran_COMPILER gfortran CACHE STRING "" FORCE)
        set(CMAKE_C_COMPILER gcc CACHE STRING "" FORCE)
    else()
        message(FATAL_ERROR "Unknown COMPILER specified: ${COMPILER}")
    endif()
endif()

# Project Name and Language
project(YSY_FC LANGUAGES Fortran C)
enable_language(Fortran C)

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING
      "Choose the type of build: Debug or Release" FORCE)
endif()
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")

# Options
option(ENABLE_OPTIMIZE "Enable optimization flags" ON)
option(ENABLE_OPENMP   "Enable OpenMP support"     ON)
option(ENABLE_MKL      "Enable MKL support"        ON)
option(ENABLE_MPI      "Enable MPI support"        OFF)
option(ENABLE_DEBUG    "Enable debug flags"        OFF)

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
add_subdirectory(src/Core)
add_subdirectory(src/Inout/Input)
add_subdirectory(src/Domain)
add_subdirectory(src/Matrix)
add_subdirectory(src/Calculate/BLAS)
add_subdirectory(src/Calculate/WRF)
add_subdirectory(src/Calculate/HCF)
add_subdirectory(src/Calculate/GCC)
add_subdirectory(src/Calculate/ThermalConductivity)
add_subdirectory(src/Properties)
add_subdirectory(src/Calculate/Density)
add_subdirectory(src/Calculate/SpecificHeat)
add_subdirectory(src/Calculate/HeatCapacity)
add_subdirectory(src/Conditions/Boundary)
add_subdirectory(src/Conditions/Initial)
# add_subdirectory(src/Calculate/Ice)
add_subdirectory(src/Solver)
add_subdirectory(src/Main/Thermal)
add_subdirectory(src/Inout/Output)
add_subdirectory(src/Main/FTDSS)

# Debug Information
message(STATUS "Fortran Compiler ID: ${CMAKE_Fortran_COMPILER_ID}")

set(MAIN_FILE "${PROJECT_SOURCE_DIR}/app/ysy_fc.F90")
set(TEST_FILE "${PROJECT_SOURCE_DIR}/test/test.F90")

# Build Targets
if(BUILD_APP STREQUAL "ysy_fc")
    # ysy_fc Target
    add_executable(ysy_fc ${MAIN_FILE})
    target_link_libraries(ysy_fc PRIVATE
        Core
        Input
        Domain
        Calc_WRF
        Calc_HCF
        Calc_GCC
        Calc_THC
        Calc_VHC
        Calc_DEN
        Calc_SPH
        Properties
        Matrix
        Calc_BLAS
        Boundary
        Initial
        Solver
        Thermal
        Output
        FTDSS
    )
    enable_build_flags(ysy_fc)
    enable_thirdparty (ysy_fc)


    # Custom Run Target
    add_custom_target(run_ysy_fc
        COMMAND OMP_NUM_THREADS=4 ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/ysy_fc
        DEPENDS ysy_fc
        WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
    )

elseif(BUILD_APP STREQUAL "test")
    add_executable(test ${TEST_FILE})
    target_link_libraries(test PRIVATE
        Core
        Input
        Domain
        Calc_WRF
        Calc_HCF
        Calc_GCC
        Calc_THC
        Calc_VHC
        Calc_DEN
        Calc_SPH
        Properties
        Matrix
        Calc_BLAS
        Boundary
        Initial
        Solver
        Thermal
        Output
        FTDSS
    )
    enable_build_flags(test)
    enable_thirdparty (test)

endif()

# Final Messages
if(BUILD_APP STREQUAL "")
    message(WARNING "No application selected to build. Set BUILD_APP to 'ysy_fc' or 'test'.")
endif()
