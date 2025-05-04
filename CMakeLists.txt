# CMake Minimum Version
cmake_minimum_required(VERSION 3.30)

# Project Name and Language
project(YSY_FC LANGUAGES Fortran)
enable_language(Fortran)

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

### Compiler Configuration
if(NOT DEFINED CMAKE_Fortran_COMPILER)
    set(CMAKE_Fortran_COMPILER ifx)
endif()

# Source Files
add_subdirectory(src/Core)
add_subdirectory(src/Matrix)
add_subdirectory(src/Inout/Input)
add_subdirectory(src/Conditions)
add_subdirectory(src/Calculate/BLAS)
add_subdirectory(src/Calculate/WRF)
add_subdirectory(src/Calculate/HCF)
add_subdirectory(src/Calculate/GCC)
add_subdirectory(src/Calculate/ThermalConductivity)
add_subdirectory(src/Calculate/Density)
add_subdirectory(src/Calculate/SpecificHeat)
add_subdirectory(src/Calculate/HeatCapacity)
add_subdirectory(src/Calculate/Ice)
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
        Matrix
        Calc_BLAS
        Conditions
        Input
        Calc_WRF
        Calc_HCF
        Calc_GCC
        Calc_Ice
        Calc_THC
        Calc_VHC
        Calc_DEN
        Calc_SPH
        Solver
        Thermal
        Output
        FTDSS
        ${LINK_LIBRARIES}
    )
    target_link_libraries(ysy_fc PRIVATE ${LINK_LIBRARIES})
    target_include_directories(ysy_fc PUBLIC ${My_INCLUDE_PATH})
    

    # Compilation Flags
    target_compile_options(ysy_fc PUBLIC -fpp -traceback)
    target_link_options   (ysy_fc PUBLIC -fpp -traceback)

    # MKL Configuration
    if(ENABLE_MKL)
        set(MKL_LINK "static")
        find_package(MKL CONFIG REQUIRED PATHS $ENV{MKLROOT})
        target_compile_options    (ysy_fc PUBLIC $<TARGET_PROPERTY:MKL::MKL,INTERFACE_COMPILE_OPTIONS>)
        target_include_directories(ysy_fc PUBLIC $<TARGET_PROPERTY:MKL::MKL,INTERFACE_INCLUDE_DIRECTORIES>)
        target_link_libraries     (ysy_fc PUBLIC $<LINK_ONLY:MKL::MKL>)
        add_definitions(-D_MKL)
    endif()

    if(ENABLE_DEBUG)
        target_compile_options(ysy_fc PUBLIC -O0 -g -check all)
        target_link_options   (ysy_fc PUBLIC -O0 -g -check all)
    elseif(ENABLE_OPTIMIZE)
        target_compile_options(ysy_fc PUBLIC -O3 -flto -xCORE-AVX2)
        target_link_options   (ysy_fc PUBLIC -O3 -flto -xCORE-AVX2)
    endif()

    # Custom Run Target
    add_custom_target(run_ysy_fc
        COMMAND OMP_NUM_THREADS=4 ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/ysy_fc
        DEPENDS ysy_fc
        WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
    )

elseif(BUILD_APP STREQUAL "test")
    add_executable(test ${TEST_FILE})
    target_include_directories(test PUBLIC ${My_INCLUDE_PATH})
    target_compile_options(test PUBLIC -fpp -traceback)
    target_link_options   (test PUBLIC -fpp -traceback)

    # Link core libraries
    target_link_libraries(test PRIVATE
        Core
        Matrix
        Calc_BLAS
        Conditions
        Input
        Calc_WRF
        Calc_HCF
        Calc_GCC
        Calc_Ice
        Calc_THC
        Calc_VHC
        Calc_DEN
        Solver
        Thermal
        Output
        FTDSS
        ${LINK_LIBRARIES}
    )

    # Link external and optional libraries
    if(ENABLE_MKL)
        set(MKL_LINK "static")
        find_package(MKL CONFIG REQUIRED PATHS $ENV{MKLROOT})
        target_compile_options(test PUBLIC $<TARGET_PROPERTY:MKL::MKL,INTERFACE_COMPILE_OPTIONS>)
        target_include_directories(test PUBLIC $<TARGET_PROPERTY:MKL::MKL,INTERFACE_INCLUDE_DIRECTORIES>)
        target_link_libraries(test PUBLIC $<LINK_ONLY:MKL::MKL>)
        add_definitions(-D_MKL)
    endif()

    if(ENABLE_OPENMP)
        find_package(OpenMP REQUIRED)
        target_link_libraries(test PUBLIC OpenMP::OpenMP_Fortran)
    endif()

    if(ENABLE_MPI)
        find_package(MPI REQUIRED)
        target_link_libraries(test PUBLIC MPI::MPI_Fortran)
    endif()

    # Debug or Optimization Flags
    if(ENABLE_DEBUG)
        target_compile_options(test PUBLIC -O0 -g -check all)
        target_link_options(test PUBLIC -O0 -g -check all)
    elseif(ENABLE_OPTIMIZE)
        target_compile_options(test PUBLIC -O3 -xCORE-AVX2)
        target_link_options(test PUBLIC -O3 -xCORE-AVX2)
    endif()

    # Run target
    add_custom_target(run_test
        COMMAND ${CMAKE_RUNTIME_OUTPUT_DIRECTORY}/test
        DEPENDS test
        WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
    )

endif()

# Final Messages
if(BUILD_APP STREQUAL "")
    message(WARNING "No application selected to build. Set BUILD_APP to 'ysy_fc' or 'test'.")
endif()
