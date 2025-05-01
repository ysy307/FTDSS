# CMake Minimum Version
cmake_minimum_required(VERSION 3.30)

# Project Name and Language
project(YSY_FC LANGUAGES Fortran)
enable_language(Fortran)

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

### Compiler Configuration
if(NOT DEFINED CMAKE_Fortran_COMPILER)
    set(CMAKE_Fortran_COMPILER ifx)
endif()

# Libraries
file(GLOB_RECURSE MY_ALL_LIBRARIES ${CMAKE_LIBRARY_OUTPUT_DIRECTORY}/*.a)
set(LINK_LIBRARIES ${MY_ALL_LIBRARIES})

if(ENABLE_OPENMP)
    find_package(OpenMP REQUIRED)
    list(APPEND LINK_LIBRARIES OpenMP::OpenMP_Fortran)
endif()

if(ENABLE_MPI)
    find_package(MPI REQUIRED)
    list(APPEND LINK_LIBRARIES MPI::MPI_Fortran)
endif()

# Include Paths
set(My_INCLUDE_PATH
    ${CMAKE_INCLUDE_PATH}/Json-Fortran
    ${CMAKE_INCLUDE_PATH}/VTKFortran
    /workspaces/FTDSS/EXTERNAL/fortran_stdlib/include/fortran_stdlib/IntelLLVM-2025.0.4
)

# External Libraries
set(fortran_stdlib_DIR "/workspaces/FTDSS/EXTERNAL/fortran_stdlib/lib/cmake/fortran_stdlib")
find_package(fortran_stdlib REQUIRED)
list(APPEND LINK_LIBRARIES fortran_stdlib::fortran_stdlib)

# Source Files
# --- Core ---
add_library(Core
    src/Core/Error.F90
    src/Core/Allocate.F90
    src/Core/BaseTypes.F90
    src/Core/Element/Element.F90
    src/Core/Element/Element_SquareFirst.F90
    src/Core/Element/Element_TriangleFirst.F90
    src/Core/Side/Side.F90
    src/Core/Side/Side_First.F90
    src/Core/Unique.F90
    src/Core/VTK.F90
)
target_include_directories(Core PUBLIC ${My_INCLUDE_PATH})

# --- Matrix CRS ---
add_library(Matrix 
    src/Matrix/CRS.F90
    src/Matrix/Assemble.F90
)
target_link_libraries(Matrix PRIVATE Core)

# --- BLAS ---
add_library(calc_blas src/Calculate/BLAS.F90)
target_link_libraries(calc_blas PRIVATE Matrix)

# --- Conditions ---
add_library(conds
    src/Conditions/Fix_Boundary_Conditions.F90
    src/Conditions/Fix_Initial_Conditions.F90
)
target_link_libraries(conds PRIVATE Core Matrix)

# --- Inout Input ---
add_library(inout_input 
    src/Inout/SetProjectPath.F90
    src/Inout/Input.F90
)
target_link_libraries(inout_input PRIVATE Core conds)
target_include_directories(inout_input PUBLIC ${My_INCLUDE_PATH})

# --- Calculate WRF, HCF, GCC ---
add_library(calc_wrf src/Calculate/WRF.F90)
target_link_libraries(calc_wrf PRIVATE)

add_library(calc_hcf
    src/Calculate/HCF/HCF.F90
    src/Calculate/HCF/HCF_BC.F90
    src/Calculate/HCF/HCF_VG.F90
    src/Calculate/HCF/HCF_KO.F90
    src/Calculate/HCF/HCF_MVG.F90
    src/Calculate/HCF/HCF_Durner.F90
    src/Calculate/HCF/HCF_DVGCH.F90
    src/Calculate/HCF/HCF_Impedance.F90
    src/Calculate/HCF/HCF_Viscosity.F90
)
target_link_libraries(calc_hcf PRIVATE calc_wrf Core)

add_library(calc_gcc src/Calculate/GCC.F90)
target_link_libraries(calc_gcc PRIVATE calc_hcf)

# --- Ice ---
add_library(calc_ice 
    src/Calculate/Ice/Ice.F90
    src/Calculate/Ice/Ice_TRM.F90
    src/Calculate/Ice/Ice_GCC.F90
    src/Calculate/Ice/Ice_EXP.F90
)
target_link_libraries(calc_ice PRIVATE calc_gcc calc_wrf Core)

# --- Material Properties ---
add_library(calc_props
    src/Calculate/ThermalConductivity.F90
    src/Calculate/VolumetricHeatCapacity.F90
    src/Calculate/Density.F90
)
target_link_libraries(calc_props PRIVATE Core calc_ice calc_gcc)

# --- Solver ---
add_library(solver 
    src/Solver/Solve.f90
    src/Solver/Solve_BiCGSTAB.F90
    src/Solver/Solve_LU.F90
    src/Solver/Solve_Preconditioner.F90
    src/Solver/Time.F90
)
target_link_libraries(solver PRIVATE Matrix inout_input Core calc_blas)

# --- Main Thermal ---
add_library(main_thermal src/Main/Thermal.F90)
target_link_libraries(main_thermal PRIVATE Core inout_input calc_ice calc_props Matrix conds solver)

# --- Output ---
add_library(inout_output src/Inout/Output.F90)
target_link_libraries(inout_output PRIVATE inout_input Core main_thermal)
target_include_directories(inout_output PUBLIC ${My_INCLUDE_PATH})

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
        calc_blas
        conds
        inout_input
        calc_wrf
        calc_hcf
        calc_gcc
        calc_ice
        calc_props
        solver
        main_thermal
        inout_output
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
    target_link_options(test PUBLIC -fpp -traceback)

    # Link core libraries
    target_link_libraries(test PRIVATE
        Core
        Matrix
        calc_blas
        conds
        inout_input
        calc_wrf
        calc_hcf
        calc_gcc
        calc_ice
        calc_props
        solver
        main_thermal
        inout_output
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
