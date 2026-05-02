# =========================================================================
# src/cmake/BuildSettings.cmake
# =========================================================================

# 1. Determine compiler-specific paths and package names
if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel|IntelLLVM")
    set(COMPILER_DIR "intel")
    if(CMAKE_Fortran_COMPILER_ID STREQUAL "IntelLLVM")
        set(JSON_FORTRAN_PKG "jsonfortran-intelllvm")
    else()
        set(JSON_FORTRAN_PKG "jsonfortran-intel")
    endif()
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(COMPILER_DIR "gnu")
    set(JSON_FORTRAN_PKG "jsonfortran-gnu")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "NVHPC|PGI")
    set(COMPILER_DIR "nvidia")
    set(JSON_FORTRAN_PKG "jsonfortran-nvhpc")
else()
    message(FATAL_ERROR "Unsupported compiler: ${CMAKE_Fortran_COMPILER_ID}")
endif()

# 2. Add local third-party directory to search path
list(PREPEND CMAKE_PREFIX_PATH "${PROJECT_SOURCE_DIR}/third_party/.local/${COMPILER_DIR}")

# 3. Find required packages
include(FindPkgConfig REQUIRED)
find_package(MPI REQUIRED)
find_package(OpenMP REQUIRED)

# 4. MKL Configuration
set(MKL_LINK static)
set(MKL_INTERFACE lp64)
set(MKL_SYCL_LINK OFF)

if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel|IntelLLVM")
    set(MKL_THREADING "intel_thread")
    set(MKL_MPI "intelmpi")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(MKL_THREADING "gnu_thread")
    set(MKL_MPI "openmpi")
else()
    set(MKL_THREADING "intel_thread")
    set(MKL_MPI "intelmpi")
endif()

set(MKL_COMPONENTS_LIST)
if(ENABLE_MPI)
    set(ENABLE_SCALAPACK ON CACHE BOOL "Enable ScaLAPACK components")
    list(APPEND MKL_COMPONENTS_LIST ScaLAPACK)
endif()

find_package(MKL CONFIG REQUIRED COMPONENTS ${MKL_COMPONENTS_LIST})

# 5. Create ALIAS targets for BLAS/LAPACK to satisfy fortran_stdlib requirements
if(NOT TARGET BLAS::BLAS)
    add_library(BLAS::BLAS INTERFACE IMPORTED)
    target_link_libraries(BLAS::BLAS INTERFACE MKL::MKL)
endif()

if(NOT TARGET LAPACK::LAPACK)
    add_library(LAPACK::LAPACK INTERFACE IMPORTED)
    target_link_libraries(LAPACK::LAPACK INTERFACE MKL::MKL)
endif()

if(ENABLE_MPI AND NOT TARGET SCALAPACK::SCALAPACK)
    add_library(SCALAPACK::SCALAPACK INTERFACE IMPORTED)
    target_link_libraries(SCALAPACK::SCALAPACK INTERFACE MKL::MKL_SCALAPACK MPI::MPI_Fortran)
endif()

# 6. Find third-party libraries
find_package(fortran_stdlib REQUIRED)
find_package(${JSON_FORTRAN_PKG} REQUIRED)
find_package(X11 REQUIRED)
find_package(VTK REQUIRED COMPONENTS CommonCore CommonDataModel IOLegacy IOXML)
find_package(IAPWS REQUIRED)

# =========================================================================
# Function: enable_build_flags
# =========================================================================
function(enable_build_flags target)
    get_target_property(TARGET_TYPE ${target} TYPE)
    set(KEYWORD PUBLIC)
    if(TARGET_TYPE STREQUAL "INTERFACE_LIBRARY")
        set(KEYWORD INTERFACE)
    endif()

    if(NOT KEYWORD STREQUAL "INTERFACE")
        target_compile_options(${target} ${KEYWORD}
            # Fortran: Intel / IntelLLVM
            $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<Fortran_COMPILER_ID:Intel,IntelLLVM>>:
            -stand f18 -fpp -fpscomp logicals -extend-source -g -traceback
            $<$<CONFIG:Release>:-O3 -xHost>
            $<$<CONFIG:Debug>:-O0 -check all -fpe0 -ftrapuv -init=snan -init=arrays -warn all -warn errors -implicitnone -fstack-protector-all>
            >
            # Fortran: GNU
            $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<Fortran_COMPILER_ID:GNU>>:
            -std=f2018 -cpp
            $<$<CONFIG:Release>:-O3 -march=native>
            $<$<CONFIG:Debug>:-g -fbacktrace -fcheck=all -ffpe-trap=invalid,zero,overflow -finit-real=snan -finit-integer=-9999999
            -Wall -Wextra -Werror
            -Wno-maybe-uninitialized -Wno-uninitialized -Wno-c-binding-type -Wno-surprising
            -Wno-unused-dummy-argument -Wno-compare-reals -Wno-unused-function -Wno-unused-value>
            >
            # Fortran: NVHPC
            $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<Fortran_COMPILER_ID:NVHPC,PGI>>:
            -Mpreprocess
            $<$<CONFIG:Release>:-fast>
            $<$<CONFIG:Debug>:-g -O0 -Mbounds -Mchkptr -traceback -Ktrap=fp>
            >

            # ---------------------------------------------------------
            # C Compile Options
            # ---------------------------------------------------------
            # C: Intel / IntelLLVM
            $<$<AND:$<COMPILE_LANGUAGE:C>,$<C_COMPILER_ID:Intel,IntelLLVM>>:
            $<$<CONFIG:Release>:-O3 -xHost -g -traceback>
            $<$<CONFIG:Debug>:-O0 -g -traceback -Wall -Wextra -Werror -fstack-protector-all -ftrapv>
            >
            # C: GNU
            $<$<AND:$<COMPILE_LANGUAGE:C>,$<C_COMPILER_ID:GNU>>:
            $<$<CONFIG:Release>:-O3 -march=native>
            $<$<CONFIG:Debug>:-g -O0 -Wall -Wextra -pedantic -Werror -fstack-protector-all -ftrapv>
            >
            # C: NVHPC
            $<$<AND:$<COMPILE_LANGUAGE:C>,$<C_COMPILER_ID:NVHPC,PGI>>:
            $<$<CONFIG:Release>:-fast>
            $<$<CONFIG:Debug>:-g -O0 -Mbounds -Mchkptr -traceback -Ktrap=fp>
            >

            # ---------------------------------------------------------
            # CXX Compile Options
            # ---------------------------------------------------------
            # CXX: Intel / IntelLLVM
            $<$<AND:$<COMPILE_LANGUAGE:CXX>,$<CXX_COMPILER_ID:Intel,IntelLLVM>>:
            $<$<CONFIG:Release>:-O3 -xHost -g -traceback>
            $<$<CONFIG:Debug>:-O0 -g -traceback -Wall -Wextra -Werror -fstack-protector-all -ftrapv>
            >
            # CXX: GNU
            $<$<AND:$<COMPILE_LANGUAGE:CXX>,$<CXX_COMPILER_ID:GNU>>:
            $<$<CONFIG:Release>:-O3 -march=native>
            $<$<CONFIG:Debug>:-g -O0 -Wall -Wextra -pedantic -Werror -fstack-protector-all -ftrapv>
            >
            # CXX: NVHPC
            $<$<AND:$<COMPILE_LANGUAGE:CXX>,$<CXX_COMPILER_ID:NVHPC,PGI>>:
            $<$<CONFIG:Release>:-fast>
            $<$<CONFIG:Debug>:-g -O0 -Mbounds -Mchkptr -traceback -Ktrap=fp>
            >
        )
    endif()

    target_compile_definitions(${target} ${KEYWORD}
        USE_OPENMP
        _MKL
        $<$<OR:$<CONFIG:Debug>,$<BOOL:${ENABLE_DEBUG}>>:USE_DEBUG>
    )

    target_link_libraries(${target} ${KEYWORD} OpenMP::OpenMP_Fortran)

    if(ENABLE_MPI)
        target_compile_definitions(${target} ${KEYWORD} _MPI)
        target_link_libraries(${target} ${KEYWORD}
            MKL::MKL_SCALAPACK MPI::MPI_Fortran)
    else()
        target_link_libraries(${target} ${KEYWORD} MKL::MKL)
    endif()
endfunction()

# =========================================================================
# Function: enable_thirdparty
# =========================================================================
function(enable_thirdparty target)
    target_include_directories(${target} PUBLIC
        $<TARGET_PROPERTY:fortran_stdlib::fortran_stdlib,INTERFACE_INCLUDE_DIRECTORIES>
        $<TARGET_PROPERTY:${JSON_FORTRAN_PKG}::jsonfortran-static,INTERFACE_INCLUDE_DIRECTORIES>
        $<TARGET_PROPERTY:IAPWS::IAPWS,INTERFACE_INCLUDE_DIRECTORIES>
    )

    target_link_libraries(${target} PUBLIC
        X11::X11
        VTK::CommonCore VTK::CommonDataModel VTK::IOLegacy VTK::IOXML
        fortran_stdlib::fortran_stdlib
        ${JSON_FORTRAN_PKG}::jsonfortran-static
        IAPWS::IAPWS
    )
endfunction()
