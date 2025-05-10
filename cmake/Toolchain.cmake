if(ENABLE_OPENMP AND NOT TARGET OpenMP::OpenMP_Fortran)
    find_package(OpenMP REQUIRED)
endif()

if(ENABLE_MPI AND NOT TARGET MPI::MPI_Fortran)
    find_package(MPI REQUIRED)
endif()

if(ENABLE_MKL AND NOT TARGET MKL::MKL)
    set(MKL_INTERFACE_FULL intel_lp64 CACHE STRING "Use 32-bit integers for MKL")
    set(MKL_LINK static CACHE STRING "Static MKL linking")
    set(MKL_THREADING intel_thread CACHE STRING "Use Intel OpenMP threading")
    set(MKL_MPI intelmpi CACHE STRING "Use Intel MPI")
    find_package(MKL CONFIG REQUIRED PATHS $ENV{MKLROOT})
endif()

if(NOT TARGET fortran_stdlib::fortran_stdlib)
    find_package(fortran_stdlib REQUIRED
        PATHS ${PROJECT_SOURCE_DIR}/third_party/.local/lib/cmake/fortran_stdlib
    )
endif()

if(NOT TARGET jsonfortran-intelllvm::jsonfortran-static)
    find_package(jsonfortran-intelllvm REQUIRED
        PATHS ${PROJECT_SOURCE_DIR}/third_party/.local/jsonfortran-intelllvm-9.0.3/cmake
    )
endif()

# --- 静的ライブラリは一度だけ探索 ---
if(NOT MY_ALL_LIBRARIES)
    file(GLOB MY_ALL_LIBRARIES CONFIGURE_DEPENDS ${PROJECT_SOURCE_DIR}/third_party/.local/VTKFortran/lib/*.a)
endif()

# --- ターゲットに対してビルドフラグを設定 ---
function(enable_build_flags target)
    # Fortran のみに -fpp を適用
    target_compile_options(${target} PUBLIC
        $<$<COMPILE_LANGUAGE:Fortran>:-fpp>
    )

    if(ENABLE_DEBUG)
        target_compile_options(${target} PUBLIC
            $<$<CONFIG:Debug>:-g -O0 -check all -traceback>
        )
    endif()

    if(ENABLE_OPTIMIZE)
        target_compile_options(${target} PUBLIC
            $<$<CONFIG:Release>:-O3 -xCORE-AVX2>
        )
    endif()

    if(ENABLE_OPENMP)
        target_link_libraries(${target} PUBLIC OpenMP::OpenMP_Fortran)
        target_compile_definitions(${target} PUBLIC USE_OPENMP)
    endif()

    if(ENABLE_MPI)
        target_link_libraries(${target} PUBLIC MPI::MPI_Fortran)
        target_compile_definitions(${target} PUBLIC _MPI)
    endif()

    if(ENABLE_MKL)
        target_compile_options(${target} PUBLIC
            $<TARGET_PROPERTY:MKL::MKL,INTERFACE_COMPILE_OPTIONS>
        )
        target_include_directories(${target} PUBLIC
            $<TARGET_PROPERTY:MKL::MKL,INTERFACE_INCLUDE_DIRECTORIES>
        )
        target_link_libraries(${target} PUBLIC MKL::MKL)
        target_compile_definitions(${target} PUBLIC _MKL)
    endif()
endfunction()


# --- サードパーティのヘッダ・ライブラリを追加 ---
function(enable_thirdparty target)
    target_include_directories(${target} PUBLIC
        ${PROJECT_SOURCE_DIR}/third_party/.local/VTKFortran/include
    )
    target_link_libraries(${target} PUBLIC ${MY_ALL_LIBRARIES})

    target_include_directories(${target} PUBLIC 
        $<TARGET_PROPERTY:fortran_stdlib::fortran_stdlib,INTERFACE_INCLUDE_DIRECTORIES>
    )
    target_link_libraries(${target} PUBLIC fortran_stdlib::fortran_stdlib)

    target_include_directories(${target} PUBLIC 
        $<TARGET_PROPERTY:jsonfortran-intelllvm::jsonfortran-static,INTERFACE_INCLUDE_DIRECTORIES>
    )
    target_link_libraries(${target} PUBLIC jsonfortran-intelllvm::jsonfortran-static)


endfunction()
