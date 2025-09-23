# =========================================================================
# 必須パッケージの探索 (OpenMP, MPI, PkgConfig)
# =========================================================================
include(FindPkgConfig REQUIRED)
find_package(MPI REQUIRED)
find_package(OpenMP REQUIRED)

# =========================================================================
# MKL (BLAS/LAPACKを提供) の探索
# =========================================================================
set(MKL_LINK static)
set(MKL_INTERFACE lp64)
if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel|IntelLLVM")
    set(MKL_THREADING "intel_thread")
    message(STATUS "MKL Threading: Intel OpenMP (for ${CMAKE_Fortran_COMPILER_ID})")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(MKL_THREADING "gnu_thread")
    message(STATUS "MKL Threading: GNU OpenMP (libgomp)")
else()
    set(MKL_THREADING "intel_thread")
    message(WARNING "MKL Threading: Unknown compiler '${CMAKE_Fortran_COMPILER_ID}', defaulting to Intel OpenMP.")
endif()
find_package(MKL CONFIG REQUIRED)
add_library(BLAS::BLAS INTERFACE IMPORTED)
add_library(LAPACK::LAPACK INTERFACE IMPORTED)
target_link_libraries(BLAS::BLAS INTERFACE MKL::MKL OpenMP::OpenMP_Fortran)
target_link_libraries(LAPACK::LAPACK INTERFACE MKL::MKL OpenMP::OpenMP_Fortran)

# =========================================================================
# サードパーティライブラリの探索
# =========================================================================
# --- ライブラリ探索パスを一元管理 ---
list(APPEND CMAKE_PREFIX_PATH ${PROJECT_SOURCE_DIR}/third_party/.local)

# --- fortran-stdlib ---
find_package(fortran_stdlib REQUIRED)

# --- json-fortran ---
find_package(jsonfortran-intelllvm REQUIRED)

# --- VTK ---
find_package(VTK REQUIRED COMPONENTS CommonCore CommonDataModel IOLegacy IOXML)

# --- VTKFortran (静的ライブラリとしてインポート) ---
add_library(VTK::VTKFortran STATIC IMPORTED GLOBAL)
set_target_properties(VTK::VTKFortran PROPERTIES
    IMPORTED_LOCATION "${PROJECT_SOURCE_DIR}/third_party/.local/lib/libvtkfortran.a"
)

# =========================================================================
# ビルドフラグとライブラリリンクを行う関数
# =========================================================================
function(enable_build_flags target)
    target_compile_options(${target} PUBLIC
        $<$<COMPILE_LANGUAGE:Fortran>:-stand f2018 -fpp -traceback>
    )

    # Release最適化オプションを常時有効化
    if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel|IntelLLVM")
        target_compile_options(${target} PUBLIC
            $<$<CONFIG:Release>:-O3 -xHost>
        )
    elseif(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
        target_compile_options(${target} PUBLIC
            $<$<CONFIG:Release>:-O3 -march=native>
        )
    endif()

    # ifx (IntelLLVM) 最適化レポート出力設定
    if(CMAKE_Fortran_COMPILER_ID MATCHES "IntelLLVM")
        file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/opt_reports")
        target_compile_options(${target} PUBLIC
            $<$<COMPILE_LANGUAGE:Fortran>:
                -qopt-report=3
                -qopt-report-phase=loop,vec
            >
        )
    endif()

    # OpenMP, MPI, MKLの定義とリンクを常時有効化
    target_compile_definitions(${target} PUBLIC USE_OPENMP _MPI _MKL)
    target_link_libraries(${target} PUBLIC MPI::MPI_Fortran MKL::MKL)
endfunction()

function(enable_thirdparty target)
    # --- ヘッダファイルのインクルードディレクトリ ---
    target_include_directories(${target} PUBLIC
        ${PETSC_INCLUDE_DIRS}/pestc/finclude
        ${PROJECT_SOURCE_DIR}/third_party/.local/include/VTKFortran
        $<TARGET_PROPERTY:fortran_stdlib::fortran_stdlib,INTERFACE_INCLUDE_DIRECTORIES>
        $<TARGET_PROPERTY:jsonfortran-intelllvm::jsonfortran-static,INTERFACE_INCLUDE_DIRECTORIES>
    )

    # --- ライブラリのリンク ---
    target_link_libraries(${target} PUBLIC
        VTK::VTKFortran
        fortran_stdlib::fortran_stdlib
        jsonfortran-intelllvm::jsonfortran-static
    )
endfunction()