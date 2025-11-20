# =========================================================================
# src/cmake/BuildSettings.cmake
# 完全修正版: 言語別フラグ分離 (Fortran/CXX) + MKL/VTK設定統合
# =========================================================================
# -------------------------------------------------------------------------
# 必須パッケージの探索
# -------------------------------------------------------------------------
include(FindPkgConfig REQUIRED)
find_package(MPI REQUIRED)
find_package(OpenMP REQUIRED)

# -------------------------------------------------------------------------
# MKL (BLAS/LAPACK/ScaLAPACK) 設定
# -------------------------------------------------------------------------
set(MKL_LINK static)
set(MKL_INTERFACE lp64)
set(MKL_MPI "intelmpi")

# コンパイラに応じたスレッド層の選択
if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel|IntelLLVM")
    set(MKL_THREADING "intel_thread")
    message(STATUS "MKL Threading: Intel OpenMP")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(MKL_THREADING "gnu_thread")
    message(STATUS "MKL Threading: GNU OpenMP")
else()
    set(MKL_THREADING "intel_thread")
    message(WARNING "Defaulting to Intel OpenMP threading.")
endif()

# MKLコンポーネントの決定 (MPI有無で分岐)
set(MKL_COMPONENTS_LIST)
if(ENABLE_MPI)
    set(ENABLE_SCALAPACK ON CACHE BOOL "Enable ScaLAPACK components")
    list(APPEND MKL_COMPONENTS_LIST ScaLAPACK)
    message(STATUS "MKL: ScaLAPACK enabled.")
endif()

# --- [Hack] MKLのSYCL誤検知回避 (CXXコンパイラの一時隠蔽) ---
set(CMAKE_CXX_COMPILER_BACKUP "${CMAKE_CXX_COMPILER}" CACHE INTERNAL "Backup CXX")
set(CMAKE_CXX_LANG_INFO_BACKUP "${CMAKE_CXX_COMPILER_ID}" CACHE INTERNAL "Backup CXX ID")
set(CMAKE_CXX_COMPILER "" CACHE STRING "Unset CXX for MKL" FORCE)
set(CMAKE_CXX_COMPILER_ID "" CACHE STRING "Unset CXX ID for MKL" FORCE)
message(STATUS "Temporarily unsetting CXX compiler for MKL find.")

# MKL探索実行
find_package(MKL CONFIG REQUIRED COMPONENTS ${MKL_COMPONENTS_LIST})

# CXXコンパイラの復元
set(CMAKE_CXX_COMPILER "${CMAKE_CXX_COMPILER_BACKUP}" CACHE STRING "Restore CXX" FORCE)
set(CMAKE_CXX_COMPILER_ID "${CMAKE_CXX_LANG_INFO_BACKUP}" CACHE STRING "Restore CXX ID" FORCE)
message(STATUS "Restored CXX compiler.")
# -------------------------------------------------------------

# BLAS/LAPACK エイリアス作成
if(NOT TARGET BLAS::BLAS)
    add_library(BLAS::BLAS INTERFACE IMPORTED)
    target_link_libraries(BLAS::BLAS INTERFACE MKL::MKL)
endif()
if(NOT TARGET LAPACK::LAPACK)
    add_library(LAPACK::LAPACK INTERFACE IMPORTED)
    target_link_libraries(LAPACK::LAPACK INTERFACE MKL::MKL)
endif()

# -------------------------------------------------------------------------
# サードパーティライブラリの探索
# -------------------------------------------------------------------------
list(APPEND CMAKE_PREFIX_PATH ${PROJECT_SOURCE_DIR}/third_party/.local)

# --- fortran-stdlib ---
find_package(fortran_stdlib REQUIRED)

# --- json-fortran ---
find_package(jsonfortran-intelllvm REQUIRED)

# --- VTK (C++本体) ---
find_package(VTK REQUIRED COMPONENTS CommonCore CommonDataModel IOLegacy IOXML)

# --- VTKFortran (ラッパー静的ライブラリ) ---
if(NOT TARGET VTK::VTKFortran)
    add_library(VTK::VTKFortran STATIC IMPORTED GLOBAL)
    set_target_properties(VTK::VTKFortran PROPERTIES
        IMPORTED_LOCATION "${PROJECT_SOURCE_DIR}/third_party/.local/lib/libvtkfortran.a"
    )
endif()

# =========================================================================
# 関数定義: enable_build_flags
#  - コンパイルオプション設定 (Fortran/C++分離)
#  - USE_DEBUG 定義
#  - OpenMP, MPI, MKL リンク
# =========================================================================
function(enable_build_flags target)
    get_target_property(TARGET_TYPE ${target} TYPE)
    set(KEYWORD PUBLIC)
    if(TARGET_TYPE STREQUAL "INTERFACE_LIBRARY")
        set(KEYWORD INTERFACE)
    endif()

    # --- 1. コンパイルオプション (言語ごとに厳密に分離) ---
    if(NOT KEYWORD STREQUAL "INTERFACE")
        
        # ---------------------------------------------------------
        # Fortran 用オプション (C++ には適用させない)
        # ---------------------------------------------------------
        # 基本フラグ (-fpp 等)
        target_compile_options(${target} ${KEYWORD} 
            $<$<COMPILE_LANGUAGE:Fortran>:-stand f2018 -fpp -traceback>
        )

        # Intel / IntelLLVM (Fortran)
        if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel|IntelLLVM")
            target_compile_options(${target} ${KEYWORD} 
                # Release: Fortran Only
                $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Release>>:-O3 -xHost>
                # Debug: Fortran Only (-check all, -fpe0 は C++ に渡さない)
                $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug>>:-g -check all -fpe0>
            )
        
        # GNU (Fortran)
        elseif(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
            target_compile_options(${target} ${KEYWORD} 
                # Release: Fortran Only
                $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Release>>:-O3 -march=native>
                # Debug: Fortran Only
                $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug>>:-g -fcheck=all -ffpe-trap=invalid,zero,overflow>
            )
        endif()

        # ---------------------------------------------------------
        # C++ (CXX) 用オプション
        # ---------------------------------------------------------
        if(CMAKE_CXX_COMPILER_ID MATCHES "Intel|IntelLLVM")
            target_compile_options(${target} ${KEYWORD}
                $<$<AND:$<COMPILE_LANGUAGE:CXX>,$<CONFIG:Release>>:-O3 -xHost>
                $<$<AND:$<COMPILE_LANGUAGE:CXX>,$<CONFIG:Debug>>:-g>
            )
        elseif(CMAKE_CXX_COMPILER_ID MATCHES "GNU")
            target_compile_options(${target} ${KEYWORD}
                $<$<AND:$<COMPILE_LANGUAGE:CXX>,$<CONFIG:Release>>:-O3 -march=native>
                $<$<AND:$<COMPILE_LANGUAGE:CXX>,$<CONFIG:Debug>>:-g>
            )
        endif()

    endif()

    # --- 2. マクロ定義 (USE_DEBUG) ---
    # BuildTypeがDebug または ENABLE_DEBUGがON の場合に定義
    if(CMAKE_BUILD_TYPE MATCHES "Debug" OR ENABLE_DEBUG)
        message(STATUS "[DEBUG] Enabling USE_DEBUG checks for target: ${target}")
        target_compile_definitions(${target} ${KEYWORD} USE_DEBUG)
    endif()

    # --- 3. リンクと共通定義 ---
    target_compile_definitions(${target} ${KEYWORD} USE_OPENMP _MKL)
    target_link_libraries(${target} ${KEYWORD} OpenMP::OpenMP_Fortran)

    if(ENABLE_MPI)
        target_compile_definitions(${target} ${KEYWORD} _MPI)
        
        # Fortranメインの場合、通常は MPI::MPI_Fortran をリンクする
        target_link_libraries(${target} ${KEYWORD} MPI::MPI_Fortran)

        # ScaLAPACKを含むターゲットをリンク
        message(STATUS "Linking MKL::MKL_SCALAPACK for target ${target}")
        target_link_libraries(${target} ${KEYWORD} MKL::MKL_SCALAPACK)
    else()
        # 通常のMKLリンク
        message(STATUS "Linking MKL::MKL for target ${target}")
        target_link_libraries(${target} ${KEYWORD} MKL::MKL)
    endif()
endfunction()

# =========================================================================
# 関数定義: enable_thirdparty
#  - インクルードパス設定
#  - VTK, stdlib, json-fortran リンク
# =========================================================================
function(enable_thirdparty target)
    # インクルードパス
    target_include_directories(${target} PUBLIC
        ${PROJECT_SOURCE_DIR}/third_party/.local/include/VTKFortran
        $<TARGET_PROPERTY:fortran_stdlib::fortran_stdlib,INTERFACE_INCLUDE_DIRECTORIES>
        $<TARGET_PROPERTY:jsonfortran-intelllvm::jsonfortran-static,INTERFACE_INCLUDE_DIRECTORIES>
    )

    # ライブラリリンク
    # 【重要】VTK本体(C++)も同時にリンクする
    target_link_libraries(${target} PUBLIC
        VTK::VTKFortran
        VTK::CommonCore VTK::CommonDataModel VTK::IOLegacy VTK::IOXML
        fortran_stdlib::fortran_stdlib
        jsonfortran-intelllvm::jsonfortran-static
    )
endfunction()