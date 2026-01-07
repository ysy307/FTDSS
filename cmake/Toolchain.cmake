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

# SYCL (DPC++) リンクを無効化
set(MKL_SYCL_LINK OFF)

# コンパイラに応じたスレッド層の選択
if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel|IntelLLVM")
    set(MKL_THREADING "intel_thread")
elseif(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
    set(MKL_THREADING "gnu_thread")
else()
    set(MKL_THREADING "intel_thread")
endif()

# MKLコンポーネントの決定 (MPI有無で分岐)
set(MKL_COMPONENTS_LIST)
if(ENABLE_MPI)
    set(ENABLE_SCALAPACK ON CACHE BOOL "Enable ScaLAPACK components")
    list(APPEND MKL_COMPONENTS_LIST ScaLAPACK)
endif()

# MKL探索実行
find_package(MKL CONFIG REQUIRED COMPONENTS ${MKL_COMPONENTS_LIST})

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

# --- IAPWS ---
find_package(IAPWS REQUIRED)

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

    if(NOT KEYWORD STREQUAL "INTERFACE")
        # ---------------------------------------------------------
        # Fortran Compile Options
        # ---------------------------------------------------------
# Intel / IntelLLVM (Fortran)
        if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel|IntelLLVM")
            target_compile_options(${target} ${KEYWORD} 
                # Common for Intel (Standard, Preprocessor, Traceback)
                $<$<COMPILE_LANGUAGE:Fortran>:-stand f18 -fpp -traceback>

                # Release: Fortran Only
                $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Release>>:-O3 -xHost>
                
                # Debug: Fortran Only 
                # -check all: 全チェック
                # -fpe0: 浮動小数点例外(NaN等)で即停止
                # -ftrapuv: 未初期化変数を変な値で埋める
                # -fp-stack-check: スタックあふれチェック
                $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug>>:-g -check all -fpe0 -ftrapuv -fp-stack-check>
            )
        
        # GNU (Fortran)
        elseif(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
            target_compile_options(${target} ${KEYWORD} 
                # Common for GNU (Standard, Preprocessor)
                $<$<COMPILE_LANGUAGE:Fortran>:-std=f2018 -cpp>

                # Release: Fortran Only
                $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Release>>:-O3 -march=native>
                
                # Debug: Fortran Only
                # -fbacktrace: 行番号を表示
                # -fcheck=all: 全チェック（配列外参照など）
                # -ffpe-trap=...: NaNやゼロ除算で即停止
                # -finit-real=snan: 実数をNaNで初期化（未初期化変数対策）
                $<$<AND:$<COMPILE_LANGUAGE:Fortran>,$<CONFIG:Debug>>:-g -fbacktrace -fcheck=all -ffpe-trap=invalid,zero,overflow -finit-real=snan -finit-integer=-9999999>
            )
        endif()
        # ---------------------------------------------------------
        # C++ Compile Options
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
        target_compile_definitions(${target} ${KEYWORD} USE_DEBUG)
    endif()

    # --- 3. リンクと共通定義 ---
    target_compile_definitions(${target} ${KEYWORD} USE_OPENMP _MKL)
    target_link_libraries(${target} ${KEYWORD} OpenMP::OpenMP_Fortran)

    if(ENABLE_MPI)
        target_compile_definitions(${target} ${KEYWORD} _MPI)
        target_link_libraries(${target} ${KEYWORD} MPI::MPI_Fortran)
        target_link_libraries(${target} ${KEYWORD} MKL::MKL_SCALAPACK)
    else()
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
        $<TARGET_PROPERTY:IAPWS::IAPWS,INTERFACE_INCLUDE_DIRECTORIES>
    )

    # ライブラリリンク
    # 【重要】VTK本体(C++)も同時にリンクする
    target_link_libraries(${target} PUBLIC
        VTK::VTKFortran
        VTK::CommonCore VTK::CommonDataModel VTK::IOLegacy VTK::IOXML
        fortran_stdlib::fortran_stdlib
        jsonfortran-intelllvm::jsonfortran-static
        IAPWS::IAPWS
    )
endfunction()