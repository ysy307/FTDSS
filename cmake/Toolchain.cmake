# =========================================================================
# 必須パッケージの探索 (OpenMP, MPI, PkgConfig)
# =========================================================================
include(FindPkgConfig REQUIRED)
find_package(MPI REQUIRED)
find_package(OpenMP REQUIRED)

# =========================================================================
# MKL (BLAS/LAPACK/ScaLAPACK を提供)
# =========================================================================
# --- MKLの挙動を制御する変数を find_package の前に設定 ---

# 1. リンク方法の指定: 'static' または 'dynamic'
set(MKL_LINK static)

# 2. インターフェース層の指定: LP64 (32-bit integer) または ILP64 (64-bit integer)
set(MKL_INTERFACE lp64)

# 3. MPIラッパーの指定: 'intelmpi' または 'openmpi' など
set(MKL_MPI "intelmpi")

# 4. スレッドライブラリの指定: コンパイラに合わせて自動選択
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

# --- MKLパッケージの探索準備 ---
# ENABLE_MPI オプションがONの場合、ScaLAPACKを有効化するオプション変数を設定する
if(ENABLE_MPI)
    set(ENABLE_SCALAPACK ON CACHE BOOL "Enable ScaLAPACK components")
    message(STATUS "ENABLE_SCALAPACK is ON.")
endif()

# --- MKLパッケージの探索 ---

# 【対策】MKLがSYCLコンパイラを誤検知する問題への強制対策
# CXXコンパイラの設定を一時的に退避させる
set(CMAKE_CXX_COMPILER_BACKUP "${CMAKE_CXX_COMPILER}" CACHE INTERNAL "Backup of CXX compiler")
set(CMAKE_CXX_LANG_INFO_BACKUP "${CMAKE_CXX_COMPILER_ID}" CACHE INTERNAL "Backup of CXX compiler ID")

# MKLのスクリプトに検知されないよう、一時的にCXXコンパイラを空にする
set(CMAKE_CXX_COMPILER "" CACHE STRING "Temporarily unset CXX compiler for MKL find" FORCE)
set(CMAKE_CXX_COMPILER_ID "" CACHE STRING "Temporarily unset CXX compiler ID for MKL find" FORCE)
message(STATUS "Temporarily unsetting CXX compiler to prevent MKL SYCL auto-detection.")

# この状態であれば、MKLはSYCL関連のライブラリを探しに行かなくなる
find_package(MKL CONFIG REQUIRED COMPONENTS ScaLAPACK)

# 退避させたCXXコンパイラ設定を元に戻す
set(CMAKE_CXX_COMPILER "${CMAKE_CXX_COMPILER_BACKUP}" CACHE STRING "Restore CXX compiler" FORCE)
set(CMAKE_CXX_COMPILER_ID "${CMAKE_CXX_LANG_INFO_BACKUP}" CACHE STRING "Restore CXX compiler ID" FORCE)
message(STATUS "Restored CXX compiler.")

# --- BLAS/LAPACKのエイリアスターゲットを作成 (任意ですが推奨) ---
add_library(BLAS::BLAS INTERFACE IMPORTED)
add_library(LAPACK::LAPACK INTERFACE IMPORTED)
target_link_libraries(BLAS::BLAS INTERFACE MKL::MKL)
target_link_libraries(LAPACK::LAPACK INTERFACE MKL::MKL)

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
# ビルドフラグとライブラリリンクを行う関数 (最終修正版)
# =========================================================================
function(enable_build_flags target)
    # --- ターゲットの種類をチェック ---
    get_target_property(TARGET_TYPE ${target} TYPE)

    # --- ターゲットの種類に応じてキーワードを決定 ---
    set(KEYWORD PUBLIC)
    if(TARGET_TYPE STREQUAL "INTERFACE_LIBRARY")
        set(KEYWORD INTERFACE)
    endif()

    # --- 1. コンパイルオプションの設定 ---
    if(NOT KEYWORD STREQUAL "INTERFACE")
        target_compile_options(${target} ${KEYWORD} $<$<COMPILE_LANGUAGE:Fortran>:-stand f2018 -fpp -traceback>)
        if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel|IntelLLVM")
            target_compile_options(${target} ${KEYWORD} $<$<CONFIG:Release>:-O3 -xHost>)
        elseif(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
            target_compile_options(${target} ${KEYWORD} $<$<CONFIG:Release>:-O3 -march=native>)
        endif()
        if(CMAKE_Fortran_COMPILER_ID MATCHES "IntelLLVM")
            file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/opt_reports")
            target_compile_options(${target} ${KEYWORD} $<$<COMPILE_LANGUAGE:Fortran>:-qopt-report=3 -qopt-report-phase=loop,vec>)
        endif()
    endif()

    # --- 2. 利用条件（リンクや定義）の設定 ---
    target_compile_definitions(${target} ${KEYWORD} USE_OPENMP _MKL)
    target_link_libraries(${target} ${KEYWORD} OpenMP::OpenMP_Fortran)

    if(ENABLE_MPI)
        # --- MPI有効時のリンク設定 ---
        target_compile_definitions(${target} ${KEYWORD} _MPI)
        target_link_libraries(${target} ${KEYWORD} MPI::MPI_Fortran)

        # MKLConfig.cmakeの仕様に基づき、ScaLAPACK/PBLAS用の公式ターゲットをリンクする
        message(STATUS "Linking MKL::MKL_SCALAPACK for MPI target ${target}")
        target_link_libraries(${target} ${KEYWORD} MKL::MKL_SCALAPACK)
    else()
        # --- MPI無効時は、標準のMKLターゲットをリンク ---
        message(STATUS "Linking MKL::MKL for non-MPI target ${target}")
        target_link_libraries(${target} ${KEYWORD} MKL::MKL)
    endif()
endfunction()

function(enable_thirdparty target)
    # --- ヘッダファイルのインクルードディレクトリ ---
    target_include_directories(${target} PUBLIC
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