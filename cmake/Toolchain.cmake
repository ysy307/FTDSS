# =========================================================================
# 必須パッケージの探索 (OpenMP, MPI)
# =========================================================================
if(ENABLE_MPI AND NOT TARGET MPI::MPI_Fortran)
    # CMAKE_Fortran_COMPILER=mpiifx を指定していれば、FindMPIは自動で設定を検出します。
    find_package(MPI REQUIRED)
endif()

# =========================================================================
# MKL (BLAS/LAPACKを提供) の探索
# =========================================================================
if(ENABLE_MKL AND NOT TARGET MKL::MKL)

    # MKLの基本的な設定
    set(MKL_LINK static)
    set(MKL_INTERFACE lp64)

    # MKLのスレッディング層をコンパイラに基づいて決定
    # ifort (Intel) と ifx (IntelLLVM) の両方に対応
    if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel|IntelLLVM")
        set(MKL_THREADING "intel_thread")
        message(STATUS "MKL Threading: Intel OpenMP (for ${CMAKE_Fortran_COMPILER_ID})")
    elseif(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
        set(MKL_THREADING "gnu_thread")
        message(STATUS "MKL Threading: GNU OpenMP (libgomp)")
    else()
        # 不明なコンパイラの場合、IntelのOpenMPをデフォルトにする
        set(MKL_THREADING "intel_thread")
        message(WARNING "MKL Threading: Unknown compiler '${CMAKE_Fortran_COMPILER_ID}', defaulting to Intel OpenMP.")
    endif()

    # OpenMPが有効な場合、OpenMPパッケージを探す
    if(ENABLE_OPENMP)
        find_package(OpenMP REQUIRED)
    endif()

    # 上記の MKL_* 変数に基づいて、MKLパッケージを探索
    find_package(MKL CONFIG REQUIRED)

    # BLAS/LAPACKのエイリアスターゲットを作成
    add_library(BLAS::BLAS INTERFACE IMPORTED)
    add_library(LAPACK::LAPACK INTERFACE IMPORTED)

    # MKLターゲットに、必要に応じてOpenMPライブラリもリンク
    if(ENABLE_OPENMP AND TARGET OpenMP::OpenMP_Fortran)
        target_link_libraries(BLAS::BLAS INTERFACE MKL::MKL OpenMP::OpenMP_Fortran)
        target_link_libraries(LAPACK::LAPACK INTERFACE MKL::MKL OpenMP::OpenMP_Fortran)
    else()
        target_link_libraries(BLAS::BLAS INTERFACE MKL::MKL)
        target_link_libraries(LAPACK::LAPACK INTERFACE MKL::MKL)
    endif()

endif()


# =========================================================================
# サードパーティライブラリの探索
# =========================================================================
# --- ライブラリ探索パスを一元管理 ---
list(APPEND CMAKE_PREFIX_PATH ${PROJECT_SOURCE_DIR}/third_party/.local)

# --- fortran-stdlib ---
if(NOT TARGET fortran_stdlib::fortran_stdlib)
    find_package(fortran_stdlib REQUIRED)
endif()

# --- json-fortran ---
if(NOT TARGET jsonfortran-intelllvm::jsonfortran-static)
    find_package(jsonfortran-intelllvm REQUIRED)
endif()

# --- VTK (Fortranラッパーではなく、本体ライブラリ) ---
find_package(VTK REQUIRED
    COMPONENTS
        CommonCore
        CommonDataModel
        IOLegacy         # .vtkリーダーのために必要
        IOXML            # .vtuリーダーのために必要
)

# --- VTKFortran (静的ライブラリとしてインポート) ---
if(NOT TARGET VTK::VTKFortran)
    add_library(VTK::VTKFortran STATIC IMPORTED GLOBAL)
    set_target_properties(VTK::VTKFortran PROPERTIES
        IMPORTED_LOCATION "${PROJECT_SOURCE_DIR}/third_party/.local/lib/libvtkfortran.a"
    )
endif()


# =========================================================================
# ビルドフラグとライブラリリンクを行う関数
# =========================================================================
function(enable_build_flags target)
    target_compile_options(${target} PUBLIC
        $<$<COMPILE_LANGUAGE:Fortran>:-stand f2018 -fpp -traceback>
    )

    # Debug用フラグ
    if(ENABLE_DEBUG)
        if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
            target_compile_options(${target} PUBLIC
                $<$<AND:$<CONFIG:Debug>,$<COMPILE_LANGUAGE:Fortran>>:-g -O0 -check all -fpe=0 -warn all -traceback>
                $<$<AND:$<CONFIG:Debug>,$<COMPILE_LANGUAGE:C>>:-g -O0 -debug all -traceback>
                $<$<AND:$<CONFIG:Debug>,$<COMPILE_LANGUAGE:CXX>>:-g -O0 -debug all -traceback>
            )
            target_compile_definitions(${target} PUBLIC USE_DEBUG)
        elseif(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
            target_compile_options(${target} PUBLIC
                $<$<AND:$<CONFIG:Debug>,$<COMPILE_LANGUAGE:Fortran>>:-g -O0 -fcheck=all -fbacktrace>
                $<$<AND:$<CONFIG:Debug>,$<COMPILE_LANGUAGE:C>>:
                    -g -O0 -Wall -Wextra -fsanitize=address -fsanitize=undefined -fno-omit-frame-pointer>
                $<$<AND:$<CONFIG:Debug>,$<COMPILE_LANGUAGE:CXX>>:
                    -g -O0 -Wall -Wextra -fsanitize=address -fsanitize=undefined -fno-omit-frame-pointer>
            )
            target_link_options(${target} PUBLIC
                $<$<AND:$<CONFIG:Debug>,$<COMPILE_LANGUAGE:C>>:
                    -fsanitize=address -fsanitize=undefined>
                $<$<AND:$<CONFIG:Debug>,$<COMPILE_LANGUAGE:CXX>>:
                    -fsanitize=address -fsanitize=undefined>
            )
        endif()
    endif()

    # Release最適化オプション
    if(ENABLE_OPTIMIZE)
        if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
            target_compile_options(${target} PUBLIC
                $<$<CONFIG:Release>:-O3 -xCORE-AVX2>
            )
        elseif(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
            target_compile_options(${target} PUBLIC
                $<$<CONFIG:Release>:-O3 -march=native>
            )
        endif()
    endif()

    # ================================
    # ifx (IntelLLVM) 最適化レポート出力設定
    # ================================
    if(CMAKE_Fortran_COMPILER_ID MATCHES "IntelLLVM")
        file(MAKE_DIRECTORY "${CMAKE_BINARY_DIR}/opt_reports")
        target_compile_options(${target} PUBLIC
            $<$<COMPILE_LANGUAGE:Fortran>:
                -qopt-report=3
                -qopt-report-phase=loop,vec
                # -qopt-report-file=${CMAKE_BINARY_DIR}/opt_reports/
            >
        )
    endif()

    # OpenMPのコンパイル定義は、MKLの有無に関わらず設定する
    if(ENABLE_OPENMP)
        target_compile_definitions(${target} PUBLIC USE_OPENMP)
    endif()

    if(ENABLE_MPI)
        target_link_libraries(${target} PUBLIC MPI::MPI_Fortran)
        target_compile_definitions(${target} PUBLIC _MPI)
    endif()

    if(ENABLE_MKL)
        target_link_libraries(${target} PUBLIC MKL::MKL)
        target_compile_definitions(${target} PUBLIC _MKL)
    elseif(ENABLE_OPENMP)
        # MKLが無効で、OpenMPが有効な場合のみ、標準のOpenMPターゲットをリンクする
        target_link_libraries(${target} PUBLIC OpenMP::OpenMP_Fortran)
    endif()
endfunction()

function(enable_thirdparty target)
    # --- ヘッダファイルのインクルードディレクトリ ---
    target_include_directories(${target} PUBLIC
        # VTKFortran
        ${PROJECT_SOURCE_DIR}/third_party/.local/include/VTKFortran
        # fortran-stdlib
        $<TARGET_PROPERTY:fortran_stdlib::fortran_stdlib,INTERFACE_INCLUDE_DIRECTORIES>
        # json-fortran
        $<TARGET_PROPERTY:jsonfortran-intelllvm::jsonfortran-static,INTERFACE_INCLUDE_DIRECTORIES>
    )

    # --- ライブラリのリンク ---
    target_link_libraries(${target} PUBLIC
        # VTKFortran (IMPORTEDターゲットをリンク)
        VTK::VTKFortran
        # fortran-stdlib
        fortran_stdlib::fortran_stdlib
        # json-fortran
        jsonfortran-intelllvm::jsonfortran-static
    )
endfunction()
