# =========================================================================
# 必須パッケージの探索 (OpenMP, MPI)
# =========================================================================
if(ENABLE_MPI AND NOT TARGET MPI::MPI_Fortran)
    find_package(MPI REQUIRED)
endif()

# =========================================================================
# MKL (BLAS/LAPACKを提供) の探索
# =========================================================================
if(ENABLE_MKL AND NOT TARGET MKL::MKL)

    set(MKL_LINK static)
    set(MKL_INTERFACE lp64)
    set(MKL_INTERFACE_LAYER "_lp64")
    set(MKL_SYCL_INTERFACE_FULL intel_lp64)

    set(MKL_THREADING "")
    set(OPENMP_FORTRAN_LIB "")

    if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
        set(MKL_THREADING "intel_thread")
        message(STATUS "MKL Threading: Intel OpenMP")
    elseif(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
        set(MKL_THREADING "gnu_thread")
        message(STATUS "MKL Threading: GNU OpenMP (libgomp)")
    else()
        set(MKL_THREADING "intel_thread")
        message(WARNING "MKL Threading: Unknown compiler, defaulting to Intel OpenMP.")
    endif()

    if(ENABLE_OPENMP)
        find_package(OpenMP REQUIRED)
        set(OPENMP_FORTRAN_LIB OpenMP::OpenMP_Fortran)
    endif()

    find_package(MKL CONFIG REQUIRED)

    add_library(BLAS::BLAS INTERFACE IMPORTED)
    add_library(LAPACK::LAPACK INTERFACE IMPORTED)

    # MKL に OpenMP もリンクさせる（必要なら）
    if(OPENMP_FORTRAN_LIB)
        set_target_properties(BLAS::BLAS PROPERTIES
            INTERFACE_LINK_LIBRARIES "MKL::MKL;${OPENMP_FORTRAN_LIB}"
        )
        set_target_properties(LAPACK::LAPACK PROPERTIES
            INTERFACE_LINK_LIBRARIES "MKL::MKL;${OPENMP_FORTRAN_LIB}"
        )
    else()
        set_target_properties(BLAS::BLAS PROPERTIES
            INTERFACE_LINK_LIBRARIES MKL::MKL
        )
        set_target_properties(LAPACK::LAPACK PROPERTIES
            INTERFACE_LINK_LIBRARIES MKL::MKL
        )
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
if(NOT TARGET VTK::CommonCore)
    find_package(VTK REQUIRED COMPONENTS CommonCore IOLegacy)
endif()

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
        $<$<COMPILE_LANGUAGE:Fortran>:-fpp -traceback>
    )

    # Debug用フラグ
    if(ENABLE_DEBUG)
        if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
            target_compile_options(${target} PUBLIC
                $<$<AND:$<CONFIG:Debug>,$<COMPILE_LANGUAGE:Fortran>>:-g -O0 -check all -traceback>
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

    # ★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★
    # ★★★         MKLとOpenMPのリンク競合を解決             ★★★
    # ★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★★

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
