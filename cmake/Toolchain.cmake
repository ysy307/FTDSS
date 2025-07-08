# =========================================================================
# 必須パッケージの探索 (OpenMP, MPI)
# =========================================================================
if(ENABLE_OPENMP AND NOT TARGET OpenMP::OpenMP_Fortran)
    find_package(OpenMP REQUIRED)
endif()

if(ENABLE_MPI AND NOT TARGET MPI::MPI_Fortran)
    find_package(MPI REQUIRED)
endif()

# =========================================================================
# MKL (BLAS/LAPACKを提供) の探索
# =========================================================================
if(ENABLE_MKL AND NOT TARGET MKL::MKL)
    option(ENABLE_MKL_ILP64 "Enable MKL ILP64 interface" OFF)

    if(ENABLE_MKL_ILP64)
        set(MKL_INTERFACE_LAYER "ILP64")
        message(STATUS "MKL Interface: ILP64 (64-bit integers)")
    else()
        set(MKL_INTERFACE_LAYER "LP64")
        message(STATUS "MKL Interface: LP64 (32-bit integers)")
    endif()

    set(MKL_LINK static)
    set(MKL_THREADING "intel_thread")
    find_package(MKL CONFIG REQUIRED)
endif()

# =========================================================================
# サードパーティライブラリの探索
# =========================================================================
# --- ライブラリ探索パスを一元管理 ---
# これにより、各find_packageでPATHSを指定する必要がなくなります。
list(APPEND CMAKE_PREFIX_PATH ${PROJECT_SOURCE_DIR}/third_party/.local)

# ★★★ 修正点 ★★★
# fortran-stdlib が依存する BLAS を明示的に探索します。
# MKLが先に見つかっていれば、MKLが提供するBLASが使われます。
if(NOT TARGET BLAS::BLAS)
    find_package(BLAS REQUIRED)
endif()

if(NOT TARGET LAPACK::LAPACK)
    find_package(LAPACK REQUIRED)
endif()

# --- fortran-stdlib ---
if(NOT TARGET fortran_stdlib::fortran_stdlib)
    find_package(fortran_stdlib REQUIRED)
endif()

# --- json-fortran ---
if(NOT TARGET jsonfortran-intelllvm::jsonfortran-static)
    # バージョン番号をパスから削除し、CMAKE_PREFIX_PATHで探索させる
    find_package(jsonfortran-intelllvm REQUIRED)
endif()

# --- VTK (Fortranラッパーではなく、本体ライブラリ) ---
if(NOT TARGET VTK::CommonCore)
    # バージョン番号をパスから削除し、CMAKE_PREFIX_PATHで探索させる
    find_package(VTK REQUIRED COMPONENTS CommonCore IOLegacy)
endif()

# --- VTKFortran (静的ライブラリとしてインポート) ---
# file(GLOB)よりもIMPORTEDターゲットを使う方が堅牢です
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
    # ... (ご提示のビルドフラグ設定は変更ありません) ...
    target_compile_options(${target} PUBLIC
        $<$<COMPILE_LANGUAGE:Fortran>:-fpp -traceback>
    )

    # Debug用フラグ（コンパイラ別）
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
        else()
            message(FATAL_ERROR "Unknown COMPILER in ENABLE_DEBUG block: ${COMPILER}")
        endif()
    endif()

    # Release最適化オプション（コンパイラ別）
    if(ENABLE_OPTIMIZE)
        if(CMAKE_Fortran_COMPILER_ID MATCHES "Intel")
            target_compile_options(${target} PUBLIC
                $<$<CONFIG:Release>:-O3 -xCORE-AVX2>
            )
        elseif(CMAKE_Fortran_COMPILER_ID MATCHES "GNU")
            target_compile_options(${target} PUBLIC
                $<$<CONFIG:Release>:-O3 -march=native>
            )
        else()
            message(FATAL_ERROR "Unknown COMPILER in ENABLE_OPTIMIZE block: ${COMPILER}")
        endif()
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
        # MKLターゲットからコンパイルオプションとインクルードディレクトリを取得
        get_target_property(MKL_COMPILE_OPTIONS MKL::MKL INTERFACE_COMPILE_OPTIONS)
        get_target_property(MKL_INCLUDE_DIRS MKL::MKL INTERFACE_INCLUDE_DIRECTORIES)

        if(ENABLE_MKL_ILP64)
            # ILP64版の場合、-i8 オプションをリストから除去
            list(REMOVE_ITEM MKL_COMPILE_OPTIONS "-i8")
            
            # ★★★ MKLのインクルードパスは追加しない ★★★
            # これにより、自前で定義した安全なインターフェースが使われる
        else()
            # LP64版の場合は、通常通りインクルードパスを追加
            target_include_directories(${target} PUBLIC ${MKL_INCLUDE_DIRS})
        endif()

        # 処理したコンパイルオプションを追加
        target_compile_options(${target} PUBLIC ${MKL_COMPILE_OPTIONS})
        
        target_link_libraries(${target} PUBLIC MKL::MKL)
        target_compile_definitions(${target} PUBLIC _MKL)
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
