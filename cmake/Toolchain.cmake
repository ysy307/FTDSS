# サブルーチン：ターゲットに対してフラグを設定
function(enable_build_flags target)
  # ── プリプロセッサは常につける ──
  target_compile_options(${target} PUBLIC -fpp)

  # ── Debugビルドのカスタムフラグ ──
  if(ENABLE_DEBUG)
    target_compile_options(${target} PUBLIC
      $<$<CONFIG:Debug>:-g -O0 -check all -traceback>
    )
  endif()

  # ── Releaseビルドのカスタム最適化 ──
  if(ENABLE_OPTIMIZE)
    target_compile_options(${target} PUBLIC
      $<$<CONFIG:Release>:-O3 -flto -xCORE-AVX2>
    )
  endif()

  # ── OpenMP / MPI / MKL オプション ──
  if(ENABLE_OPENMP)
    find_package(OpenMP REQUIRED)
    target_link_libraries(${target} PUBLIC OpenMP::OpenMP_Fortran)
    target_compile_definitions(${target} PUBLIC USE_OPENMP)
  endif()

  if(ENABLE_MPI)
    find_package(MPI REQUIRED)
    target_link_libraries(${target} PUBLIC MPI::MPI_Fortran)
    target_compile_definitions(${target} PUBLIC USE_MPI)
  endif()

  if(ENABLE_MKL)
    # 有効なMKLオプションに修正
    set(MKL_INTERFACE_FULL intel_lp64 CACHE STRING "Use 32-bit integers for MKL")
    set(MKL_LINK static CACHE STRING "Static MKL linking")
    set(MKL_THREADING intel_thread CACHE STRING "Use Intel OpenMP threading")
    set(MKL_MPI intelmpi CACHE STRING "Use Intel MPI")

    find_package(MKL CONFIG REQUIRED PATHS $ENV{MKLROOT})

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



# サブルーチン：ターゲットにサードパーティのライブラリをリンク・インクルード
function(enable_thirdparty target)
  # Include paths for Json-Fortran / VTKFortran
  target_include_directories(${target} PUBLIC
    /workspaces/FTDSS/include/Json-Fortran
    /workspaces/FTDSS/include/VTKFortran
  )

  # Include path for fortran_stdlib
  target_include_directories(${target} PUBLIC
    /workspaces/FTDSS/EXTERNAL/fortran_stdlib/include/fortran_stdlib/IntelLLVM-2025.0.4
  )

  # 明示的にリンク（名前付きで）
    find_package(fortran_stdlib REQUIRED
    PATHS /workspaces/FTDSS/EXTERNAL/fortran_stdlib/lib/cmake/fortran_stdlib
    )

  target_link_libraries(${target} PUBLIC fortran_stdlib::fortran_stdlib)

  # プロジェクト内静的ライブラリを追加
  file(GLOB_RECURSE MY_ALL_LIBRARIES /workspaces/FTDSS/lib/*.a)
  target_link_libraries(${target} PUBLIC ${MY_ALL_LIBRARIES})
endfunction()
