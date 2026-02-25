cmake_minimum_required(VERSION 3.20)

# ====================================================================
# 1. Project Setup
# ====================================================================
project(FTDSS LANGUAGES Fortran C CXX)

option(BUILD_TESTING "Build the testing tree" OFF)
option(ENABLE_OPENMP "Enable OpenMP support" ON)
option(ENABLE_MKL    "Enable MKL support" ON)
option(ENABLE_MPI    "Enable MPI support" ON)

# 出力先ディレクトリを統一
set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/modules)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib)
set(CMAKE_ARCHIVE_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib)

# カスタムモジュールの読み込み
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
include(Toolchain OPTIONAL)
include(fypp OPTIONAL)

# ====================================================================
# 2. Dependencies
# ====================================================================
# if(ENABLE_MPI)
#     find_package(MPI REQUIRED)
# endif()

# if(ENABLE_OPENMP)
#     find_package(OpenMP REQUIRED)
# endif()

# ====================================================================
# 3. Source Tree (Libraries)
# ====================================================================
# 各サブディレクトリ内の CMakeLists.txt で target_link_libraries() を用い，
# 個別に依存関係を定義することで，CMakeが自動的にビルド順序を解決する．
add_subdirectory(src/core)
add_subdirectory(src/numerical)
add_subdirectory(src/control)
add_subdirectory(src/domain) 
add_subdirectory(src/io)      
add_subdirectory(src/physics)   
add_subdirectory(src/app/ftdss)
# ====================================================================
# 4. Executable
# ====================================================================
# プロジェクト直下の app ディレクトリにあるエントリーポイント
add_executable(ftdss app/ftdss.f90)

# 実行ファイルに必要な最上位ライブラリターゲットをリンクする（名称は仮）
# 必要なフラグ（インクルードパスやOpenMP等）もターゲット経由で自動伝搬する
target_link_libraries(ftdss PRIVATE SOLVER::FTDSS)

if(ENABLE_MPI AND MPI_Fortran_FOUND)
    target_link_libraries(ftdss PRIVATE MPI::MPI_Fortran)
    add_custom_target(run 
        COMMAND ${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 4 $<TARGET_FILE:ftdss>
        DEPENDS ftdss
        WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
    )
else()
    add_custom_target(run 
        COMMAND ${CMAKE_COMMAND} -E env OMP_NUM_THREADS=4 $<TARGET_FILE:ftdss>
        DEPENDS ftdss
        WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
    )
endif()

# ====================================================================
# 5. Testing
# ====================================================================
if(BUILD_TESTING)
    enable_testing()
    add_subdirectory(test)
endif()