cmake_minimum_required(VERSION 3.20)

# ====================================================================
# 1. Options and Toolchain Setup
# ====================================================================

# --------------------------------------------------------------------
# ★ ビルド目的を決定する変数
# --------------------------------------------------------------------
# 1. ビルドするアプリケーションを選択 ('ysy_fc' または 'test')
set(BUILD_APP "ysy_fc" CACHE STRING "Select the application to build: ysy_fc or test")

# 2. 'test' をビルドする場合、どのテストを実行するか指定
set(TEST_NAME "input" CACHE STRING "Specify the test name (e.g., input, domain)")
# --------------------------------------------------------------------

# その他のオプション
option(ENABLE_OPENMP "Enable OpenMP support" ON)
option(ENABLE_MKL    "Enable MKL support"    ON)
option(ENABLE_MPI    "Enable MPI support"    ON)
option(ENABLE_DEBUG  "Enable debug flags"    OFF)

# ビルドするモジュールのリストを準備
set(REQUIRED_MODULES "")

if(BUILD_APP STREQUAL "ysy_fc")
    # --- アプリケーションビルドモード ---
    message(STATUS "Build mode: Main application (ysy_fc)")
    list(APPEND REQUIRED_MODULES "core" "inout" "linalg" "control" "conditions" "domain" "physics" "parallel" "solver" "field")

elseif(BUILD_APP STREQUAL "test")
    # --- テストビルドモード ---
    message(STATUS "Build mode: Test (selected test: ${TEST_NAME})")

    if(TEST_NAME STREQUAL "input")
        list(APPEND REQUIRED_MODULES "core" "inout")
    elseif(TEST_NAME STREQUAL "control")
        list(APPEND REQUIRED_MODULES "core" "inout" "linalg" "control")
    elseif(TEST_NAME STREQUAL "domain")
        list(APPEND REQUIRED_MODULES "core" "inout" "linalg" "control" "conditions" "domain")
    
    else()
        message(WARNING "Unknown TEST_NAME: ${TEST_NAME}. No specific modules required.")
    endif()

else()
    message(FATAL_ERROR "Invalid BUILD_APP value: ${BUILD_APP}. Choose 'ysy_fc' or 'test'.")
endif()


# コンパイラ設定
if(ENABLE_MPI)
    message(STATUS "MPI support is ON. Setting compiler to MPI wrappers.")
    set(CMAKE_Fortran_COMPILER mpiifx CACHE STRING "MPI Fortran Compiler" FORCE)
    set(CMAKE_CXX_COMPILER   mpiicpx CACHE STRING "MPI CXX Compiler" FORCE)
    set(CMAKE_C_COMPILER     mpiicx  CACHE STRING "MPI C Compiler" FORCE)
else()
    message(STATUS "MPI support is OFF. Setting compiler to standard Intel compilers.")
    set(CMAKE_Fortran_COMPILER ifx CACHE STRING "Fortran Compiler" FORCE)
    set(CMAKE_CXX_COMPILER   icpx CACHE STRING "CXX Compiler" FORCE)
    set(CMAKE_C_COMPILER     icx  CACHE STRING "C Compiler" FORCE)
endif()

# Project Name and Language
project(YSY_FC LANGUAGES Fortran C CXX)

# CTestを有効化
enable_testing()

# Build Type
if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build: Debug or Release" FORCE)
endif()
message(STATUS "Build type: ${CMAKE_BUILD_TYPE}")

# Output Directories
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/.mod)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib)

# Include custom CMake modules
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
include(Toolchain)


# ====================================================================
# 2. Source Tree Definition
# ====================================================================

# REQUIRED_MODULESリストに含まれているモジュールだけをビルド対象にする関数
function(add_required_subdirectory module_name)
    list(FIND REQUIRED_MODULES "${module_name}" _index)
    if(NOT _index EQUAL -1)
        message(STATUS "Including module: ${module_name}")
        add_subdirectory("src/${module_name}")
    else()
        message(STATUS "Skipping module: ${module_name}")
    endif()
endfunction()

# この関数を使って各モジュールを追加
add_required_subdirectory(core)
add_required_subdirectory(inout)
add_required_subdirectory(linalg)
add_required_subdirectory(control)
add_required_subdirectory(conditions)
add_required_subdirectory(domain)
add_required_subdirectory(physics)
add_required_subdirectory(parallel)
add_required_subdirectory(solver)
add_required_subdirectory(field)


# ====================================================================
# 3. Build Targets
# ====================================================================

if(BUILD_APP STREQUAL "ysy_fc")
    # アプリケーションをビルド
    add_executable(ysy_fc app/ysy_fc.F90)
    
    target_link_libraries(ysy_fc PRIVATE
        Solver::Core
        Solver::Input
        # ... アプリケーションに必要な全てのライブラリ ...
    )

elseif(BUILD_APP STREQUAL "test")
    add_subdirectory(test)
endif()


# ====================================================================
# 4. Custom Targets
# ====================================================================
if(BUILD_APP STREQUAL "ysy_fc" AND TARGET ysy_fc)
    # --- メインアプリ実行 ---
    if(ENABLE_MPI)
        add_custom_target(run
            COMMAND mpirun -np 4 $<TARGET_FILE:ysy_fc>
            DEPENDS ysy_fc
            WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
        )
    else()
        add_custom_target(run
            COMMAND OMP_NUM_THREADS=4 $<TARGET_FILE:ysy_fc>
            DEPENDS ysy_fc
            WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
        )
    endif()
elseif(BUILD_APP STREQUAL "test")
    # --- テスト実行 ---
    set(TEST_EXE "test_${TEST_NAME}")
    if(TARGET ${TEST_EXE})
        if(ENABLE_MPI)
            add_custom_target(run_${TEST_NAME}
                COMMAND mpirun -np 4 $<TARGET_FILE:${TEST_EXE}> >| log/output_${TEST_NAME}.md
                DEPENDS ${TEST_EXE}
                WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
            )
        else()
            add_custom_target(run_${TEST_NAME}
                COMMAND OMP_NUM_THREADS=4 $<TARGET_FILE:${TEST_EXE}> >| log/output_${TEST_NAME}.md
                DEPENDS ${TEST_EXE}
                WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
            )
        endif()
    else()
        message(WARNING "Test target '${TEST_EXE}' not found. No run target created.")
    endif()
endif()