cmake_minimum_required(VERSION 3.20)

# ====================================================================
# 1. Options and Toolchain Setup
# ====================================================================

set(BUILD_APP "ftdss" CACHE STRING "Select the application to build: ftdss or test")
set(TEST_NAME "input" CACHE STRING "Specify the test name (e.g., input, domain)")

option(ENABLE_OPENMP "Enable OpenMP support" ON)
option(ENABLE_MKL    "Enable MKL support"    ON)
option(ENABLE_MPI    "Enable MPI support"    ON)
option(ENABLE_DEBUG  "Enable debug flags"    ON)

if(ENABLE_MPI)
    message(STATUS "MPI support is ON.")
    set(CMAKE_Fortran_COMPILER mpiifx CACHE STRING "MPI Fortran Compiler" FORCE)
    set(CMAKE_CXX_COMPILER   mpiicpx CACHE STRING "MPI CXX Compiler" FORCE)
    set(CMAKE_C_COMPILER     mpiicx  CACHE STRING "MPI C Compiler" FORCE)
else()
    message(STATUS "MPI support is OFF.")
    set(CMAKE_Fortran_COMPILER ifx CACHE STRING "Fortran Compiler" FORCE)
    set(CMAKE_CXX_COMPILER   icpx CACHE STRING "CXX Compiler" FORCE)
    set(CMAKE_C_COMPILER     icx  CACHE STRING "C Compiler" FORCE)
endif()

project(FTDSS LANGUAGES Fortran C CXX)
enable_testing()

if(NOT CMAKE_BUILD_TYPE)
  set(CMAKE_BUILD_TYPE Release CACHE STRING "Choose the type of build" FORCE)
endif()

set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_BINARY_DIR}/.mod)
set(CMAKE_RUNTIME_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/bin)
set(CMAKE_LIBRARY_OUTPUT_DIRECTORY ${PROJECT_SOURCE_DIR}/lib)

# ★Toolchainの読み込み（修正箇所）
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
include(Toolchain)

# ====================================================================
# 2. Module Dependencies Definition
# ====================================================================

# モジュール依存関係 (論理名)
set(DEPS_core       core)
set(DEPS_input      core input)
set(DEPS_linalg     core linalg)
set(DEPS_physics    core physics)
set(DEPS_solver     core linalg solver)
set(DEPS_control    core input linalg control)
set(DEPS_conditions core input linalg control conditions)
set(DEPS_domain     core input linalg control conditions domain)
set(DEPS_field      core input linalg control conditions domain field)
set(DEPS_parallel   core input parallel)
set(DEPS_output     core input domain physics output)

# アプリケーションライブラリ (src/main以下にあるもの)
set(DEPS_main       core input output linalg control conditions domain physics parallel solver field thermal hydraulic ftdss)
set(DEPS_ftdss     ${DEPS_main}) 

# ====================================================================
# 3. Resolve Required Modules
# ====================================================================

set(REQUIRED_MODULES "")

if(BUILD_APP STREQUAL "ftdss")
    list(APPEND REQUIRED_MODULES ${DEPS_ftdss})
elseif(BUILD_APP STREQUAL "test")
    if(DEFINED DEPS_${TEST_NAME})
        list(APPEND REQUIRED_MODULES ${DEPS_${TEST_NAME}})
    else()
        message(FATAL_ERROR "Unknown TEST_NAME: ${TEST_NAME}")
    endif()
else()
    message(FATAL_ERROR "Invalid BUILD_APP value")
endif()

list(REMOVE_DUPLICATES REQUIRED_MODULES)

# ★ディレクトリ集約ロジック
# thermal, hydraulic, ftdss (ライブラリ) は 'src/main' にまとまっているので
# これらが必要な場合は 'main_libs' (src/main) をビルド対象に加え、個別の名前はリストから外す
if("thermal" IN_LIST REQUIRED_MODULES OR "hydraulic" IN_LIST REQUIRED_MODULES OR "ftdss" IN_LIST REQUIRED_MODULES)
    list(APPEND REQUIRED_MODULES main_libs)
    list(REMOVE_ITEM REQUIRED_MODULES thermal hydraulic ftdss)
endif()
list(REMOVE_DUPLICATES REQUIRED_MODULES)

# ====================================================================
# 4. Source Tree Definition
# ====================================================================

# ★ディレクトリ構造へのマッピング
set(PATH_input      "src/inout/input")   # input は src/inout/input
set(PATH_output     "src/inout/output")  # output は src/inout/output
set(PATH_main_libs  "src/main")          # thermal/hydraulic/ftdss は src/main

function(add_required_subdirectory module_name)
    list(FIND REQUIRED_MODULES "${module_name}" _index)
    if(NOT _index EQUAL -1)
        if(DEFINED PATH_${module_name})
            add_subdirectory("${PATH_${module_name}}" "${CMAKE_BINARY_DIR}/src/${module_name}")
        else()
            add_subdirectory("src/${module_name}")
        endif()
    endif()
endfunction()

# ビルド順序
set(ALL_MODULES 
    core 
    input 
    output
    linalg 
    control 
    conditions 
    domain 
    physics 
    parallel 
    solver 
    field 
    main_libs 
)

foreach(mod ${ALL_MODULES})
    add_required_subdirectory(${mod})
endforeach()

# ====================================================================
# 5. Build Targets
# ====================================================================

if(BUILD_APP STREQUAL "ftdss")
    # 実行ファイルのエントリーポイントは app/ftdss.f90
    add_executable(ftdss app/ftdss.f90)
    
    # 依存ライブラリのリンク
    foreach(mod ${DEPS_ftdss})
        string(SUBSTRING ${mod} 0 1 FIRST_CHAR)
        string(TOUPPER ${FIRST_CHAR} FIRST_CHAR)
        string(SUBSTRING ${mod} 1 -1 REMAINDER)
        
        if(TARGET "Solver::${FIRST_CHAR}${REMAINDER}")
            target_link_libraries(ftdss PRIVATE "Solver::${FIRST_CHAR}${REMAINDER}")
        endif()
    endforeach()

    # Runターゲット
    if(ENABLE_MPI)
        add_custom_target(run COMMAND mpirun -np 4 $<TARGET_FILE:ftdss> DEPENDS ftdss WORKING_DIRECTORY ${PROJECT_SOURCE_DIR})
    else()
        add_custom_target(run COMMAND ${CMAKE_COMMAND} -E env OMP_NUM_THREADS=4 $<TARGET_FILE:ftdss> DEPENDS ftdss WORKING_DIRECTORY ${PROJECT_SOURCE_DIR})
    endif()

elseif(BUILD_APP STREQUAL "test")
    add_subdirectory(test)
endif()