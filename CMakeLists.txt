cmake_minimum_required(VERSION 3.20)

# ====================================================================
# 1. Options and Toolchain Setup
# ====================================================================

# 1. ビルドするアプリケーションを選択 ('ftdss' または 'test')
set(BUILD_APP "ftdss" CACHE STRING "Select the application to build: ftdss or test")

# 2. 'test' をビルドする場合、どのテストを実行するか指定
set(TEST_NAME "input" CACHE STRING "Specify the test name (e.g., input, domain)")

# オプション定義
option(ENABLE_OPENMP "Enable OpenMP support" ON)
option(ENABLE_MKL    "Enable MKL support"    ON)
option(ENABLE_MPI    "Enable MPI support"    ON)
option(ENABLE_DEBUG  "Enable debug flags"    ON)

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

project(FTDSS LANGUAGES Fortran C CXX)
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

# Include custom CMake modules if exists
list(APPEND CMAKE_MODULE_PATH "${CMAKE_SOURCE_DIR}/cmake")
include(Toolchain)

# ====================================================================
# 2. Module Dependencies Definition (構成定義)
# ====================================================================

# ★ IOモジュールの管理
# output モジュールを有効化
# set(MODULES_IO input output) 
set(MODULES_IO input) 

# 各テスト・モードが必要とするモジュール構成
# 依存順序を気にする必要はありません（後のALL_MODULES順でaddされます）
set(DEPS_core       core)
set(DEPS_input      core input)
# set(DEPS_output     core output)

set(DEPS_linalg     core linalg)
set(DEPS_physics    core physics)
set(DEPS_solver     core linalg solver)
set(DEPS_control    core input linalg control)
set(DEPS_conditions core input linalg control conditions)
set(DEPS_domain     core input linalg control conditions domain)
set(DEPS_field      core input linalg control conditions domain field)
set(DEPS_parallel   core input parallel)
set(DEPS_main       core ${MODULES_IO} linalg control conditions domain physics parallel solver field thermal hydraulic ftdss)

# アプリケーション本体用
set(DEPS_ftdss     ${DEPS_main}) 

# ====================================================================
# 3. Resolve Required Modules (ビルド対象の決定)
# ====================================================================

set(REQUIRED_MODULES "")

if(BUILD_APP STREQUAL "ftdss")
    message(STATUS "Build mode: Main application (ftdss)")
    list(APPEND REQUIRED_MODULES ${DEPS_ftdss})

elseif(BUILD_APP STREQUAL "test")
    message(STATUS "Build mode: Test (selected test: ${TEST_NAME})")
    
    if(DEFINED DEPS_${TEST_NAME})
        list(APPEND REQUIRED_MODULES ${DEPS_${TEST_NAME}})
    else()
        message(FATAL_ERROR "Unknown TEST_NAME: ${TEST_NAME}. Please define DEPS_${TEST_NAME} in CMakeLists.txt.")
    endif()
    
else()
    message(FATAL_ERROR "Invalid BUILD_APP value: ${BUILD_APP}. Choose 'ftdss' or 'test'.")
endif()

# 重複削除
list(REMOVE_DUPLICATES REQUIRED_MODULES)


# ====================================================================
# 4. Source Tree Definition (サブディレクトリ追加)
# ====================================================================

# ★ディレクトリパスのカスタム設定
# 標準の "src/モジュール名" 以外の場所にある場合はここで定義します
set(PATH_input      "src/inout/input")
# set(PATH_output     "src/inout/output")
set(PATH_thermal    "src/main/thermal")
set(PATH_hydraulic  "src/main/hydraulic")
set(PATH_ftdss      "src/main/ftdss")

# 指定されたモジュールが REQUIRED_MODULES に含まれる場合のみ追加する関数
function(add_required_subdirectory module_name)
    list(FIND REQUIRED_MODULES "${module_name}" _index)
    if(NOT _index EQUAL -1)
        message(STATUS "Including module: ${module_name}")
        
        # カスタムパスが定義されているか確認
        if(DEFINED PATH_${module_name})
            add_subdirectory("${PATH_${module_name}}" "${CMAKE_BINARY_DIR}/src/${module_name}")
        else()
            add_subdirectory("src/${module_name}")
        endif()
        
    else()
        # message(STATUS "Skipping module: ${module_name}")
    endif()
endfunction()

# ★ 全モジュールのリスト
set(ALL_MODULES 
    core 
    input 
    # output
    linalg 
    control 
    conditions 
    domain 
    physics 
    parallel 
    solver 
    field 
    thermal
    hydraulic
    ftdss
)

foreach(mod ${ALL_MODULES})
    add_required_subdirectory(${mod})
endforeach()

# ====================================================================
# 5. Build Targets (メインアプリまたはテスト)
# ====================================================================

if(BUILD_APP STREQUAL "ftdss")
    # --- アプリケーションビルド ---
    # パスを src/main/ftdss/ftdss.F90 に変更
    add_executable(ftdss src/main/ftdss/ftdss.F90)
    
    # 依存ライブラリのリンク自動化
    foreach(mod ${DEPS_ftdss})
        string(SUBSTRING ${mod} 0 1 FIRST_CHAR)
        string(TOUPPER ${FIRST_CHAR} FIRST_CHAR)
        string(SUBSTRING ${mod} 1 -1 REMAINDER)
        target_link_libraries(ftdss PRIVATE "Solver::${FIRST_CHAR}${REMAINDER}")
    endforeach()

    # run ターゲット設定
    if(ENABLE_MPI)
        set(RUN_CMD mpirun -np 4)
        set(RUN_ENV "")
    else()
        set(RUN_CMD "")
        set(RUN_ENV "OMP_NUM_THREADS=4")
    endif()
    
    add_custom_target(run
        COMMAND ${RUN_ENV} ${RUN_CMD} $<TARGET_FILE:ftdss>
        DEPENDS ftdss
        WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
    )

elseif(BUILD_APP STREQUAL "test")
    # --- テストビルド ---
    add_subdirectory(test)
endif()