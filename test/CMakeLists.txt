# ====================================================================
# Test Target: test_main
# ====================================================================
set(target_name "test_main")

if(NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${target_name}.F90")
    message(FATAL_ERROR "Test source file not found: ${target_name}.F90")
endif()

add_executable(${target_name} ${target_name}.F90)
set_target_properties(${target_name} PROPERTIES LINKER_LANGUAGE Fortran)

# 定義済みのエイリアスをリンクする
target_link_libraries(${target_name} PRIVATE 
    SOLVER::CORE
    SOLVER::LINALG
    SOLVER::PARALLEL
    SOLVER::SOLVER
    SOLVER::SYSTEM
    SOLVER::DOMAIN
    SOLVER::INPUT
    SOLVER::OUTPUT
    SOLVER::CONSTITUTIVE
    SOLVER::GOVERNING
    SOLVER::FTDSS
)

# CTest登録
add_test(NAME ${target_name} COMMAND $<TARGET_FILE:${target_name}>)

# ログ出力先ディレクトリの確保
file(MAKE_DIRECTORY ${PROJECT_SOURCE_DIR}/log)

# カスタム実行ターゲット (run_main)
if(ENABLE_MPI AND MPI_Fortran_FOUND)
    add_custom_target(run_main
        COMMAND sh -c "${MPIEXEC_EXECUTABLE} ${MPIEXEC_NUMPROC_FLAG} 4 $<TARGET_FILE:${target_name}> > ${PROJECT_SOURCE_DIR}/log/results.md"
        DEPENDS ${target_name}
        WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
    )
else()
    add_custom_target(run_main
        COMMAND sh -c "env OMP_NUM_THREADS=4 $<TARGET_FILE:${target_name}> > ${PROJECT_SOURCE_DIR}/log/results.md"
        DEPENDS ${target_name}
        WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
    )
endif()