# ====================================================================
# Helper Function: Create Solver Test
# ====================================================================
function(create_solver_test test_name dependencies)
    set(target_name "test_${test_name}")

    if(NOT EXISTS "${CMAKE_CURRENT_SOURCE_DIR}/${target_name}.F90")
        message(FATAL_ERROR "Test source file not found: ${target_name}.F90")
    endif()

    # 実行ファイル作成
    add_executable(${target_name} ${target_name}.F90)
    set_target_properties(${target_name} PROPERTIES LINKER_LANGUAGE Fortran)

    # 依存ライブラリのリンク自動化 (例: core -> Solver::Core)
    foreach(dep ${dependencies})
        string(SUBSTRING ${dep} 0 1 FIRST_CHAR)
        string(TOUPPER ${FIRST_CHAR} FIRST_CHAR)
        string(SUBSTRING ${dep} 1 -1 REMAINDER)
        target_link_libraries(${target_name} PRIVATE "Solver::${FIRST_CHAR}${REMAINDER}")
    endforeach()

    # 個別のビルドフラグや設定が必要な場合のフック (physics等)
    if(test_name STREQUAL "physics")
        if(COMMAND enable_build_flags)
             enable_build_flags(${target_name})
        endif()
        if(COMMAND enable_thirdparty)
             enable_thirdparty(${target_name})
        endif()
    endif()

    # CTest登録
    add_test(NAME ${target_name} COMMAND $<TARGET_FILE:${target_name}>)

    # カスタム実行ターゲット (run_xxx)
    if(ENABLE_MPI)
        add_custom_target(run_${test_name}
            COMMAND mpirun -np 4 $<TARGET_FILE:${target_name}> >| log/output_${test_name}.md
            DEPENDS ${target_name}
            WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
        )
    else()
        add_custom_target(run_${test_name}
            COMMAND OMP_NUM_THREADS=4 $<TARGET_FILE:${target_name}> >| log/output_${test_name}.md
            DEPENDS ${target_name}
            WORKING_DIRECTORY ${PROJECT_SOURCE_DIR}
        )
    endif()

endfunction()

# ====================================================================
# Define Test Target
# ====================================================================

# 親のCMakeLists.txtで定義された依存関係リスト(DEPS_xxx)を利用してテストを生成
if(DEFINED DEPS_${TEST_NAME})
    message(STATUS "Creating test target: test_${TEST_NAME}")
    create_solver_test(${TEST_NAME} "${DEPS_${TEST_NAME}}")
else()
    message(WARNING "No dependencies defined for test: ${TEST_NAME}. Test target skipped.")
endif()