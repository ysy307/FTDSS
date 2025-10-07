# test/CMakeLists.txt

# 現在選択されているテスト名(${TEST_NAME})に応じて、ビルドするテストを決定する

if(TEST_NAME STREQUAL "input")
    # test_input実行ファイルを作成
    add_executable(test_input test_input.F90)
    set_target_properties(test_input PROPERTIES LINKER_LANGUAGE Fortran)
    # 必要なライブラリをリンク
    target_link_libraries(test_input PRIVATE
        Solver::Core
        Solver::Input
    )

    # CTestにテストとして登録
    add_test(NAME test_input COMMAND $<TARGET_FILE:test_input>)

elseif(TEST_NAME STREQUAL "control")
    add_executable(test_control test_control.F90)
    set_target_properties(test_control PROPERTIES LINKER_LANGUAGE Fortran)
    target_link_libraries(test_control PRIVATE
        Solver::Core
        Solver::Input
        Solver::Linalg
        Solver::Control
    )
    add_test(NAME test_control COMMAND $<TARGET_FILE:test_control>)
elseif(TEST_NAME STREQUAL "domain")
    add_executable(test_domain test_domain.F90)
    set_target_properties(test_domain PROPERTIES LINKER_LANGUAGE Fortran)
    target_link_libraries(test_domain PRIVATE
        Solver::Core
        Solver::Input
        Solver::Linalg
        Solver::Control
        Solver::Conditions
        Solver::Domain
    )
    add_test(NAME test_domain COMMAND $<TARGET_FILE:test_domain>)
    
endif()